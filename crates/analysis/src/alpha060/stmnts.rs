use std::collections::HashMap;
use std::sync::atomic::{
    AtomicUsize,
    Ordering,
};

use crate::files::{
    FileVersion,
    Files,
};
use crate::types::{
    make_union_type,
    matches_type,
    GenericsMap,
};
use crate::{
    get_symbol_definition_info,
    insert_symbol_definition,
    insert_symbol_reference,
    BlockContext,
    Context,
    DataType,
    SymbolInfo,
    SymbolLocation,
    SymbolType,
    VariableSymbol,
};
use amber_grammar::alpha060::{
    Block,
    Comment,
    ElseCondition,
    Expression,
    FailableHandler,
    FailureHandler,
    IfChainContent,
    IfCondition,
    IterLoopVars,
    Statement,
    VariableInitType,
};
use amber_grammar::{
    CommandModifier,
    Span,
    Spanned,
};
use amber_types::paths::FileId;

/// Positions at or above this threshold are synthetic shadow definitions
/// created by type narrowing, not real source positions.
pub const SYNTHETIC_THRESHOLD: usize = usize::MAX / 2;

/// Counter for generating unique synthetic offsets for type narrowing shadow definitions.
static SYNTHETIC_OFFSET: AtomicUsize = AtomicUsize::new(SYNTHETIC_THRESHOLD);

use super::exp::{
    analyze_exp,
    ExpAnalysisResult,
};

#[derive(Debug, Clone)]
pub struct StmntAnalysisResult {
    pub is_propagating_failure: bool,
    pub return_ty: Option<DataType>,
}

/// Report errors for duplicate command modifiers in a modifier list.
pub fn check_duplicate_modifiers(
    modifiers: &[Spanned<CommandModifier>],
    file: &(FileId, FileVersion),
    files: &Files,
) {
    let mut seen = std::collections::HashSet::new();
    for (modifier, span) in modifiers {
        if !seen.insert(modifier) {
            files.report_error(
                file,
                &format!("Duplicate command modifier '{modifier:?}'"),
                *span,
            );
        }
    }
}

/// Returns `true` if any inline or block-context modifier is `Unsafe` or `Trust`.
pub fn has_unsafe_or_trust(modifiers: &[Spanned<CommandModifier>], contexts: &[Context]) -> bool {
    modifiers
        .iter()
        .any(|(m, _)| *m == CommandModifier::Unsafe || *m == CommandModifier::Trust)
        || contexts.iter().any(|ctx| match ctx {
            Context::Block(BlockContext { modifiers }) => modifiers
                .iter()
                .any(|m| *m == CommandModifier::Unsafe || *m == CommandModifier::Trust),
            _ => false,
        })
}

/// Returns `true` if any inline or block-context modifier is `Trust`.
pub fn has_trust(modifiers: &[Spanned<CommandModifier>], contexts: &[Context]) -> bool {
    modifiers.iter().any(|(m, _)| *m == CommandModifier::Trust)
        || contexts.iter().any(|ctx| match ctx {
            Context::Block(BlockContext { modifiers }) => {
                modifiers.contains(&CommandModifier::Trust)
            }
            _ => false,
        })
}

/// Returns `true` if the failable handler list contains at least one non-comment handler.
pub fn has_failure_handler(failable_handlers: &[Spanned<FailableHandler>]) -> bool {
    failable_handlers
        .iter()
        .any(|(h, _)| !matches!(h, FailableHandler::Comment(_)))
}

/// A type narrowing: either a positive inclusion or a negative exclusion.
#[derive(Debug, Clone)]
enum Narrowing {
    /// Variable is narrowed to exactly this type (e.g. `a is Int` → `Include(Int)`).
    Include(DataType),
    /// Variable should have these types excluded (e.g. `not (a is Int)` → `Exclude([Int])`).
    Exclude(Vec<DataType>),
}

/// Extract type narrowings from a condition expression.
/// Returns a map of variable name → narrowing when the condition is TRUE.
///
/// For `a is Int`: narrows `a` to `Int`.
/// For `not (a is Int)`: excludes `Int` from `a`.
/// For `a is Int and b is Text`: narrows both `a` to `Int` and `b` to `Text`.
/// For `a is Int or a is Bool`: narrows `a` to `Int | Bool`.
fn extract_narrowings(expr: &Expression) -> HashMap<String, Narrowing> {
    match expr {
        Expression::Is(inner_expr, _, (data_type, _)) => {
            if let Expression::Var((name, _)) = &inner_expr.0 {
                if !matches!(data_type, DataType::Error) {
                    let mut map = HashMap::new();
                    map.insert(name.clone(), Narrowing::Include(data_type.clone()));
                    return map;
                }
            }
            HashMap::new()
        }
        Expression::Not(_, inner) => {
            // `not expr` flips narrowings to exclusions and vice versa
            // When `not expr` is TRUE, `expr` is FALSE → use exclusions from inner
            let inner_exclusions = extract_exclusions(&inner.0);
            let mut result = HashMap::new();
            for (name, narrowing) in inner_exclusions {
                // Flip: exclusions become inclusions, inclusions become exclusions
                match narrowing {
                    Narrowing::Exclude(types) => {
                        result.insert(name, Narrowing::Include(make_union_type(types)));
                    }
                    Narrowing::Include(ty) => {
                        result.insert(name, Narrowing::Exclude(vec![ty]));
                    }
                }
            }
            // Also convert inner narrowings into exclusions
            let inner_narrowings = extract_narrowings(&inner.0);
            for (name, narrowing) in inner_narrowings {
                match narrowing {
                    Narrowing::Include(ty) => {
                        result.insert(name, Narrowing::Exclude(vec![ty]));
                    }
                    Narrowing::Exclude(types) => {
                        result.insert(name, Narrowing::Include(make_union_type(types)));
                    }
                }
            }
            result
        }
        Expression::And(lhs, _, rhs) => {
            let mut narrowings = extract_narrowings(&lhs.0);
            let rhs_narrowings = extract_narrowings(&rhs.0);
            for (name, narrowing) in rhs_narrowings {
                narrowings
                    .entry(name)
                    .and_modify(|existing| match (existing.clone(), &narrowing) {
                        (Narrowing::Exclude(mut a), Narrowing::Exclude(b)) => {
                            a.extend(b.iter().cloned());
                            *existing = Narrowing::Exclude(a);
                        }
                        _ => {
                            *existing = narrowing.clone();
                        }
                    })
                    .or_insert(narrowing);
            }
            narrowings
        }
        Expression::Or(lhs, _, rhs) => {
            let lhs_narrowings = extract_narrowings(&lhs.0);
            let rhs_narrowings = extract_narrowings(&rhs.0);
            let mut result = HashMap::new();
            for (name, lhs_n) in &lhs_narrowings {
                if let Some(rhs_n) = rhs_narrowings.get(name) {
                    match (lhs_n, rhs_n) {
                        (Narrowing::Include(lhs_ty), Narrowing::Include(rhs_ty)) => {
                            let union_ty = make_union_type(vec![lhs_ty.clone(), rhs_ty.clone()]);
                            result.insert(name.clone(), Narrowing::Include(union_ty));
                        }
                        (Narrowing::Exclude(a), Narrowing::Exclude(b)) => {
                            // Intersection of exclusions: only exclude types in both
                            let common: Vec<DataType> =
                                a.iter().filter(|ty| b.contains(ty)).cloned().collect();
                            if !common.is_empty() {
                                result.insert(name.clone(), Narrowing::Exclude(common));
                            }
                        }
                        _ => {}
                    }
                }
            }
            result
        }
        Expression::Parentheses(inner) => extract_narrowings(&inner.0),
        _ => HashMap::new(),
    }
}

/// Combine two narrowings under AND semantics (both must hold simultaneously).
fn combine_narrowings_and(a: &Narrowing, b: &Narrowing) -> Narrowing {
    match (a, b) {
        (Narrowing::Exclude(a_types), Narrowing::Exclude(b_types)) => {
            let mut combined = a_types.clone();
            combined.extend(b_types.iter().cloned());
            Narrowing::Exclude(combined)
        }
        (Narrowing::Include(ty), Narrowing::Exclude(excluded))
        | (Narrowing::Exclude(excluded), Narrowing::Include(ty)) => {
            Narrowing::Include(subtract_types(ty, excluded))
        }
        (Narrowing::Include(_), Narrowing::Include(b_ty)) => Narrowing::Include(b_ty.clone()),
    }
}

/// Extract type narrowings from a condition expression for when the condition is FALSE.
/// Returns a map of variable name → narrowing to apply in the else branch or
/// after an early-return if-block.
///
/// For `a is Int`: excludes `Int` from `a`.
/// For `not (a is Int)`: includes `Int` for `a` (since `not (a is Int)` being false means `a is Int`).
/// For `a is Int or a is Bool`: excludes both `Int` and `Bool` from `a`.
/// For `a is Int and b is Text`: can't exclude anything (De Morgan's law).
fn extract_exclusions(expr: &Expression) -> HashMap<String, Narrowing> {
    match expr {
        Expression::Is(inner_expr, _, (data_type, _)) => {
            if let Expression::Var((name, _)) = &inner_expr.0 {
                if !matches!(data_type, DataType::Error) {
                    let mut map = HashMap::new();
                    map.insert(name.clone(), Narrowing::Exclude(vec![data_type.clone()]));
                    return map;
                }
            }
            HashMap::new()
        }
        Expression::Not(_, inner) => {
            // not(expr) is FALSE → expr is TRUE → use the narrowings of the inner
            extract_narrowings(&inner.0)
        }
        Expression::Or(lhs, _, rhs) => {
            // NOT (A or B) = NOT A AND NOT B
            let mut result = extract_exclusions(&lhs.0);
            let rhs_exclusions = extract_exclusions(&rhs.0);
            for (name, narrowing) in rhs_exclusions {
                result
                    .entry(name)
                    .and_modify(|existing| {
                        *existing = combine_narrowings_and(existing, &narrowing);
                    })
                    .or_insert(narrowing);
            }
            result
        }
        Expression::And(lhs, _, rhs) => {
            // NOT (A and B) = NOT A OR NOT B — can't exclude individually in general
            // But if both sides affect the same variable, we can combine
            let lhs_exclusions = extract_exclusions(&lhs.0);
            let rhs_exclusions = extract_exclusions(&rhs.0);
            let mut result = HashMap::new();
            for (name, lhs_n) in &lhs_exclusions {
                if let Some(rhs_n) = rhs_exclusions.get(name) {
                    if let (Narrowing::Exclude(a), Narrowing::Exclude(b)) = (lhs_n, rhs_n) {
                        let mut combined = a.clone();
                        combined.extend(b.iter().cloned());
                        result.insert(name.clone(), Narrowing::Exclude(combined));
                    }
                }
            }
            result
        }
        Expression::Parentheses(inner) => extract_exclusions(&inner.0),
        _ => HashMap::new(),
    }
}

/// Subtract excluded types from a union type.
fn subtract_types(original: &DataType, excluded: &[DataType]) -> DataType {
    match original {
        DataType::Union(types) => {
            let remaining: Vec<DataType> = types
                .iter()
                .filter(|ty| !excluded.contains(ty))
                .cloned()
                .collect();
            if remaining.is_empty() {
                original.clone()
            } else {
                make_union_type(remaining)
            }
        }
        _ => {
            if excluded.contains(original) {
                DataType::Null
            } else {
                original.clone()
            }
        }
    }
}

/// Apply type narrowings to a block by inserting shadow variable definitions.
fn apply_type_narrowings(
    file_id: FileId,
    file_version: FileVersion,
    narrowings: &HashMap<String, Narrowing>,
    block_span: Span,
    files: &Files,
    contexts: &[Context],
) {
    if narrowings.is_empty() {
        return;
    }

    let file = (file_id, file_version);

    for (var_name, narrowing) in narrowings {
        // Look up the original definition to get symbol info
        let original_info = get_symbol_definition_info(files, var_name, &file, block_span.start);

        if let Some(original_info) = original_info {
            let narrowed_type = match narrowing {
                Narrowing::Include(ty) => {
                    if *ty == original_info.data_type {
                        continue;
                    }
                    ty.clone()
                }
                Narrowing::Exclude(excluded) => {
                    let result = subtract_types(&original_info.data_type, excluded);
                    if result == original_info.data_type {
                        continue;
                    }
                    result
                }
            };

            let synthetic_pos = SYNTHETIC_OFFSET.fetch_add(1, Ordering::SeqCst);
            let synthetic_span = Span::from(synthetic_pos..synthetic_pos + 1);

            let narrowed_info = SymbolInfo {
                name: var_name.clone(),
                symbol_type: original_info.symbol_type.clone(),
                data_type: narrowed_type,
                is_definition: true,
                undefined: false,
                span: synthetic_span,
                contexts: contexts.to_vec(),
            };

            let mut symbol_table = files.symbol_table.entry(file).or_default();

            symbol_table
                .symbols
                .insert(synthetic_pos..=synthetic_pos, narrowed_info);

            let defs = symbol_table
                .definitions
                .entry(var_name.clone())
                .or_default();
            defs.insert(
                block_span.start..=block_span.end,
                SymbolLocation {
                    file,
                    start: synthetic_pos,
                    end: synthetic_pos,
                },
            );
        }
    }
}

/// Analyze a statement.
///
/// Returns the data type of the return statement and a boolean indicating if the
/// statement is propagating a failure.
#[tracing::instrument(skip_all)]
pub fn analyze_stmnt(
    file_id: FileId,
    file_version: FileVersion,
    (stmnt, span): &Spanned<Statement>,
    files: &Files,
    scope_end: usize,
    scoped_generic_types: &GenericsMap,
    contexts: &mut Vec<Context>,
) -> StmntAnalysisResult {
    let file = (file_id, file_version);

    match stmnt {
        Statement::Block(block) => analyze_block(
            file_id,
            file_version,
            block,
            files,
            scoped_generic_types,
            contexts,
        ),
        Statement::IfChain(_, if_chain) => {
            let mut stmnts = vec![];
            let mut exps = vec![];
            let mut all_exclusions: HashMap<String, Narrowing> = HashMap::new();

            for (if_chain_content, _) in if_chain.iter() {
                match if_chain_content {
                    IfChainContent::IfCondition((condition, _)) => match condition {
                        IfCondition::IfCondition(exp, block) => {
                            let exp_result = analyze_exp(
                                file_id,
                                file_version,
                                exp,
                                DataType::Boolean,
                                files,
                                scoped_generic_types,
                                contexts,
                            );

                            let narrowings = extract_narrowings(&exp.0);
                            apply_type_narrowings(
                                file_id,
                                file_version,
                                &narrowings,
                                block.1,
                                files,
                                contexts,
                            );

                            let stmnt = analyze_block(
                                file_id,
                                file_version,
                                block,
                                files,
                                scoped_generic_types,
                                contexts,
                            );

                            let exclusions = extract_exclusions(&exp.0);
                            for (name, narrowing) in exclusions {
                                all_exclusions
                                    .entry(name)
                                    .and_modify(|existing| {
                                        *existing = combine_narrowings_and(existing, &narrowing);
                                    })
                                    .or_insert(narrowing);
                            }

                            exps.push(exp_result);
                            stmnts.push(stmnt);
                        }
                        IfCondition::Error => {}
                        IfCondition::Comment((Comment::DocString(docs), _)) => {
                            let _ = handle_doc_strings(docs, contexts);
                        }
                        IfCondition::Comment(_) => {}
                    },
                    IfChainContent::Else((else_cond, _)) => match else_cond {
                        ElseCondition::Else(_, block) => {
                            // Compute narrowings for else branch from accumulated exclusions
                            if !all_exclusions.is_empty() {
                                apply_type_narrowings(
                                    file_id,
                                    file_version,
                                    &all_exclusions,
                                    block.1,
                                    files,
                                    contexts,
                                );
                            }

                            let stmnt = analyze_block(
                                file_id,
                                file_version,
                                block,
                                files,
                                scoped_generic_types,
                                contexts,
                            );

                            stmnts.push(stmnt);
                        }
                    },
                    IfChainContent::Comment((Comment::DocString(docs), _)) => {
                        let _ = handle_doc_strings(docs, contexts);
                    }
                    IfChainContent::Comment(_) => {}
                }
            }

            get_stmnt_analysis_result(stmnts, exps)
        }
        Statement::IfCondition(_, if_cond, comments, else_cond) => {
            let mut stmnts = vec![];
            let mut exps = vec![];
            let mut condition_exclusions: HashMap<String, Narrowing> = HashMap::new();

            match &if_cond.0 {
                IfCondition::IfCondition(exp, block) => {
                    let exp_result = analyze_exp(
                        file_id,
                        file_version,
                        exp,
                        DataType::Boolean,
                        files,
                        scoped_generic_types,
                        contexts,
                    );

                    let narrowings = extract_narrowings(&exp.0);
                    apply_type_narrowings(
                        file_id,
                        file_version,
                        &narrowings,
                        block.1,
                        files,
                        contexts,
                    );

                    let block = analyze_block(
                        file_id,
                        file_version,
                        block,
                        files,
                        scoped_generic_types,
                        contexts,
                    );

                    condition_exclusions = extract_exclusions(&exp.0);

                    stmnts.push(block);
                    exps.push(exp_result);
                }
                IfCondition::Comment((Comment::DocString(docs), _)) => {
                    let _ = handle_doc_strings(docs, contexts);
                }
                _ => {}
            }

            comments.iter().for_each(|(comment, _)| {
                if let Comment::DocString(docs) = comment {
                    let _ = handle_doc_strings(docs, contexts);
                }
            });

            if let Some(else_cond) = else_cond {
                match &else_cond.0 {
                    ElseCondition::Else(_, block) => {
                        if !condition_exclusions.is_empty() {
                            apply_type_narrowings(
                                file_id,
                                file_version,
                                &condition_exclusions,
                                block.1,
                                files,
                                contexts,
                            );
                        }

                        let block = analyze_block(
                            file_id,
                            file_version,
                            block,
                            files,
                            scoped_generic_types,
                            contexts,
                        );

                        stmnts.push(block);
                    }
                }
            } else if !condition_exclusions.is_empty() {
                // Early-return fall-through: if the if-block terminates and
                // there's no else, apply condition exclusions to subsequent
                // code in the parent scope.
                let if_terminates = match &if_cond.0 {
                    IfCondition::IfCondition(_, block) => block_terminates(&block.0),
                    _ => false,
                };
                if if_terminates {
                    // Start one position after the if-statement to avoid
                    // picking up the synthetic narrowing from the if-block.
                    let fallthrough_start = span.end + 1;
                    if fallthrough_start < scope_end {
                        let fallthrough_span = Span::from(fallthrough_start..scope_end);
                        apply_type_narrowings(
                            file_id,
                            file_version,
                            &condition_exclusions,
                            fallthrough_span,
                            files,
                            contexts,
                        );
                    }
                }
            }

            get_stmnt_analysis_result(stmnts, exps)
        }
        Statement::InfiniteLoop(_, block) => {
            let mut new_contexts = contexts.clone();
            new_contexts.push(Context::Loop);

            analyze_block(
                file_id,
                file_version,
                block,
                files,
                scoped_generic_types,
                &new_contexts,
            )
        }
        Statement::WhileLoop(_, exp, block) => {
            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );

            let mut new_contexts = contexts.clone();
            new_contexts.push(Context::Loop);

            let block_analysis = analyze_block(
                file_id,
                file_version,
                block,
                files,
                scoped_generic_types,
                &new_contexts,
            );

            get_stmnt_analysis_result(vec![block_analysis], vec![exp_analysis])
        }
        Statement::IterLoop(_, (vars, _), _, exp, block) => {
            let block_span = block.1;

            let exp = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Array(Box::new(DataType::Any)),
                files,
                scoped_generic_types,
                contexts,
            );

            // TODO: Check if DataType is generic
            let iter_type = match exp.exp_ty.clone() {
                DataType::Array(ty) => *ty,
                DataType::Failable(ty) => {
                    if let DataType::Array(inner_ty) = *ty {
                        *inner_ty
                    } else {
                        DataType::Any
                    }
                }
                _ => DataType::Any,
            };

            match &vars {
                IterLoopVars::WithIndex((var1, var1_span), (var2, var2_span)) => {
                    let mut symbol_table = files.symbol_table.entry(file).or_default();
                    insert_symbol_definition(
                        &mut symbol_table,
                        &SymbolInfo {
                            name: var1.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol {
                                is_const: false,
                                is_public: false,
                            }),
                            data_type: DataType::Int,
                            is_definition: true,
                            undefined: false,
                            span: *var1_span,
                            contexts: contexts.clone(),
                        },
                        file,
                        block_span.start..=block_span.end,
                        false,
                    );

                    insert_symbol_definition(
                        &mut symbol_table,
                        &SymbolInfo {
                            name: var2.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol {
                                is_const: false,
                                is_public: false,
                            }),
                            data_type: iter_type,
                            is_definition: true,
                            undefined: false,
                            span: *var2_span,
                            contexts: contexts.clone(),
                        },
                        file,
                        block_span.start..=block_span.end,
                        false,
                    );
                }
                IterLoopVars::Single((var, var_span)) => {
                    let mut symbol_table = files.symbol_table.entry(file).or_default();
                    insert_symbol_definition(
                        &mut symbol_table,
                        &SymbolInfo {
                            name: var.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol {
                                is_const: false,
                                is_public: false,
                            }),
                            data_type: iter_type,
                            is_definition: true,
                            undefined: false,
                            span: *var_span,
                            contexts: contexts.clone(),
                        },
                        file,
                        block_span.start..=block_span.end,
                        false,
                    );
                }
                _ => {}
            }

            let mut new_contexts = contexts.clone();
            new_contexts.push(Context::Loop);

            let block = analyze_block(
                file_id,
                file_version,
                block,
                files,
                scoped_generic_types,
                &new_contexts,
            );

            get_stmnt_analysis_result(vec![block], vec![exp])
        }
        Statement::VariableInit(_, (var_name, var_span), (value, _)) => {
            let exp = match value {
                VariableInitType::Expression(exp) => analyze_exp(
                    file_id,
                    file_version,
                    exp,
                    DataType::Any,
                    files,
                    scoped_generic_types,
                    contexts,
                ),
                VariableInitType::DataType((ty, _)) => ExpAnalysisResult {
                    exp_ty: ty.clone(),
                    is_propagating_failure: false,
                    return_ty: None,
                },
                _ => ExpAnalysisResult {
                    exp_ty: DataType::Error,
                    is_propagating_failure: false,
                    return_ty: None,
                },
            };

            let mut symbol_table = files.symbol_table.entry(file).or_default();

            let var_type = match exp.exp_ty {
                DataType::Failable(ty) => scoped_generic_types.deref_type(&ty),
                ty => scoped_generic_types.deref_type(&ty),
            };

            insert_symbol_definition(
                &mut symbol_table,
                &SymbolInfo {
                    name: var_name.to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbol {
                        is_const: false,
                        is_public: false,
                    }),
                    data_type: var_type,
                    is_definition: true,
                    undefined: false,
                    span: *var_span,
                    contexts: contexts.clone(),
                },
                file,
                span.end..=scope_end,
                false,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::ArrayDestructInit(_, names, (value, _)) => {
            let exp = match value {
                VariableInitType::Expression(exp) => analyze_exp(
                    file_id,
                    file_version,
                    exp,
                    DataType::Any,
                    files,
                    scoped_generic_types,
                    contexts,
                ),
                VariableInitType::DataType((ty, _)) => ExpAnalysisResult {
                    exp_ty: ty.clone(),
                    is_propagating_failure: false,
                    return_ty: None,
                },
                _ => ExpAnalysisResult {
                    exp_ty: DataType::Error,
                    is_propagating_failure: false,
                    return_ty: None,
                },
            };

            let rhs_ty = match exp.exp_ty {
                DataType::Failable(ty) => scoped_generic_types.deref_type(&ty),
                ty => scoped_generic_types.deref_type(&ty),
            };

            // Extract element type from the array type
            let element_ty = match &rhs_ty {
                DataType::Array(inner) => scoped_generic_types.deref_type(inner),
                _ => {
                    files.report_error(&file, "Array destructuring requires an array value", *span);
                    DataType::Error
                }
            };

            let mut symbol_table = files.symbol_table.entry(file).or_default();

            for (var_name, var_span) in names {
                insert_symbol_definition(
                    &mut symbol_table,
                    &SymbolInfo {
                        name: var_name.to_string(),
                        symbol_type: SymbolType::Variable(VariableSymbol {
                            is_const: false,
                            is_public: false,
                        }),
                        data_type: element_ty.clone(),
                        is_definition: true,
                        undefined: false,
                        span: *var_span,
                        contexts: contexts.clone(),
                    },
                    file,
                    span.end..=scope_end,
                    false,
                );
            }

            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::ConstInit(_, (var_name, var_span), exp) => {
            let exp = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            let mut symbol_table = files.symbol_table.entry(file).or_default();

            let var_type = match exp.exp_ty {
                DataType::Failable(ty) => scoped_generic_types.deref_type(&ty),
                ty => scoped_generic_types.deref_type(&ty),
            };

            insert_symbol_definition(
                &mut symbol_table,
                &SymbolInfo {
                    name: var_name.to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbol {
                        is_const: true,
                        is_public: false,
                    }),
                    data_type: var_type,
                    is_definition: true,
                    undefined: false,
                    span: *var_span,
                    contexts: contexts.clone(),
                },
                file,
                span.end..=scope_end,
                false,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::Expression(exp) => {
            let exp = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::Fail(_, exp) => {
            if !contexts
                .iter()
                .any(|c| matches!(c, Context::Function(_) | Context::Main))
            {
                files.report_error(
                    &file,
                    "Fail statements can only be used inside of functions or the main block",
                    *span,
                );
            }

            if let Some(exp) = exp {
                let exp = analyze_exp(
                    file_id,
                    file_version,
                    exp,
                    DataType::Int,
                    files,
                    scoped_generic_types,
                    contexts,
                );

                return StmntAnalysisResult {
                    is_propagating_failure: true,
                    return_ty: exp.return_ty,
                };
            }

            StmntAnalysisResult {
                is_propagating_failure: true,
                return_ty: None,
            }
        }
        Statement::Return(_, exp) => {
            if !contexts.iter().any(|c| matches!(c, Context::Function(_))) {
                files.report_error(&file, "Return statement outside of function", *span);
            }

            if let Some(exp) = exp {
                let exp_analysis = analyze_exp(
                    file_id,
                    file_version,
                    exp,
                    DataType::Any,
                    files,
                    scoped_generic_types,
                    contexts,
                );

                if let Some(ty) = exp_analysis.return_ty {
                    return StmntAnalysisResult {
                        is_propagating_failure: exp_analysis.is_propagating_failure,
                        return_ty: Some(make_union_type(vec![ty, exp_analysis.exp_ty])),
                    };
                }

                return StmntAnalysisResult {
                    is_propagating_failure: exp_analysis.is_propagating_failure,
                    return_ty: Some(exp_analysis.exp_ty),
                };
            }

            StmntAnalysisResult {
                is_propagating_failure: false,
                return_ty: None,
            }
        }
        Statement::ShorthandAdd((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, var, &file, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(var) if var.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }

                    info.data_type
                }
                None => DataType::Any,
            };

            let default_ty = DataType::Union(vec![
                DataType::Text,
                DataType::Number,
                DataType::Int,
                DataType::Array(Box::new(DataType::Union(vec![
                    DataType::Text,
                    DataType::Number,
                    DataType::Int,
                ]))),
            ]);

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                var_ty.clone(),
                files,
                scoped_generic_types,
                contexts,
            );

            if !matches_type(&default_ty, &var_ty, scoped_generic_types)
                || !matches_type(&exp_analysis.exp_ty, &var_ty, scoped_generic_types)
            {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot add to variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    *var_span,
                );
            }

            if let DataType::Generic(id) = &var_ty {
                scoped_generic_types.constrain_generic_type(*id, exp_analysis.exp_ty.clone());
            }

            // If the variable was initialized as an empty array [Any], refine its
            // type from the RHS so that `let x = []; x += [1]` infers x as [Int].
            if let DataType::Array(inner) = &var_ty {
                if matches!(inner.as_ref(), DataType::Any) {
                    if let DataType::Array(_) = &exp_analysis.exp_ty {
                        // Look up the original definition location
                        // (read guard is dropped at the end of this block)
                        let def_location = {
                            files.symbol_table.get(&file).and_then(|st| {
                                st.definitions
                                    .get(var)
                                    .and_then(|defs| defs.get(&var_span.start).cloned())
                            })
                        };

                        if let Some(def_location) = def_location {
                            if let Some(mut st) = files.symbol_table.get_mut(&def_location.file) {
                                if let Some(sym_info) = st.symbols.get(&def_location.start).cloned()
                                {
                                    let mut updated = sym_info;
                                    updated.data_type = exp_analysis.exp_ty.clone();
                                    st.symbols
                                        .insert(def_location.start..=def_location.end, updated);
                                }
                            }
                        }
                    }
                }
            }

            insert_symbol_reference(
                var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::ShorthandDiv((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, var, &file, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(var) if var.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }

                    info.data_type
                }
                None => DataType::Any,
            };

            if !matches_type(
                &DataType::Union([DataType::Number, DataType::Int].to_vec()),
                &var_ty,
                scoped_generic_types,
            ) {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot divide variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    *var_span,
                );
            }

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );

            if let DataType::Generic(id) = var_ty {
                scoped_generic_types.constrain_generic_type(id, exp_analysis.exp_ty);
            }

            insert_symbol_reference(
                var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::ShorthandModulo((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, var, &file, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(var) if var.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }

                    info.data_type
                }
                None => DataType::Any,
            };

            if !matches_type(
                &DataType::Union([DataType::Number, DataType::Int].to_vec()),
                &var_ty,
                scoped_generic_types,
            ) {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot use modulo with variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    *var_span,
                );
            }

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );

            if let DataType::Generic(id) = var_ty {
                scoped_generic_types.constrain_generic_type(id, exp_analysis.exp_ty);
            }

            insert_symbol_reference(
                var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::ShorthandMul((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, var, &file, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(var) if var.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }

                    info.data_type
                }
                None => DataType::Any,
            };

            if !matches_type(
                &DataType::Union([DataType::Number, DataType::Int].to_vec()),
                &var_ty,
                scoped_generic_types,
            ) {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot use multiply with variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    *var_span,
                );
            }

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );

            if let DataType::Generic(id) = var_ty {
                scoped_generic_types.constrain_generic_type(id, exp_analysis.exp_ty);
            }

            insert_symbol_reference(
                var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::ShorthandSub((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, var, &file, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(var) if var.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }

                    info.data_type
                }
                None => DataType::Any,
            };

            if !matches_type(
                &DataType::Union([DataType::Number, DataType::Int].to_vec()),
                &var_ty,
                scoped_generic_types,
            ) {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot use subtract with variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    *var_span,
                );
            }

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );

            if let DataType::Generic(id) = var_ty {
                scoped_generic_types.constrain_generic_type(id, exp_analysis.exp_ty);
            }

            insert_symbol_reference(
                var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::VariableSet((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, var, &file, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(var) if var.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }

                    info.data_type
                }
                None => DataType::Any,
            };

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                var_ty.clone(),
                files,
                scoped_generic_types,
                contexts,
            );

            // If the variable was an empty array [Any], refine its type from the RHS.
            if let DataType::Array(inner) = &var_ty {
                if matches!(inner.as_ref(), DataType::Any) {
                    if let DataType::Array(_) = &exp_analysis.exp_ty {
                        let def_location = {
                            files.symbol_table.get(&file).and_then(|st| {
                                st.definitions
                                    .get(var)
                                    .and_then(|defs| defs.get(&var_span.start).cloned())
                            })
                        };

                        if let Some(def_location) = def_location {
                            if let Some(mut st) = files.symbol_table.get_mut(&def_location.file) {
                                if let Some(sym_info) = st.symbols.get(&def_location.start).cloned()
                                {
                                    let mut updated = sym_info;
                                    updated.data_type = exp_analysis.exp_ty.clone();
                                    st.symbols
                                        .insert(def_location.start..=def_location.end, updated);
                                }
                            }
                        }
                    }
                }
            }

            insert_symbol_reference(
                var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::ArrayDestructSet(names, exp) => {
            // Check all target variables exist and are assignable
            for (var, var_span) in names {
                if let Some(info) = get_symbol_definition_info(files, var, &file, var_span.start) {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(v) if v.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }
                }
            }

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            let rhs_ty = scoped_generic_types.deref_type(&exp_analysis.exp_ty);

            match &rhs_ty {
                DataType::Array(_) => {}
                _ => {
                    files.report_error(&file, "Array destructuring requires an array value", *span);
                }
            }

            for (var, var_span) in names {
                insert_symbol_reference(
                    var,
                    files,
                    &SymbolLocation {
                        file,
                        start: var_span.start,
                        end: var_span.end,
                    },
                    scoped_generic_types,
                    contexts,
                );
            }

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::ArrayIndexSet((var, var_span), index_exp, value_exp) => {
            let var_ty = match get_symbol_definition_info(files, var, &file, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(var) if var.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }

                    info.data_type
                }
                None => DataType::Any,
            };

            let element_ty = match &var_ty {
                DataType::Array(inner) => *inner.clone(),
                DataType::Generic(id) => {
                    match scoped_generic_types.get_recursive(*id) {
                        DataType::Array(inner) => match *inner {
                            DataType::Any => {
                                // Generic array with unknown inner type; create a
                                // parametric inner type so it can be resolved later.
                                let inner_id = scoped_generic_types.new_generic_id();
                                scoped_generic_types.constrain_generic_type(
                                    *id,
                                    DataType::Array(Box::new(DataType::Generic(inner_id))),
                                );
                                DataType::Generic(inner_id)
                            }
                            other => other,
                        },
                        DataType::Any => {
                            // Unconstrained generic; we now know it is used as an array.
                            let inner_id = scoped_generic_types.new_generic_id();
                            scoped_generic_types.constrain_generic_type(
                                *id,
                                DataType::Array(Box::new(DataType::Generic(inner_id))),
                            );
                            DataType::Generic(inner_id)
                        }
                        _ => {
                            files.report_error(
                                &file,
                                &format!(
                                    "Cannot index into value of type {}",
                                    var_ty.to_string(scoped_generic_types)
                                ),
                                *var_span,
                            );
                            DataType::Any
                        }
                    }
                }
                DataType::Any => DataType::Any,
                _ => {
                    files.report_error(
                        &file,
                        &format!(
                            "Cannot index into value of type {}",
                            var_ty.to_string(scoped_generic_types)
                        ),
                        *var_span,
                    );
                    DataType::Any
                }
            };

            let index_analysis = analyze_exp(
                file_id,
                file_version,
                index_exp,
                DataType::Int,
                files,
                scoped_generic_types,
                contexts,
            );

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                value_exp,
                element_ty.clone(),
                files,
                scoped_generic_types,
                contexts,
            );

            // Constrain the generic element type if we learned a concrete type.
            if let DataType::Generic(id) = &element_ty {
                scoped_generic_types.constrain_generic_type(*id, exp_analysis.exp_ty);
            }

            insert_symbol_reference(
                var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure
                    || index_analysis.is_propagating_failure,
                return_ty: match (index_analysis.return_ty, exp_analysis.return_ty) {
                    (Some(a), Some(b)) => Some(make_union_type(vec![a, b])),
                    (Some(a), None) | (None, Some(a)) => Some(a),
                    (None, None) => None,
                },
            }
        }
        Statement::Break => {
            if !contexts.iter().any(|c| matches!(c, Context::Loop)) {
                files.report_error(&file, "Break statement outside of loop", *span);
            }

            StmntAnalysisResult {
                is_propagating_failure: false,
                return_ty: None,
            }
        }
        Statement::Continue => {
            if !contexts.iter().any(|c| matches!(c, Context::Loop)) {
                files.report_error(&file, "Continue statement outside of loop", *span);
            }

            StmntAnalysisResult {
                is_propagating_failure: false,
                return_ty: None,
            }
        }
        Statement::Comment((Comment::DocString(docs), _)) => handle_doc_strings(docs, contexts),
        Statement::Comment(_) | Statement::Shebang(_) | Statement::Error => StmntAnalysisResult {
            is_propagating_failure: false,
            return_ty: None,
        },
    }
}

fn handle_doc_strings(docs: &String, contexts: &mut Vec<Context>) -> StmntAnalysisResult {
    match contexts.last() {
        Some(Context::DocString(doc_string)) => {
            let new_doc_string = format!("{doc_string}\n{docs}");
            *contexts.last_mut().unwrap() = Context::DocString(new_doc_string);
        }
        _ => {
            contexts.push(Context::DocString(docs.clone()));
        }
    }

    StmntAnalysisResult {
        is_propagating_failure: false,
        return_ty: None,
    }
}

/// Returns `true` if the given block unconditionally terminates
/// (i.e. its last statement is a terminating statement).
fn block_terminates(block: &Block) -> bool {
    match block {
        Block::Block(_, stmnts) => stmnts
            .last()
            .is_some_and(|s| is_terminating_statement(&s.0)),
        Block::Singleline(stmnt) => is_terminating_statement(&stmnt.0),
        Block::Error => false,
    }
}

/// Returns `true` if the given statement unconditionally terminates
/// the current block's control flow (return, fail, break, continue,
/// or an if/else where all branches terminate).
pub fn is_terminating_statement(stmnt: &Statement) -> bool {
    match stmnt {
        Statement::Return(..) | Statement::Fail(..) | Statement::Break | Statement::Continue => {
            true
        }
        Statement::IfCondition(_, if_cond, _, Some(else_cond)) => {
            let if_terminates = match &if_cond.0 {
                IfCondition::IfCondition(_, block) => block_terminates(&block.0),
                _ => false,
            };
            let else_terminates = match &else_cond.0 {
                ElseCondition::Else(_, block) => block_terminates(&block.0),
            };
            if_terminates && else_terminates
        }
        Statement::IfChain(_, chain) => {
            let mut has_else = false;
            let mut all_terminate = true;
            for (content, _) in chain.iter() {
                match content {
                    IfChainContent::IfCondition((cond, _)) => match cond {
                        IfCondition::IfCondition(_, block) => {
                            if !block_terminates(&block.0) {
                                all_terminate = false;
                            }
                        }
                        _ => {
                            all_terminate = false;
                        }
                    },
                    IfChainContent::Else((else_cond, _)) => {
                        has_else = true;
                        match else_cond {
                            ElseCondition::Else(_, block) => {
                                if !block_terminates(&block.0) {
                                    all_terminate = false;
                                }
                            }
                        }
                    }
                    IfChainContent::Comment(_) => {}
                }
            }
            has_else && all_terminate
        }
        _ => false,
    }
}

pub fn analyze_block(
    file_id: FileId,
    file_version: FileVersion,
    (block, span): &Spanned<Block>,
    files: &Files,
    scoped_generic_types: &GenericsMap,
    contexts: &[Context],
) -> StmntAnalysisResult {
    let mut types: Vec<DataType> = vec![];

    let mut is_propagating = false;

    match block {
        Block::Block(modifiers, stmnt) => {
            check_duplicate_modifiers(modifiers, &(file_id, file_version), files);
            let mut new_contexts = contexts.to_owned();
            new_contexts.push(Context::Block(BlockContext {
                modifiers: modifiers.iter().map(|(m, _)| m.clone()).collect(),
            }));

            let mut terminator_seen = false;

            for stmnt in stmnt.iter() {
                if terminator_seen
                    && !matches!(
                        stmnt.0,
                        Statement::Comment(_) | Statement::Shebang(_) | Statement::Error
                    )
                {
                    files.report_unused(&(file_id, file_version), "Unreachable code", stmnt.1);
                }

                let StmntAnalysisResult {
                    return_ty,
                    is_propagating_failure,
                } = analyze_stmnt(
                    file_id,
                    file_version,
                    stmnt,
                    files,
                    span.end,
                    scoped_generic_types,
                    &mut new_contexts,
                );

                if let Some(ty) = return_ty {
                    types.push(ty);
                }

                is_propagating |= is_propagating_failure;

                if !terminator_seen && is_terminating_statement(&stmnt.0) {
                    terminator_seen = true;
                }
            }
        }
        Block::Singleline(stmnt) => {
            let mut working_contexts = contexts.to_owned();
            let StmntAnalysisResult {
                return_ty,
                is_propagating_failure,
            } = analyze_stmnt(
                file_id,
                file_version,
                stmnt,
                files,
                span.end,
                scoped_generic_types,
                &mut working_contexts,
            );

            if let Some(ty) = return_ty {
                types.push(ty);
            }

            is_propagating |= is_propagating_failure;
        }
        Block::Error => {}
    }

    if types.is_empty() {
        StmntAnalysisResult {
            is_propagating_failure: is_propagating,
            return_ty: None,
        }
    } else {
        StmntAnalysisResult {
            is_propagating_failure: is_propagating,
            return_ty: Some(make_union_type(types)),
        }
    }
}

pub fn analyze_failable_handlers(
    file_id: FileId,
    file_version: FileVersion,
    failable_handlers: &[Spanned<FailableHandler>],
    modifiers: &[Spanned<CommandModifier>],
    files: &Files,
    scoped_generic_types: &GenericsMap,
    contexts: &[Context],
) -> StmntAnalysisResult {
    let mut types: Vec<DataType> = vec![];
    let mut is_propagating = false;
    let mut contexts = contexts.to_owned();

    let mut has_exited_handler = false; // exited
    let mut has_status_handler = false; // Succeeded/Failed
    let mut failure_count = 0u32;
    let mut succeeded_count = 0u32;
    let mut exited_count = 0u32;

    // The 'trust' modifier cannot be combined with any failure handler
    if has_trust(modifiers, &contexts) && has_failure_handler(failable_handlers) {
        // Report error on the first non-comment handler's span
        if let Some((_, span)) = failable_handlers
            .iter()
            .find(|(h, _)| !matches!(h, FailableHandler::Comment(_)))
        {
            files.report_error(
                &(file_id, file_version),
                "The 'trust' modifier cannot be used with failure handlers",
                *span,
            );
        }
    }

    failable_handlers
        .iter()
        .for_each(|(handler, _)| match handler {
            FailableHandler::Failure((failure, span)) => {

                let mut keyword_span = *span;
                match failure {
                    FailureHandler::Handle((_, name_span), exit_code, block) => {
                        keyword_span = *name_span;

                        if let Some((code_var, code_var_span)) = exit_code {
                            let mut symbol_table = files
                                .symbol_table
                                .entry((file_id, file_version))
                                .or_default();

                            insert_symbol_definition(
                                &mut symbol_table,
                                &SymbolInfo {
                                    name: code_var.to_string(),
                                    symbol_type: SymbolType::Variable(VariableSymbol {
                                        is_const: false,
                                        is_public: false,
                                    }),
                                    data_type: DataType::Int,
                                    is_definition: true,
                                    undefined: false,
                                    span: *code_var_span,
                                    contexts: vec![],
                                },
                                (file_id, file_version),
                                code_var_span.end..=span.end,
                                false,
                            );
                        }

                        let StmntAnalysisResult {
                            return_ty,
                            is_propagating_failure,
                        } = analyze_stmnt(
                            file_id,
                            file_version,
                            &(Statement::Block(*block.clone()), block.1),
                            files,
                            span.end,
                            scoped_generic_types,
                            &mut contexts,
                        );

                        types.extend(return_ty);
                        is_propagating |= is_propagating_failure;
                    }
                    FailureHandler::Propagate => {
                        if !contexts
                            .iter()
                            .any(|c| *c == Context::Main || matches!(c, Context::Function(_)))
                        {
                            files.report_error(
                                &(file_id, file_version),
                                "Propagate can only be used inside of main block or function",
                                *span,
                            );
                        }

                        let has_other_handlers = failable_handlers.iter().any(|(h, _)| {
                            matches!(
                                h,
                                FailableHandler::Succeeded(_, _)
                                    | FailableHandler::Exited(_, _, _)
                            ) || matches!(
                                h,
                                FailableHandler::Failure((FailureHandler::Handle(_, _, _), _))
                            )
                        });

                        if has_other_handlers {
                            files.report_error(
                                &(file_id, file_version),
                                "The '?' operator cannot be used with other failure handlers",
                                *span,
                            );
                        }

                        is_propagating = true;
                    }
                };

                failure_count += 1;
                if failure_count > 1 {
                    files.report_error(
                        &(file_id, file_version),
                        "Duplicate 'failed' handler",
                        keyword_span,
                    );
                }

                has_status_handler = true;

                if has_exited_handler {
                    files.report_error(
                        &(file_id, file_version),
                        "Exited handler should be used without any other handlers. Remove this handler or `exited` handler",
                        keyword_span,
                    );
                }
            }
            FailableHandler::Succeeded((_, span), block) => {
                let StmntAnalysisResult {
                    return_ty,
                    is_propagating_failure,
                } = analyze_stmnt(
                    file_id,
                    file_version,
                    &(Statement::Block(*block.clone()), block.1),
                    files,
                    block.1.end,
                    scoped_generic_types,
                    &mut contexts,
                );

                has_status_handler = true;

                succeeded_count += 1;
                if succeeded_count > 1 {
                    files.report_error(
                        &(file_id, file_version),
                        "Duplicate 'succeeded' handler",
                        *span,
                    );
                }

                if has_exited_handler {
                    files.report_error(
                        &(file_id, file_version),
                        "Exited handler should be used without any other handlers. Remove this handler or `exited` handler",
                        *span,
                    );
                }

                types.extend(return_ty);
                is_propagating |= is_propagating_failure;
            }
            FailableHandler::Exited((_, name_span), (code_var, code_var_span), block) => {
                {
                    let mut symbol_table = files
                        .symbol_table
                        .entry((file_id, file_version))
                        .or_default();

                    insert_symbol_definition(
                        &mut symbol_table,
                        &SymbolInfo {
                            name: code_var.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false, is_public: false }),
                            data_type: DataType::Int,
                            is_definition: true,
                            undefined: false,
                            span: *code_var_span,
                            contexts: vec![],
                        },
                        (file_id, file_version),
                        code_var_span.end..=block.1.end,
                        false,
                    );
                }

                let StmntAnalysisResult {
                    return_ty,
                    is_propagating_failure,
                } = analyze_stmnt(
                    file_id,
                    file_version,
                    &(Statement::Block(*block.clone()), block.1),
                    files,
                    block.1.end,
                    scoped_generic_types,
                    &mut contexts,
                );

                has_exited_handler = true;

                exited_count += 1;
                if exited_count > 1 {
                    files.report_error(
                        &(file_id, file_version),
                        "Duplicate 'exited' handler",
                        *name_span,
                    );
                }

                if has_status_handler {
                    files.report_error(
                        &(file_id, file_version),
                        "Exited handler should be used without any other handlers",
                        *name_span,
                    );
                }

                types.extend(return_ty);
                is_propagating |= is_propagating_failure;
            }
            FailableHandler::Comment(_) => {}
        });

    if types.is_empty() {
        StmntAnalysisResult {
            is_propagating_failure: is_propagating,
            return_ty: None,
        }
    } else {
        StmntAnalysisResult {
            is_propagating_failure: is_propagating,
            return_ty: Some(make_union_type(types)),
        }
    }
}

fn get_stmnt_analysis_result(
    stmnt_analysis: Vec<StmntAnalysisResult>,
    exp_analysis: Vec<ExpAnalysisResult>,
) -> StmntAnalysisResult {
    let mut is_propagating_failure = false;
    let mut return_ty = vec![];

    for stmnt in stmnt_analysis {
        if stmnt.is_propagating_failure {
            is_propagating_failure = true;
        }
        if let Some(ty) = stmnt.return_ty {
            return_ty.push(ty);
        }
    }

    for exp in exp_analysis {
        if exp.is_propagating_failure {
            is_propagating_failure = true;
        }
        if let Some(ty) = exp.return_ty {
            return_ty.push(ty);
        }
    }

    StmntAnalysisResult {
        is_propagating_failure,
        return_ty: if !return_ty.is_empty() {
            Some(make_union_type(return_ty))
        } else {
            None
        },
    }
}
