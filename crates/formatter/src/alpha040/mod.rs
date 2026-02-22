use crate::{FmtContext, Output, SpanTextOutput, TextOutput, fragments::CommentVariant};
use amber_grammar::{
    CommandModifier, CompilerFlag,
    alpha040::{
        Block, Comment, ElseCondition, Expression, FailureHandler, FunctionArgument,
        GlobalStatement, IfChainContent, IfCondition, ImportContent, InterpolatedCommand,
        IterLoopVars, Statement, VariableInitType,
    },
};
use amber_types::{DataType, token::Span};

type Gen = (GlobalStatement, Span);

impl TextOutput<Gen> for GlobalStatement {
    fn output(&self, _span: &Span, output: &mut Output, ctx: &mut FmtContext<Gen>) {
        match self {
            GlobalStatement::Import(public, import, content, from, path) => {
                if public.0 {
                    output.text("pub ");
                }

                output
                    .output(ctx, import)
                    .space()
                    .output(ctx, content)
                    .space()
                    .output(ctx, from)
                    .space()
                    .char('"')
                    .output(ctx, path)
                    .char('"')
                    .newline();

                if ctx
                    .next_global()
                    .is_some_and(|next| !matches!(next.0, GlobalStatement::Import(..)))
                {
                    output.newline();
                }
            }
            GlobalStatement::FunctionDefinition(
                compiler_flags,
                public,
                function_keyword,
                name,
                args,
                return_type,
                contents,
            ) => {
                for flag in compiler_flags {
                    output.output(ctx, flag);
                    output.newline();
                }

                if public.0 {
                    output.text("pub ");
                }

                output.output(ctx, function_keyword);
                output.space();
                output.output(ctx, name);

                output.char('(');
                // Handle adding variables with proper spacing
                {
                    for arg in args.iter().take(args.len().saturating_sub(1)) {
                        output.output(ctx, arg);
                        output.char(',');
                        output.space();
                    }

                    if let Some(arg) = args.last() {
                        output.output(ctx, arg);
                    }
                }
                output.char(')').space();

                if let Some(returns) = return_type {
                    output.char(':');
                    output.space();
                    output.output(ctx, returns);
                }

                output.char('{').increase_indentation().newline();

                let mut last_span_end = None;
                for content in contents {
                    if let Some(last_span_end) = last_span_end {
                        if !matches!(content.0, Statement::Comment(..))
                            || ctx.source_has_newline(last_span_end..=content.1.start)
                        {
                            output.end_newline()
                        }

                        ctx.allow_newline(output, last_span_end..=content.1.start);
                    }

                    output.end_output(ctx, content);
                    last_span_end = Some(content.1.end);
                }

                output
                    .remove_trailing_whitespace()
                    .decrease_indentation()
                    .newline()
                    .char('}');

                if ctx.next_global().is_some() {
                    output.newline().newline();
                }
            }
            GlobalStatement::Main(main, args, statements) => {
                output.output(ctx, main);

                output.char('(');
                if let Some(args) = args {
                    output.output(ctx, args);
                }
                output
                    .char(')')
                    .space()
                    .char('{')
                    .increase_indentation()
                    .newline();

                let mut last_span_end = None;
                for statement in statements {
                    if let Some(last_span_end) = last_span_end {
                        if !matches!(statement.0, Statement::Comment(..))
                            || ctx.source_has_newline(last_span_end..=statement.1.start)
                        {
                            output.end_newline()
                        }

                        ctx.allow_newline(output, last_span_end..=statement.1.start);
                    }

                    output.output(ctx, statement);
                    last_span_end = Some(statement.1.end);
                }

                output
                    .remove_trailing_whitespace()
                    .decrease_indentation()
                    .newline()
                    .char('}');

                if ctx.next_global().is_some() {
                    output.newline().newline();
                }
            }
            GlobalStatement::Statement(statement) => {
                output.output(ctx, statement).newline();
            }
        }
    }
}

impl TextOutput<Gen> for ImportContent {
    fn output(&self, _span: &Span, output: &mut Output, ctx: &mut FmtContext<Gen>) {
        match self {
            ImportContent::ImportAll => {
                output.char('*');
            }
            ImportContent::ImportSpecific(items) => {
                output.text("{ ");

                for identifier in items.iter().take(items.len().saturating_sub(1)) {
                    output.output(ctx, identifier).char(',').space();
                }
                if let Some(item) = items.last() {
                    output.output(ctx, item).end_space();
                }

                output.char('}');
            }
        }
    }
}

impl TextOutput<Gen> for FunctionArgument {
    fn output(&self, span: &Span, output: &mut Output, ctx: &mut FmtContext<Gen>) {
        fn push_arg(
            output: &mut Output,
            ctx: &mut FmtContext<Gen>,
            is_ref: bool,
            text: &impl SpanTextOutput<Gen>,
        ) {
            if is_ref {
                output.text("ref");
                output.space();
            }
            output.output(ctx, text);
        }

        match self {
            FunctionArgument::Generic(is_ref, text) => push_arg(output, ctx, is_ref.0, text),
            FunctionArgument::Optional(is_ref, text, _, _) => push_arg(output, ctx, is_ref.0, text),
            FunctionArgument::Typed(is_ref, text, _) => push_arg(output, ctx, is_ref.0, text),
            FunctionArgument::Error => {
                output.error(span);
            }
        }
    }
}

impl TextOutput<Gen> for CompilerFlag {
    fn output(&self, _span: &Span, output: &mut Output, _ctx: &mut FmtContext<Gen>) {
        output.text(format!("#[{self}]"));
    }
}

impl TextOutput<Gen> for String {
    fn output(&self, _span: &Span, output: &mut Output, _ctx: &mut FmtContext<Gen>) {
        output.text(self.clone());
    }
}

impl TextOutput<Gen> for DataType {}

impl TextOutput<Gen> for Statement {
    fn output(&self, span: &Span, output: &mut Output, ctx: &mut FmtContext<Gen>) {
        fn shorthand(
            output: &mut Output,
            ctx: &mut FmtContext<Gen>,
            variable: &impl SpanTextOutput<Gen>,
            separator: &str,
            expression: &impl SpanTextOutput<Gen>,
        ) {
            output
                .output(ctx, variable)
                .space()
                .text(separator)
                .space()
                .output(ctx, expression);
        }

        match self {
            Statement::Expression(expression) => {
                output.output(ctx, expression);
            }
            Statement::VariableInit(keyword, name, init) => {
                output
                    .output(ctx, keyword)
                    .space()
                    .output(ctx, name)
                    .space()
                    .char('=')
                    .space()
                    .output(ctx, init);
            }
            Statement::ConstInit(keyword, name, init) => {
                output
                    .output(ctx, keyword)
                    .space()
                    .output(ctx, name)
                    .space()
                    .char('=')
                    .space()
                    .output(ctx, init);
            }
            Statement::VariableSet(name, new_value) => {
                output
                    .output(ctx, name)
                    .space()
                    .char('=')
                    .space()
                    .output(ctx, new_value);
            }
            Statement::IfCondition(r#if, condition, comments, else_condition) => {
                output.output(ctx, r#if).space().output(ctx, condition);

                if let Some(comment) = comments.first() {
                    ctx.allow_newline(output, condition.1.end..=comment.1.start);
                    output.output(ctx, comment);
                }

                for comment in comments.iter().skip(1) {
                    output.output(ctx, comment);
                }

                if let Some(else_condition) = else_condition {
                    output.space().output(ctx, else_condition);
                }
            }
            Statement::IfChain(r#if, items) => {
                output
                    .output(ctx, r#if)
                    .space()
                    .char('{')
                    .increase_indentation();

                for ele in items {
                    output.newline().output(ctx, ele);
                }

                output.decrease_indentation();
                if items.len() > 0 {
                    output.newline();
                }
                output.char('}');
            }
            Statement::ShorthandAdd(variable, expr) => shorthand(output, ctx, variable, "+=", expr),
            Statement::ShorthandSub(variable, expr) => shorthand(output, ctx, variable, "-=", expr),
            Statement::ShorthandMul(variable, expr) => shorthand(output, ctx, variable, "*=", expr),
            Statement::ShorthandDiv(variable, expr) => shorthand(output, ctx, variable, "/=", expr),
            Statement::ShorthandModulo(variable, expr) => {
                shorthand(output, ctx, variable, "%=", expr)
            }
            Statement::InfiniteLoop(r#loop, block) => {
                output.output(ctx, r#loop).space().output(ctx, block);
            }
            Statement::IterLoop(r#for, element, r#in, expr, block) => {
                output
                    .output(ctx, r#for)
                    .space()
                    .output(ctx, element)
                    .space()
                    .output(ctx, r#in)
                    .space()
                    .output(ctx, expr)
                    .space()
                    .end_output(ctx, block);
            }
            Statement::Break => output.end_text("break"),
            Statement::Continue => output.end_text("continue"),
            Statement::Return(r#return, expr) => {
                output.end_output(ctx, r#return);

                if let Some(expr) = expr {
                    output.space().end_output(ctx, expr);
                }
            }
            Statement::Fail(fail, expr) => {
                output.end_output(ctx, fail);

                if let Some(expr) = expr {
                    output.space().end_output(ctx, expr);
                }
            }
            Statement::Echo(echo, text) => output.output(ctx, echo).space().end_output(ctx, text),
            Statement::Cd(cd, text) => output.output(ctx, cd).space().end_output(ctx, text),
            Statement::MoveFiles(modifiers, mv, source, destination, failure_handler) => {
                for modifier in modifiers {
                    output.output(ctx, modifier).space();
                }

                output
                    .output(ctx, mv)
                    .space()
                    .output(ctx, source)
                    .space()
                    .output(ctx, destination);

                if let Some(failure_handler) = failure_handler {
                    output.space().output(ctx, failure_handler);
                }
            }
            Statement::Block(block) => output.end_output(ctx, block),
            Statement::Comment(comment) => output.end_output(ctx, comment),
            Statement::Shebang(shebang) => output.end_text(shebang.as_str()),
            Statement::Error => output.error(span),
        }
    }
}

impl TextOutput<Gen> for IterLoopVars {
    fn output(&self, span: &Span, output: &mut Output, _ctx: &mut FmtContext<Gen>) {
        output.span(span);
    }
}

impl TextOutput<Gen> for Block {
    fn output(&self, span: &Span, output: &mut Output, ctx: &mut FmtContext<Gen>) {
        // output.debug_point("Block").span(span);
        match self {
            Block::Block(modifiers, statements) => {
                output.char('{').increase_indentation().end_newline();

                for modifier in modifiers {
                    output.output(ctx, modifier).end_space();
                }
                for statement in statements {
                    output.output(ctx, statement).end_newline();
                }

                output
                    .remove_newline()
                    .decrease_indentation()
                    .newline()
                    .char('}');
            }
            Block::Error => output.end_span(span),
        }
    }
}

impl TextOutput<Gen> for IfChainContent {
    fn output(&self, span: &Span, output: &mut Output, _ctx: &mut FmtContext<Gen>) {
        output.span(span);
    }
}

impl TextOutput<Gen> for ElseCondition {
    fn output(&self, span: &Span, output: &mut Output, _ctx: &mut FmtContext<Gen>) {
        output.span(span);
    }
}

impl TextOutput<Gen> for Comment {
    fn output(&self, span: &Span, output: &mut Output, _ctx: &mut FmtContext<Gen>) {
        match self {
            Comment::Comment(comment) => {
                output.end_comment(CommentVariant::Regular, comment.as_str(), span)
            }
            Comment::DocString(doc_comment) => {
                output.end_comment(CommentVariant::Doc, doc_comment.as_str(), span)
            }
        }
    }
}

impl TextOutput<Gen> for IfCondition {
    fn output(&self, span: &Span, output: &mut Output, ctx: &mut FmtContext<Gen>) {
        match self {
            IfCondition::IfCondition(condition, block) => {
                output
                    .output(ctx, condition)
                    .space()
                    .output(ctx, block)
                    .newline();
            }
            IfCondition::InlineIfCondition(condition, statement) => {
                output
                    .output(ctx, condition)
                    .char(':')
                    .space()
                    .output(ctx, statement);
            }
            IfCondition::Comment(comment) => output.end_output(ctx, comment),
            IfCondition::Error => output.error(span),
        }
    }
}

impl TextOutput<Gen> for VariableInitType {
    fn output(&self, span: &Span, output: &mut Output, ctx: &mut FmtContext<Gen>) {
        match self {
            VariableInitType::Expression(expr) => output.end_output(ctx, expr),
            VariableInitType::DataType(r#type) => output.end_output(ctx, r#type),
            VariableInitType::Error => output.error(span),
        }
    }
}

impl TextOutput<Gen> for Expression {
    fn output(&self, span: &Span, output: &mut Output, ctx: &mut FmtContext<Gen>) {
        fn char_separated(
            output: &mut Output,
            ctx: &mut FmtContext<Gen>,
            rhs: &impl SpanTextOutput<Gen>,
            middle: char,
            lhs: &impl SpanTextOutput<Gen>,
        ) {
            output.output(ctx, rhs);
            output.space();
            output.char(middle);
            output.space();
            output.output(ctx, lhs);
        }

        fn string_separated(
            output: &mut Output,
            ctx: &mut FmtContext<Gen>,
            rhs: &impl SpanTextOutput<Gen>,
            middle: &str,
            lhs: &impl SpanTextOutput<Gen>,
        ) {
            output.output(ctx, rhs);
            output.space();
            output.text(middle);
            output.space();
            output.output(ctx, lhs);
        }

        fn output_separated(
            output: &mut Output,
            ctx: &mut FmtContext<Gen>,
            rhs: &impl SpanTextOutput<Gen>,
            middle: &impl SpanTextOutput<Gen>,
            lhs: &impl SpanTextOutput<Gen>,
        ) {
            output.output(ctx, rhs);
            output.space();
            output.output(ctx, middle);
            output.space();
            output.output(ctx, lhs);
        }

        match self {
            Expression::Number(num) => {
                output.output(ctx, num);
            }
            Expression::Boolean(boolean) => {
                output.output(ctx, boolean);
            }
            Expression::Text(_) => {
                // Take raw text from file, as string content should not be modified
                output.end_span(span);
            }
            Expression::Parentheses(parentheses) => {
                output.output(ctx, parentheses);
            }
            Expression::Var(var) => output.end_output(ctx, var),
            Expression::Add(rhs, lhs) => char_separated(output, ctx, rhs, '+', lhs),
            Expression::Subtract(rhs, lhs) => char_separated(output, ctx, rhs, '-', lhs),
            Expression::Multiply(rhs, lhs) => char_separated(output, ctx, rhs, '*', lhs),
            Expression::Divide(rhs, lhs) => char_separated(output, ctx, rhs, '/', lhs),
            Expression::Modulo(rhs, lhs) => char_separated(output, ctx, rhs, '%', lhs),
            Expression::Neg(neg, lhs) => {
                output.output(ctx, neg);
                output.output(ctx, lhs);
            }
            Expression::And(rhs, and, lhs) => output_separated(output, ctx, rhs, and, lhs),
            Expression::Or(rhs, or, lhs) => output_separated(output, ctx, rhs, or, lhs),
            Expression::Gt(rhs, lhs) => char_separated(output, ctx, rhs, '>', lhs),
            Expression::Ge(rhs, lhs) => string_separated(output, ctx, rhs, ">=", lhs),
            Expression::Lt(rhs, lhs) => char_separated(output, ctx, rhs, '<', lhs),
            Expression::Le(rhs, lhs) => string_separated(output, ctx, rhs, ">=", lhs),
            Expression::Eq(rhs, lhs) => string_separated(output, ctx, rhs, "==", lhs),
            Expression::Neq(rhs, lhs) => string_separated(output, ctx, rhs, "!=", lhs),
            Expression::Not(not, lhs) => {
                output.output(ctx, not);
                output.space();
                output.output(ctx, lhs);
            }
            Expression::Ternary(condition, then, if_then, r#else, if_else) => {
                // TODO(tye-exe): Allow single line ternary if short enough. Use given span to measure length?
                output.increase_indentation();
                output.output(ctx, condition);
                output.newline();
                output.output(ctx, then);
                output.space();
                output.output(ctx, if_then);
                output.newline();
                output.output(ctx, r#else);
                output.space();
                output.output(ctx, if_else);
                output.decrease_indentation();
            }
            Expression::FunctionInvocation(modifiers, function_name, args, failure_handler) => {
                for modifier in modifiers {
                    output.output(ctx, modifier);
                    output.space();
                }

                output.output(ctx, function_name).char('(');

                for arg in args.iter().take(args.len().saturating_sub(1)) {
                    output.output(ctx, arg).char(',').space();
                }
                if let Some(arg) = args.last() {
                    output.output(ctx, arg);
                }

                output.char(')');

                if let Some(failure_handler) = failure_handler {
                    output.output(ctx, failure_handler);
                }
            }
            Expression::Command(modifiers, commands, failure_handler) => {
                for modifier in modifiers {
                    output.output(ctx, modifier);
                    output.space();
                }

                // Do not format bash commands
                for command in commands {
                    output.end_span(&command.1);
                }

                if let Some(failure_handler) = failure_handler {
                    output.space().output(ctx, failure_handler);
                }
            }
            Expression::Array(array) => {
                output.char('[');
                for expression in array {
                    output.output(ctx, expression);
                    output.char(',');
                    output.space();
                }
                output.remove_space();
                output.char(']');
            }
            Expression::Range(lhs, rhs) => {
                output.output(ctx, lhs);
                output.text("..");
                output.output(ctx, rhs);
            }
            Expression::Null => {
                output.text("Null");
            }
            Expression::Cast(lhs, r#as, rhs) => string_separated(output, ctx, rhs, &r#as.0, lhs),
            Expression::Status => {
                output.text("status");
            }
            Expression::Nameof(name_of, variable) => {
                output.output(ctx, name_of);
                output.space();
                output.output(ctx, variable);
            }
            Expression::Is(lhs, is, rhs) => {
                output
                    .output(ctx, lhs)
                    .space()
                    .output(ctx, is)
                    .space()
                    .output(ctx, rhs);
            }
            Expression::ArrayIndex(array, index) => output
                .output(ctx, array)
                .char('[')
                .output(ctx, index)
                .end_char(']'),
            Expression::Exit(exit, value) => {
                output.output(ctx, exit);

                if let Some(value) = value {
                    output.space().output(ctx, value);
                }
            }
            Expression::Error => {
                output.error(span);
            }
        }
    }
}

impl TextOutput<Gen> for FailureHandler {
    fn output(&self, _span: &Span, output: &mut Output, ctx: &mut FmtContext<Gen>) {
        match self {
            FailureHandler::Propagate => {
                output.char('?');
            }
            FailureHandler::Handle(failed, statements) => {
                output.output(ctx, failed);
                output.space();
                output.char('{');
                output.increase_indentation();
                output.newline();

                for statement in statements {
                    output.output(ctx, statement);
                    output.newline();
                }

                output.remove_newline();
                output.decrease_indentation();
                output.newline();

                output.char('}');
            }
        }
    }
}

impl TextOutput<Gen> for CommandModifier {
    fn output(&self, _span: &Span, output: &mut Output, _ctx: &mut FmtContext<Gen>) {
        match self {
            CommandModifier::Unsafe => output.end_text("unsafe"),
            CommandModifier::Trust => output.end_text("trust"),
            CommandModifier::Silent => output.end_text("silent"),
            CommandModifier::Sudo => output.end_text("sudo"),
        }
    }
}

impl TextOutput<Gen> for InterpolatedCommand {
    fn output(&self, _span: &Span, output: &mut Output, ctx: &mut FmtContext<Gen>) {
        match self {
            InterpolatedCommand::Escape(escape) => output
                .debug_point("InterpolatedCommand escape")
                .end_text(escape.as_str()),
            InterpolatedCommand::CommandOption(option) => output.end_text(option.as_str()),
            InterpolatedCommand::Expression(expr) => output.end_output(ctx, expr),
            InterpolatedCommand::Text(text) => output.end_text(text.as_str()),
        }
    }
}

impl TextOutput<Gen> for f32 {}
impl TextOutput<Gen> for bool {}
