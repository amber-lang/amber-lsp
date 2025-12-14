use amber_grammar::alpha040::{
    Block, Comment, ElseCondition, Expression, FailureHandler, FunctionArgument, GlobalStatement,
    IfChainContent, IfCondition, ImportContent, InterpolatedCommand, IterLoopVars, Statement,
    VariableInitType,
};
use amber_grammar::{CommandModifier, CompilerFlag};
use amber_types::DataType;
use amber_types::token::Span;

use crate::SpanTextOutput;
use crate::{Output, TextOutput};

impl TextOutput for GlobalStatement {
    fn output(&self, _span: &Span, output: &mut Output) {
        match self {
            GlobalStatement::Import(public, import, content, from, path) => {
                if public.0 {
                    output.text("pub ");
                }

                output.output(import);
                output.space();
                output.output(content);
                output.space();
                output.output(from);
                output.space();
                output.output(path);
                output.newline();
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
                output.newline();

                for flag in compiler_flags {
                    output.output(flag);
                    output.newline();
                }

                if public.0 {
                    output.text("pub ");
                }

                output.output(function_keyword);
                output.space();
                output.output(name);

                output.char('(');
                // Handle adding variables with proper spacing
                {
                    for arg in args.iter().take(args.len().saturating_sub(1)) {
                        output.output(arg);
                        output.char(',');
                        output.space();
                    }

                    if let Some(arg) = args.last() {
                        output.output(arg);
                    }
                }
                output.char(')');

                if let Some(returns) = return_type {
                    output.char(':');
                    output.space();
                    output.output(returns);
                }

                output.char('{').increase_indentation();
                for content in contents {
                    // dbg!(content);
                    output.newline().end_output(content);
                }
                output.decrease_indentation().newline().char('}');
            }
            GlobalStatement::Main(main, args, statements) => {
                output.newline();
                output.output(main);

                output.char('(');
                if let Some(args) = args {
                    output.output(args);
                }
                output.char(')').space().char('{').increase_indentation();

                for statement in statements {
                    dbg!(statement);
                    output.newline().output(statement);
                }

                output.decrease_indentation().newline().end_char('}');
            }
            GlobalStatement::Statement(statement) => output.end_output(statement),
        }
    }
}

impl TextOutput for ImportContent {
    fn output(&self, _span: &Span, output: &mut Output) {
        match self {
            ImportContent::ImportAll => {
                output.char('*');
            }
            ImportContent::ImportSpecific(items) => {
                output.text("{ ");
                for identifier in items {
                    output.output(identifier);
                    output.space();
                }
                output.char('}');
            }
        }
    }
}

impl TextOutput for FunctionArgument {
    fn output(&self, span: &Span, output: &mut Output) {
        fn push_arg(output: &mut Output, is_ref: bool, text: &impl SpanTextOutput) {
            if is_ref {
                output.text("ref");
                output.space();
            }
            output.output(text);
        }

        match self {
            FunctionArgument::Generic(is_ref, text) => push_arg(output, is_ref.0, text),
            FunctionArgument::Optional(is_ref, text, _, _) => push_arg(output, is_ref.0, text),
            FunctionArgument::Typed(is_ref, text, _) => push_arg(output, is_ref.0, text),
            FunctionArgument::Error => {
                output.error(span);
            }
        }
    }
}

impl TextOutput for CompilerFlag {
    fn output(&self, _span: &Span, output: &mut Output) {
        output.text(format!("#[{self}]"));
    }
}

impl TextOutput for String {
    fn output(&self, _span: &Span, output: &mut Output) {
        output.text(self.clone());
    }
}

impl TextOutput for DataType {}

impl TextOutput for Statement {
    fn output(&self, span: &Span, output: &mut Output) {
        fn shorthand(
            output: &mut Output,
            variable: &impl SpanTextOutput,
            separator: &str,
            expression: &impl SpanTextOutput,
        ) {
            output
                .output(variable)
                .space()
                .text(separator)
                .space()
                .output(expression);
        }

        match self {
            Statement::Expression(expression) => {
                output.output(expression);
            }
            Statement::VariableInit(keyword, name, init) => {
                output
                    .output(keyword)
                    .space()
                    .output(name)
                    .space()
                    .char('=')
                    .space()
                    .output(init);
            }
            Statement::ConstInit(keyword, name, init) => {
                output
                    .output(keyword)
                    .space()
                    .output(name)
                    .space()
                    .char('=')
                    .space()
                    .output(init);
            }
            Statement::VariableSet(name, new_value) => {
                output
                    .output(name)
                    .space()
                    .char('=')
                    .space()
                    .output(new_value);
            }
            Statement::IfCondition(r#if, condition, items, else_condition) => {
                output.output(r#if).space().output(condition).end_space();
                // .debug_point("If condition items");

                for ele in items {
                    output.output(ele);
                }

                if let Some(else_condition) = else_condition {
                    output.output(else_condition);
                }
            }
            Statement::IfChain(r#if, items) => {
                output.output(r#if).space().char('{').increase_indentation();

                for ele in items {
                    output.newline().output(ele);
                }

                output.decrease_indentation();
                if items.len() > 0 {
                    output.newline();
                }
                output.char('}');
            }
            Statement::ShorthandAdd(variable, expr) => shorthand(output, variable, "+=", expr),
            Statement::ShorthandSub(variable, expr) => shorthand(output, variable, "-=", expr),
            Statement::ShorthandMul(variable, expr) => shorthand(output, variable, "*=", expr),
            Statement::ShorthandDiv(variable, expr) => shorthand(output, variable, "/=", expr),
            Statement::ShorthandModulo(variable, expr) => shorthand(output, variable, "%=", expr),
            Statement::InfiniteLoop(r#loop, block) => {
                output.output(r#loop).space().output(block);
            }
            Statement::IterLoop(r#for, element, r#in, expr, block) => {
                output
                    .output(r#for)
                    .space()
                    .output(element)
                    .space()
                    .output(r#in)
                    .space()
                    .output(expr)
                    .space()
                    .end_output(block);
            }
            Statement::Break => output.end_text("break"),
            Statement::Continue => output.end_text("continue"),
            Statement::Return(r#return, expr) => {
                output.end_output(r#return);

                if let Some(expr) = expr {
                    output.space().end_output(expr);
                }
            }
            Statement::Fail(fail, expr) => {
                output.end_output(fail);

                if let Some(expr) = expr {
                    output.space().end_output(expr);
                }
            }
            Statement::Echo(echo, text) => output.output(echo).space().end_output(text),
            Statement::Cd(cd, text) => output.output(cd).space().end_output(text),
            Statement::MoveFiles(modifiers, mv, source, destination, failure_handler) => {
                for modifier in modifiers {
                    output.output(modifier).space();
                }

                output
                    .output(mv)
                    .space()
                    .output(source)
                    .space()
                    .output(destination);

                if let Some(failure_handler) = failure_handler {
                    output.space().output(failure_handler);
                }
            }
            Statement::Block(block) => output.end_output(block),
            Statement::Comment(comment) => output.end_output(comment),
            Statement::Shebang(shebang) => output.end_text(shebang.as_str()),
            Statement::Error => output.error(span),
        }
    }
}

impl TextOutput for IterLoopVars {
    fn output(&self, span: &Span, output: &mut Output) {
        output.span(span);
    }
}

impl TextOutput for Block {
    fn output(&self, span: &Span, output: &mut Output) {
        // output.debug_point("Block").span(span);
        match self {
            Block::Block(modifiers, statements) => {
                output.char('{').increase_indentation().end_newline();

                for modifier in modifiers {
                    output.output(modifier).end_space();
                }
                for statement in statements {
                    output.output(statement).end_newline();
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

impl TextOutput for IfChainContent {
    fn output(&self, span: &Span, output: &mut Output) {
        output.span(span);
    }
}

impl TextOutput for ElseCondition {
    fn output(&self, span: &Span, output: &mut Output) {
        output.span(span);
    }
}

impl TextOutput for Comment {
    fn output(&self, span: &Span, output: &mut Output) {
        match self {
            Comment::Comment(comment) => output.end_comment("//", comment.as_str(), span),
            Comment::DocString(doc_comment) => {
                output
                    .text("///")
                    .space()
                    .end_comment("///", doc_comment.as_str(), span)
            }
        }
    }
}

impl TextOutput for IfCondition {
    fn output(&self, span: &Span, output: &mut Output) {
        match self {
            IfCondition::IfCondition(condition, block) => {
                output.output(condition).space().output(block).newline();
            }
            IfCondition::InlineIfCondition(condition, statement) => {
                output.output(condition).char(':').space().output(statement);
            }
            IfCondition::Comment(comment) => output.end_output(comment),
            IfCondition::Error => output.error(span),
        }
    }
}

impl TextOutput for VariableInitType {
    fn output(&self, span: &Span, output: &mut Output) {
        match self {
            VariableInitType::Expression(expr) => output.end_output(expr),
            VariableInitType::DataType(r#type) => output.end_output(r#type),
            VariableInitType::Error => output.error(span),
        }
    }
}

impl TextOutput for Expression {
    fn output(&self, span: &Span, output: &mut Output) {
        fn char_separated(
            output: &mut Output,
            rhs: &impl SpanTextOutput,
            middle: char,
            lhs: &impl SpanTextOutput,
        ) {
            output.output(rhs);
            output.space();
            output.char(middle);
            output.space();
            output.output(lhs);
        }

        fn string_separated(
            output: &mut Output,
            rhs: &impl SpanTextOutput,
            middle: &str,
            lhs: &impl SpanTextOutput,
        ) {
            output.output(rhs);
            output.space();
            output.text(middle);
            output.space();
            output.output(lhs);
        }

        fn output_separated(
            output: &mut Output,
            rhs: &impl SpanTextOutput,
            middle: &impl SpanTextOutput,
            lhs: &impl SpanTextOutput,
        ) {
            output.output(rhs);
            output.space();
            output.output(middle);
            output.space();
            output.output(lhs);
        }

        match self {
            Expression::Number(num) => {
                output.output(num);
            }
            Expression::Boolean(boolean) => {
                output.output(boolean);
            }
            Expression::Text(_) => {
                // Take raw text from file, as string content should not be modified
                output.end_span(span);
            }
            Expression::Parentheses(parentheses) => {
                output.output(parentheses);
            }
            Expression::Var(var) => output.end_output(var),
            Expression::Add(rhs, lhs) => char_separated(output, rhs, '+', lhs),
            Expression::Subtract(rhs, lhs) => char_separated(output, rhs, '-', lhs),
            Expression::Multiply(rhs, lhs) => char_separated(output, rhs, '*', lhs),
            Expression::Divide(rhs, lhs) => char_separated(output, rhs, '/', lhs),
            Expression::Modulo(rhs, lhs) => char_separated(output, rhs, '%', lhs),
            Expression::Neg(neg, lhs) => {
                output.output(neg);
                output.output(lhs);
            }
            Expression::And(rhs, and, lhs) => output_separated(output, rhs, and, lhs),
            Expression::Or(rhs, or, lhs) => output_separated(output, rhs, or, lhs),
            Expression::Gt(rhs, lhs) => char_separated(output, rhs, '>', lhs),
            Expression::Ge(rhs, lhs) => string_separated(output, rhs, ">=", lhs),
            Expression::Lt(rhs, lhs) => char_separated(output, rhs, '<', lhs),
            Expression::Le(rhs, lhs) => string_separated(output, rhs, ">=", lhs),
            Expression::Eq(rhs, lhs) => string_separated(output, rhs, "==", lhs),
            Expression::Neq(rhs, lhs) => string_separated(output, rhs, "!=", lhs),
            Expression::Not(not, lhs) => {
                output.output(not);
                output.space();
                output.output(lhs);
            }
            Expression::Ternary(condition, then, if_then, r#else, if_else) => {
                // TODO(tye-exe): Allow single line ternary if short enough. Use given span to measure length?
                output.increase_indentation();
                output.output(condition);
                output.newline();
                output.output(then);
                output.space();
                output.output(if_then);
                output.newline();
                output.output(r#else);
                output.space();
                output.output(if_else);
                output.decrease_indentation();
            }
            Expression::FunctionInvocation(modifiers, function_name, args, failure_handler) => {
                for modifier in modifiers {
                    output.output(modifier);
                    output.space();
                }

                output.output(function_name).char('(');

                for arg in args.iter().take(args.len().saturating_sub(1)) {
                    output.output(arg).char(',').space();
                }
                if let Some(arg) = args.last() {
                    output.output(arg);
                }

                output.char(')');

                if let Some(failure_handler) = failure_handler {
                    output.output(failure_handler);
                }
            }
            Expression::Command(modifiers, commands, failure_handler) => {
                for modifier in modifiers {
                    output.output(modifier);
                    output.space();
                }

                // Do not format bash commands
                for command in commands {
                    output.end_span(&command.1);
                }

                if let Some(failure_handler) = failure_handler {
                    output.space().output(failure_handler);
                }
            }
            Expression::Array(array) => {
                output.char('[');
                for expression in array {
                    output.output(expression);
                    output.char(',');
                    output.space();
                }
                output.remove_space();
                output.char(']');
            }
            Expression::Range(lhs, rhs) => {
                output.output(lhs);
                output.text("..");
                output.output(rhs);
            }
            Expression::Null => {
                output.text("Null");
            }
            Expression::Cast(lhs, r#as, rhs) => string_separated(output, rhs, &r#as.0, lhs),
            Expression::Status => {
                output.text("status");
            }
            Expression::Nameof(name_of, variable) => {
                output.output(name_of);
                output.space();
                output.output(variable);
            }
            Expression::Is(lhs, is, rhs) => {
                output.output(lhs).space().output(is).space().output(rhs);
            }
            Expression::ArrayIndex(array, index) => {
                output.output(array).char('[').output(index).end_char(']')
            }
            Expression::Exit(exit, value) => {
                output.output(exit);

                if let Some(value) = value {
                    output.space().output(value);
                }
            }
            Expression::Error => {
                output.error(span);
            }
        }
    }
}

impl TextOutput for FailureHandler {
    fn output(&self, _span: &Span, output: &mut Output) {
        match self {
            FailureHandler::Propagate => {
                output.char('?');
            }
            FailureHandler::Handle(failed, statements) => {
                output.output(failed);
                output.space();
                output.char('{');
                output.increase_indentation();
                output.newline();

                for statement in statements {
                    output.output(statement);
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

impl TextOutput for CommandModifier {
    fn output(&self, _span: &Span, output: &mut Output) {
        match self {
            CommandModifier::Unsafe => output.end_text("unsafe"),
            CommandModifier::Trust => output.end_text("trust"),
            CommandModifier::Silent => output.end_text("silent"),
            CommandModifier::Sudo => output.end_text("sudo"),
        }
    }
}

impl TextOutput for InterpolatedCommand {
    fn output(&self, _span: &Span, output: &mut Output) {
        match self {
            InterpolatedCommand::Escape(escape) => output
                .debug_point("InterpolatedCommand escape")
                .end_text(escape.as_str()),
            InterpolatedCommand::CommandOption(option) => output.end_text(option.as_str()),
            InterpolatedCommand::Expression(expr) => output.end_output(expr),
            InterpolatedCommand::Text(text) => output.end_text(text.as_str()),
        }
    }
}

impl TextOutput for f32 {}
impl TextOutput for bool {}
