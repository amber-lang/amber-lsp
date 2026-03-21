use crate::{
    SpanTextOutput, TextOutput,
    alpha040::Gen,
    format::{FmtContext, Output},
    line_wrapping::Wrap,
};
use amber_grammar::{Span, alpha040::Statement};

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
                .space(Wrap::LAST)
                .text(separator)
                .space(Wrap::LAST)
                .output(ctx, expression);
        }

        match self {
            Statement::Expression(expression) => {
                output.output(ctx, expression);
            }
            Statement::VariableInit(keyword, name, init) => {
                output
                    .output(ctx, keyword)
                    .space(Wrap::NEVER)
                    .output(ctx, name)
                    .space(Wrap::NEVER)
                    .char('=')
                    .space(Wrap::NEVER)
                    .output(ctx, init);
            }
            Statement::ConstInit(keyword, name, init) => {
                output
                    .output(ctx, keyword)
                    .space(Wrap::NEVER)
                    .output(ctx, name)
                    .space(Wrap::NEVER)
                    .char('=')
                    .space(Wrap::NEVER)
                    .output(ctx, init);
            }
            Statement::VariableSet(name, new_value) => {
                output
                    .output(ctx, name)
                    .space(Wrap::NEVER)
                    .char('=')
                    .space(Wrap::NEVER)
                    .output(ctx, new_value);
            }
            Statement::IfCondition(r#if, condition, comments, else_condition) => {
                output
                    .output(ctx, r#if)
                    .space(Wrap::NEVER)
                    .output(ctx, condition);

                if let Some(comment) = comments.first() {
                    ctx.allow_newline(output, condition.1.end..=comment.1.start);
                    output.output(ctx, comment);
                }

                for comment in comments.iter().skip(1) {
                    output.output(ctx, comment);
                }

                if let Some(else_condition) = else_condition {
                    output.space(Wrap::NEVER).output(ctx, else_condition);
                }
            }
            Statement::IfChain(r#if, items) => {
                output
                    .output(ctx, r#if)
                    .space(Wrap::NEVER)
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
                output
                    .output(ctx, r#loop)
                    .space(Wrap::NEVER)
                    .output(ctx, block);
            }
            Statement::IterLoop(r#for, element, r#in, expr, block) => {
                output
                    .output(ctx, r#for)
                    .space(Wrap::NEVER)
                    .output(ctx, element)
                    .space(Wrap::NEVER)
                    .output(ctx, r#in)
                    .space(Wrap::NEVER)
                    .output(ctx, expr)
                    .space(Wrap::NEVER)
                    .end_output(ctx, block);
            }
            Statement::Break => output.end_text("break"),
            Statement::Continue => output.end_text("continue"),
            Statement::Return(r#return, expr) => {
                output.end_output(ctx, r#return);

                if let Some(expr) = expr {
                    output.space(Wrap::NEVER).end_output(ctx, expr);
                }
            }
            Statement::Fail(fail, expr) => {
                output.end_output(ctx, fail);

                if let Some(expr) = expr {
                    output.space(Wrap::NEVER).end_output(ctx, expr);
                }
            }
            Statement::Echo(echo, text) => output
                .output(ctx, echo)
                .space(Wrap::NEVER)
                .end_output(ctx, text),
            Statement::Cd(cd, text) => output
                .output(ctx, cd)
                .space(Wrap::NEVER)
                .end_output(ctx, text),
            Statement::MoveFiles(modifiers, mv, source, destination, failure_handler) => {
                for modifier in modifiers {
                    output.output(ctx, modifier).space(Wrap::NEVER);
                }

                output
                    .output(ctx, mv)
                    .space(Wrap::WITH_LOW_MIDDLE)
                    .output(ctx, source)
                    .space(Wrap::WITH_LOW_MIDDLE)
                    .output(ctx, destination);

                if let Some(failure_handler) = failure_handler {
                    output
                        .space(Wrap::WITH_LOW_MIDDLE)
                        .output(ctx, failure_handler);
                }
            }
            Statement::Block(block) => output.end_output(ctx, block),
            Statement::Comment(comment) => output.end_output(ctx, comment),
            Statement::Shebang(shebang) => output.end_text(shebang.as_str()),
            Statement::Error => output.error(span),
        }
    }
}
