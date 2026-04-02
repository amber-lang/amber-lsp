use crate::{
    SpanTextOutput, TextOutput,
    alpha040::Gen,
    format::{FmtContext, Output},
    line_wrapping::Wrap,
};
use amber_grammar::{Span, alpha040::Expression};

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
            output.space(Wrap::LAST);
            output.char(middle);
            output.space(Wrap::LAST);
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
            output.space(Wrap::LAST);
            output.text(middle);
            output.space(Wrap::LAST);
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
            output.space(Wrap::LAST);
            output.output(ctx, middle);
            output.space(Wrap::LAST);
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
                output.space(Wrap::LAST);
                output.output(ctx, lhs);
            }
            Expression::Ternary(condition, then, if_then, r#else, if_else) => {
                output.increase_indentation();
                output.output(ctx, condition);
                output.space(Wrap::WITH_FIRST);
                output.output(ctx, then);
                output.space(Wrap::NEVER);
                output.output(ctx, if_then);
                output.space(Wrap::WITH_FIRST);
                output.output(ctx, r#else);
                output.space(Wrap::NEVER);
                output.output(ctx, if_else);
                output.newline();
                output.decrease_indentation();
            }
            Expression::FunctionInvocation(modifiers, function_name, args, failure_handler) => {
                for modifier in modifiers {
                    output.output(ctx, modifier);
                    output.space(Wrap::LAST);
                }

                output
                    .output(ctx, function_name)
                    .char('(')
                    .wrap(Wrap::WITH_MIDDLE);

                for arg in args.iter().take(args.len().saturating_sub(1)) {
                    output.output(ctx, arg).char(',').space(Wrap::WITH_MIDDLE);
                }
                if let Some(arg) = args.last() {
                    output.output(ctx, arg);
                }

                output.wrap(Wrap::WITH_MIDDLE).char(')');

                if let Some(failure_handler) = failure_handler {
                    output.output(ctx, failure_handler);
                }
            }
            Expression::Command(modifiers, commands, failure_handler) => {
                for modifier in modifiers {
                    output.output(ctx, modifier);
                    output.space(Wrap::LAST);
                }

                // Do not format bash commands
                for command in commands {
                    output.end_span(&command.1);
                }

                if let Some(failure_handler) = failure_handler {
                    output.space(Wrap::NEVER).output(ctx, failure_handler);
                }
            }
            Expression::Array(array) => {
                output.char('[').wrap(Wrap::WITH_FIRST);
                for expression in array {
                    output
                        .output(ctx, expression)
                        .char(',')
                        .space(Wrap::WITH_FIRST);
                }
                output.remove_space().wrap(Wrap::WITH_FIRST).char(']');
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
                output.space(Wrap::NEVER);
                output.output(ctx, variable);
            }
            Expression::Is(lhs, is, rhs) => {
                output
                    .output(ctx, lhs)
                    .space(Wrap::NEVER)
                    .output(ctx, is)
                    .space(Wrap::NEVER)
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
                    output.space(Wrap::NEVER).output(ctx, value);
                }
            }
            Expression::Error => {
                output.error(span);
            }
        }
    }
}
