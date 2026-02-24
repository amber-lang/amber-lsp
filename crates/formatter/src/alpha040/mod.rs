use crate::{FmtContext, Output, SpanTextOutput, TextOutput, fragments::CommentVariant};
use amber_grammar::{
    CommandModifier, CompilerFlag,
    alpha040::{
        Block, Comment, ElseCondition, FailureHandler, FunctionArgument, GlobalStatement,
        IfChainContent, IfCondition, ImportContent, InterpolatedCommand, IterLoopVars,
        VariableInitType,
    },
};
use amber_types::{DataType, token::Span};

mod expression;
mod global_statement;
mod statement;

type Gen = (GlobalStatement, Span);

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

impl TextOutput<Gen> for IterLoopVars {
    fn output(&self, span: &Span, output: &mut Output, _ctx: &mut FmtContext<Gen>) {
        output.span(span);
    }
}

impl TextOutput<Gen> for Block {
    fn output(&self, span: &Span, output: &mut Output, ctx: &mut FmtContext<Gen>) {
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
                output.output(ctx, condition).space().end_output(ctx, block)
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
impl TextOutput<Gen> for DataType {}
