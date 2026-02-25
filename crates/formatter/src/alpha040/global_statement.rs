use crate::{
    TextOutput,
    alpha040::Gen,
    format::{FmtContext, Output},
};
use amber_grammar::{
    Span,
    alpha040::{GlobalStatement, Statement},
};

impl TextOutput<Gen> for GlobalStatement {
    fn output(&self, span: &Span, output: &mut Output, ctx: &mut FmtContext<Gen>) {
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

                if let Some(next_global) = ctx.next_global()
                    && matches!(next_global.0, GlobalStatement::Import(..))
                {
                    ctx.allow_newline(output, span.end..=next_global.1.start);
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
                if ctx.previous_global().is_some() {
                    output.newline();
                }

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
                if ctx.previous_global().is_some() {
                    output.newline();
                }

                output.output(ctx, main);

                if let Some(args) = args {
                    output.char('(').output(ctx, args).char(')');
                }
                output.space().char('{').increase_indentation().newline();

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

                if let Some(next_global) = ctx.next_global()
                    && matches!(next_global.0, GlobalStatement::Statement(..))
                {
                    ctx.allow_newline(output, span.end..=next_global.1.start);
                }
            }
        }
    }
}
