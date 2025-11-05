use crate::SpanTextOutput;
use lib::{
    analysis::types::DataType,
    grammar::{
        CompilerFlag,
        alpha040::{FunctionArgument, GlobalStatement, ImportContent},
    },
};

use crate::{Output, TextOutput};

impl TextOutput for GlobalStatement {
    fn output(&self, span: &lib::grammar::Span, output: &mut Output) {
        match self {
            GlobalStatement::Import(public, import, content, from, path) => {
                if public.0 {
                    output.push_text("pub ");
                }

                output.push_output(import);
                output.push_space();
                output.push_output(content);
                output.push_space();
                output.push_output(from);
                output.push_space();
                output.push_output(path);
                output.push_newline();
            }
            GlobalStatement::FunctionDefinition(
                compiler_flags,
                public,
                function_keyword,
                name,
                args,
                return_type,
                content,
            ) => {
                for flag in compiler_flags {
                    output.push_output(flag);
                    output.push_newline();
                }

                if public.0 {
                    output.push_text("pub ");
                }

                output.push_output(function_keyword);
                output.push_space();
                output.push_output(name);

                output.push_char('(');
                // Handle adding variables with proper spacing
                {
                    for arg in args.iter().take(args.len().saturating_sub(1)) {
                        output.push_output(arg);
                        output.push_char(',');
                        output.push_space();
                    }

                    if let Some(arg) = args.last() {
                        output.push_output(arg);
                    }
                }
                output.push_char(')');

                if let Some(returns) = return_type {
                    output.push_char(':');
                    output.push_space();
                    output.push_output(returns);
                }

                output.push_text("{ Null }");
                output.push_newline();
            }
            GlobalStatement::Main(_, _, items) => {}
            GlobalStatement::Statement(_) => {}
        }
    }
}

impl TextOutput for ImportContent {
    fn output(&self, span: &lib::grammar::Span, output: &mut Output) {
        match self {
            ImportContent::ImportAll => output.push_char('*'),
            ImportContent::ImportSpecific(items) => {
                output.push_text("{ ");
                for identifier in items {
                    output.push_output(identifier);
                    output.push_space();
                }
                output.push_char('}');
            }
        }
    }
}

impl TextOutput for FunctionArgument {
    fn output(&self, span: &lib::grammar::Span, output: &mut Output) {
        fn push_arg(output: &mut Output, is_ref: bool, text: &impl SpanTextOutput) {
            if is_ref {
                output.push_text("ref");
                output.push_space();
            }
            output.push_output(text);
        }

        match self {
            FunctionArgument::Generic(is_ref, text) => push_arg(output, is_ref.0, text),
            FunctionArgument::Optional(is_ref, text, _, _) => push_arg(output, is_ref.0, text),
            FunctionArgument::Typed(is_ref, text, _) => push_arg(output, is_ref.0, text),
            FunctionArgument::Error => output.push_span(span),
        }
    }
}

impl TextOutput for CompilerFlag {
    fn output(&self, span: &lib::grammar::Span, output: &mut Output) {
        output.push_text(format!("#[{self}]"));
    }
}

impl TextOutput for String {
    fn output(&self, span: &lib::grammar::Span, output: &mut Output) {
        output.push_text(self.clone());
    }
}

impl TextOutput for DataType {}
