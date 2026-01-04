use crate::format::{FmtContext, Output};
use amber_types::{Spanned, token::Span};

mod alpha040;
mod format;
mod fragments;

pub use format::format;

/// The content of an indentation.
const INDENT: &str = "    ";

const WHITESPACE: [char; 4] = [' ', '\n', '\r', '\t'];
const WHITESPACE_BYTES: [u8; 4] = {
    let mut array = [0; WHITESPACE.len()];
    let mut index = 0;

    while index < WHITESPACE.len() {
        array[index] = WHITESPACE[index] as u8;
        index += 1;
    }

    array
};

pub trait SpanTextOutput<T> {
    fn output(&self, output: &mut Output, ctx: &mut FmtContext<T>);
}

pub trait TextOutput<T> {
    /// Gets the formatted string representation of an AST element.
    /// The string representation should be written to the output buffer.
    #[allow(unused_variables)] // Not used in default impl.
    fn output(&self, span: &Span, output: &mut Output, ctx: &mut FmtContext<T>) {
        output.span(span);
    }
}

impl<D, T: TextOutput<D>> SpanTextOutput<D> for Spanned<T> {
    fn output(&self, output: &mut Output, ctx: &mut FmtContext<D>) {
        self.0.output(&self.1, output, ctx);
    }
}

impl<D, T: TextOutput<D>> SpanTextOutput<D> for Box<Spanned<T>> {
    fn output(&self, output: &mut Output, ctx: &mut FmtContext<D>) {
        self.0.output(&self.1, output, ctx);
    }
}
