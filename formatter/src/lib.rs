use lib::grammar::{Span, Spanned};
use std::string::FromUtf8Error;

mod alpha040;

#[derive(Default)]
pub struct Output {
    buffer: Vec<Fragment>,
}

pub enum Fragment {
    Space,
    Newline,
    Indentation,
    Text(Box<str>),
    Span {
        /// Start byte offset into source file.
        start_offset: usize,
        /// End byte offset into source file.
        end_offset: usize,
    },
}

pub trait SpanTextOutput {
    fn output(&self, output: &mut Output);
}

pub trait TextOutput {
    /// Gets the formatted string representation of an AST element.
    /// The string representation should be written to the output buffer.
    ///
    /// It is the responsibility of the caller to ensure that the buffer is in the correct state to
    /// have text appended. E.G. The buffer is at the start of a new line.
    ///
    /// It is the responsibility of the function implementation to add a space to the end of the
    /// buffer before returning.
    fn output(&self, span: &Span, output: &mut Output) {
        output.push_span(span);
    }
}

#[derive(thiserror::Error, Debug)]
pub enum FormattingError {
    /// The span does not exist within the source file.
    #[error("Invalid span. Starts: {start}; Ends: {end}")]
    SpanDoesntExist { start: usize, end: usize },
    /// The span cannot be converted into UTF8 text.
    #[error(transparent)]
    InvalidSpan(#[from] FromUtf8Error),
}

impl<T: TextOutput> SpanTextOutput for Spanned<T> {
    fn output(&self, output: &mut Output) {
        self.0.output(&self.1, output);
    }
}

impl Output {
    fn push_space(&mut self) {
        self.buffer.push(Fragment::Space);
    }

    fn push_newline(&mut self) {
        self.buffer.push(Fragment::Newline);
    }

    fn push_indentation(&mut self) {
        todo!()
    }

    fn push_text(&mut self, text: impl Into<Box<str>>) {
        self.buffer.push(Fragment::Text(text.into()));
    }

    fn push_char(&mut self, character: char) {
        self.buffer
            .push(Fragment::Text(character.to_string().into_boxed_str()));
    }

    fn push_output<TOutput>(&mut self, output: &TOutput)
    where
        TOutput: SpanTextOutput,
    {
        output.output(self);
    }

    fn push_span(&mut self, span: &Span) {
        self.buffer.push(Fragment::Span {
            start_offset: span.start,
            end_offset: span.end,
        });
    }

    pub fn format(self, file_content: &str) -> Result<String, FormattingError> {
        let mut text = String::new();

        for fragment in self.buffer {
            match fragment {
                Fragment::Space => text.push(' '),
                Fragment::Newline => text.push('\n'),
                Fragment::Indentation => text.push_str("    "),
                Fragment::Text(frag_text) => text.push_str(&frag_text),
                Fragment::Span {
                    start_offset,
                    end_offset,
                } => {
                    let start = start_offset.min(end_offset);
                    let end = start_offset.max(end_offset);

                    let span = file_content
                        .as_bytes()
                        .get(start..=end)
                        .ok_or_else(|| FormattingError::SpanDoesntExist { start, end })?;

                    let span_text = String::from_utf8(span.to_vec())?;
                    text.push_str(&span_text);
                }
            }
        }

        Ok(text)
    }
}
