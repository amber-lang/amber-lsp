use lib::grammar::{Span, Spanned};
use std::{panic::Location, string::FromUtf8Error};

mod alpha040;

/// The content of an indentation.
const INDENT: &str = "    ";

#[derive(Default)]
pub struct Output {
    buffer: Vec<Fragment>,
}

pub struct FragmentSpan {
    /// Start byte offset into source file.
    start_offset: usize,
    /// End byte offset into source file.
    end_offset: usize,
}

impl FragmentSpan {
    pub fn new(start_offset: usize, end_offset: usize) -> Self {
        Self {
            start_offset: start_offset.min(end_offset),
            end_offset: start_offset.max(end_offset),
        }
    }

    pub fn end_offset(&self) -> usize {
        self.end_offset
    }

    pub fn start_offset(&self) -> usize {
        self.start_offset
    }
}

impl From<&Span> for FragmentSpan {
    fn from(value: &Span) -> Self {
        FragmentSpan {
            start_offset: value.start,
            end_offset: value.end.saturating_sub(1),
        }
    }
}

pub enum Fragment {
    Space,
    Newline,
    /// Denotes an indentation change.
    /// This has no output in its current position, but will change the indentation after every newline.
    IndentationChange(Indentation),
    Text(Box<str>),
    Span(FragmentSpan),
    ParseError(FragmentSpan),
}

pub enum Indentation {
    Increase,
    Decrease,
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
    /// It is the responsibilirt of the caller to append a space to the buffer if required.
    /// An implementation should not end with adding a space.
    fn output(&self, span: &Span, output: &mut Output) {
        output.span(span);
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

impl<T: TextOutput> SpanTextOutput for Box<Spanned<T>> {
    fn output(&self, output: &mut Output) {
        self.0.output(&self.1, output);
    }
}

impl Output {
    #[track_caller]
    fn debug_point(&mut self, info: &str) -> &mut Self {
        self.text(format!(
            "%%Debug point : {info} : at {}%%",
            Location::caller()
        ))
    }

    fn error(&mut self, span: &Span) {
        self.span(span);
    }

    fn space(&mut self) -> &mut Self {
        self.buffer.push(Fragment::Space);
        self
    }

    fn newline(&mut self) -> &mut Self {
        self.buffer.push(Fragment::Newline);
        self
    }

    fn text(&mut self, text: impl Into<Box<str>>) -> &mut Self {
        self.buffer.push(Fragment::Text(text.into()));
        self
    }

    fn char(&mut self, character: char) -> &mut Self {
        self.buffer
            .push(Fragment::Text(character.to_string().into_boxed_str()));
        self
    }

    fn output<TOutput>(&mut self, output: &TOutput) -> &mut Self
    where
        TOutput: SpanTextOutput,
    {
        output.output(self);
        self
    }

    fn span(&mut self, span: &Span) -> &mut Self {
        self.buffer.push(Fragment::Span(span.into()));
        self
    }

    fn end_space(&mut self) {
        self.buffer.push(Fragment::Space);
    }

    fn end_newline(&mut self) {
        self.buffer.push(Fragment::Newline);
    }

    fn end_text(&mut self, text: impl Into<Box<str>>) {
        self.buffer.push(Fragment::Text(text.into()));
    }

    fn end_char(&mut self, character: char) {
        self.buffer
            .push(Fragment::Text(character.to_string().into_boxed_str()));
    }

    fn end_output<TOutput>(&mut self, output: &TOutput)
    where
        TOutput: SpanTextOutput,
    {
        output.output(self);
    }

    fn end_span(&mut self, span: &Span) {
        self.span(span);
    }

    fn increase_indentation(&mut self) -> &mut Self {
        self.buffer
            .push(Fragment::IndentationChange(Indentation::Increase));
        self
    }

    fn decrease_indentation(&mut self) -> &mut Self {
        self.buffer
            .push(Fragment::IndentationChange(Indentation::Decrease));
        self
    }

    /// Removes the last fragment from the buffer if it is a space.
    fn remove_space(&mut self) -> &mut Self {
        self.buffer.pop_if(|last| matches!(last, Fragment::Space));
        self
    }

    /// Removes the last fragment from the buffer if it is a newline.
    fn remove_newline(&mut self) -> &mut Self {
        self.buffer.pop_if(|last| matches!(last, Fragment::Newline));
        self
    }

    pub fn format(self, file_content: &str) -> Result<String, FormattingError> {
        let mut text = String::new();
        let mut indentation = String::new();

        for fragment in self.buffer {
            match fragment {
                Fragment::Space => text.push(' '),
                Fragment::Newline => {
                    text.push('\n');
                    text.push_str(&indentation);
                }
                Fragment::IndentationChange(indent) => match indent {
                    Indentation::Increase => indentation += INDENT,
                    Indentation::Decrease => {
                        indentation.truncate(indentation.len().saturating_sub(INDENT.len()));
                    }
                },
                Fragment::Text(frag_text) => text.push_str(&frag_text),
                Fragment::Span(span) => {
                    let span = file_content
                        .as_bytes()
                        .get(span.start_offset()..=span.end_offset())
                        .ok_or_else(|| FormattingError::SpanDoesntExist {
                            start: span.start_offset(),
                            end: span.end_offset(),
                        })?;

                    let span_text = String::from_utf8(span.to_vec())?;
                    text.push_str(&span_text);
                }
                Fragment::ParseError(span) => {
                    let span = file_content
                        .as_bytes()
                        .get(span.start_offset()..=span.end_offset())
                        .ok_or_else(|| FormattingError::SpanDoesntExist {
                            start: span.start_offset(),
                            end: span.end_offset(),
                        })?;

                    let span_text = String::from_utf8(span.to_vec())?;
                    eprintln!("Unable to parse '{span_text}'. Failing back to sourcefile content");
                    text.push_str(&span_text);
                }
            }
        }

        Ok(text)
    }
}
