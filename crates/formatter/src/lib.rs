use amber_grammar::alpha040::GlobalStatement;
use amber_types::{Spanned, token::Span};
use std::{panic::Location, string::FromUtf8Error};

mod alpha040;

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

/// Contains string fragments of the code ready for finial formatting into a string.
#[derive(Default, Debug)]
pub struct Output {
    buffer: Vec<Fragment>,
}

/// Context about the general structure of the program to use during formatting.
pub struct FmtContext<'a, T> {
    /// The parsed top level statements of the program.
    items: &'a [T],
    /// The location of the statement currently being formatted.
    index: usize,
}

impl<'a, T> FmtContext<'a, T> {
    /// Moves the context to the next top level statement.
    fn increment(&mut self) {
        self.index += 1;
    }

    /// Returns the previous top level statement if it exists.
    pub fn previous_global(&self) -> Option<&T> {
        self.items.get(self.index.checked_sub(1)?)
    }

    /// Returns the next top level statement if it exists.
    pub fn next_global(&self) -> Option<&T> {
        self.items.get(self.index.checked_add(1)?)
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Fragment {
    Space,
    Newline,
    /// Denotes an indentation change.
    /// This has no output in its current position, but will change the indentation after every newline.
    IndentationChange(Indentation),
    Text(Box<str>),
    Comment {
        /// Dentoes the start of a comment. E.G "//" or "///".
        variant: CommentVariant,
        text: Box<str>,
        /// Byte index of the source file where the comment starts.
        start_index: usize,
    },
    Span(FragmentSpan),
    ParseError(FragmentSpan),
}

#[derive(Debug)]
pub enum Indentation {
    Increase,
    Decrease,
}

/// Comments can either be doc comments or regular comments.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CommentVariant {
    /// Doc comment
    Doc,
    /// Regular comment
    Regular,
}

impl CommentVariant {
    /// Returns the string that this comment variant is denoted by.
    fn denoted_by(self) -> &'static str {
        match self {
            CommentVariant::Doc => "///",
            CommentVariant::Regular => "//",
        }
    }
}

pub trait SpanTextOutput<T> {
    fn output(&self, output: &mut Output, ctx: &mut FmtContext<T>);
}

pub trait TextOutput<T> {
    /// Gets the formatted string representation of an AST element.
    /// The string representation should be written to the output buffer.
    ///
    /// It is the responsibility of the caller to ensure that the buffer is in the correct state to
    /// have text appended. E.G. The buffer is at the start of a new line.
    ///
    /// It is the responsibilirt of the caller to append a space to the buffer if required.
    /// An implementation should not end with adding a space.
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

#[derive(thiserror::Error, Debug)]
pub enum FormattingError {
    /// The span does not exist within the source file.
    #[error("Invalid span. Starts: {start}; Ends: {end}")]
    SpanDoesntExist { start: usize, end: usize },
    /// The span cannot be converted into UTF8 text.
    #[error(transparent)]
    InvalidSpan(#[from] FromUtf8Error),
}

impl Output {
    pub fn parse(items: &[(GlobalStatement, Span)]) -> Self {
        let mut index = 0;

        let mut output = Output::default();
        let mut ctx = FmtContext { items, index };

        while let Some(item) = items.get(index) {
            index += 1;
            item.output(&mut output, &mut ctx);
            ctx.increment();
        }

        output
    }

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

    fn comment(
        &mut self,
        variant: CommentVariant,
        text: impl Into<Box<str>>,
        span: &Span,
    ) -> &mut Self {
        self.buffer.push(Fragment::Comment {
            variant,
            text: text.into(),
            start_index: span.start,
        });
        self
    }

    fn char(&mut self, character: char) -> &mut Self {
        self.buffer
            .push(Fragment::Text(character.to_string().into_boxed_str()));
        self
    }

    fn output<T, TOutput>(&mut self, ctx: &mut FmtContext<T>, output: &TOutput) -> &mut Self
    where
        TOutput: SpanTextOutput<T>,
    {
        output.output(self, ctx);
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

    fn end_comment(&mut self, variant: CommentVariant, text: impl Into<Box<str>>, span: &Span) {
        self.buffer.push(Fragment::Comment {
            variant,
            text: text.into(),
            start_index: span.start,
        });
    }

    fn end_char(&mut self, character: char) {
        self.buffer
            .push(Fragment::Text(character.to_string().into_boxed_str()));
    }

    fn end_output<T, TOutput>(&mut self, ctx: &mut FmtContext<T>, output: &TOutput)
    where
        TOutput: SpanTextOutput<T>,
    {
        output.output(self, ctx);
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

        let mut iter = self.buffer.into_iter().peekable();
        while let Some(fragment) = iter.next() {
            match fragment {
                Fragment::Space => text.push(' '),
                Fragment::Newline => {
                    // Don't add newline if comment is on sameline
                    if let Some(Fragment::Comment {
                        variant: _,
                        text: _,
                        start_index,
                    }) = iter.peek()
                    {
                        let mut index = *start_index;
                        let mut on_newline = false;

                        while let Some(character) = file_content.as_bytes().get(index).cloned() {
                            index -= 1;
                            let character: char = character.into();

                            if character == '\n' || character == '\r' {
                                on_newline = true;
                                break;
                            }

                            if ![' ', '\t', '/'].contains(&character) {
                                break;
                            }
                        }

                        if !on_newline {
                            continue;
                        }
                    }

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
                Fragment::Comment {
                    variant,
                    text: comment,
                    start_index: _,
                } => {
                    // Ensure that there is a space between the comment and previous code
                    if let Some(previous) = text.as_bytes().last()
                        && !WHITESPACE_BYTES.contains(previous)
                    {
                        text.push(' ');
                    }

                    text.push_str(variant.denoted_by());
                    text.push(' ');
                    text.push_str(&comment);

                    if let Some(Fragment::Comment { variant, .. }) = iter.peek()
                        && *variant == CommentVariant::Doc
                        && matches!(variant, CommentVariant::Doc)
                    {
                        text.push('\n');
                    }
                }
            }
        }

        Ok(text)
    }
}
