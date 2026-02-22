use super::{INDENT, SpanTextOutput, WHITESPACE_BYTES};
use crate::{
    WHITESPACE,
    fragments::{CommentVariant, Fragment, Indentation},
};
use amber_grammar::alpha040::GlobalStatement;
use amber_types::token::Span;
use std::{ops::RangeBounds, panic::Location, string::FromUtf8Error};

/// Formats the file.
///
/// items is the parsed file content.
/// file_content is the raw file content.
pub fn format(
    items: &[(GlobalStatement, Span)],
    file_content: &str,
) -> Result<String, FormattingError> {
    // eprintln!("{items:?}");
    let mut index = 0;

    let mut output = Output::new();
    let consecutive_newlines = consecutive_newlines(file_content);
    let source_newlines = file_content
        .bytes()
        .enumerate()
        .filter_map(|(index, byte)| if byte == b'\n' { Some(index) } else { None })
        .collect();

    // eprintln!("{consecutive_newlines:?}");
    let mut ctx = FmtContext {
        items,
        index,
        consecutive_newlines,
        source_newlines,
    };

    while let Some(item) = items.get(index) {
        index += 1;
        item.output(&mut output, &mut ctx);
        ctx.increment();
    }

    // eprintln!("{output:?}");
    output.format(file_content)
}

/// Index of newlines in the content only separated by whitepspace (ignoring the first newline in a consecutive sequence).
fn consecutive_newlines(file_content: &str) -> Vec<usize> {
    // amber-lsp parses by bytes, not by chars
    let char_indices: Vec<u8> = file_content.bytes().collect();
    let mut newlines = Vec::new();
    let mut index = 0;

    while index < char_indices.len() {
        let character = char_indices.get(index).unwrap();
        index += 1;

        if *character != b'\n' {
            continue;
        }

        // Finds subsequent newline chars
        while index < char_indices.len() {
            let character = char_indices.get(index).unwrap();
            index += 1;

            match *character {
                b' ' | b'\t' | b'\r' => {
                    continue;
                }
                b'\n' => {
                    newlines.push(index - 1);
                }
                _ => {
                    break;
                }
            }
        }
    }

    newlines
}

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
    /// The byte index of consecutive newlines in the source file.
    consecutive_newlines: Vec<usize>,
    /// Byte index of newlines in the source file.
    source_newlines: Box<[usize]>,
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

    pub fn allow_newline<R: RangeBounds<usize>>(&self, output: &mut Output, range: R) {
        if self
            .consecutive_newlines
            .iter()
            .any(|newline| range.contains(&(newline + 2)))
        {
            output.end_newline();
        }
    }

    pub fn source_has_newline<R: RangeBounds<usize>>(&self, range: R) -> bool {
        self.source_newlines
            .iter()
            .any(|newline| range.contains(newline))
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
    pub fn new() -> Self {
        Self { buffer: Vec::new() }
    }

    #[track_caller]
    pub(crate) fn debug_point(&mut self, info: &str) -> &mut Self {
        self.text(format!(
            "%%Debug point : {info} : at {}%%",
            Location::caller()
        ))
    }

    pub(crate) fn error(&mut self, span: &Span) {
        self.span(span);
    }

    pub(crate) fn space(&mut self) -> &mut Self {
        self.buffer.push(Fragment::Space);
        self
    }

    pub(crate) fn newline(&mut self) -> &mut Self {
        self.buffer.push(Fragment::Newline);
        self
    }

    pub(crate) fn text(&mut self, text: impl Into<Box<str>>) -> &mut Self {
        self.buffer.push(Fragment::Text(text.into()));
        self
    }

    pub(crate) fn comment(
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

    pub(crate) fn char(&mut self, character: char) -> &mut Self {
        self.buffer
            .push(Fragment::Text(character.to_string().into_boxed_str()));
        self
    }

    pub(crate) fn output<T, TOutput>(
        &mut self,
        ctx: &mut FmtContext<T>,
        output: &TOutput,
    ) -> &mut Self
    where
        TOutput: SpanTextOutput<T>,
    {
        output.output(self, ctx);
        self
    }

    pub(crate) fn span(&mut self, span: &Span) -> &mut Self {
        self.buffer.push(Fragment::Span(span.into()));
        self
    }

    pub(crate) fn end_space(&mut self) {
        self.buffer.push(Fragment::Space);
    }

    pub(crate) fn end_newline(&mut self) {
        self.buffer.push(Fragment::Newline);
    }

    pub(crate) fn end_text(&mut self, text: impl Into<Box<str>>) {
        self.buffer.push(Fragment::Text(text.into()));
    }

    pub(crate) fn end_comment(
        &mut self,
        variant: CommentVariant,
        text: impl Into<Box<str>>,
        span: &Span,
    ) {
        self.buffer.push(Fragment::Comment {
            variant,
            text: text.into(),
            start_index: span.start,
        });
    }

    pub(crate) fn end_char(&mut self, character: char) {
        self.buffer
            .push(Fragment::Text(character.to_string().into_boxed_str()));
    }

    pub(crate) fn end_output<T, TOutput>(&mut self, ctx: &mut FmtContext<T>, output: &TOutput)
    where
        TOutput: SpanTextOutput<T>,
    {
        output.output(self, ctx);
    }

    pub(crate) fn end_span(&mut self, span: &Span) {
        self.span(span);
    }

    pub(crate) fn increase_indentation(&mut self) -> &mut Self {
        self.buffer
            .push(Fragment::IndentationChange(Indentation::Increase));
        self
    }

    pub(crate) fn decrease_indentation(&mut self) -> &mut Self {
        self.buffer
            .push(Fragment::IndentationChange(Indentation::Decrease));
        self
    }

    /// Removes the last fragment from the buffer if it is a space.
    pub(crate) fn remove_space(&mut self) -> &mut Self {
        self.buffer.pop_if(|last| matches!(last, Fragment::Space));
        self
    }

    /// Removes the last fragment from the buffer if it is a newline.
    pub(crate) fn remove_newline(&mut self) -> &mut Self {
        self.buffer.pop_if(|last| matches!(last, Fragment::Newline));
        self
    }

    /// Removes the any trailing whitespace fragments.
    ///
    /// Indentation changes are ignored but still preserved.
    pub(crate) fn remove_trailing_whitespace(&mut self) -> &mut Self {
        let mut indentation = Vec::new();

        let is_text = |last: &mut Fragment| {
            matches!(
                last,
                Fragment::IndentationChange(..) | Fragment::Space | Fragment::Newline
            )
        };

        while let Some(fragment) = self.buffer.pop_if(is_text) {
            if matches!(fragment, Fragment::IndentationChange(..)) {
                indentation.push(fragment);
            }
        }

        self.buffer.append(&mut indentation);
        self
    }

    fn format(self, file_content: &str) -> Result<String, FormattingError> {
        let mut text = String::new();
        let mut indentation = String::new();
        let mut consecutive_newlines = 0;

        let mut iter = self.buffer.into_iter().peekable();
        while let Some(fragment) = iter.next() {
            if let Fragment::Newline = fragment {
                consecutive_newlines += 1;
            } else {
                consecutive_newlines = 0;
            }

            match fragment {
                Fragment::Space => text.push(' '),
                Fragment::Newline => {
                    if consecutive_newlines > 2 {
                        continue;
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_newline() {
        let newlines = "Some\nText\nWith\n\nNewlines";
        let consecutive_newlines = consecutive_newlines(newlines);
        assert_eq!(consecutive_newlines.as_slice(), &[15]);
    }

    #[test]
    fn multiple_newline() {
        let newlines = "Some\nText\nWith\n\n\nNewlines\n\nMultiple";
        let consecutive_newlines = consecutive_newlines(newlines);
        assert_eq!(consecutive_newlines.as_slice(), &[15, 16, 26]);
    }

    #[test]
    fn multiple_newline_whitespace() {
        let newlines = "Some\nText\nWith\n  \n  \n  Newlines\n\t\nMultiple";
        let consecutive_newlines = consecutive_newlines(newlines);
        assert_eq!(consecutive_newlines.as_slice(), &[17, 20, 33]);
    }
}
