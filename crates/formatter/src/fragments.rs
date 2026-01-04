use amber_types::token::Span;

#[derive(Debug)]
pub struct FragmentSpan {
    /// Start byte offset into source file.
    pub(crate) start_offset: usize,
    /// End byte offset into source file.
    pub(crate) end_offset: usize,
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
    pub fn denoted_by(self) -> &'static str {
        match self {
            CommentVariant::Doc => "///",
            CommentVariant::Regular => "//",
        }
    }
}
