use chumsky::span::SimpleSpan;
use std::fmt::{
    self,
    Display,
};

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);
pub type SpannedSemanticToken = Spanned<usize>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token(pub String);

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromIterator<Token> for String {
    fn from_iter<I: IntoIterator<Item = Token>>(iter: I) -> Self {
        iter.into_iter().map(|t| t.0).collect()
    }
}

#[macro_export]
macro_rules! T {
    [$text:expr] => {
        $crate::token::Token($text.to_string())
    };
}
