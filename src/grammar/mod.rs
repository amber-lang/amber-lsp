use chumsky::error::Rich;
use chumsky::span::SimpleSpan;
use std::fmt::{
    self,
    Debug,
    Display,
};

pub mod alpha034;
pub mod alpha035;
pub mod alpha040;
pub mod alpha050;

#[derive(Debug, PartialEq, Clone)]
pub enum Grammar {
    Alpha034(Option<Vec<Spanned<alpha034::GlobalStatement>>>),
    Alpha035(Option<Vec<Spanned<alpha035::GlobalStatement>>>),
    Alpha040(Option<Vec<Spanned<alpha040::GlobalStatement>>>),
    Alpha050(Option<Vec<Spanned<alpha050::GlobalStatement>>>),
}

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
        Token($text.to_string())
    };
}

pub struct ParserResponse<'a> {
    pub ast: Grammar,
    pub errors: Vec<Rich<'a, String>>,
    pub semantic_tokens: Vec<SpannedSemanticToken>,
}

pub trait LSPAnalysis: Sync + Send + Debug {
    fn tokenize(&self, input: &str) -> Vec<Spanned<Token>>;
    fn parse<'a>(&self, input: &'a [Spanned<Token>]) -> ParserResponse<'a>;
}

#[derive(PartialEq)]
pub enum JumpDefinitionResult {
    InFile(Span),
    OpenFile(String),
    None,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum CommandModifier {
    Unsafe,
    Trust,
    Silent,
}

#[derive(PartialEq, Debug, Clone, Eq)]
pub enum CompilerFlag {
    AllowNestedIfElse,
    AllowGenericReturn,
    AllowAbsurdCast,
    Error,
}

impl fmt::Display for CompilerFlag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompilerFlag::AllowNestedIfElse => write!(f, "allow_nested_if_else"),
            CompilerFlag::AllowGenericReturn => write!(f, "allow_generic_return"),
            CompilerFlag::AllowAbsurdCast => write!(f, "allow_absurd_cast"),
            CompilerFlag::Error => write!(f, "<Invalid flag>"),
        }
    }
}
