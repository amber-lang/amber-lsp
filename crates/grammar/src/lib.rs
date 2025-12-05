use chumsky::error::Rich;
use std::fmt::{
    self,
    Debug,
};

pub mod alpha034;
pub mod alpha035;
pub mod alpha040;
pub mod alpha050;

// Re-export types from amber_types for use within this crate and by consumers
pub use amber_types::token::Span;
pub use amber_types::{
    DataType,
    Spanned,
    SpannedSemanticToken,
    Token,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Grammar {
    Alpha034(Option<Vec<Spanned<alpha034::GlobalStatement>>>),
    Alpha035(Option<Vec<Spanned<alpha035::GlobalStatement>>>),
    Alpha040(Option<Vec<Spanned<alpha040::GlobalStatement>>>),
    Alpha050(Option<Vec<Spanned<alpha050::GlobalStatement>>>),
}

#[macro_export]
macro_rules! T {
    [$text:expr] => {
        $crate::Token($text.to_string())
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
    Sudo,
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
