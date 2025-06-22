use crate::analysis::types::DataType;

pub use super::Spanned;
use super::{CommandModifier, CompilerFlag, Grammar, LSPAnalysis, ParserResponse, Span};
use chumsky::{
    error::Rich,
    extra::Err,
    input::{Input, SpannedInput},
    span::SimpleSpan,
    Parser,
};
use heraclitus_compiler::prelude::*;
use lexer::{get_rules, Token};
use prelude::lexer::Lexer;
use semantic_tokens::semantic_tokens_from_ast;

pub mod expressions;
pub mod global;
pub mod lexer;
pub mod parser;
pub mod semantic_tokens;
pub mod statements;

#[derive(PartialEq, Debug, Clone)]
pub enum InterpolatedText {
    Escape(Spanned<String>),
    Expression(Box<Spanned<Expression>>),
    Text(Spanned<String>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Block {
    Block(Vec<Spanned<CommandModifier>>, Vec<Spanned<Statement>>),
    Error,
}

#[derive(PartialEq, Debug, Clone)]
pub enum FailureHandler {
    Propagate,
    Handle(Spanned<String>, Vec<Spanned<Statement>>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum InterpolatedCommand {
    Escape(String),
    CommandOption(String),
    Expression(Box<Spanned<Expression>>),
    Text(String),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Number(Spanned<f32>),
    Boolean(Spanned<bool>),
    Text(Vec<Spanned<InterpolatedText>>),
    Parentheses(Box<Spanned<Expression>>),
    Var(Spanned<String>),
    Add(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Subtract(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Multiply(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Divide(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Modulo(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Neg(Spanned<String>, Box<Spanned<Expression>>),
    And(
        Box<Spanned<Expression>>,
        Spanned<String>,
        Box<Spanned<Expression>>,
    ),
    Or(
        Box<Spanned<Expression>>,
        Spanned<String>,
        Box<Spanned<Expression>>,
    ),
    Gt(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Ge(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Lt(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Le(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Eq(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Neq(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Not(Spanned<String>, Box<Spanned<Expression>>),
    Ternary(
        Box<Spanned<Expression>>,
        Spanned<String>,
        Box<Spanned<Expression>>,
        Spanned<String>,
        Box<Spanned<Expression>>,
    ),
    FunctionInvocation(
        Vec<Spanned<CommandModifier>>,
        Spanned<String>,
        Vec<Spanned<Expression>>,
        Option<Spanned<FailureHandler>>,
    ),
    Command(
        Vec<Spanned<CommandModifier>>,
        Vec<Spanned<InterpolatedCommand>>,
        Option<Spanned<FailureHandler>>,
    ),
    Array(Vec<Spanned<Expression>>),
    Range(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Null,
    Cast(Box<Spanned<Expression>>, Spanned<String>, Spanned<DataType>),
    Status,
    Nameof(Spanned<String>, Box<Spanned<Expression>>),
    Is(Box<Spanned<Expression>>, Spanned<String>, Spanned<DataType>),
    Error,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ImportContent {
    ImportAll,
    ImportSpecific(Vec<Spanned<String>>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum FunctionArgument {
    Generic(Spanned<bool>, Spanned<String>),
    Typed(Spanned<bool>, Spanned<String>, Spanned<DataType>),
    Error,
}

#[derive(PartialEq, Debug, Clone)]
pub enum IfCondition {
    IfCondition(Box<Spanned<Expression>>, Spanned<Block>),
    InlineIfCondition(Box<Spanned<Expression>>, Box<Spanned<Statement>>),
    Comment(Spanned<Comment>),
    Error,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ElseCondition {
    Else(Spanned<String>, Spanned<Block>),
    InlineElse(Spanned<String>, Box<Spanned<Statement>>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum IfChainContent {
    IfCondition(Spanned<IfCondition>),
    Else(Spanned<ElseCondition>),
    Comment(Spanned<Comment>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum IterLoopVars {
    Single(Spanned<String>),
    WithIndex(Spanned<String>, Spanned<String>),
    Error,
}

#[derive(PartialEq, Debug, Clone)]
pub enum VariableInitType {
    Expression(Spanned<Expression>),
    DataType(Spanned<DataType>),
    Error,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Expression(Box<Spanned<Expression>>),
    VariableInit(Spanned<String>, Spanned<String>, Spanned<VariableInitType>),
    ConstInit(Spanned<String>, Spanned<String>, Box<Spanned<Expression>>),
    VariableSet(Spanned<String>, Box<Spanned<Expression>>),
    IfCondition(
        Spanned<String>,
        Spanned<IfCondition>,
        Vec<Spanned<Comment>>,
        Option<Spanned<ElseCondition>>,
    ),
    IfChain(Spanned<String>, Vec<Spanned<IfChainContent>>),
    ShorthandAdd(Spanned<String>, Box<Spanned<Expression>>),
    ShorthandSub(Spanned<String>, Box<Spanned<Expression>>),
    ShorthandMul(Spanned<String>, Box<Spanned<Expression>>),
    ShorthandDiv(Spanned<String>, Box<Spanned<Expression>>),
    ShorthandModulo(Spanned<String>, Box<Spanned<Expression>>),
    InfiniteLoop(Spanned<String>, Spanned<Block>),
    IterLoop(
        Spanned<String>,
        Spanned<IterLoopVars>,
        Spanned<String>,
        Box<Spanned<Expression>>,
        Spanned<Block>,
    ),
    Break,
    Continue,
    Return(Spanned<String>, Option<Box<Spanned<Expression>>>),
    Fail(Spanned<String>, Option<Box<Spanned<Expression>>>),
    Echo(Spanned<String>, Box<Spanned<Expression>>),
    Block(Spanned<Block>),
    Comment(Spanned<Comment>),
    Shebang(String),
    Error,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Comment {
    Comment(String),
}

#[derive(PartialEq, Debug, Clone)]
pub enum GlobalStatement {
    /// Import statement
    ///
    /// is_public, "import", import_content, "from", path
    Import(
        Spanned<bool>,
        Spanned<String>,
        Spanned<ImportContent>,
        Spanned<String>,
        Spanned<String>,
    ),
    /// Function definition
    ///
    /// is_public, "fun", name, args, return_type, body
    FunctionDefinition(
        Vec<Spanned<CompilerFlag>>,
        Spanned<bool>,
        Spanned<String>,
        Spanned<String>,
        Vec<Spanned<FunctionArgument>>,
        Option<Spanned<DataType>>,
        Vec<Spanned<Statement>>,
    ),
    Main(
        Spanned<String>,
        Option<Spanned<String>>,
        Vec<Spanned<Statement>>,
    ),
    Statement(Spanned<Statement>),
}

#[derive(Debug)]
pub struct AmberCompiler {
    lexer: Lexer,
}

impl Default for AmberCompiler {
    fn default() -> Self {
        Self::new()
    }
}

impl AmberCompiler {
    pub fn new() -> Self {
        let lexer = Lexer::new(get_rules());

        AmberCompiler { lexer }
    }

    pub fn parser<'a>(&self) -> impl AmberParser<'a, Vec<Spanned<GlobalStatement>>> {
        global::global_statement_parser()
    }
}

impl LSPAnalysis for AmberCompiler {
    #[tracing::instrument(skip_all)]
    fn tokenize(&self, input: &str) -> Vec<Spanned<Token>> {
        // It should never fail
        self.lexer
            .tokenize(&input.replace("\r\n", "\n").replace("\r", "\n"))
            .expect("Failed to tokenize input")
            .iter()
            .filter_map(|t| {
                if t.word == "\n" {
                    return None;
                }

                Some((
                    Token(t.word.clone()),
                    SimpleSpan::new(t.start, t.start + t.word.chars().count()),
                ))
            })
            .collect()
    }

    #[tracing::instrument(skip_all)]
    fn parse<'a>(&self, tokens: &'a [Spanned<Token>]) -> ParserResponse<'a> {
        let len = tokens.last().map(|t| t.1.end).unwrap_or(0);
        let parser_input = tokens.spanned(Span::new(len, len));

        let result = self.parser().parse(parser_input);

        let semantic_tokens = semantic_tokens_from_ast(result.output());

        let string_errors = result
            .errors()
            .map(|e| e.clone().map_token(|t| t.0))
            .collect();

        ParserResponse {
            ast: Grammar::Alpha034(result.into_output()),
            errors: string_errors,
            semantic_tokens,
        }
    }
}

pub type RichError<'src> = Err<Rich<'src, Token>>;
type AmberInput<'src> = SpannedInput<Token, Span, &'src [Spanned<Token>]>;

pub trait AmberParser<'src, Output>:
    Parser<'src, AmberInput<'src>, Output, RichError<'src>> + Clone + Sized + 'src
{
}

impl<'src, Output, T> AmberParser<'src, Output> for T where
    T: Parser<'src, AmberInput<'src>, Output, RichError<'src>> + Clone + Sized + 'src
{
}
