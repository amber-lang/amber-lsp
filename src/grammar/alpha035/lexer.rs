use logos::Logos;

use super::Spanned;
use crate::grammar::SimpleSpan;
pub use crate::grammar::Token;

/// Main token context - general code
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r]+")]
pub enum TokenKind {
    // Compound operators
    #[token("<=")]
    LessEquals,
    #[token(">=")]
    GreaterEquals,
    #[token("!=")]
    NotEquals,
    #[token("==")]
    EqualsEquals,
    #[token("+=")]
    PlusEquals,
    #[token("-=")]
    MinusEquals,
    #[token("*=")]
    MulEquals,
    #[token("/=")]
    DivEquals,
    #[token("%=")]
    ModEquals,
    #[token("..")]
    DotDot,

    // Single operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("=")]
    Equals,
    #[token("!")]
    Bang,
    #[token("?")]
    Question,

    // Delimiters
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,

    // Punctuation
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("\\")]
    Backslash,

    // String start (transition to StringContext)
    #[token("\"")]
    Quote,

    // Command start/end (transition to CommandContext)
    #[token("$")]
    Dollar,

    // Compiler flag start (transition to CompilerFlagContext)
    #[token("#[")]
    CompilerFlagStart,

    // Comments (include trailing newline to match Heraclitus behavior)
    #[regex(r"//[^\n]*\n?", priority = 2)]
    Comment,
    #[regex(r"#![^\n]*\n?", priority = 2)]
    Shebang,

    // Identifier
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,

    // Number
    #[regex(r"([0-9]+(\.[0-9]+)?)|(\.[0-9]+)")]
    Number,

    // Newline
    #[token("\n")]
    Newline,
}

/// String content context - inside "..."
#[derive(Logos, Debug, Clone, PartialEq)]
pub enum StringContext {
    // String end
    #[token("\"")]
    Quote,

    // Interpolation start
    #[token("{")]
    OpenBrace,

    // Text content (not quotes, backslashes, or braces)
    #[regex(r#"[^"\\{]+"#)]
    Content,

    // Escape sequence
    #[regex(r#"\\."#)]
    Escape,
}

/// Command content context - inside $...$
#[derive(Logos, Debug, Clone, PartialEq)]
pub enum CommandContext {
    // Command end
    #[token("$")]
    Dollar,

    // Interpolation start
    #[token("{")]
    OpenBrace,

    // Text content (not $, backslash, or braces)
    #[regex(r"[^$\\{]+")]
    Content,

    // Escape sequence
    #[regex(r#"\\."#)]
    Escape,
}

/// Context stack to track where we are in parsing
#[derive(Debug, Clone, PartialEq)]
enum LexerContext {
    Main,
    String,
    Command,
}

/// Stateful tokenizer that manages context switching
pub struct StatefulTokenizer<'source> {
    source: &'source str,
    position: usize,
    context_stack: Vec<LexerContext>,
    brace_depth: usize, // Track brace nesting inside interpolations
}

impl<'source> StatefulTokenizer<'source> {
    fn new(source: &'source str) -> Self {
        Self {
            source,
            position: 0,
            context_stack: vec![LexerContext::Main],
            brace_depth: 0,
        }
    }

    pub fn tokenize(source: &'source str) -> Vec<Spanned<Token>> {
        let mut tokenizer = Self::new(source);
        let mut tokens = Vec::new();

        while tokenizer.position < source.len() {
            if let Some((token, span)) = tokenizer.next_token() {
                tokens.push((token, span));
            } else {
                break;
            }
        }

        // Filter out newline tokens - they're used for position tracking but not needed for parsing
        tokens
            .into_iter()
            .filter(|(token, _)| token.0 != "\n")
            .collect()
    }

    fn next_token(&mut self) -> Option<(Token, SimpleSpan)> {
        if self.position >= self.source.len() {
            return None;
        }

        let remaining = &self.source[self.position..];
        let context = self.context_stack.last()?.clone();

        match context {
            LexerContext::Main => self.lex_main_context(remaining),
            LexerContext::String => self.lex_string_context(remaining),
            LexerContext::Command => self.lex_command_context(remaining),
        }
    }

    fn lex_main_context(&mut self, remaining: &str) -> Option<(Token, SimpleSpan)> {
        let mut lex = TokenKind::lexer(remaining);
        let token_result = lex.next()?;
        let span = lex.span();
        let slice = lex.slice();

        // span is relative to `remaining`, so add current position to get absolute
        let start = self.position + span.start;
        let end = self.position + span.end;
        self.position = end;

        match token_result {
            Ok(TokenKind::Quote) => {
                // Entering string context
                self.context_stack.push(LexerContext::String);
                Some((Token("\"".to_string()), SimpleSpan::new(start, end)))
            }
            Ok(TokenKind::Dollar) => {
                // Check if this starts a command.
                // If the next char is another '$', do not start a command (this is the first of $$).
                // If this is the second of a $$ (previous char was '$'), only start a command
                // when the following char is not whitespace. Otherwise single $ starts a command.
                let prev_is_dollar = start > 0 && self.source[start - 1..].starts_with('$');

                let is_command_start = if span.end < remaining.len() {
                    // Look at the next char after this $ in the remaining slice
                    let mut next_chars = remaining[span.end..].chars();
                    match next_chars.next() {
                        Some(next_ch) => {
                            if next_ch == '$' {
                                // next is a dollar -> this $ does not start a command
                                false
                            } else if prev_is_dollar {
                                // this is the second $ in $$ — only start a command if not followed by whitespace
                                !next_ch.is_whitespace()
                            } else {
                                // single $ (not followed by $) -> start a command
                                true
                            }
                        }
                        None => false,
                    }
                } else {
                    false
                };

                if is_command_start {
                    // Entering command context
                    self.context_stack.push(LexerContext::Command);
                }
                Some((Token("$".to_string()), SimpleSpan::new(start, end)))
            }
            Ok(TokenKind::OpenBrace) => {
                // Only track brace depth if we're inside an interpolation (context stack > 1)
                if self.context_stack.len() > 1 {
                    self.brace_depth += 1;
                }
                Some((Token("{".to_string()), SimpleSpan::new(start, end)))
            }
            Ok(TokenKind::CloseBrace) => {
                // Only track brace depth if we're inside an interpolation
                if self.context_stack.len() > 1 && self.brace_depth > 0 {
                    self.brace_depth -= 1;
                    if self.brace_depth == 0 {
                        // Exiting interpolation, pop back to string/command context
                        self.context_stack.pop();
                    }
                }
                Some((Token("}".to_string()), SimpleSpan::new(start, end)))
            }
            Ok(kind) => {
                let token_str = token_kind_to_string(&kind, slice);
                Some((Token(token_str), SimpleSpan::new(start, end)))
            }
            Err(_) => Some((Token(slice.to_string()), SimpleSpan::new(start, end))),
        }
    }

    fn lex_string_context(&mut self, remaining: &str) -> Option<(Token, SimpleSpan)> {
        let mut lex = StringContext::lexer(remaining);
        let token_result = lex.next()?;
        let span = lex.span();
        let slice = lex.slice();

        // span is relative to `remaining`, so add current position to get absolute
        let start = self.position + span.start;
        let end = self.position + span.end;
        self.position = end;

        match token_result {
            Ok(StringContext::Quote) => {
                // Exiting string context
                self.context_stack.pop();
                Some((Token("\"".to_string()), SimpleSpan::new(start, end)))
            }
            Ok(StringContext::OpenBrace) => {
                // Entering interpolation - switch back to main context
                self.context_stack.push(LexerContext::Main);
                self.brace_depth = 1;
                Some((Token("{".to_string()), SimpleSpan::new(start, end)))
            }
            Ok(StringContext::Content) | Ok(StringContext::Escape) => {
                Some((Token(slice.to_string()), SimpleSpan::new(start, end)))
            }
            Err(_) => Some((Token(slice.to_string()), SimpleSpan::new(start, end))),
        }
    }

    fn lex_command_context(&mut self, remaining: &str) -> Option<(Token, SimpleSpan)> {
        let mut lex = CommandContext::lexer(remaining);
        let token_result = lex.next()?;
        let span = lex.span();
        let slice = lex.slice();

        // span is relative to `remaining`, so add current position to get absolute
        let start = self.position + span.start;
        let end = self.position + span.end;
        self.position = end;

        match token_result {
            Ok(CommandContext::Dollar) => {
                // Exiting command context
                self.context_stack.pop();
                Some((Token("$".to_string()), SimpleSpan::new(start, end)))
            }
            Ok(CommandContext::OpenBrace) => {
                // Entering interpolation - switch back to main context
                self.context_stack.push(LexerContext::Main);
                self.brace_depth = 1;
                Some((Token("{".to_string()), SimpleSpan::new(start, end)))
            }
            Ok(CommandContext::Content) | Ok(CommandContext::Escape) => {
                // If this command context was entered as the second `$` of `$$` and
                // the content starts with whitespace (e.g. `$$ failed {`), trim the
                // leading whitespace so the identifier/tokenization matches the
                // parser's expectations (no leading spaces).
                let token_text = if start > 0 && self.source[start - 1..].starts_with('$') {
                    // second of $$ - trim leading whitespace from content
                    slice.trim_start().to_string()
                } else {
                    slice.to_string()
                };
                Some((Token(token_text), SimpleSpan::new(start, end)))
            }
            Err(_) => Some((Token(slice.to_string()), SimpleSpan::new(start, end))),
        }
    }
}

fn token_kind_to_string(kind: &TokenKind, slice: &str) -> String {
    match kind {
        TokenKind::LessEquals => "<=".to_string(),
        TokenKind::GreaterEquals => ">=".to_string(),
        TokenKind::NotEquals => "!=".to_string(),
        TokenKind::EqualsEquals => "==".to_string(),
        TokenKind::PlusEquals => "+=".to_string(),
        TokenKind::MinusEquals => "-=".to_string(),
        TokenKind::MulEquals => "*=".to_string(),
        TokenKind::DivEquals => "/=".to_string(),
        TokenKind::ModEquals => "%=".to_string(),
        TokenKind::DotDot => "..".to_string(),
        TokenKind::Plus => "+".to_string(),
        TokenKind::Minus => "-".to_string(),
        TokenKind::Star => "*".to_string(),
        TokenKind::Slash => "/".to_string(),
        TokenKind::Percent => "%".to_string(),
        TokenKind::Less => "<".to_string(),
        TokenKind::Greater => ">".to_string(),
        TokenKind::Equals => "=".to_string(),
        TokenKind::Bang => "!".to_string(),
        TokenKind::Question => "?".to_string(),
        TokenKind::OpenParen => "(".to_string(),
        TokenKind::CloseParen => ")".to_string(),
        TokenKind::OpenBracket => "[".to_string(),
        TokenKind::CloseBracket => "]".to_string(),
        TokenKind::OpenBrace => "{".to_string(),
        TokenKind::CloseBrace => "}".to_string(),
        TokenKind::Semicolon => ";".to_string(),
        TokenKind::Colon => ":".to_string(),
        TokenKind::Comma => ",".to_string(),
        TokenKind::Dot => ".".to_string(),
        TokenKind::Backslash => "\\".to_string(),
        TokenKind::Quote => "\"".to_string(),
        TokenKind::Dollar => "$".to_string(),
        TokenKind::Newline => "\n".to_string(),
        TokenKind::CompilerFlagStart => "#[".to_string(),
        // For these, use the actual slice
        TokenKind::Comment | TokenKind::Shebang | TokenKind::Identifier | TokenKind::Number => {
            slice.to_string()
        }
    }
}

/// Public tokenize function
pub fn tokenize(input: &str) -> Vec<Spanned<Token>> {
    StatefulTokenizer::tokenize(input)
}
