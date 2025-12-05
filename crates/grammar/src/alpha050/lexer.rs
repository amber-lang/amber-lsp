use logos::Logos;

use super::{
    Spanned,
    Token,
};
use chumsky::span::SimpleSpan;

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
                Some((Token("\"".to_string()), SimpleSpan::from(start..end)))
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
                            } else {
                                // single $ (not followed by $) -> start a command
                                !prev_is_dollar
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
                Some((Token("$".to_string()), SimpleSpan::from(start..end)))
            }
            Ok(TokenKind::OpenBrace) => {
                // Only track brace depth if we're inside an interpolation (context stack > 1)
                if self.context_stack.len() > 1 {
                    self.brace_depth += 1;
                }
                Some((Token("{".to_string()), SimpleSpan::from(start..end)))
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
                Some((Token("}".to_string()), SimpleSpan::from(start..end)))
            }
            Ok(kind) => {
                let token_str = token_kind_to_string(&kind, slice);
                Some((Token(token_str), SimpleSpan::from(start..end)))
            }
            Err(_) => Some((Token(slice.to_string()), SimpleSpan::from(start..end))),
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
                Some((Token("\"".to_string()), SimpleSpan::from(start..end)))
            }
            Ok(StringContext::OpenBrace) => {
                // Entering interpolation - switch back to main context
                self.context_stack.push(LexerContext::Main);
                self.brace_depth = 1;
                Some((Token("{".to_string()), SimpleSpan::from(start..end)))
            }
            Ok(StringContext::Content) | Ok(StringContext::Escape) => {
                Some((Token(slice.to_string()), SimpleSpan::from(start..end)))
            }
            Err(_) => Some((Token(slice.to_string()), SimpleSpan::from(start..end))),
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
                Some((Token("$".to_string()), SimpleSpan::from(start..end)))
            }
            Ok(CommandContext::OpenBrace) => {
                // Entering interpolation - switch back to main context
                self.context_stack.push(LexerContext::Main);
                self.brace_depth = 1;
                Some((Token("{".to_string()), SimpleSpan::from(start..end)))
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
                Some((Token(token_text), SimpleSpan::from(start..end)))
            }
            Err(e) => {
                eprintln!(
                    "CommandContext lexer error at pos {}: {:?}, slice: {:?}, remaining: {:?}",
                    self.position,
                    e,
                    slice,
                    &remaining[..remaining.len().min(50)]
                );
                Some((Token(slice.to_string()), SimpleSpan::from(start..end)))
            }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_numbers() {
        let input = r#".5 2.4 2"#;
        let tokens = tokenize(input);

        assert_eq!(tokens[0].0 .0, ".5");
        assert_eq!(tokens[1].0 .0, "2.4");
        assert_eq!(tokens[2].0 .0, "2");
    }

    #[test]
    fn test_simple_string() {
        let input = r#""hello""#;
        let tokens = tokenize(input);
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].0 .0, "\"");
        assert_eq!(tokens[1].0 .0, "hello");
        assert_eq!(tokens[2].0 .0, "\"");
    }

    #[test]
    fn test_string_with_interpolation() {
        let input = r#""Hello {name}!""#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(
            token_strs,
            vec!["\"", "Hello ", "{", "name", "}", "!", "\""]
        );
    }

    #[test]
    fn test_command() {
        let input = r#"$echo test$"#;
        let tokens = tokenize(input);
        assert_eq!(tokens[0].0 .0, "$");
        assert_eq!(tokens[tokens.len() - 1].0 .0, "$");
    }

    #[test]
    fn test_keywords() {
        let input = "if else fun return";
        let tokens = tokenize(input);
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].0 .0, "if");
        assert_eq!(tokens[1].0 .0, "else");
    }

    #[test]
    fn test_nested_string_interpolation() {
        let input = r#""outer {inner "nested {x}"}""#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(
            token_strs,
            vec!["\"", "outer ", "{", "inner", "\"", "nested ", "{", "x", "}", "\"", "}", "\""]
        );
    }

    #[test]
    fn test_string_with_escape() {
        let input = r#""hello \"world\"""#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(
            token_strs,
            vec!["\"", "hello ", r#"\""#, "world", r#"\""#, "\""]
        );
    }

    #[test]
    fn test_string_with_multiple_interpolations() {
        let input = r#""Hello {name}, you are {age} years old!""#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        println!("Actual tokens: {:?}", token_strs);
        // After closing }, we should return to string context, not stay in main
        // This is a bug - need to fix context management
        assert_eq!(
            token_strs,
            vec![
                "\"",
                "Hello ",
                "{",
                "name",
                "}",
                ", you are ",
                "{",
                "age",
                "}",
                " years old!",
                "\""
            ]
        );
    }

    #[test]
    fn test_empty_string() {
        let input = r#""""#;
        let tokens = tokenize(input);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].0 .0, "\"");
        assert_eq!(tokens[1].0 .0, "\"");
    }

    #[test]
    fn test_string_with_expression_interpolation() {
        let input = r#""Result: {a + b}""#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(
            token_strs,
            vec!["\"", "Result: ", "{", "a", "+", "b", "}", "\""]
        );
    }

    #[test]
    fn test_command_simple() {
        let input = r#"$ls -la$"#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(token_strs, vec!["$", "ls -la", "$"]);
    }

    #[test]
    fn test_command_with_interpolation() {
        let input = r#"$echo {msg}$"#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(token_strs, vec!["$", "echo ", "{", "msg", "}", "$"]);
    }

    #[test]
    fn test_command_with_multiple_interpolations() {
        let input = r#"$cp {src} {dst}$"#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(
            token_strs,
            vec!["$", "cp ", "{", "src", "}", " ", "{", "dst", "}", "$"]
        );
    }

    #[test]
    fn test_command_with_string_in_interpolation() {
        let input = r#"$echo {"Hello World"}$"#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(
            token_strs,
            vec!["$", "echo ", "{", "\"", "Hello World", "\"", "}", "$"]
        );
    }

    #[test]
    fn test_command_empty() {
        let input = r#"
        $$?
        "#;
        let tokens = tokenize(input);
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].0 .0, "$");
        assert_eq!(tokens[1].0 .0, "$");
        assert_eq!(tokens[2].0 .0, "?");
    }

    #[test]
    fn test_string_only_interpolation() {
        let input = r#""{x}""#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(token_strs, vec!["\"", "{", "x", "}", "\""]);
    }

    #[test]
    fn test_mixed_code_with_strings() {
        let input = r#"let x = "hello {name}""#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(
            token_strs,
            vec!["let", "x", "=", "\"", "hello ", "{", "name", "}", "\""]
        );
    }

    #[test]
    fn test_comment_tokenization() {
        let input = "// this is a comment";
        let tokens = tokenize(input);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0 .0, "// this is a comment");
    }

    #[test]
    fn test_shebang_tokenization() {
        let input = "#!/usr/bin/env amber";
        let tokens = tokenize(input);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0 .0, "#!/usr/bin/env amber");
    }

    #[test]
    fn test_compiler_flag() {
        let input = "#[allow_nested_if_else]";
        let tokens = tokenize(input);
        // Compiler flags are now split into: #[, content, ]
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].0 .0, "#[");
        assert_eq!(tokens[1].0 .0, "allow_nested_if_else");
        assert_eq!(tokens[2].0 .0, "]");
    }

    #[test]
    fn test_operators_and_delimiters() {
        let input = "a <= b and c >= d";
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(token_strs, vec!["a", "<=", "b", "and", "c", ">=", "d"]);
    }

    #[test]
    fn test_array_with_range() {
        let input = "[1..10]";
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(token_strs, vec!["[", "1", "..", "10", "]"]);
    }

    #[test]
    fn test_function_call_with_string() {
        let input = r#"echo("test {x}")"#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(
            token_strs,
            vec!["echo", "(", "\"", "test ", "{", "x", "}", "\"", ")"]
        );
    }

    #[test]
    fn test_deeply_nested_braces() {
        let input = r#""a {b {c {d}}}""#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(
            token_strs,
            vec!["\"", "a ", "{", "b", "{", "c", "{", "d", "}", "}", "}", "\""]
        );
    }

    #[test]
    fn test_string_with_backslash_escape() {
        let input = r#""path\\to\\file""#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(
            token_strs,
            vec!["\"", "path", r#"\\"#, "to", r#"\\"#, "file", "\""]
        );
    }

    #[test]
    fn test_command_with_escape() {
        let input = r#"$echo \$$"#;
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(token_strs, vec!["$", "echo ", r#"\$"#, "$"]);
    }

    #[test]
    fn test_numbers_integers_and_floats() {
        let input = "42 3.14 100";
        let tokens = tokenize(input);
        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        assert_eq!(token_strs, vec!["42", "3.14", "100"]);
    }

    #[test]
    fn test_span_accuracy() {
        let input = r#""hello {name}""#;
        let tokens = tokenize(input);

        // Verify spans are correct
        assert_eq!(tokens[0].1, SimpleSpan::from(0..1)); // "
        assert_eq!(tokens[1].1, SimpleSpan::from(1..7)); // hello
        assert_eq!(tokens[2].1, SimpleSpan::from(7..8)); // {
        assert_eq!(tokens[3].1, SimpleSpan::from(8..12)); // name
        assert_eq!(tokens[4].1, SimpleSpan::from(12..13)); // }
        assert_eq!(tokens[5].1, SimpleSpan::from(13..14)); // "
    }

    #[test]
    fn test_comment_with_newline() {
        let input = "echo \"x\" // test comment\nif x";
        let tokens = tokenize(input);

        // Print for debugging
        for (i, (token, span)) in tokens.iter().enumerate() {
            eprintln!("{}: {:?} @ {:?}", i, token.0, span);
        }

        let token_strs: Vec<&str> = tokens.iter().map(|(t, _)| t.0.as_str()).collect();
        // Should see: echo, ", x, ", comment, if, x
        assert!(token_strs.contains(&"echo"));
        assert!(token_strs.contains(&"if"));
    }

    #[test]
    fn test_logos_newline_directly() {
        let input = "}\n\nif";
        let mut lex = TokenKind::lexer(input);

        let mut count = 0;
        while let Some(result) = lex.next() {
            eprintln!(
                "Token {}: {:?} = {:?} @ {:?}",
                count,
                result,
                lex.slice(),
                lex.span()
            );
            count += 1;
        }

        eprintln!("Total: {} tokens", count);
        assert!(count >= 4, "Expected at least 4 tokens from Logos directly");
    }

    #[test]
    fn test_newlines_tokenization() {
        let input = "}\n\nif";
        let tokens = tokenize(input);

        eprintln!("Total tokens: {}", tokens.len());
        for (i, (token, span)) in tokens.iter().enumerate() {
            eprintln!(
                "{}: {:?} (bytes: {:?}) @ {:?}",
                i,
                token.0,
                token.0.as_bytes(),
                span
            );
        }

        // Newlines are filtered out, so should be: }, if
        assert_eq!(tokens.len(), 2, "Expected 2 tokens (newlines filtered)");
        assert_eq!(tokens[0].0 .0, "}");
        assert_eq!(tokens[1].0 .0, "if");
    }

    #[test]
    fn test_after_closing_brace() {
        let input = "}\n\n    if age";
        let tokens = tokenize(input);

        eprintln!("Total tokens: {}", tokens.len());
        for (i, (token, span)) in tokens.iter().enumerate() {
            eprintln!("{}: {:?} @ {:?}", i, token.0, span);
        }

        // Newlines are filtered out, so should be: }, if, age
        assert_eq!(tokens.len(), 3, "Expected 3 tokens (newlines filtered)");
        assert_eq!(tokens[0].0 .0, "}");
        assert_eq!(tokens[1].0 .0, "if");
        assert_eq!(tokens[2].0 .0, "age");
    }

    #[test]
    fn test_full_comments_in_ifs_tokenization() {
        let input = r#"
    #[test]
    fn test_full_comments_in_ifs_tokenization() {
        let input = r#"
    if {
        1 == 2: echo "x" // test comment
        // another comment
        2 == 2 {
            echo "y"
        }
        // another
        else: echo "z" // comment
        // super comment
        /// doc comment
    }

    if age >= 16: echo "Welcome" // comment
    // comment in between
    else: echo "Entry not allowed" // another comment
"#;
        eprintln!("Input length: {}", input.len());
        eprintln!(
            "Input around position 233: {:?}",
            &input.chars().skip(233).take(20).collect::<String>()
        );

        let tokens = tokenize(input);

        eprintln!("Total tokens: {}", tokens.len());
        for (i, (token, span)) in tokens.iter().enumerate() {
            eprintln!("{}: {:?} @ {:?}", i, token.0, span);
        }

        // Should have more than 40 tokens for this input
        assert!(
            tokens.len() > 40,
            "Expected more than 40 tokens, got {}",
            tokens.len()
        );
    }

    #[test]
    fn test_compiler_flag_tokenization() {
        let input = "#[allow_absurd_cast]\npub fun test() {}";
        let tokens = tokenize(input);

        eprintln!("Total tokens: {}", tokens.len());
        for (i, (token, span)) in tokens.iter().enumerate() {
            eprintln!("{}: {:?} @ {:?}", i, token.0, span);
        }

        assert!(!tokens.is_empty(), "Expected tokens");
        // Compiler flags are now split into tokens: #[, content, ]
        assert_eq!(tokens[0].0 .0, "#[");
        assert_eq!(tokens[1].0 .0, "allow_absurd_cast");
        assert_eq!(tokens[2].0 .0, "]");
        assert_eq!(tokens[3].0 .0, "pub");
    }

    #[test]
    fn test_stdlib_math_full_file() {
        use std::fs::read_to_string;

        let input =
            read_to_string("../../resources/alpha050/std/math.ab").expect("Failed to read math.ab");

        let tokens = tokenize(&input);

        eprintln!("Total tokens from full file: {}", tokens.len());
        eprintln!("First 30 tokens:");
        for (i, (token, span)) in tokens.iter().enumerate().take(30) {
            eprintln!("{}: {:?} @ {:?}", i, token.0, span);
        }

        // The file should have many tokens
        assert!(
            tokens.len() > 100,
            "Expected more than 100 tokens, got {}",
            tokens.len()
        );
    }

    #[test]
    fn test_command_with_single_quotes_and_dollar() {
        let input = r#"trust $ echo "{text}" | sed -e 's/^[[:space:]]*//' $"#;
        let tokens = tokenize(input);

        eprintln!("Total tokens: {}", tokens.len());
        for (i, (token, span)) in tokens.iter().enumerate() {
            eprintln!("{}: {:?} @ {:?}", i, token.0, span);
        }

        // The command should end with a separate $ token
        let last_token = &tokens[tokens.len() - 1];
        assert_eq!(
            last_token.0 .0, "$",
            "Last token should be the closing $ of the command"
        );

        // Find the token before the last $
        let before_last = &tokens[tokens.len() - 2];
        assert!(
            !before_last.0 .0.contains("$"),
            "Token before last should not contain $, got: {:?}",
            before_last.0 .0
        );
    }

    #[test]
    fn test_command_context_regex_directly() {
        use logos::Logos;

        // Test the CommandContext lexer directly with the problematic input
        let input = " | sed -e 's/^[[:space:]]*//' ";
        let mut lex = CommandContext::lexer(input);

        eprintln!("Testing CommandContext with: {:?}", input);
        while let Some(result) = lex.next() {
            eprintln!("Token: {:?} = {:?} @ {:?}", result, lex.slice(), lex.span());
        }
    }

    #[test]
    fn test_complete_if_blocks() {
        let input = r#"
    if {
        1 == 2: echo "x" // test comment
        // another comment
        2 == 2 {
            echo "y"
        }
        // another
        else: echo "z" // comment
        // super comment
        /// doc comment
    }

    if age >= 16: echo "Welcome" // comment
    // comment in between
    else: echo "Entry not allowed" // another comment
"#;
        let tokens = tokenize(input);

        eprintln!("Total tokens: {}", tokens.len());
        for (i, (token, span)) in tokens.iter().enumerate() {
            eprintln!("{}: {:?} @ {:?}", i, token.0, span);
        }

        // Should tokenize the complete input
        assert!(
            tokens.len() >= 45,
            "Expected at least 45 tokens, got {}",
            tokens.len()
        );

        // Verify we have the second if statement
        assert!(
            tokens.iter().any(|(t, _)| t.0 == "age"),
            "Should have 'age' identifier"
        );
        assert!(
            tokens.iter().any(|(t, _)| t.0 == "Entry not allowed"),
            "Should have text from second if block"
        );
    }
}
