use logos::Logos;

use crate::ast::TextSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) span: TextSpan,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(skip(r"//[^\r\n]*", allow_greedy = true))]
#[logos(skip r"/\*([^*]|\*+[^*/])*\*+/")]
pub(crate) enum TokenKind {
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("use")]
    Use,
    #[token("pub")]
    Pub,
    #[token("mod")]
    Mod,
    #[token("include")]
    Include,
    #[token("external")]
    External,
    #[token("type")]
    Type,
    #[token("spawn")]
    Spawn,
    #[token("receive")]
    Receive,
    #[token("if")]
    If,
    #[token("while")]
    While,
    #[token("match")]
    Match,
    #[token("else")]
    Else,
    #[token("true")]
    True,
    #[token("false")]
    False,

    #[token("==")]
    EqEq,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("|")]
    Pipe,
    #[token("->")]
    Arrow,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("..")]
    DotDot,
    #[token(".")]
    Dot,
    #[token("=")]
    Eq,
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
    #[token("!")]
    Bang,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,

    #[regex(r"[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?")]
    #[regex(r"[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*")]
    Float,
    #[regex(r"0[xX][0-9A-Fa-f][0-9A-Fa-f_]*")]
    #[regex(r"0[bB][01][01_]*")]
    #[regex(r"[0-9][0-9_]*")]
    Int,
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
    #[regex(r#"'([^'\\]|\\.)'"#)]
    #[regex(r#"'([^'\\(){}\[\]<>,;:\s]|\\.){2,}'"#, priority = 5)]
    Char,
    #[regex(r"'[A-Za-z_][A-Za-z0-9_']*")]
    #[regex(r"[A-Za-z_][A-Za-z0-9_']*")]
    Ident,

    Eof,
}

#[derive(Debug, Clone)]
pub(crate) struct LexError {
    pub(crate) span: TextSpan,
    pub(crate) message: String,
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct Lexer;

impl Lexer {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn lex(&self, source: &str) -> Result<Vec<Token>, LexError> {
        if let Some(span) = find_unterminated_block_comment(source) {
            return Err(LexError {
                span,
                message: "unterminated block comment".to_owned(),
            });
        }
        if let Some(span) = find_unterminated_string_literal(source) {
            return Err(LexError {
                span,
                message: "unterminated string literal".to_owned(),
            });
        }

        let mut lexer = TokenKind::lexer(source);
        let mut tokens = Vec::new();

        while let Some(result) = lexer.next() {
            let range = lexer.span();
            let span = TextSpan::new(range.start, range.end);
            let kind = result.map_err(|()| LexError {
                span,
                message: "unexpected character".to_owned(),
            })?;
            tokens.push(Token { kind, span });
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            span: TextSpan::new(source.len(), source.len()),
        });

        Ok(tokens)
    }
}

fn find_unterminated_block_comment(source: &str) -> Option<TextSpan> {
    let bytes = source.as_bytes();
    let mut index = 0;

    while index < bytes.len() {
        match bytes[index] {
            b'"' => {
                index += 1;
                while index < bytes.len() {
                    match bytes[index] {
                        b'\\' => index += 2,
                        b'"' => {
                            index += 1;
                            break;
                        }
                        _ => index += 1,
                    }
                }
            }
            b'\'' => {
                index += 1;
                while index < bytes.len() {
                    match bytes[index] {
                        b'\\' => index += 2,
                        b'\'' => {
                            index += 1;
                            break;
                        }
                        _ => index += 1,
                    }
                }
            }
            b'/' if bytes.get(index + 1) == Some(&b'/') => {
                index += 2;
                while index < bytes.len() && !matches!(bytes[index], b'\n' | b'\r') {
                    index += 1;
                }
            }
            b'/' if bytes.get(index + 1) == Some(&b'*') => {
                let start = index;
                index += 2;
                let mut closed = false;
                while index + 1 < bytes.len() {
                    if bytes[index] == b'*' && bytes[index + 1] == b'/' {
                        index += 2;
                        closed = true;
                        break;
                    }
                    index += 1;
                }
                if !closed {
                    return Some(TextSpan::new(start, (start + 2).min(source.len())));
                }
            }
            _ => index += 1,
        }
    }

    None
}

fn find_unterminated_string_literal(source: &str) -> Option<TextSpan> {
    let bytes = source.as_bytes();
    let mut index = 0;

    while index < bytes.len() {
        match bytes[index] {
            b'"' => {
                let start = index;
                index += 1;
                let mut closed = false;
                while index < bytes.len() {
                    match bytes[index] {
                        b'\\' => index += 2,
                        b'"' => {
                            index += 1;
                            closed = true;
                            break;
                        }
                        b'\n' | b'\r' => break,
                        _ => index += 1,
                    }
                }
                if !closed {
                    return Some(TextSpan::new(start, (start + 1).min(source.len())));
                }
            }
            b'\'' => {
                index += 1;
                while index < bytes.len() {
                    match bytes[index] {
                        b'\\' => index += 2,
                        b'\'' => {
                            index += 1;
                            break;
                        }
                        _ => index += 1,
                    }
                }
            }
            b'/' if bytes.get(index + 1) == Some(&b'/') => {
                index += 2;
                while index < bytes.len() && !matches!(bytes[index], b'\n' | b'\r') {
                    index += 1;
                }
            }
            b'/' if bytes.get(index + 1) == Some(&b'*') => {
                index += 2;
                while index + 1 < bytes.len() {
                    if bytes[index] == b'*' && bytes[index + 1] == b'/' {
                        index += 2;
                        break;
                    }
                    index += 1;
                }
            }
            _ => index += 1,
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::{Lexer, TokenKind};

    #[test]
    fn reserves_while_as_control_flow_keyword() {
        let tokens = Lexer::new().lex("while worker").unwrap();

        assert_eq!(tokens[0].kind, TokenKind::While);
        assert_eq!(tokens[1].kind, TokenKind::Ident);
        assert_eq!(tokens[2].kind, TokenKind::Eof);
    }

    #[test]
    fn reports_unterminated_block_comments_before_parser_tokens() {
        let error = Lexer::new()
            .lex("fn main() { /* missing close\n  dbg(1) }")
            .unwrap_err();

        assert_eq!(error.message, "unterminated block comment");
        assert_eq!(error.span.start, 12);
        assert_eq!(error.span.end, 14);
    }

    #[test]
    fn ignores_block_comment_markers_inside_strings() {
        let tokens = Lexer::new()
            .lex("fn main() { dbg(\"/* not a comment\") }")
            .unwrap();

        assert!(tokens.iter().any(|token| token.kind == TokenKind::String));
    }

    #[test]
    fn reports_unterminated_strings_before_generic_lex_errors() {
        let error = Lexer::new()
            .lex("fn main() { dbg(\"missing close) }")
            .unwrap_err();

        assert_eq!(error.message, "unterminated string literal");
        assert_eq!(error.span.start, 16);
        assert_eq!(error.span.end, 17);
    }

    #[test]
    fn ignores_string_markers_inside_comments() {
        let tokens = Lexer::new()
            .lex("fn main() { /* \" not a string */ dbg(1) }")
            .unwrap();

        assert!(tokens.iter().any(|token| token.kind == TokenKind::Int));
    }

    #[test]
    fn tokenizes_multi_character_literals_without_stealing_type_variables() {
        let char_tokens = Lexer::new().lex("dbg('ab')").unwrap();
        assert!(char_tokens.iter().any(|token| token.kind == TokenKind::Char));

        let type_tokens = Lexer::new().lex("type List<'value> = Nil").unwrap();
        assert!(type_tokens.iter().any(|token| token.kind == TokenKind::Ident));
        assert!(!type_tokens.iter().any(|token| token.kind == TokenKind::Char));
    }
}
