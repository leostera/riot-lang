use logos::Logos;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn join(self, other: Span) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }

    pub fn as_source_span(self) -> SourceSpan {
        (self.start, self.end - self.start).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(skip(r"//[^\n]*", allow_greedy = true))]
pub enum TokenKind {
    // Keywords
    #[token("let")]
    Let,
    #[token("rec")]
    Rec,
    #[token("fn")]
    Fn,
    #[token("type")]
    Type,
    #[token("match")]
    Match,
    #[token("use")]
    Use,

    // Literals
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", |lex| lex.slice().to_string())]
    Float(String),
    #[regex(r"0x[0-9a-fA-F]+|0b[01]+|0o[0-7]+|[0-9]+", |lex| lex.slice().to_string())]
    Int(String),
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().to_string())]
    String(String),
    #[regex(r"'[a-z_][a-zA-Z0-9_']*", |lex| lex.slice()[1..].to_string())]
    TypeVar(String),
    #[regex(r"[a-z_][a-zA-Z0-9_']*", |lex| lex.slice().to_string())]
    Ident(String),
    #[regex(r"[A-Z][a-zA-Z0-9_']*", |lex| lex.slice().to_string())]
    Constructor(String),

    // Symbols
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("()")]
    Unit,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token(".")]
    Dot,
    #[token("|")]
    Pipe,
    #[token("=")]
    Equal,
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

    Eof,
}

#[derive(Debug, Error, Diagnostic)]
#[error("unexpected token")]
pub struct LexError {
    #[source_code]
    pub src: String,
    #[label("I don't know how to lex this")]
    pub span: SourceSpan,
}

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = TokenKind::lexer(src);
    let mut tokens = Vec::new();

    while let Some(next) = lexer.next() {
        let range = lexer.span();
        let kind = next.map_err(|()| LexError {
            src: src.to_string(),
            span: (range.start, range.end - range.start).into(),
        })?;
        tokens.push(Token {
            kind,
            span: Span::new(range.start, range.end),
        });
    }

    tokens.push(Token {
        kind: TokenKind::Eof,
        span: Span::new(src.len(), src.len()),
    });

    Ok(tokens)
}
