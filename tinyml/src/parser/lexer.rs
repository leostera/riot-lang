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

#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
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
    #[regex(r"'[a-zA-Z_][a-zA-Z0-9_']*", |lex| lex.slice()[1..].to_string())]
    TypeVar(String),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_']*", |lex| lex.slice().to_string())]
    Ident(String),

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

#[derive(Debug)]
pub struct Lexer<'a> {
    src: &'a str,
    tokens: Vec<Token>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Result<Self, LexError> {
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

        Ok(Self {
            src,
            tokens,
            pos: 0,
        })
    }

    pub fn from_str(src: &'a str) -> Result<Self, LexError> {
        Self::new(src)
    }

    pub fn source(&self) -> &str {
        &self.src
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn at(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(kind)
    }

    pub fn peek(&self) -> &TokenKind {
        &self.tokens[self.pos].kind
    }

    pub fn peek_n(&self, n: usize) -> &TokenKind {
        &self.tokens[(self.pos + n).min(self.tokens.len() - 1)].kind
    }

    pub fn current_span(&self) -> Span {
        self.tokens[self.pos].span
    }

    pub fn prev_span(&self) -> Span {
        self.tokens[self.pos.saturating_sub(1)].span
    }

    pub fn eat(&mut self, kind: &TokenKind) -> Option<Token> {
        match self.at(kind) {
            true => Some(self.bump()),
            false => None,
        }
    }

    pub fn next(&mut self) -> TokenKind {
        self.bump().kind
    }

    pub fn bump(&mut self) -> Token {
        let span = self.tokens[self.pos].span;
        let tok = std::mem::replace(
            &mut self.tokens[self.pos],
            Token {
                kind: TokenKind::Eof,
                span,
            },
        );
        self.pos += 1;
        tok
    }
}
