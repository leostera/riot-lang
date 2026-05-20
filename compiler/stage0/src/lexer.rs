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

pub(crate) fn lex(source: &str) -> Result<Vec<Token>, LexError> {
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
