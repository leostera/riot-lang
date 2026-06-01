//! Parser modules.
//!
//! The parser package contains tokens/lexing, identifiers, source AST nodes, and
//! the recursive-descent plus Pratt parser implementation.

pub mod ast;
pub mod ident;
pub mod lexer;
pub mod parse;

pub use ast::Module as Ast;
pub use lexer::LexError;
pub use parse::ParseError;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Lex(#[from] LexError),
    #[error(transparent)]
    Parse(#[from] ParseError),
}

/// Parse TinyML source into the untyped source AST.
pub fn parse(src: &str) -> Result<Ast, Error> {
    let lexer = lexer::Lexer::new(src)?;
    Ok(parse::Parser::new(lexer).parse_module()?)
}
