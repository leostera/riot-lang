use camino::Utf8Path;

use crate::ast::AstProgram;
use crate::diagnostic::to_source_diagnostic;
use crate::lexer::Lexer;

use super::Parser;

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct SourceParser;

impl SourceParser {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn parse(&self, source_path: &Utf8Path, source: &str) -> miette::Result<AstProgram> {
        let tokens = Lexer::new().lex(source).map_err(|error| {
            to_source_diagnostic(
                source_path,
                source,
                error.span,
                "could not lex Riot ML source",
                error.message,
                Some("stage0 currently accepts a small Riot ML subset"),
            )
        })?;

        Parser::new(source, tokens)
            .parse_program()
            .map_err(|error| {
                to_source_diagnostic(
                    source_path,
                    source,
                    error.span,
                    "could not parse Riot ML source",
                    error.message,
                    error.help,
                )
                .into()
            })
    }
}
