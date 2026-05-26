use camino::Utf8Path;

use crate::ast::AstProgram;
use crate::diagnostic::SourceDiagnostics;
use crate::lexer::Lexer;

use super::Parser;

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct SourceParser;

impl SourceParser {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn parse(&self, source_path: &Utf8Path, source: &str) -> miette::Result<AstProgram> {
        let diagnostics = SourceDiagnostics::new(source_path, source);
        let tokens = Lexer::new().lex(source).map_err(|error| {
            diagnostics.at(
                error.span,
                "could not lex Riot ML source",
                error.message,
                Some("stage0 currently accepts a small Riot ML subset"),
            )
        })?;

        Parser::new(source, tokens)
            .parse_program()
            .map_err(|error| {
                diagnostics
                    .at(
                        error.span,
                        "could not parse Riot ML source",
                        error.message,
                        error.help,
                    )
                    .into()
            })
    }
}
