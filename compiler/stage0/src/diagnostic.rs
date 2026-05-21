use camino::Utf8Path;
use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::ast::TextSpan;

#[derive(Debug, Error, Diagnostic)]
#[error("{message}")]
#[diagnostic(code(stage0::source))]
pub(crate) struct SourceDiagnostic {
    #[source_code]
    src: NamedSource<String>,

    #[label("{label}")]
    span: SourceSpan,

    message: String,
    label: String,

    #[help]
    help: Option<String>,
}

pub(crate) fn to_source_diagnostic(
    source_path: &Utf8Path,
    source: &str,
    span: TextSpan,
    message: impl Into<String>,
    label: impl Into<String>,
    help: Option<&'static str>,
) -> SourceDiagnostic {
    SourceDiagnostic {
        src: NamedSource::new(source_path, source.to_owned()),
        span: to_miette_span(span),
        message: message.into(),
        label: label.into(),
        help: help.map(str::to_owned),
    }
}

fn to_miette_span(span: TextSpan) -> SourceSpan {
    let start = span.start;
    let len = span.end.saturating_sub(span.start);
    (start, len).into()
}
