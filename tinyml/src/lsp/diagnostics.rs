//! Conversion from TinyML frontend diagnostics to LSP diagnostics.

use crate::{
    checker::CheckDiagnostic,
    parser::{lexer::Span, parse::ParseError},
};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString};

use super::position::span_to_range;

pub fn checker_diagnostics(source: &str, diagnostics: &[CheckDiagnostic]) -> Vec<Diagnostic> {
    diagnostics
        .iter()
        .map(|diagnostic| Diagnostic {
            range: span_to_range(source, diagnostic.span()),
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String(
                diagnostic_code(diagnostic).to_string(),
            )),
            code_description: None,
            source: Some("tinyml".to_string()),
            message: diagnostic.to_string(),
            related_information: None,
            tags: None,
            data: None,
        })
        .collect()
}

pub fn span_diagnostic(source: &str, span: Span, message: impl Into<String>) -> Diagnostic {
    Diagnostic {
        range: span_to_range(source, span),
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("tinyml".to_string()),
        message: message.into(),
        related_information: None,
        tags: None,
        data: None,
    }
}

pub fn parse_diagnostic(source: &str, error: &ParseError) -> Diagnostic {
    let start = usize::from(error.span.offset());
    let len = error.span.len();
    span_diagnostic(source, Span::new(start, start + len), error.message.clone())
}

fn diagnostic_code(diagnostic: &CheckDiagnostic) -> &'static str {
    match diagnostic {
        CheckDiagnostic::DuplicateTopLevelValue { .. } => "duplicate_top_level_value",
        CheckDiagnostic::DuplicateTypeDeclaration { .. } => "duplicate_type_declaration",
        CheckDiagnostic::DuplicateConstructor { .. } => "duplicate_constructor",
        CheckDiagnostic::DuplicateRecordField { .. } => "duplicate_record_field",
        CheckDiagnostic::DuplicateTypeParameter { .. } => "duplicate_type_parameter",
        CheckDiagnostic::UnknownValue { .. } => "unknown_value",
        CheckDiagnostic::UnknownConstructor { .. } => "unknown_constructor",
        CheckDiagnostic::UnknownType { .. } => "unknown_type",
        CheckDiagnostic::UnknownField { .. } => "unknown_field",
        CheckDiagnostic::MissingField { .. } => "missing_field",
        CheckDiagnostic::TypeArityMismatch { .. } => "type_arity_mismatch",
        CheckDiagnostic::MissingConstructor { .. } => "missing_constructor",
        CheckDiagnostic::RedundantMatchArm { .. } => "redundant_match_arm",
        CheckDiagnostic::InvalidRecursiveConstant { .. } => "invalid_recursive_constant",
        CheckDiagnostic::UnsupportedQualifiedTopLevelConstant { .. } => {
            "unsupported_qualified_top_level_constant"
        }
        CheckDiagnostic::UnsupportedTopLevelFunction { .. } => "unsupported_top_level_function",
        CheckDiagnostic::UnsupportedTypeDeclaration { .. } => "unsupported_type_declaration",
        CheckDiagnostic::UnsupportedUseDeclaration { .. } => "unsupported_use_declaration",
        CheckDiagnostic::UnsupportedExpression { .. } => "unsupported_expression",
        CheckDiagnostic::TypeMismatch { .. } => "type_mismatch",
    }
}
