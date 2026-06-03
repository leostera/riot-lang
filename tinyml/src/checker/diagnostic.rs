//! Typed checker diagnostics.
//!
//! Diagnostics are structured enum values so tests and later tooling can inspect
//! checker results without parsing strings.

use miette::Diagnostic;
use thiserror::Error;

use crate::parser::lexer::Span;

use super::unifier::UnifyError;

#[derive(Debug, Clone, PartialEq, Error, Diagnostic)]
pub enum CheckDiagnostic {
    #[error("duplicate top-level value `{name}`")]
    #[diagnostic(code(tinyml::checker::duplicate_top_level_value))]
    DuplicateTopLevelValue { name: String, span: Span },

    #[error("duplicate type declaration `{name}`")]
    #[diagnostic(code(tinyml::checker::duplicate_type_declaration))]
    DuplicateTypeDeclaration { name: String, span: Span },

    #[error("duplicate constructor `{name}`")]
    #[diagnostic(code(tinyml::checker::duplicate_constructor))]
    DuplicateConstructor { name: String, span: Span },

    #[error("duplicate record field `{name}`")]
    #[diagnostic(code(tinyml::checker::duplicate_record_field))]
    DuplicateRecordField { name: String, span: Span },

    #[error("duplicate type parameter `{name}`")]
    #[diagnostic(code(tinyml::checker::duplicate_type_parameter))]
    DuplicateTypeParameter { name: String, span: Span },

    #[error("unknown value `{name}`")]
    #[diagnostic(code(tinyml::checker::unknown_value))]
    UnknownValue { name: String, span: Span },

    #[error("unknown constructor `{name}`")]
    #[diagnostic(code(tinyml::checker::unknown_constructor))]
    UnknownConstructor { name: String, span: Span },

    #[error("unknown type `{name}`")]
    #[diagnostic(code(tinyml::checker::unknown_type))]
    UnknownType { name: String, span: Span },

    #[error("unknown field `{name}`")]
    #[diagnostic(code(tinyml::checker::unknown_field))]
    UnknownField { name: String, span: Span },

    #[error("missing required field `{name}`")]
    #[diagnostic(code(tinyml::checker::missing_field))]
    MissingField { name: String, span: Span },

    #[error("nominal type `{name}` expects {expected} argument(s), got {actual}")]
    #[diagnostic(code(tinyml::checker::type_arity_mismatch))]
    TypeArityMismatch {
        name: String,
        expected: usize,
        actual: usize,
        span: Span,
    },

    #[error("match is missing constructor `{name}`")]
    #[diagnostic(code(tinyml::checker::missing_constructor))]
    MissingConstructor { name: String, span: Span },

    #[error("redundant match arm")]
    #[diagnostic(code(tinyml::checker::redundant_match_arm))]
    RedundantMatchArm { span: Span },

    #[error("non-function recursive constant `{name}` is not supported")]
    #[diagnostic(code(tinyml::checker::invalid_recursive_constant))]
    InvalidRecursiveConstant { name: String, span: Span },

    #[error("qualified top-level constants are not supported yet")]
    #[diagnostic(code(tinyml::checker::unsupported_qualified_top_level_constant))]
    UnsupportedQualifiedTopLevelConstant { span: Span },

    #[error("top-level function declarations are not supported yet")]
    #[diagnostic(code(tinyml::checker::unsupported_top_level_function))]
    UnsupportedTopLevelFunction { span: Span },

    #[error("type declarations are not supported yet")]
    #[diagnostic(code(tinyml::checker::unsupported_type_declaration))]
    UnsupportedTypeDeclaration { span: Span },

    #[error("use declarations are not supported yet")]
    #[diagnostic(code(tinyml::checker::unsupported_use_declaration))]
    UnsupportedUseDeclaration { span: Span },

    #[error("expression is not supported yet")]
    #[diagnostic(code(tinyml::checker::unsupported_expression))]
    UnsupportedExpression { span: Span },

    #[error("type mismatch")]
    #[diagnostic(code(tinyml::checker::type_mismatch))]
    TypeMismatch {
        span: Span,
        #[source]
        error: UnifyError,
    },
}

impl CheckDiagnostic {
    pub fn span(&self) -> Span {
        match self {
            CheckDiagnostic::DuplicateTopLevelValue { span, .. }
            | CheckDiagnostic::DuplicateTypeDeclaration { span, .. }
            | CheckDiagnostic::DuplicateConstructor { span, .. }
            | CheckDiagnostic::DuplicateRecordField { span, .. }
            | CheckDiagnostic::DuplicateTypeParameter { span, .. }
            | CheckDiagnostic::UnknownValue { span, .. }
            | CheckDiagnostic::UnknownConstructor { span, .. }
            | CheckDiagnostic::UnknownType { span, .. }
            | CheckDiagnostic::UnknownField { span, .. }
            | CheckDiagnostic::MissingField { span, .. }
            | CheckDiagnostic::TypeArityMismatch { span, .. }
            | CheckDiagnostic::MissingConstructor { span, .. }
            | CheckDiagnostic::RedundantMatchArm { span }
            | CheckDiagnostic::InvalidRecursiveConstant { span, .. }
            | CheckDiagnostic::UnsupportedQualifiedTopLevelConstant { span }
            | CheckDiagnostic::UnsupportedTopLevelFunction { span }
            | CheckDiagnostic::UnsupportedTypeDeclaration { span }
            | CheckDiagnostic::UnsupportedUseDeclaration { span }
            | CheckDiagnostic::UnsupportedExpression { span }
            | CheckDiagnostic::TypeMismatch { span, .. } => *span,
        }
    }

    pub fn label(&self) -> &'static str {
        match self {
            CheckDiagnostic::DuplicateTopLevelValue { .. } => "already defined here",
            CheckDiagnostic::DuplicateTypeDeclaration { .. } => "duplicate type declaration",
            CheckDiagnostic::DuplicateConstructor { .. } => "duplicate constructor",
            CheckDiagnostic::DuplicateRecordField { .. } => "duplicate field",
            CheckDiagnostic::DuplicateTypeParameter { .. } => "duplicate type parameter",
            CheckDiagnostic::UnknownValue { .. } => "unknown value",
            CheckDiagnostic::UnknownConstructor { .. } => "unknown constructor",
            CheckDiagnostic::UnknownType { .. } => "unknown type",
            CheckDiagnostic::UnknownField { .. } => "unknown field",
            CheckDiagnostic::MissingField { .. } => "missing field",
            CheckDiagnostic::TypeArityMismatch { .. } => "wrong number of type arguments",
            CheckDiagnostic::MissingConstructor { .. } => "missing constructor",
            CheckDiagnostic::RedundantMatchArm { .. } => "redundant arm",
            CheckDiagnostic::InvalidRecursiveConstant { .. } => "invalid recursive constant",
            CheckDiagnostic::UnsupportedQualifiedTopLevelConstant { .. } => {
                "qualified top-level constant"
            }
            CheckDiagnostic::UnsupportedTopLevelFunction { .. } => "top-level function declaration",
            CheckDiagnostic::UnsupportedTypeDeclaration { .. } => "type declaration",
            CheckDiagnostic::UnsupportedUseDeclaration { .. } => "use declaration",
            CheckDiagnostic::UnsupportedExpression { .. } => "unsupported expression",
            CheckDiagnostic::TypeMismatch { .. } => "type mismatch",
        }
    }
}
