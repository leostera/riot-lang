use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use super::lexer::{Span, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expected {
    Item,
    TypeCapability,
    Expression,
    FunctionParameter,
    MatchArm,
    Pattern,
    ConstructorPatternPayload,
    Type,
    NamedRecordType,
    UseSuffix,
    Identifier,
    TypeVariable,
    RecordFieldPattern,
    Token(TokenKind),
}

impl std::fmt::Display for Expected {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expected::Item => f.write_str("expected item: use, pub, type, or let"),
            Expected::TypeCapability => f.write_str("expected `view` or `make` in pub(...)"),
            Expected::Expression => f.write_str("expected expression"),
            Expected::FunctionParameter => f.write_str("expected function parameter pattern"),
            Expected::MatchArm => f.write_str("expected at least one match arm"),
            Expected::Pattern => f.write_str("expected pattern"),
            Expected::ConstructorPatternPayload => {
                f.write_str("expected constructor pattern payload in parentheses")
            }
            Expected::Type => f.write_str("expected type"),
            Expected::NamedRecordType => f.write_str("expected named record type"),
            Expected::UseSuffix => f.write_str("expected `*` or `{` after `.` in use declaration"),
            Expected::Identifier => f.write_str("expected identifier"),
            Expected::TypeVariable => f.write_str("expected type variable"),
            Expected::RecordFieldPattern => {
                f.write_str("expected pattern after qualified record field")
            }
            Expected::Token(kind) => write!(f, "expected {kind:?}"),
        }
    }
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
pub enum ParseError {
    #[error("{expected}")]
    #[diagnostic(code(tinyml::parse::expected))]
    Expected {
        #[source_code]
        src: String,
        #[label("{expected}")]
        span: SourceSpan,
        expected: Expected,
    },

    #[error("{message}")]
    #[diagnostic(code(tinyml::parse::invalid_identifier))]
    InvalidIdentifier {
        #[source_code]
        src: String,
        #[label("{message}")]
        span: SourceSpan,
        message: &'static str,
    },

    #[error("expected type variable starting with lowercase after '")]
    #[diagnostic(code(tinyml::parse::invalid_type_variable))]
    InvalidTypeVariableName {
        #[source_code]
        src: String,
        #[label("expected type variable starting with lowercase after '")]
        span: SourceSpan,
    },

    #[error("pub(...) modifiers are only valid on type declarations")]
    #[diagnostic(code(tinyml::parse::type_modifier_on_value))]
    TypeModifierOnLet {
        #[source_code]
        src: String,
        #[label("pub(...) modifiers are only valid on type declarations")]
        span: SourceSpan,
    },

    #[error("visibility modifiers are not supported on use declarations")]
    #[diagnostic(code(tinyml::parse::visibility_on_use))]
    VisibilityOnUse {
        #[source_code]
        src: String,
        #[label("visibility modifiers are not supported on use declarations")]
        span: SourceSpan,
    },

    #[error("module-level let declarations are already corecursive; remove `rec`")]
    #[diagnostic(code(tinyml::parse::module_let_rec))]
    ModuleLetRec {
        #[source_code]
        src: String,
        #[label("remove `rec`")]
        span: SourceSpan,
    },
}

impl ParseError {
    pub fn expected(src: String, span: Span, expected: Expected) -> Self {
        Self::Expected {
            src,
            span: span.as_source_span(),
            expected,
        }
    }

    pub fn invalid_identifier(src: String, span: Span, message: &'static str) -> Self {
        Self::InvalidIdentifier {
            src,
            span: span.as_source_span(),
            message,
        }
    }

    pub fn invalid_type_variable_name(src: String, span: Span) -> Self {
        Self::InvalidTypeVariableName {
            src,
            span: span.as_source_span(),
        }
    }

    pub fn type_modifier_on_let(src: String, span: Span) -> Self {
        Self::TypeModifierOnLet {
            src,
            span: span.as_source_span(),
        }
    }

    pub fn visibility_on_use(src: String, span: Span) -> Self {
        Self::VisibilityOnUse {
            src,
            span: span.as_source_span(),
        }
    }

    pub fn module_let_rec(src: String, span: Span) -> Self {
        Self::ModuleLetRec {
            src,
            span: span.as_source_span(),
        }
    }

    pub fn message(&self) -> String {
        self.to_string()
    }

    pub fn source_span(&self) -> SourceSpan {
        match self {
            ParseError::Expected { span, .. }
            | ParseError::InvalidIdentifier { span, .. }
            | ParseError::InvalidTypeVariableName { span, .. }
            | ParseError::TypeModifierOnLet { span, .. }
            | ParseError::VisibilityOnUse { span, .. }
            | ParseError::ModuleLetRec { span, .. } => *span,
        }
    }
}
