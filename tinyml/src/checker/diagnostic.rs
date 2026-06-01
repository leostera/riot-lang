//! Typed checker diagnostics.
//!
//! Diagnostics are structured enum values so tests and later tooling can inspect
//! checker results without parsing strings.

use super::unifier::UnifyError;

#[derive(Debug, Clone, PartialEq)]
pub enum CheckDiagnostic {
    DuplicateTopLevelValue { name: String },
    UnsupportedQualifiedTopLevelConstant,
    UnsupportedTopLevelFunction,
    UnsupportedTypeDeclaration,
    UnsupportedUseDeclaration,
    UnsupportedExpression,
    TypeMismatch { error: UnifyError },
}
