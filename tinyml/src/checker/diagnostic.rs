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
