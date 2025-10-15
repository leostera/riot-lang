open Std

type id =
  | E0001_MalformedTypeVariable
  | E0002_MissingLetBindingPattern
  | E0003_MissingLetBindingEquals
  | E0004_MissingLetBindingExpr
  | E0005_UnexpectedStructureItem
  | E0006_UnexpectedSignatureItem
  | E0007_InvalidPattern
  | E0008_InvalidExpression
  | E0009_InvalidConstant
  | E0010_InvalidTypeExpression
  | E0011_MissingLetKeyword
  | E0012_MissingTypeKeyword
  | E0013_MissingTypeDeclEquals
  | E0014_UnclosedDelimiter
  | E0015_MissingTypeName
  | E0016_EmptyCharLiteral
  | E0017_MultiCharLiteral
  | E0018_UnclosedCharLiteral
  | E0019_UnclosedTypeParams
  | E0020_MissingBinaryOperand
  | E0021_ConsecutiveBinaryOperators
  | E0022_InvalidTypeParameter
  | E0023_UppercaseTypeVariable
  | E0024_UppercaseTypeName
  | E0025_BracketedTypeParameters

val id_to_string : id -> string
val id_of_string : string -> id option
val name : id -> string
val explain : id -> string
