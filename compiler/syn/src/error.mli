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
  | E0026_ListDoubleSemicolon
  | E0027_IfMissingThen
  | E0028_MatchMissingScrutinee
  | E0029_MatchMissingWith
  | E0030_MatchMissingPattern
  | E0031_MatchGuardMissingExpr
  | E0032_TuplePatternExtraComma
  | E0033_ConstructorPatternNeedsParens
  | E0034_ConsPatternMissingHead
  | E0035_ConsPatternMissingTail
  | E0036_OrPatternMissing
  | E0037_OrPatternDouble
  | E0038_MutableFieldMissingName
  | E0039_RecordFieldMissingColon
  | E0040_RecordFieldMissingType
  | E0041_PolyTypeMissingVarName
  | E0042_PolyTypeMissingDot
  | E0043_UnexpectedClosingDelimiter
  | E0044_MissingModuleDeclEquals
  | E0045_MissingExternalColon
  | E0046_MissingExceptionName
  | E0047_MissingModulePath
  | E0048_MissingModuleTypeName
  | E0049_MissingModuleTypeExpr
  | E0050_MissingModuleExpr
  | E0051_MissingWithKeyword
  | E0052_InvalidModuleName

val id_to_string: id -> string

val id_of_string: string -> id option

val name: id -> string

val explain: id -> string
