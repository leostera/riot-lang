// Target-independent, typed RiotML core language.
use Prelude.{List}
use Syntax.Ast.{
  BinaryOperator, IntegerLiteral, TextLiteral, ValueName
}
use Syntax.Lexer.{SourceSpan}
use Check.Inference.{
  InferenceType, TypeScheme, TypeSubstitution, TypeVisibility,
  BindingIdentity, ConstructorIdentity
}

// Module declarations share a recursive scope; references retain binding identity.
type LambdaModule = LambdaModule {
  nominal_types: List<TypeVisibility>,
  substitutions: List<TypeSubstitution>,
  recursive_values: List<LambdaValueDeclaration>
}

type LambdaValueDeclaration = LambdaValueDeclaration {
  binding: BindingIdentity,
  scheme: TypeScheme,
  body: LambdaExpression,
  span: SourceSpan
}

type LambdaExpression = LambdaExpression {
  checked_type: InferenceType,
  kind: LambdaExpressionKind,
  span: SourceSpan
}

type LambdaExpressionKind =
  | LambdaConstantExpr(LambdaConstant)
  | LambdaValueReference(BindingIdentity)
  | LambdaConstruct(LambdaConstructorExpression)
  | LambdaFunction(LambdaFunctionExpression)
  | LambdaApply(LambdaApplyExpression)
  | LambdaLet(LambdaLetExpression)
  | LambdaBinaryPrimitive(LambdaBinaryPrimitiveExpression)
  | LambdaNegatePrimitive(LambdaNegatePrimitiveExpression)
  | LambdaTuple(List<LambdaExpression>)
  | LambdaRecord(LambdaRecordExpression)
  | LambdaMatch(LambdaMatchExpression)
  | LambdaBlock(List<LambdaExpression>)

type LambdaConstant =
  | LambdaUnit
  | LambdaInteger(IntegerLiteral)
  | LambdaBoolean(bool)
  | LambdaString(TextLiteral)

type LambdaConstructorExpression = LambdaConstructorExpression {
  entity: ConstructorIdentity,
  arguments: List<LambdaExpression>
}

type LambdaFunctionExpression = LambdaFunctionExpression {
  parameter: LambdaPattern,
  body: LambdaExpression
}

// A resolved value-reference callee is a direct-call candidate; computed callees
// remain closure calls. The distinction does not expose target calling details.
type LambdaApplyExpression = LambdaApplyExpression {
  callee: LambdaExpression,
  argument: LambdaExpression
}

type LambdaLetExpression = LambdaLetExpression {
  binding: LambdaPattern,
  value: LambdaExpression,
  body: LambdaExpression
}

type LambdaBinaryPrimitiveExpression = LambdaBinaryPrimitiveExpression {
  operator: BinaryOperator,
  operand_type: InferenceType,
  left: LambdaExpression,
  right: LambdaExpression
}

type LambdaNegatePrimitiveExpression = LambdaNegatePrimitiveExpression {
  operand_type: InferenceType,
  operand: LambdaExpression
}

type LambdaRecordExpression = LambdaRecordExpression {
  entity: ConstructorIdentity,
  fields: List<LambdaRecordFieldValue>
}

type LambdaRecordFieldValue = LambdaRecordFieldValue {
  name: ValueName,
  value: LambdaExpression
}

type LambdaMatchExpression = LambdaMatchExpression {
  scrutinee: LambdaExpression,
  arms: List<LambdaMatchArm>
}

type LambdaMatchArm = LambdaMatchArm {
  pattern: LambdaPattern,
  body: LambdaExpression
}

type LambdaPattern = LambdaPattern {
  checked_type: InferenceType,
  kind: LambdaPatternKind,
  span: SourceSpan
}

type LambdaPatternKind =
  | LambdaBind(BindingIdentity)
  | LambdaWildcard
  | LambdaConstantPattern(LambdaConstant)
  | LambdaConstructPattern(LambdaConstructorPattern)
  | LambdaTuplePattern(List<LambdaPattern>)
  | LambdaRecordPatternNode(LambdaRecordPattern)

type LambdaConstructorPattern = LambdaConstructorPattern {
  entity: ConstructorIdentity,
  arguments: List<LambdaPattern>
}

type LambdaRecordPattern = LambdaRecordPattern {
  entity: ConstructorIdentity,
  fields: List<LambdaRecordPatternField>
}

type LambdaRecordPatternField = LambdaRecordPatternField {
  name: ValueName,
  pattern: LambdaPattern
}
