// RiotML source syntax shared by the frontend and later compiler phases.
use Prelude.{List, Option}
use Syntax.Lexer.{SourceSpan}

type ValueName = ValueName(string)
type TypeName = TypeName(string)
type ConstructorName = ConstructorName(string)
type ModuleName = ModuleName(string)
type MemberName = MemberName(string)
type TypeVariableName = TypeVariableName(string)
type IntegerLiteral = IntegerLiteral(string)
type TextLiteral = TextLiteral(string)

type ModulePath = ModulePath(List<ModuleName>)

type ImportedName = ImportedName(MemberName)

type Import = Import {
  module_path: ModulePath,
  selected_names: List<ImportedName>
}

type TypeExpr =
  | NamedType(NamedTypeExpression)
  | ArrowType(ArrowTypeExpression)
  | TupleType(List<TypeExpr>)
  | RecordType(List<RecordTypeField>)
  | VariantType(List<VariantDeclaration>)
  | TypeVariable(TypeVariableName)

type NamedTypeExpression = NamedTypeExpression {
  name: TypeName,
  arguments: List<TypeExpr>
}

type ArrowTypeExpression = ArrowTypeExpression {
  argument: TypeExpr,
  result: TypeExpr
}

type RecordTypeField = RecordTypeField {
  name: ValueName,
  field_type: TypeExpr
}

type VariantDeclaration =
  | TupleVariant(TupleVariantData)
  | RecordVariant(RecordVariantData)

type TupleVariantData = TupleVariantData {
  constructor_name: ConstructorName,
  fields: List<TypeExpr>
}

type RecordVariantData = RecordVariantData {
  constructor_name: ConstructorName,
  fields: List<RecordTypeField>
}

type TypeDefinition =
  | TypeAlias(TypeExpr)
  | TypeRecord(List<RecordTypeField>)
  | TypeVariants(List<VariantDeclaration>)

type TypeDeclarationData = TypeDeclarationData {
  name: TypeName,
  parameters: List<TypeVariableName>,
  definition: TypeDefinition,
  span: SourceSpan
}

type Pattern = Pattern {
  node: PatternKind,
  span: SourceSpan
}

type PatternKind =
  | Variable(ValueName)
  | PatternUnit
  | Wildcard
  | PatternInteger(IntegerLiteral)
  | PatternBoolean(bool)
  | PatternString(TextLiteral)
  | TuplePattern(List<Pattern>)
  | ConstructorPattern(ConstructorPatternData)
  | RecordPattern(RecordPatternData)
  | AscribedPattern(AscribedPatternData)

type ConstructorPatternData = ConstructorPatternData {
  constructor_name: ConstructorName,
  arguments: List<Pattern>
}

type RecordPatternData = RecordPatternData {
  constructor_name: ConstructorName,
  fields: List<RecordPatternField>
}

type RecordPatternField = RecordPatternField {
  name: ValueName,
  pattern: Pattern
}

type AscribedPatternData = AscribedPatternData {
  pattern: Pattern,
  annotation: TypeExpr
}

type BinaryOperator =
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | Remainder
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual

type Expr = Expr {
  node: ExprKind,
  span: SourceSpan
}

type ExprKind =
  | Integer(IntegerLiteral)
  | Boolean(bool)
  | StringLiteral(TextLiteral)
  | Unit
  | VariableExpr(ValueName)
  | TupleExpr(List<Expr>)
  | RecordExpr(RecordExpression)
  | Binary(BinaryExpression)
  | Negate(Expr)
  | Function(FunctionExpression)
  | Apply(ApplicationExpression)
  | Let(LetExpression)
  | Match(MatchExpression)
  | ConstructorExpr(ConstructorExpression)
  | Ascription(AscriptionExpression)
  | Block(List<Expr>)

type BinaryExpression = BinaryExpression {
  operator: BinaryOperator,
  left: Expr,
  right: Expr
}

type RecordExpression = RecordExpression {
  constructor_name: ConstructorName,
  fields: List<RecordExpressionField>
}

type RecordExpressionField = RecordExpressionField {
  name: ValueName,
  value: Expr
}

type FunctionExpression = FunctionExpression {
  parameter: Pattern,
  body: Expr
}

type ApplicationExpression = ApplicationExpression {
  callee: Expr,
  argument: Expr
}

type LetExpression = LetExpression {
  pattern: Pattern,
  annotation: Option<TypeExpr>,
  value: Expr,
  body: Expr
}

type MatchArm = MatchArm {
  pattern: Pattern,
  body: Expr
}

type MatchExpression = MatchExpression {
  scrutinee: Expr,
  arms: List<MatchArm>
}

type ConstructorExpression = ConstructorExpression {
  constructor_name: ConstructorName,
  arguments: List<Expr>
}

type AscriptionExpression = AscriptionExpression {
  expression: Expr,
  annotation: TypeExpr
}

type ValueDeclarationData = ValueDeclarationData {
  name: ValueName,
  body: Expr,
  span: SourceSpan
}

type Declaration =
  | ValueDeclaration(ValueDeclarationData)
  | TypeDeclaration(TypeDeclarationData)

type SourceModule = SourceModule {
  imports: List<Import>,
  declarations: List<Declaration>
}

let expression_kind = fn (Expr { node, span: _ }) -> node
let expression_span = fn (Expr { node: _, span }) -> span
let expression_with_span = fn (Expr { node, span: _ }) -> fn span -> Expr {
  node: node,
  span: span
}
let make_expression = fn node -> Expr {
  node: node,
  span: SourceSpan { start_codepoint: 0, end_codepoint: 0 }
}

let pattern_kind = fn (Pattern { node, span: _ }) -> node
let pattern_span = fn (Pattern { node: _, span }) -> span
let pattern_with_span = fn (Pattern { node, span: _ }) -> fn span -> Pattern {
  node: node,
  span: span
}
let make_pattern = fn node -> Pattern {
  node: node,
  span: SourceSpan { start_codepoint: 0, end_codepoint: 0 }
}
