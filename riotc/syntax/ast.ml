// RiotML source syntax shared by the frontend and later compiler phases.
use Prelude.{List, Option}

type ValueName = ValueName(string)
type TypeName = TypeName(string)
type ConstructorName = ConstructorName(string)
type ModuleName = ModuleName(string)
type TypeVariableName = TypeVariableName(string)
type IntegerLiteral = IntegerLiteral(string)
type TextLiteral = TextLiteral(string)

type ModulePath = ModulePath(List<ModuleName>)

type ImportedName =
  | ImportedValue(ValueName)
  | ImportedType(TypeName)
  | ImportedConstructor(ConstructorName)

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
  definition: TypeDefinition
}

type Pattern =
  | Variable(ValueName)
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

type Expr =
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
  body: Expr
}

type Declaration =
  | ValueDeclaration(ValueDeclarationData)
  | TypeDeclaration(TypeDeclarationData)

type SourceModule = SourceModule {
  imports: List<Import>,
  declarations: List<Declaration>
}

type Program =
  | Main(Expr)

type ValueType =
  | I32
  | BoolType
