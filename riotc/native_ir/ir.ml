// Target-independent SSA for checked RiotML programs.
use Prelude.{List}
use Syntax.Ast.{BinaryOperator, IntegerLiteral, TextLiteral, ValueName}
use Syntax.Lexer.{SourceSpan}
use Check.Inference.{
  InferenceType, TypeScheme, TypeSubstitution, TypeVisibility, BindingIdentity,
  ConstructorIdentity
}

// Types retain their checker identities and are interpreted through the final
// substitution table; no runtime layout or target register is implied here.
type NativeModule = NativeModule {
  module_identity: string,
  nominal_types: List<TypeVisibility>,
  substitutions: List<TypeSubstitution>,
  globals: List<NativeGlobal>,
  initializer: NativeFunctionId,
  functions: List<NativeFunction>
}

type NativeGlobal = NativeGlobal {
  binding: BindingIdentity,
  scheme: TypeScheme,
  span: SourceSpan
}

type NativeFunctionId =
  | NativeTopLevelFunction(BindingIdentity)
  | NativeNestedFunction(NativeNestedFunctionId)
  | NativeModuleInitializer(string)

type NativeNestedFunctionId = NativeNestedFunctionId {
  parent: BindingIdentity,
  source_span: SourceSpan,
  ordinal: i32
}

// Block/value ordinals are function-local and must be unique; verification checks
// their definitions and every edge/use against this function's block graph.
type NativeBlockId = NativeBlockId(i32)
type NativeValueId = NativeValueId(i32)

type NativeCapture = NativeCapture {
  binding: BindingIdentity,
  checked_type: InferenceType,
  span: SourceSpan
}

type NativeFunction = NativeFunction {
  identity: NativeFunctionId,
  captures: List<NativeCapture>,
  result_type: InferenceType,
  entry: NativeBlockId,
  blocks: List<NativeBlock>,
  span: SourceSpan
}

type NativeBlock = NativeBlock {
  identity: NativeBlockId,
  parameters: List<NativeBlockParameter>,
  instructions: List<NativeInstruction>,
  terminator: NativeTerminator,
  span: SourceSpan
}

type NativeBlockParameter = NativeBlockParameter {
  value: NativeValueId,
  checked_type: InferenceType,
  span: SourceSpan
}

// Every instruction defines exactly one typed SSA value.
type NativeInstruction = NativeInstruction {
  result: NativeValueId,
  result_type: InferenceType,
  operation: NativeOperation,
  effects: NativeEffectSet,
  span: SourceSpan
}

type NativeOperation =
  | NativeConstant(NativeConstant)
  | NativePrimitiveOperation(NativePrimitive)
  | NativeCallOperation(NativeCall)
  | NativeCreateClosure(NativeClosure)
  | NativeCreateAggregate(NativeAggregate)
  | NativeProjectAggregate(NativeProjection)
  | NativeTestValue(NativeValueTest)
  | NativeLoadCapture(i32)
  | NativeLoadGlobal(BindingIdentity)
  | NativeStoreGlobal(NativeGlobalStore)

type NativeConstant =
  | NativeUnit
  | NativeInteger(IntegerLiteral)
  | NativeBoolean(bool)
  | NativeString(TextLiteral)

type NativePrimitive =
  | NativePrimitiveBinary(NativeBinaryPrimitive)
  | NativePrimitiveNegate(NativeNegatePrimitive)

type NativeBinaryPrimitive = NativeBinaryPrimitive {
  operator: BinaryOperator,
  operand_type: InferenceType,
  left: NativeValueId,
  right: NativeValueId
}

type NativeNegatePrimitive = NativeNegatePrimitive {
  operand_type: InferenceType,
  operand: NativeValueId
}

type NativeCall = NativeCall {
  target: NativeCallTarget,
  arguments: List<NativeValueId>,
  argument_types: List<InferenceType>,
  result_type: InferenceType
}

type NativeCallTarget =
  | NativeDirectCall(NativeFunctionId)
  | NativeClosureCall(NativeValueId)
  | NativeHostCall(BindingIdentity)

type NativeClosureCapture = NativeClosureCapture {
  capture: NativeCapture,
  value: NativeValueId
}

type NativeClosure = NativeClosure {
  function: NativeFunctionId,
  captures: List<NativeClosureCapture>
}

type NativeAggregate = NativeAggregate {
  kind: NativeAggregateKind,
  fields: List<NativeAggregateValue>
}

type NativeAggregateValue = NativeAggregateValue {
  field: NativeAggregateField,
  value: NativeValueId
}

type NativeAggregateKind =
  | NativeTupleAggregate
  | NativeRecordAggregate(ConstructorIdentity)
  | NativeVariantAggregate(ConstructorIdentity)

type NativeProjection = NativeProjection {
  aggregate: NativeValueId,
  field: NativeAggregateField
}

type NativeAggregateField =
  | NativeTupleField(i32)
  | NativeRecordField(ValueName)
  | NativeConstructorField(i32)

type NativeValueTest = NativeValueTest {
  value: NativeValueId,
  test: NativeTest
}

type NativeTest =
  | NativeLiteralTest(NativeConstant)
  | NativeConstructorTest(ConstructorIdentity)
  | NativeTupleArityTest(i32)
  | NativePatternMatchTest(NativePatternTest)

type NativePatternTest =
  | NativeAnyPatternTest
  | NativeLiteralPatternTest(NativeConstant)
  | NativeNestedConstructorPatternTest(NativeConstructorPatternTest)
  | NativeTuplePatternTest(List<NativePatternTest>)
  | NativeNestedRecordPatternTest(NativeRecordPatternTest)

type NativeConstructorPatternTest = NativeConstructorPatternTest {
  entity: ConstructorIdentity,
  arguments: List<NativePatternTest>
}

type NativeRecordPatternTest = NativeRecordPatternTest {
  entity: ConstructorIdentity,
  fields: List<NativeRecordPatternFieldTest>
}

type NativeRecordPatternFieldTest = NativeRecordPatternFieldTest {
  name: ValueName,
  pattern: NativePatternTest
}

type NativeGlobalStore = NativeGlobalStore {
  binding: BindingIdentity,
  value: NativeValueId
}

type NativeEffectSet = NativeEffectSet(List<NativeEffect>)

type NativeEffect =
  | NativeMayTrap(NativeTrapKind)
  | NativeAllocate
  | NativeReadMutableState
  | NativeWriteMutableState
  | NativeHostRead
  | NativeHostWrite
  | NativeHostProcess
  | NativeUnknownCall

type NativeTrapKind =
  | NativeDivisionByZero
  | NativeRemainderByZero
  | NativeArithmeticOverflow
  | NativePatternFailure
  | NativeExplicitTrap

type NativeEdge = NativeEdge {
  target: NativeBlockId,
  arguments: List<NativeValueId>
}

type NativeSwitchCase = NativeSwitchCase {
  test: NativeTest,
  edge: NativeEdge
}

// A block owns exactly one terminator; branch edges explicitly carry arguments.
type NativeTerminator =
  | NativeJump(NativeEdge)
  | NativeConditionalBranch(NativeBranch)
  | NativeSwitchTerminator(NativeSwitch)
  | NativeReturn(NativeValueId)
  | NativeTrap(NativeTrapKind)
  | NativeUnreachable

type NativeBranch = NativeBranch {
  condition: NativeValueId,
  when_true: NativeEdge,
  when_false: NativeEdge
}

type NativeSwitch = NativeSwitch {
  value: NativeValueId,
  cases: List<NativeSwitchCase>,
  default: NativeEdge
}
