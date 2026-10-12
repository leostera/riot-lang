// ABI-free AArch64 virtual-register IR for scalar operations and control flow.
// Calls retain logical targets/arguments; ABI assignment and runtime values stay unlowered.
use Prelude.{List}
use Syntax.Ast.{IntegerLiteral}
use Syntax.Lexer.{SourceSpan}
use Check.Inference.{BindingIdentity}
use NativeIr.Ir.{
  NativeFunctionId, NativeBlockId, NativeTrapKind, NativeEffectSet
}

type A64VirtualRegister = A64VirtualRegister(i32)
type A64Label = A64Label(NativeBlockId)

type A64ValueType = A64Unit | A64I32 | A64I64 | A64Boolean | A64Pointer

type A64Function = A64Function {
  identity: NativeFunctionId,
  result_type: A64ValueType,
  entry: A64Label,
  blocks: List<A64Block>,
  span: SourceSpan
}

type A64Block = A64Block {
  label: A64Label,
  parameters: List<A64BlockParameter>,
  instructions: List<A64Instruction>,
  terminator: A64Terminator,
  span: SourceSpan
}

type A64BlockParameter = A64BlockParameter {
  register: A64VirtualRegister,
  value_type: A64ValueType,
  span: SourceSpan
}

type A64Instruction = A64Instruction {
  destination: A64VirtualRegister,
  result_type: A64ValueType,
  operation: A64Operation,
  effects: NativeEffectSet,
  span: SourceSpan
}

type A64Operation =
  | A64LoadUnit
  | A64LoadI32(IntegerLiteral)
  | A64LoadI64(IntegerLiteral)
  | A64LoadBoolean(bool)
  | A64TestUnitLiteral(A64VirtualRegister)
  | A64TestI32Literal(A64I32LiteralTest)
  | A64TestBooleanLiteral(A64BooleanLiteralTest)
  | A64Call(A64CallSite)
  | A64RuntimeCall(A64RuntimeCallSite)
  | A64AddI32(A64BinaryOperands)
  | A64SubtractI32(A64BinaryOperands)
  | A64MultiplyI32(A64BinaryOperands)
  | A64CompareI32(A64IntegerComparison)
  | A64EqualBoolean(A64BinaryOperands)
  | A64NotEqualBoolean(A64BinaryOperands)
  | A64NegateI32(A64VirtualRegister)
  | A64NegateI64(A64VirtualRegister)

// This call pseudo carries typed virtual arguments but assigns no physical ABI registers.
type A64I32LiteralTest = A64I32LiteralTest {
  value: A64VirtualRegister,
  literal: IntegerLiteral
}

type A64BooleanLiteralTest = A64BooleanLiteralTest {
  value: A64VirtualRegister,
  literal: bool
}

type A64CallSite = A64CallSite {
  target: A64CallTarget,
  arguments: List<A64CallArgument>,
  result_type: A64ValueType
}

type A64CallTarget =
  | A64DirectCall(NativeFunctionId)
  | A64HostCall(BindingIdentity)

type A64CallArgument = A64CallArgument {
  register: A64VirtualRegister,
  value_type: A64ValueType
}

type A64RuntimeHelper =
  | A64RuntimeI32Division
  | A64RuntimeI32Remainder
  | A64RuntimeHostArgs
type A64RuntimeCallSite = A64RuntimeCallSite {
  helper: A64RuntimeHelper,
  arguments: List<A64CallArgument>,
  result_type: A64ValueType
}

type A64BinaryOperands = A64BinaryOperands {
  left: A64VirtualRegister,
  right: A64VirtualRegister
}

type A64IntegerComparison = A64IntegerComparison {
  condition: A64Condition,
  left: A64VirtualRegister,
  right: A64VirtualRegister
}

type A64Condition =
  | A64Equal
  | A64NotEqual
  | A64SignedLessThan
  | A64SignedLessThanOrEqual
  | A64SignedGreaterThan
  | A64SignedGreaterThanOrEqual

type A64Edge = A64Edge {
  target: A64Label,
  arguments: List<A64VirtualRegister>
}

type A64Branch = A64Branch {
  condition: A64VirtualRegister,
  when_true: A64Edge,
  when_false: A64Edge
}

// A64Trap is a semantic machine pseudo; codegen must choose its trap sequence.
type A64Terminator =
  | A64Jump(A64Edge)
  | A64ConditionalBranch(A64Branch)
  | A64Return(A64VirtualRegister)
  | A64Trap(NativeTrapKind)
  | A64Unreachable

type A64SelectionFailure = A64SelectionFailure {
  function: NativeFunctionId,
  block: NativeBlockId,
  span: SourceSpan,
  reason: string
}

// Before LinearIR this IR deliberately has only semantic virtual registers.
// Closure representation, aggregate layout, and runtime value representation are not modeled.
