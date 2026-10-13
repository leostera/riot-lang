// Physical-register, stack-slot AArch64 IR after ABI lowering and control-flow linearization.
// i32/bool use 32-bit slots; i64 and opaque pointers use aligned 64-bit slots.
use Prelude.{List, Option, Some, None}
use Syntax.Ast.{IntegerLiteral}
use Syntax.Lexer.{SourceSpan}
use NativeIr.Ir.{NativeFunctionId, NativeTrapKind}
use Backend.Native.Aarch64.MachineIr.{
  A64VirtualRegister, A64Label, A64ValueType, A64Unit, A64I32, A64I64, A64Boolean, A64Pointer,
  A64Condition, A64Equal, A64NotEqual, A64SignedLessThan,
  A64SignedLessThanOrEqual, A64SignedGreaterThan, A64SignedGreaterThanOrEqual,
  A64RuntimeHelper, A64RuntimeI32Division, A64RuntimeI32Remainder,
  A64RuntimeI64Division, A64RuntimeI64Remainder, A64RuntimeAllocate,
  A64ClosureLayout, A64CaptureField
}

type A64RegisterWidth = A64W32 | A64X64
type A64PhysicalRegister = A64PhysicalRegister {
  number: i32,
  width: A64RegisterWidth
}

type A64ValueLocation = A64ValueLocation {
  value: A64VirtualRegister,
  value_type: A64ValueType,
  slot: Option<A64FrameSlot>
}

type A64FrameSlot = A64FrameSlot {
  value: A64VirtualRegister,
  offset: i32,
  value_type: A64ValueType
}

type A64EnvironmentSlot = A64EnvironmentSlot { offset: i32 }

type A64LinearLabel =
  | A64BasicBlockLabel(A64Label)
  | A64TrueEdgeLabel(A64Label)
  | A64FalseEdgeLabel(A64Label)

type A64LinearInstruction =
  | A64MoveZero(A64PhysicalRegister)
  | A64LoadI32Literal(A64PhysicalRegister, IntegerLiteral)
  | A64LoadI64Literal(A64PhysicalRegister, IntegerLiteral)
  | A64LoadBooleanLiteral(A64PhysicalRegister, bool)
  | A64ReloadSpill(A64PhysicalRegister, A64FrameSlot)
  | A64SpillRegister(A64FrameSlot, A64PhysicalRegister)
  | A64AddW(A64PhysicalRegister, A64PhysicalRegister, A64PhysicalRegister)
  | A64AddX(A64PhysicalRegister, A64PhysicalRegister, A64PhysicalRegister)
  | A64SubtractW(A64PhysicalRegister, A64PhysicalRegister, A64PhysicalRegister)
  | A64SubtractX(A64PhysicalRegister, A64PhysicalRegister, A64PhysicalRegister)
  | A64MultiplyW(A64PhysicalRegister, A64PhysicalRegister, A64PhysicalRegister)
  | A64MultiplyX(A64PhysicalRegister, A64PhysicalRegister, A64PhysicalRegister)
  | A64NegateW(A64PhysicalRegister, A64PhysicalRegister)
  | A64NegateX(A64PhysicalRegister, A64PhysicalRegister)
  | A64CompareSetW(A64Condition, A64PhysicalRegister, A64PhysicalRegister,
      A64PhysicalRegister)
  | A64CompareSetX(A64Condition, A64PhysicalRegister, A64PhysicalRegister,
      A64PhysicalRegister)
  | A64LoadMemory(A64PhysicalRegister, A64PhysicalRegister, i32)
  | A64StoreMemory(A64PhysicalRegister, i32, A64PhysicalRegister)
  | A64LoadFunctionAddress(A64PhysicalRegister, NativeFunctionId)
  | A64ReloadEnvironment(A64PhysicalRegister, A64EnvironmentSlot)
  | A64SpillEnvironment(A64EnvironmentSlot, A64PhysicalRegister)
  | A64CallDirect(NativeFunctionId)
  | A64CallIndirect(A64PhysicalRegister)
  | A64CallRuntime(A64RuntimeHelper)

type A64LinearTerminator =
  | A64LinearJump(A64LinearLabel)
  | A64LinearBranch(A64PhysicalRegister, A64LinearLabel, A64LinearLabel)
  | A64LinearReturn
  | A64LinearTrap(NativeTrapKind)
  | A64LinearUnreachable

type A64LinearItem =
  | A64LinearLabelItem(A64LinearLabel)
  | A64LinearInstructionItem(A64LinearInstruction)
  | A64LinearTerminatorItem(A64LinearTerminator)

type A64LinearFunction = A64LinearFunction {
  identity: NativeFunctionId,
  result_type: A64ValueType,
  capture_layout: A64ClosureLayout,
  environment_slot: Option<A64EnvironmentSlot>,
  entry: A64LinearLabel,
  values: List<A64ValueLocation>,
  frame_size: i32,
  items: List<A64LinearItem>,
  span: SourceSpan
}

type A64LinearFailure = A64LinearFailure {
  function: NativeFunctionId,
  block: A64Label,
  span: SourceSpan,
  reason: string
}

let width_of_type = fn value_type -> match value_type {
  | A64Unit -> A64W32
  | A64I32 -> A64W32
  | A64I64 -> A64X64
  | A64Boolean -> A64W32
  | A64Pointer -> A64X64
}

// These are the only scratch and ABI registers emitted in this bootstrap slice.
// x18 is intentionally never allocated; x9-x11 are caller-saved scratch registers.
let w0 = A64PhysicalRegister { number: 0, width: A64W32 }
let w1 = A64PhysicalRegister { number: 1, width: A64W32 }
let x1 = A64PhysicalRegister { number: 1, width: A64X64 }
let w9 = A64PhysicalRegister { number: 9, width: A64W32 }
let w10 = A64PhysicalRegister { number: 10, width: A64W32 }
let w11 = A64PhysicalRegister { number: 11, width: A64W32 }
let x9 = A64PhysicalRegister { number: 9, width: A64X64 }
let x10 = A64PhysicalRegister { number: 10, width: A64X64 }
let x11 = A64PhysicalRegister { number: 11, width: A64X64 }
let x16 = A64PhysicalRegister { number: 16, width: A64X64 }
let x0 = A64PhysicalRegister { number: 0, width: A64X64 }

let slot_size = fn value_type -> match value_type {
  | A64Unit -> 0
  | A64I32 -> 4
  | A64I64 -> 8
  | A64Boolean -> 4
  | A64Pointer -> 8
}

let slot_alignment = fn value_type -> match value_type {
  | A64Unit -> 1
  | A64I32 -> 4
  | A64I64 -> 8
  | A64Boolean -> 4
  | A64Pointer -> 8
}

let align_frame = fn bytes -> match (bytes % 16) {
  | 0 -> bytes
  | remainder -> bytes + (16 - remainder)
}

let is_unit = fn value_type -> match value_type {
  | A64Unit -> true
  | _ -> false
}
