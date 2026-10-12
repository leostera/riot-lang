// Deterministic printer for the physical-register AArch64 LinearIR.
use Prelude.{List, Nil, Cons, Option, Some, None}
use Syntax.Lexer.{SourceSpan}
use Syntax.Ast.{IntegerLiteral}
use NativeIr.Ir.{NativeBlockId, NativeDivisionByZero, NativeRemainderByZero,
  NativeArithmeticOverflow, NativePatternFailure, NativeExplicitTrap}
use NativeIr.Print.{print_function_id}
use Backend.Native.Aarch64.MachineIr.{
  A64VirtualRegister, A64Label, A64Unit, A64I32, A64Boolean, A64Pointer, A64Condition,
  A64Equal, A64NotEqual,
  A64SignedLessThan, A64SignedLessThanOrEqual, A64SignedGreaterThan,
  A64SignedGreaterThanOrEqual, A64RuntimeI32Division, A64RuntimeI32Remainder,
  A64RuntimeHostArgs
}
use Backend.Native.Aarch64.LinearIr.{
  A64RegisterWidth, A64W32, A64X64, A64PhysicalRegister, A64ValueLocation,
  A64FrameSlot, A64LinearLabel, A64BasicBlockLabel, A64TrueEdgeLabel,
  A64FalseEdgeLabel, A64LinearInstruction, A64MoveZero, A64LoadI32Literal,
  A64LoadBooleanLiteral, A64ReloadSpill, A64SpillRegister, A64AddW,
  A64SubtractW, A64MultiplyW, A64NegateW, A64CompareSetW, A64CallDirect,
  A64CallRuntime,
  A64LinearTerminator, A64LinearJump, A64LinearBranch, A64LinearReturn,
  A64LinearTrap, A64LinearUnreachable, A64LinearItem, A64LinearLabelItem,
  A64LinearInstructionItem, A64LinearTerminatorItem, A64LinearFunction,
  A64LinearFailure
}

let decimal_digit = fn value -> match value {
  | 0 -> "0" | 1 -> "1" | 2 -> "2" | 3 -> "3" | 4 -> "4"
  | 5 -> "5" | 6 -> "6" | 7 -> "7" | 8 -> "8" | _ -> "9"
}
let decimal_text = fn value -> match (value < 10) {
  | true -> decimal_digit value
  | false -> decimal_text (value / 10) + decimal_digit (value % 10)
}
let register_text = fn (A64PhysicalRegister { number, width }) -> match width {
  | A64W32 -> "w" + decimal_text number
  | A64X64 -> "x" + decimal_text number
}
let virtual_number = fn register -> match register {
  | A64VirtualRegister(number) -> number
}
let virtual_text = fn register -> "%v" + decimal_text (virtual_number register)
let label_text = fn label -> match label {
  | A64BasicBlockLabel(A64Label(NativeBlockId(number))) -> ".Lbb" + decimal_text number
  | A64TrueEdgeLabel(A64Label(NativeBlockId(number))) -> ".Ledge" + decimal_text number + "_true"
  | A64FalseEdgeLabel(A64Label(NativeBlockId(number))) -> ".Ledge" + decimal_text number + "_false"
}
let value_type_text = fn value_type -> match value_type {
  | A64Unit -> "unit"
  | A64I32 -> "i32"
  | A64Boolean -> "bool"
  | A64Pointer -> "ptr"
}
let condition_text = fn condition -> match condition {
  | A64Equal -> "eq"
  | A64NotEqual -> "ne"
  | A64SignedLessThan -> "lt"
  | A64SignedLessThanOrEqual -> "le"
  | A64SignedGreaterThan -> "gt"
  | A64SignedGreaterThanOrEqual -> "ge"
}
let print_slot = fn (A64FrameSlot { value, offset, value_type }) ->
  "[sp,#" + decimal_text offset + "] (" + value_type_text value_type + " "
    + virtual_text value + ")"
let print_location = fn (A64ValueLocation { value, value_type, slot }) -> match slot {
  | None -> "  " + virtual_text value + ":" + value_type_text value_type + " = erased\n"
  | Some(frame_slot) -> "  " + virtual_text value + ":" + value_type_text value_type
      + " = " + print_slot frame_slot + "\n"
}
let print_locations = fn values -> match values {
  | Nil -> ""
  | Cons(value, remaining) -> print_location value + print_locations remaining
}
let trap_text = fn reason -> match reason {
  | NativeDivisionByZero -> "division_by_zero"
  | NativeRemainderByZero -> "remainder_by_zero"
  | NativeArithmeticOverflow -> "arithmetic_overflow"
  | NativePatternFailure -> "pattern_failure"
  | NativeExplicitTrap -> "explicit_trap"
}
let print_instruction = fn instruction -> match instruction {
  | A64MoveZero(destination) -> "  mov " + register_text destination + ", xzr\n"
  | A64LoadI32Literal(destination, IntegerLiteral(value)) -> "  mov_imm " + register_text destination + ", #" + value + "\n"
  | A64LoadBooleanLiteral(destination, value) -> "  mov " + register_text destination + ", #" + (match value { | true -> "1" | false -> "0" }) + "\n"
  | A64ReloadSpill(destination, slot) -> "  ldr " + register_text destination + ", " + print_slot slot + "\n"
  | A64SpillRegister(slot, source) -> "  str " + register_text source + ", " + print_slot slot + "\n"
  | A64AddW(destination, left, right) -> "  add " + register_text destination + ", " + register_text left + ", " + register_text right + "\n"
  | A64SubtractW(destination, left, right) -> "  sub " + register_text destination + ", " + register_text left + ", " + register_text right + "\n"
  | A64MultiplyW(destination, left, right) -> "  mul " + register_text destination + ", " + register_text left + ", " + register_text right + "\n"
  | A64NegateW(destination, operand) -> "  neg " + register_text destination + ", " + register_text operand + "\n"
  | A64CompareSetW(condition, destination, left, right) -> "  cmp_set." + condition_text condition + " "
      + register_text destination + ", " + register_text left + ", " + register_text right + "\n"
  | A64CallDirect(function) -> "  bl " + print_function_id function + "\n"
  | A64CallRuntime(A64RuntimeI32Division) -> "  bl rt_native_i32_div\n"
  | A64CallRuntime(A64RuntimeI32Remainder) -> "  bl rt_native_i32_rem\n"
  | A64CallRuntime(A64RuntimeHostArgs) -> "  bl rt_native_host_args\n"
}
let print_terminator = fn terminator -> match terminator {
  | A64LinearJump(target) -> "  b " + label_text target + "\n"
  | A64LinearBranch(condition, when_true, when_false) -> "  cbnz " + register_text condition + ", "
      + label_text when_true + "\n  b " + label_text when_false + "\n"
  | A64LinearReturn -> "  ret\n"
  | A64LinearTrap(reason) -> "  trap " + trap_text reason + "\n"
  | A64LinearUnreachable -> "  unreachable\n"
}
let print_item = fn item -> match item {
  | A64LinearLabelItem(label) -> label_text label + ":\n"
  | A64LinearInstructionItem(instruction) -> print_instruction instruction
  | A64LinearTerminatorItem(terminator) -> print_terminator terminator
}
let print_items = fn items -> match items {
  | Nil -> ""
  | Cons(item, remaining) -> print_item item + print_items remaining
}
let print_failure = fn (A64LinearFailure {
  function, block: A64Label(NativeBlockId(block_number)),
  span: SourceSpan { start_codepoint, end_codepoint }, reason
}) -> "AArch64 LinearIR lowering failed in " + print_function_id function
  + " block " + decimal_text block_number + " at codepoints "
  + decimal_text start_codepoint + ".." + decimal_text end_codepoint
  + ": " + reason

let print_linear_failure = fn failure -> print_failure failure

let print_function = fn (A64LinearFunction {
  identity, result_type, entry, values, frame_size, items, span: _
}) -> "function " + print_function_id identity + " -> " + value_type_text result_type
  + " entry " + label_text entry + " frame " + decimal_text frame_size + " {\n"
  + print_locations values + print_items items + "}\n"

let print_linear_function = fn linear_function -> print_function linear_function
