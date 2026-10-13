// Deterministic printer for the ABI-free AArch64 virtual-register IR.
use Prelude.{List, Nil, Cons}
use Syntax.Ast.{IntegerLiteral}
use Syntax.Lexer.{SourceSpan}
use NativeIr.Ir.{
  NativeBlockId, NativeDivisionByZero, NativeRemainderByZero,
  NativeArithmeticOverflow, NativePatternFailure, NativeExplicitTrap,
  NativeEffectSet, NativeMayTrap, NativeAllocate, NativeReadMutableState,
  NativeWriteMutableState, NativeHostRead, NativeHostWrite, NativeHostProcess,
  NativeUnknownCall
}
use NativeIr.Print.{print_function_id, print_binding}
use Backend.Native.Aarch64.MachineIr.{
  A64VirtualRegister, A64Label, A64Unit, A64I32, A64I64, A64Boolean, A64Pointer,
  A64Function, A64Block,
  A64BlockParameter, A64Instruction, A64Operation, A64LoadUnit, A64LoadI32, A64LoadI64,
  A64LoadBoolean, A64TestUnitLiteral,
  A64TestI32Literal, A64TestBooleanLiteral, A64I32LiteralTest,
  A64BooleanLiteralTest, A64Call, A64CallSite, A64DirectCall, A64HostCall,
  A64CallArgument, A64RuntimeCall, A64RuntimeCallSite, A64RuntimeI32Division,
  A64RuntimeI32Remainder, A64RuntimeHostArgs,
  A64AddI32, A64SubtractI32, A64MultiplyI32, A64AddI64, A64SubtractI64,
  A64MultiplyI64, A64CompareI32, A64CompareI64,
  A64EqualBoolean, A64NotEqualBoolean, A64NegateI32, A64NegateI64,
  A64BinaryOperands, A64IntegerComparison, A64Condition, A64Equal, A64NotEqual,
  A64SignedLessThan, A64SignedLessThanOrEqual, A64SignedGreaterThan,
  A64SignedGreaterThanOrEqual, A64Edge, A64Branch, A64Terminator, A64Jump,
  A64ConditionalBranch, A64Return, A64Trap, A64Unreachable,
  A64SelectionFailure
}

let decimal_digit = fn value -> match value {
  | 0 -> "0"
  | 1 -> "1"
  | 2 -> "2"
  | 3 -> "3"
  | 4 -> "4"
  | 5 -> "5"
  | 6 -> "6"
  | 7 -> "7"
  | 8 -> "8"
  | _ -> "9"
}

let decimal_text = fn value -> match (value < 10) {
  | true -> decimal_digit value
  | false -> decimal_text (value / 10) + decimal_digit (value % 10)
}

let print_register = fn (A64VirtualRegister(number)) -> "%v" + decimal_text number

let print_label = fn (A64Label(NativeBlockId(number))) -> ".Lbb" + decimal_text number

let print_value_type = fn value_type -> match value_type {
  | A64Unit -> "unit"
  | A64I32 -> "i32"
  | A64I64 -> "i64"
  | A64Boolean -> "bool"
  | A64Pointer -> "ptr"
}

let print_condition = fn condition -> match condition {
  | A64Equal -> "eq"
  | A64NotEqual -> "ne"
  | A64SignedLessThan -> "slt"
  | A64SignedLessThanOrEqual -> "sle"
  | A64SignedGreaterThan -> "sgt"
  | A64SignedGreaterThanOrEqual -> "sge"
}

let print_binary_operands = fn (A64BinaryOperands { left, right }) ->
  print_register left + ", " + print_register right

let print_call_target = fn target -> match target {
  | A64DirectCall(function) -> print_function_id function
  | A64HostCall(binding) -> "host " + print_binding binding
}

let print_runtime_helper = fn helper -> match helper {
  | A64RuntimeI32Division -> "rt_native_i32_div"
  | A64RuntimeI32Remainder -> "rt_native_i32_rem"
  | A64RuntimeHostArgs -> "rt_native_host_args"
}

let print_call_arguments = fn arguments -> match arguments {
  | Nil -> ""
  | Cons(A64CallArgument { register, value_type }, Nil) ->
    print_register register + ":" + print_value_type value_type
  | Cons(A64CallArgument { register, value_type }, remaining) ->
    print_register register + ":" + print_value_type value_type + ", "
      + print_call_arguments remaining
}

let print_effect = fn effect -> match effect {
  | NativeMayTrap(reason) -> "trap:" + print_trap_kind reason
  | NativeAllocate -> "allocate"
  | NativeReadMutableState -> "read_mutable_state"
  | NativeWriteMutableState -> "write_mutable_state"
  | NativeHostRead -> "host_read"
  | NativeHostWrite -> "host_write"
  | NativeHostProcess -> "host_process"
  | NativeUnknownCall -> "unknown_call"
}

let print_effect_list = fn effects -> match effects {
  | Nil -> ""
  | Cons(effect, Nil) -> print_effect effect
  | Cons(effect, remaining) -> print_effect effect + ", " + print_effect_list remaining
}

let print_effect_suffix = fn (NativeEffectSet(effects)) -> match effects {
  | Nil -> ""
  | _ -> " !{" + print_effect_list effects + "}"
}

let print_operation = fn operation -> match operation {
  | A64LoadUnit -> "load_unit"
  | A64TestUnitLiteral(_) -> "cmp_set.eq_unit"
  | A64LoadI32(IntegerLiteral(value)) -> "load_i32 " + value
  | A64LoadI64(IntegerLiteral(value)) -> "load_i64 " + value
  | A64LoadBoolean(value) -> match value {
    | true -> "load_bool true"
    | false -> "load_bool false"
  }
  | A64TestI32Literal(A64I32LiteralTest {
      value, literal: IntegerLiteral(text)
    }) -> "cmp_set.eq_w " + print_register value + ", #" + text
  | A64TestBooleanLiteral(A64BooleanLiteralTest { value, literal }) ->
    "cmp_set.eq_bool " + print_register value + ", " + (match literal {
      | true -> "true"
      | false -> "false"
    })
  | A64Call(A64CallSite { target, arguments, result_type }) ->
    "call " + print_call_target target + "(" + print_call_arguments arguments
      + ") -> " + print_value_type result_type
  | A64RuntimeCall(A64RuntimeCallSite { helper, arguments, result_type }) ->
    "call runtime " + print_runtime_helper helper + "("
      + print_call_arguments arguments + ") -> " + print_value_type result_type
  | A64AddI32(operands) -> "add_w " + print_binary_operands operands
  | A64SubtractI32(operands) -> "sub_w " + print_binary_operands operands
  | A64MultiplyI32(operands) -> "mul_w " + print_binary_operands operands
  | A64AddI64(operands) -> "add_x " + print_binary_operands operands
  | A64SubtractI64(operands) -> "sub_x " + print_binary_operands operands
  | A64MultiplyI64(operands) -> "mul_x " + print_binary_operands operands
  | A64CompareI32(A64IntegerComparison { condition, left, right }) ->
    "cmp_set." + print_condition condition + "_w "
      + print_register left + ", " + print_register right
  | A64CompareI64(A64IntegerComparison { condition, left, right }) ->
    "cmp_set." + print_condition condition + "_x "
      + print_register left + ", " + print_register right
  | A64EqualBoolean(operands) -> "cmp_set.eq_bool " + print_binary_operands operands
  | A64NotEqualBoolean(operands) -> "cmp_set.ne_bool " + print_binary_operands operands
  | A64NegateI32(operand) -> "neg_w " + print_register operand
  | A64NegateI64(operand) -> "neg_x " + print_register operand
}

let print_parameters = fn parameters -> match parameters {
  | Nil -> ""
  | Cons(A64BlockParameter { register, value_type, span: _ }, remaining) ->
    print_register register + ":" + print_value_type value_type
      + print_parameter_suffix remaining
}

let print_parameter_suffix = fn parameters -> match parameters {
  | Nil -> ""
  | Cons(A64BlockParameter { register, value_type, span: _ }, remaining) ->
    ", " + print_register register + ":" + print_value_type value_type
      + print_parameter_suffix remaining
}

let print_instruction = fn (A64Instruction {
  destination, result_type, operation, effects, span: _
}) ->
  "    " + print_register destination + ":" + print_value_type result_type
    + " = " + print_operation operation + print_effect_suffix effects + "\n"

let print_instructions = fn instructions -> match instructions {
  | Nil -> ""
  | Cons(instruction, remaining) -> print_instruction instruction
      + print_instructions remaining
}

let print_edge = fn (A64Edge { target, arguments }) ->
  print_label target + "(" + print_registers arguments + ")"

let print_registers = fn registers -> match registers {
  | Nil -> ""
  | Cons(register, Nil) -> print_register register
  | Cons(register, remaining) -> print_register register + ", " + print_registers remaining
}

let print_trap_kind = fn kind -> match kind {
  | NativeDivisionByZero -> "division_by_zero"
  | NativeRemainderByZero -> "remainder_by_zero"
  | NativeArithmeticOverflow -> "arithmetic_overflow"
  | NativePatternFailure -> "pattern_failure"
  | NativeExplicitTrap -> "explicit_trap"
}

let print_terminator = fn terminator -> match terminator {
  | A64Jump(edge) -> "    jump " + print_edge edge + "\n"
  | A64ConditionalBranch(A64Branch { condition, when_true, when_false }) ->
    "    branch " + print_register condition + ", " + print_edge when_true
      + ", " + print_edge when_false + "\n"
  | A64Return(value) -> "    return " + print_register value + "\n"
  | A64Trap(reason) -> "    trap " + print_trap_kind reason + "\n"
  | A64Unreachable -> "    unreachable\n"
}

let print_block = fn (A64Block {
  label, parameters, instructions, terminator, span: _
}) -> print_label label + "(" + print_parameters parameters + "):\n"
  + print_instructions instructions + print_terminator terminator

let print_blocks = fn blocks -> match blocks {
  | Nil -> ""
  | Cons(block, remaining) -> print_block block + print_blocks remaining
}

let print_function = fn (A64Function {
  identity, result_type, entry, blocks, span: _
}) -> "function " + print_function_id identity + " -> "
  + print_value_type result_type + " entry " + print_label entry + " {\n"
  + print_blocks blocks + "}\n"

let print_failure = fn (A64SelectionFailure {
  function, block: NativeBlockId(block),
  span: SourceSpan { start_codepoint, end_codepoint }, reason
}) -> "AArch64 selection failed in " + print_function_id function
  + " block " + decimal_text block + " at codepoints "
  + decimal_text start_codepoint + ".." + decimal_text end_codepoint
  + ": " + reason
