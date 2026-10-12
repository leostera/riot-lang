// Emit verified scalar and opaque-pointer AArch64 LinearIR as Mach-O assembly.
use Prelude.{List, Nil, Cons, Result, Ok, Err, Option, Some, None}
use String.{equal}
use Syntax.Ast.{IntegerLiteral}
use NativeIr.Ir.{
  NativeBlockId, NativeFunctionId, NativeDivisionByZero, NativeRemainderByZero,
  NativeArithmeticOverflow, NativePatternFailure, NativeExplicitTrap
}
use NativeIr.Print.{print_function_id}
use Backend.Native.Aarch64.MachineIr.{
  A64Label, A64Unit, A64I32, A64Boolean, A64Pointer, A64Equal, A64NotEqual,
  A64RuntimeI32Division, A64RuntimeI32Remainder,
  A64SignedLessThan, A64SignedLessThanOrEqual, A64SignedGreaterThan,
  A64SignedGreaterThanOrEqual
}
use Backend.Native.Aarch64.LinearIr.{
  A64RegisterWidth, A64W32, A64X64, A64PhysicalRegister, A64FrameSlot,
  A64LinearLabel, A64BasicBlockLabel, A64TrueEdgeLabel, A64FalseEdgeLabel,
  A64LinearInstruction, A64MoveZero, A64LoadI32Literal, A64LoadBooleanLiteral,
  A64ReloadSpill, A64SpillRegister, A64AddW, A64SubtractW, A64MultiplyW,
  A64NegateW, A64CompareSetW, A64CallDirect, A64CallRuntime,
  A64LinearTerminator,
  A64LinearJump, A64LinearBranch, A64LinearReturn, A64LinearTrap,
  A64LinearUnreachable, A64LinearItem, A64LinearLabelItem,
  A64LinearInstructionItem, A64LinearTerminatorItem, A64LinearFunction,
  w1
}
use Backend.Native.Aarch64.VerifyLinearIr.{verify_function}

type A64StackDirection = A64AllocateStack | A64ReleaseStack
type A64CodegenFunction = A64CodegenFunction {
  symbol: string,
  linear_function: A64LinearFunction
}

let digit_text = fn value -> match value {
  | 0 -> "0" | 1 -> "1" | 2 -> "2" | 3 -> "3" | 4 -> "4"
  | 5 -> "5" | 6 -> "6" | 7 -> "7" | 8 -> "8" | _ -> "9"
}
let decimal_text = fn value -> match (value < 10) {
  | true -> digit_text value
  | false -> decimal_text (value / 10) + digit_text (value % 10)
}
let register_text = fn (A64PhysicalRegister { number, width }) -> match width {
  | A64W32 -> "w" + decimal_text number
  | A64X64 -> "x" + decimal_text number
}
let zero_register = fn (A64PhysicalRegister { number: _, width }) -> match width {
  | A64W32 -> "wzr"
  | A64X64 -> "xzr"
}
let slot_offset = fn (A64FrameSlot { value: _, offset, value_type: _ }) -> offset
let label_text = fn symbol -> fn label -> match label {
  | A64BasicBlockLabel(A64Label(NativeBlockId(number))) ->
    ".L" + symbol + "_bb" + decimal_text number
  | A64TrueEdgeLabel(A64Label(NativeBlockId(number))) ->
    ".L" + symbol + "_edge" + decimal_text number + "_true"
  | A64FalseEdgeLabel(A64Label(NativeBlockId(number))) ->
    ".L" + symbol + "_edge" + decimal_text number + "_false"
}
let condition_text = fn condition -> match condition {
  | A64Equal -> "eq"
  | A64NotEqual -> "ne"
  | A64SignedLessThan -> "lt"
  | A64SignedLessThanOrEqual -> "le"
  | A64SignedGreaterThan -> "gt"
  | A64SignedGreaterThanOrEqual -> "ge"
}
let same_function = fn first -> fn second ->
  equal (print_function_id first) (print_function_id second)
let same_label = fn first -> fn second -> match first {
  | A64BasicBlockLabel(A64Label(NativeBlockId(left))) -> match second {
    | A64BasicBlockLabel(A64Label(NativeBlockId(right))) -> left == right
    | _ -> false
  }
  | A64TrueEdgeLabel(A64Label(NativeBlockId(left))) -> match second {
    | A64TrueEdgeLabel(A64Label(NativeBlockId(right))) -> left == right
    | _ -> false
  }
  | A64FalseEdgeLabel(A64Label(NativeBlockId(left))) -> match second {
    | A64FalseEdgeLabel(A64Label(NativeBlockId(right))) -> left == right
    | _ -> false
  }
}
let is_source_argument_register = fn (A64PhysicalRegister { number, width: _ }) ->
  number == 1
let find_function_symbol = fn wanted -> fn functions -> match functions {
  | Nil -> None
  | Cons(A64CodegenFunction {
      symbol, linear_function: A64LinearFunction {
        identity, result_type: _, entry: _, values: _, frame_size: _, items: _, span: _
      }
    }, remaining) -> match (same_function wanted identity) {
    | true -> Some(symbol)
    | false -> find_function_symbol wanted remaining
  }
}
let function_symbols = fn linear_functions -> fn ordinal -> match linear_functions {
  | Nil -> Nil
  | Cons(linear_function, remaining) -> Cons(A64CodegenFunction {
    symbol: "_riot_fn_" + decimal_text ordinal,
    linear_function: linear_function
  }, function_symbols remaining (ordinal + 1))
}

let adjust_mnemonic = fn direction -> match direction {
  | A64AllocateStack -> "sub"
  | A64ReleaseStack -> "add"
}
let emit_stack_adjust = fn direction -> fn bytes -> {
  let high = bytes / 4096;
  let low = bytes % 4096;
  let high_text = match high {
    | 0 -> ""
    | _ -> adjust_mnemonic direction + " sp, sp, #" + decimal_text high + ", lsl #12\n"
  };
  let low_text = match low {
    | 0 -> ""
    | _ -> adjust_mnemonic direction + " sp, sp, #" + decimal_text low + "\n"
  };
  high_text + low_text
}
let emit_load_spill = fn destination -> fn slot ->
  "ldr " + register_text destination + ", [sp,#"
    + decimal_text (slot_offset slot) + "]\n"
let emit_store_spill = fn slot -> fn source ->
  "str " + register_text source + ", [sp,#"
    + decimal_text (slot_offset slot) + "]\n"

let find_entry_source_argument = fn entry -> fn items -> match items {
  | Nil -> false
  | Cons(A64LinearLabelItem(label), remaining) -> match (same_label entry label) {
    | false -> find_entry_source_argument entry remaining
    | true -> match remaining {
      | Cons(A64LinearInstructionItem(A64SpillRegister(_, register)), _) -> is_source_argument_register register
      | _ -> false
    }
  }
  | Cons(_, remaining) -> find_entry_source_argument entry remaining
}

let emit_instruction = fn functions -> fn instruction -> match instruction {
  | A64MoveZero(destination) -> Ok("mov " + register_text destination + ", "
      + zero_register destination + "\n")
  | A64LoadI32Literal(destination, IntegerLiteral(value)) ->
    Ok("ldr " + register_text destination + ", =" + value + "\n")
  | A64LoadBooleanLiteral(destination, value) -> Ok("mov " + register_text destination + ", #"
      + (match value { | true -> "1" | false -> "0" }) + "\n")
  | A64ReloadSpill(destination, slot) -> Ok(emit_load_spill destination slot)
  | A64SpillRegister(slot, source) -> Ok(emit_store_spill slot source)
  | A64AddW(destination, left, right) -> Ok("add " + register_text destination + ", "
      + register_text left + ", " + register_text right + "\n")
  | A64SubtractW(destination, left, right) -> Ok("sub " + register_text destination + ", "
      + register_text left + ", " + register_text right + "\n")
  | A64MultiplyW(destination, left, right) -> Ok("mul " + register_text destination + ", "
      + register_text left + ", " + register_text right + "\n")
  | A64NegateW(destination, operand) -> Ok("neg " + register_text destination + ", "
      + register_text operand + "\n")
  | A64CompareSetW(condition, destination, left, right) -> Ok("cmp " + register_text left
      + ", " + register_text right + "\ncset " + register_text destination + ", "
      + condition_text condition + "\n")
  | A64CallDirect(target) -> match (find_function_symbol target functions) {
    | None -> Err("direct-call target is absent from the Codegen function set: "
        + print_function_id target)
    | Some(target_symbol) -> Ok("bl " + target_symbol + "\n")
  }
  | A64CallRuntime(A64RuntimeI32Division) -> Ok("bl _rt_native_i32_div\n")
  | A64CallRuntime(A64RuntimeI32Remainder) -> Ok("bl _rt_native_i32_rem\n")
}

let trap_immediate = fn reason -> match reason {
  | NativeDivisionByZero -> 257
  | NativeRemainderByZero -> 258
  | NativeArithmeticOverflow -> 259
  | NativePatternFailure -> 260
  | NativeExplicitTrap -> 261
}
let emit_terminator = fn symbol -> fn frame_size -> fn terminator -> match terminator {
  | A64LinearJump(target) -> "b " + label_text symbol target + "\n"
  | A64LinearBranch(condition, when_true, when_false) ->
    "cbnz " + register_text condition + ", " + label_text symbol when_true + "\nb "
      + label_text symbol when_false + "\n"
  | A64LinearReturn -> emit_stack_adjust A64ReleaseStack frame_size
      + "ldp x29, x30, [sp], #16\nret\n"
  | A64LinearTrap(NativePatternFailure) -> "bl _rt_pattern_fail\nbrk #260\n"
  | A64LinearTrap(reason) -> "brk #" + decimal_text (trap_immediate reason) + "\n"
  | A64LinearUnreachable -> "brk #511\n"
}
let emit_label = fn symbol -> fn label -> label_text symbol label + ":\n"
let emit_items = fn symbol -> fn frame_size -> fn functions -> fn items -> match items {
  | Nil -> Ok("")
  | Cons(A64LinearLabelItem(label), remaining) -> match (emit_items symbol frame_size functions remaining) {
    | Err(message) -> Err(message)
    | Ok(rest) -> Ok(emit_label symbol label + rest)
  }
  | Cons(A64LinearInstructionItem(instruction), remaining) ->
    match (emit_instruction functions instruction) {
      | Err(message) -> Err(message)
      | Ok(assembly) -> match (emit_items symbol frame_size functions remaining) {
        | Err(message) -> Err(message)
        | Ok(rest) -> Ok("    " + assembly + rest)
      }
    }
  | Cons(A64LinearTerminatorItem(terminator), remaining) ->
    match (emit_items symbol frame_size functions remaining) {
      | Err(message) -> Err(message)
      | Ok(rest) -> Ok("    " + emit_terminator symbol frame_size terminator + rest)
    }
}

let emit_function = fn codegen_function -> fn functions -> {
    let A64CodegenFunction { symbol, linear_function } = codegen_function;
    let A64LinearFunction {
      identity: _, result_type: _, entry, values: _, frame_size, items, span: _
    } = linear_function;
    match (emit_items symbol frame_size functions items) {
      | Err(message) -> Err(message)
      | Ok(body) -> Ok(".globl " + symbol + "\n.p2align 2\n" + symbol + ":\n"
        + "stp x29, x30, [sp, #-16]!\nmov x29, sp\n"
        + emit_stack_adjust A64AllocateStack frame_size
        + "b " + label_text symbol entry + "\n" + body)
    }
}

let emit_functions = fn functions -> fn all_functions -> match functions {
  | Nil -> Ok("")
  | Cons(function, remaining) -> match (emit_function function all_functions) {
    | Err(message) -> Err(message)
    | Ok(current) -> match (emit_functions remaining all_functions) {
      | Err(message) -> Err(message)
      | Ok(rest) -> Ok(current + rest)
    }
  }
}
let find_entry_function = fn wanted -> fn functions -> match functions {
  | Nil -> None
  | Cons(function, remaining) -> {
    let A64CodegenFunction { symbol: _, linear_function: A64LinearFunction {
      identity, result_type: _, entry: _, values: _, frame_size: _, items: _, span: _
    } } = function;
    match (same_function wanted identity) {
      | true -> Some(function)
      | false -> find_entry_function wanted remaining
    }
  }
}
let entry_has_unit_or_no_argument = fn function -> {
  let A64CodegenFunction { symbol: _, linear_function: A64LinearFunction {
    identity: _, result_type: _, entry, values: _, frame_size: _, items, span: _
  } } = function;
  match (find_entry_source_argument entry items) {
    | true -> false
    | false -> true
  }
}
let emit_entry_adapter = fn target_symbol ->
  ".globl _main\n.p2align 2\n_main:\n"
    + "stp x29, x30, [sp, #-16]!\nmov x29, sp\nmov x0, xzr\n"
    + "bl " + target_symbol + "\nldp x29, x30, [sp], #16\nret\n"

let verify_functions = fn functions -> match functions {
  | Nil -> Ok(0)
  | Cons(A64CodegenFunction { symbol: _, linear_function }, remaining) ->
    match (verify_function linear_function) {
      | Err(message) -> Err("LinearIR verifier rejected function: " + message)
      | Ok(_) -> verify_functions remaining
    }
}
let verify_codegen_frame_sizes = fn functions -> match functions {
  | Nil -> Ok(0)
  | Cons(A64CodegenFunction { symbol: _, linear_function: A64LinearFunction {
      identity: _, result_type: _, entry: _, values: _, frame_size, items: _, span: _
    } }, remaining) -> match (frame_size <= 16380) {
    | false -> Err("Codegen frame exceeds the current 16-KiB spill-addressing limit")
    | true -> verify_codegen_frame_sizes remaining
  }
}

// This entry adapter is a function-level harness. It intentionally does not
// emit module initializers, globals, closures, or general Host marshalling;
// only explicitly selected runtime helpers have dedicated call lowerings.
let emit_program = fn linear_functions -> fn entry_identity -> {
  let functions = function_symbols linear_functions 0;
  match (verify_functions functions) {
    | Err(message) -> Err(message)
    | Ok(_) -> match (verify_codegen_frame_sizes functions) {
      | Err(message) -> Err(message)
      | Ok(_) -> match (find_entry_function entry_identity functions) {
        | None -> Err("requested native entry function is not present in LinearIR")
        | Some(entry) -> {
          let A64CodegenFunction { symbol, linear_function: A64LinearFunction {
            identity: _, result_type, entry: _, values: _, frame_size: _, items: _, span: _
          } } = entry;
          match result_type {
            | A64Unit -> Err("function-level entry must return i32")
            | A64Boolean -> Err("function-level entry must return i32")
            | A64Pointer -> Err("function-level entry must return i32")
            | A64I32 -> match (entry_has_unit_or_no_argument entry) {
              | false -> Err("function-level entry cannot accept a source argument")
              | true -> match (emit_functions functions functions) {
                | Err(message) -> Err(message)
                | Ok(function_assembly) -> Ok(
                  ".section __TEXT,__text,regular,pure_instructions\n"
                  + function_assembly + emit_entry_adapter symbol)
              }
            }
          }
        }
      }
    }
  }
}
