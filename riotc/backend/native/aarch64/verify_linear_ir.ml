// Structural and target-convention checks for AArch64 LinearIR.
use Prelude.{List, Nil, Cons, Result, Ok, Err, Option, Some, None}
use NativeIr.Ir.{
  NativeBlockId, NativeTopLevelFunction, NativeNestedFunction,
  NativeModuleInitializer
}
use Syntax.Ast.{IntegerLiteral}
use Backend.Native.Aarch64.MachineIr.{
  A64VirtualRegister, A64Label, A64Unit, A64I32, A64I64, A64Boolean, A64Pointer,
  A64Equal, A64NotEqual, A64SignedLessThan, A64SignedLessThanOrEqual,
  A64SignedGreaterThan, A64SignedGreaterThanOrEqual,
  A64RuntimeI32Division, A64RuntimeI32Remainder, A64RuntimeI64Division,
  A64RuntimeI64Remainder, A64RuntimeHostArgs, A64RuntimeAllocate,
  A64ClosureLayout, A64CaptureField
}
use Backend.Native.Aarch64.LinearIr.{
  A64RegisterWidth, A64W32, A64X64, A64PhysicalRegister, A64ValueLocation,
  A64FrameSlot, A64EnvironmentSlot, A64LinearLabel, A64BasicBlockLabel, A64TrueEdgeLabel,
  A64FalseEdgeLabel, A64LinearInstruction, A64MoveZero, A64LoadI32Literal,
  A64LoadI64Literal, A64LoadBooleanLiteral, A64ReloadSpill, A64SpillRegister, A64AddW,
  A64AddX, A64SubtractW, A64SubtractX, A64MultiplyW, A64MultiplyX,
  A64NegateW, A64NegateX, A64CompareSetW, A64CompareSetX,
  A64LoadMemory, A64StoreMemory, A64LoadFunctionAddress,
  A64ReloadEnvironment, A64SpillEnvironment, A64CallDirect, A64CallIndirect,
  A64CallRuntime,
  A64LinearTerminator, A64LinearJump, A64LinearBranch, A64LinearReturn,
  A64LinearTrap, A64LinearUnreachable, A64LinearItem, A64LinearLabelItem,
  A64LinearInstructionItem, A64LinearTerminatorItem, A64LinearFunction,
  w0, w1, w9, w10, w11, x0, x1, x9, x10, x11, x16,
  width_of_type, slot_size
}

let bool_and = fn first -> fn second -> match first {
  | true -> second
  | false -> false
}
let bool_not = fn value -> match value { | true -> false | false -> true }
let bool_or = fn first -> fn second -> match first {
  | true -> true
  | false -> second
}
let register_number = fn register -> match register {
  | A64VirtualRegister(number) -> number
}
let same_register = fn first -> fn second ->
  register_number first == register_number second
let physical_number = fn (A64PhysicalRegister { number, width: _ }) -> number
let physical_width = fn (A64PhysicalRegister { number: _, width }) -> width
let same_width = fn first -> fn second -> match first {
  | A64W32 -> match second { | A64W32 -> true | _ -> false }
  | A64X64 -> match second { | A64X64 -> true | _ -> false }
}
let same_physical = fn first -> fn second -> bool_and
  (physical_number first == physical_number second)
  (same_width (physical_width first) (physical_width second))
let same_label = fn first -> fn second -> match first {
  | A64BasicBlockLabel(A64Label(NativeBlockId(a))) -> match second {
    | A64BasicBlockLabel(A64Label(NativeBlockId(b))) -> a == b
    | _ -> false
  }
  | A64TrueEdgeLabel(A64Label(NativeBlockId(a))) -> match second {
    | A64TrueEdgeLabel(A64Label(NativeBlockId(b))) -> a == b
    | _ -> false
  }
  | A64FalseEdgeLabel(A64Label(NativeBlockId(a))) -> match second {
    | A64FalseEdgeLabel(A64Label(NativeBlockId(b))) -> a == b
    | _ -> false
  }
}

let has_duplicate_values = fn values -> match values {
  | Nil -> false
  | Cons(A64ValueLocation { value, value_type: _, slot: _ }, remaining) ->
    match (contains_value value remaining) {
      | true -> true
      | false -> has_duplicate_values remaining
    }
}
let contains_value = fn wanted -> fn values -> match values {
  | Nil -> false
  | Cons(A64ValueLocation { value, value_type: _, slot: _ }, remaining) ->
    match (same_register wanted value) {
      | true -> true
      | false -> contains_value wanted remaining
    }
}

let valid_slot = fn (A64FrameSlot { value: _, offset, value_type }) -> match value_type {
  | A64Unit -> false
  | A64I32 -> match (offset >= 0) {
    | false -> false
    | true -> match (offset <= 2147483628) { | true -> offset % 4 == 0 | false -> false }
  }
  | A64Boolean -> match (offset >= 0) {
    | false -> false
    | true -> match (offset <= 2147483628) { | true -> offset % 4 == 0 | false -> false }
  }
  | A64I64 -> match (offset >= 0) {
    | false -> false
    | true -> match (offset <= 2147483632) { | true -> offset % 8 == 0 | false -> false }
  }
  | A64Pointer -> match (offset >= 0) {
    | false -> false
    | true -> match (offset <= 2147483632) { | true -> offset % 8 == 0 | false -> false }
  }
}
let frame_slot_value = fn (A64FrameSlot { value, offset: _, value_type: _ }) -> value
let frame_slot_offset = fn (A64FrameSlot { value: _, offset, value_type: _ }) -> offset
let frame_slot_type = fn (A64FrameSlot { value: _, offset: _, value_type }) -> value_type
let environment_offset = fn (A64EnvironmentSlot { offset }) -> offset
let same_environment_slot = fn first -> fn second ->
  environment_offset first == environment_offset second
let valid_environment_slot = fn slot -> {
  let offset = environment_offset slot;
  match (offset >= 0) {
    | false -> false
    | true -> match (offset <= 2147483632) { | true -> offset % 8 == 0 | false -> false }
  }
}
let slot_is_known = fn wanted -> fn values -> match values {
  | Nil -> false
  | Cons(A64ValueLocation { value: _, value_type: _, slot: None }, remaining) ->
    slot_is_known wanted remaining
  | Cons(A64ValueLocation { value, value_type, slot: Some(slot) }, remaining) ->
    match (same_slot wanted slot) {
      | true -> true
      | false -> slot_is_known wanted remaining
    }
}
let same_slot = fn first -> fn second ->
  bool_and (same_register (frame_slot_value first) (frame_slot_value second))
    (bool_and (frame_slot_offset first == frame_slot_offset second)
      (same_type (frame_slot_type first) (frame_slot_type second)))
let same_type = fn first -> fn second -> match first {
  | A64Unit -> match second { | A64Unit -> true | _ -> false }
  | A64I32 -> match second { | A64I32 -> true | _ -> false }
  | A64I64 -> match second { | A64I64 -> true | _ -> false }
  | A64Boolean -> match second { | A64Boolean -> true | _ -> false }
  | A64Pointer -> match second { | A64Pointer -> true | _ -> false }
}

let location_end = fn location -> match location {
  | A64ValueLocation { value: _, value_type: A64Unit, slot: None } -> 0
  | A64ValueLocation { value: _, value_type, slot: Some(A64FrameSlot { value: _, offset, value_type: _ }) } -> offset + slot_size value_type
  | _ -> -1
}
let max_int = fn first -> fn second -> match (first >= second) {
  | true -> first
  | false -> second
}
let maximum_end = fn values -> match values {
  | Nil -> 0
  | Cons(value, remaining) -> max_int (location_end value) (maximum_end remaining)
}
let slot_ranges_overlap = fn first -> fn second -> {
  let first_offset = frame_slot_offset first;
  let second_offset = frame_slot_offset second;
  match (first_offset <= second_offset) {
    | true -> second_offset - first_offset < slot_size (frame_slot_type first)
    | false -> first_offset - second_offset < slot_size (frame_slot_type second)
  }
}
let contains_overlapping_slot = fn wanted -> fn values -> match values {
  | Nil -> false
  | Cons(A64ValueLocation { value: _, value_type: _, slot: None }, remaining) ->
    contains_overlapping_slot wanted remaining
  | Cons(A64ValueLocation { value: _, value_type: _, slot: Some(slot) }, remaining) ->
    match (slot_ranges_overlap wanted slot) {
      | true -> true
      | false -> contains_overlapping_slot wanted remaining
    }
}
let has_overlapping_slot_ranges = fn values -> match values {
  | Nil -> false
  | Cons(A64ValueLocation { value: _, value_type: _, slot: None }, remaining) ->
    has_overlapping_slot_ranges remaining
  | Cons(A64ValueLocation { value: _, value_type: _, slot: Some(slot) }, remaining) ->
    match (contains_overlapping_slot slot remaining) {
      | true -> true
      | false -> has_overlapping_slot_ranges remaining
    }
}
let validate_locations = fn values -> match (has_duplicate_values values) {
  | true -> Err("LinearIR assigns more than one location to an SSA value")
  | false -> match (validate_location_list values) {
    | Err(message) -> Err(message)
    | Ok(_) -> match (has_overlapping_slot_ranges values) {
      | true -> Err("LinearIR assigns overlapping SSA spill slots")
      | false -> Ok(0)
    }
  }
}
let validate_location_list = fn values -> match values {
  | Nil -> Ok(0)
  | Cons(A64ValueLocation { value, value_type, slot }, remaining) -> match value_type {
    | A64Unit -> match slot {
      | None -> validate_location_list remaining
      | Some(_) -> Err("unit must have no spill slot")
    }
    | A64I32 -> match slot {
      | None -> Err("i32 is missing its unboxed spill slot")
      | Some(frame_slot) -> match (same_register value (frame_slot_value frame_slot)) {
        | false -> Err("spill slot is assigned to a different SSA value")
        | true -> match (same_type value_type (frame_slot_type frame_slot)) {
          | false -> Err("spill slot type differs from its SSA value type")
          | true -> match (valid_slot frame_slot) {
            | true -> validate_location_list remaining
            | false -> Err("spill slot has an invalid offset or unit representation")
          }
        }
      }
    }
    | A64I64 -> match slot {
      | None -> Err("i64 is missing its unboxed spill slot")
      | Some(frame_slot) -> match (same_register value (frame_slot_value frame_slot)) {
        | false -> Err("spill slot is assigned to a different SSA value")
        | true -> match (same_type value_type (frame_slot_type frame_slot)) {
          | false -> Err("spill slot type differs from its SSA value type")
          | true -> match (valid_slot frame_slot) {
            | true -> validate_location_list remaining
            | false -> Err("spill slot has an invalid offset or unit representation")
          }
        }
      }
    }
    | A64Boolean -> match slot {
      | None -> Err("bool is missing its unboxed spill slot")
      | Some(frame_slot) -> match (same_register value (frame_slot_value frame_slot)) {
        | false -> Err("spill slot is assigned to a different SSA value")
        | true -> match (same_type value_type (frame_slot_type frame_slot)) {
          | false -> Err("spill slot type differs from its SSA value type")
          | true -> match (valid_slot frame_slot) {
            | true -> validate_location_list remaining
            | false -> Err("spill slot has an invalid offset or unit representation")
          }
        }
      }
    }
    | A64Pointer -> match slot {
      | None -> Err("pointer is missing its 64-bit spill slot")
      | Some(frame_slot) -> match (same_register value (frame_slot_value frame_slot)) {
        | false -> Err("spill slot is assigned to a different SSA value")
        | true -> match (same_type value_type (frame_slot_type frame_slot)) {
          | false -> Err("spill slot type differs from its SSA value type")
          | true -> match (valid_slot frame_slot) {
            | true -> validate_location_list remaining
            | false -> Err("spill slot has an invalid offset or unit representation")
          }
        }
      }
    }
  }
}

let physical_matches_type = fn register -> fn value_type ->
  same_width (physical_width register) (width_of_type value_type)
let physical_number_is = fn wanted -> fn register -> physical_number register == wanted
let valid_memory_offset = fn register -> fn offset -> match (physical_width register) {
  | A64W32 -> bool_and (offset >= 0) (bool_and (offset <= 16380) (offset % 4 == 0))
  | A64X64 -> bool_and (offset >= 0) (bool_and (offset <= 32760) (offset % 8 == 0))
}
let physical_register_allowed = fn (A64PhysicalRegister { number, width }) -> match number {
  | 0 -> true
  | 1 -> true
  | 9 -> true
  | 10 -> true
  | 11 -> true
  | _ -> false
}
let valid_instruction_registers = fn instruction -> fn values ->
  fn environment_slot -> match instruction {
  | A64MoveZero(register) -> match register {
    | A64PhysicalRegister { number: 0, width: A64X64 } -> true
    | _ -> false
  }
  | A64LoadI32Literal(register, _) -> bool_or (same_physical register w9) (same_physical register w10)
  | A64LoadI64Literal(register, _) -> bool_or (same_physical register x9)
      (bool_or (same_physical register x10) (same_physical register x0))
  | A64LoadBooleanLiteral(register, _) -> bool_or (same_physical register w9) (same_physical register w10)
  | A64ReloadSpill(register, slot) -> bool_and (physical_register_allowed register)
      (bool_and (physical_matches_type register (frame_slot_type slot)) (slot_is_known slot values))
  | A64SpillRegister(slot, register) -> bool_and (physical_register_allowed register)
      (bool_and (physical_matches_type register (frame_slot_type slot)) (slot_is_known slot values))
  | A64LoadMemory(destination, base, offset) -> bool_and (same_physical base x9)
      (bool_and (valid_memory_offset destination offset)
        (bool_or (same_physical destination w10)
          (bool_or (same_physical destination x10) (same_physical destination x16))))
  | A64StoreMemory(base, offset, source) -> bool_and (same_physical base x9)
      (bool_and (valid_memory_offset source offset)
        (bool_or (same_physical source w10) (same_physical source x10)))
  | A64LoadFunctionAddress(destination, _) -> same_physical destination x10
  | A64ReloadEnvironment(destination, slot) -> match environment_slot {
    | None -> false
    | Some(expected) -> bool_and (same_physical destination x9)
        (same_environment_slot slot expected)
  }
  | A64SpillEnvironment(slot, source) -> match environment_slot {
    | None -> false
    | Some(expected) -> bool_and (same_physical source x0)
        (same_environment_slot slot expected)
  }
  | A64CallIndirect(target) -> same_physical target x16
  | A64AddW(destination, left, right) -> bool_and (same_physical destination w11)
      (bool_and (same_physical left w9) (same_physical right w10))
  | A64AddX(destination, left, right) -> bool_and (same_physical destination x11)
      (bool_and (same_physical left x9) (same_physical right x10))
  | A64SubtractW(destination, left, right) -> bool_and (same_physical destination w11)
      (bool_and (same_physical left w9) (same_physical right w10))
  | A64SubtractX(destination, left, right) -> bool_and (same_physical destination x11)
      (bool_and (same_physical left x9) (same_physical right x10))
  | A64MultiplyW(destination, left, right) -> bool_and (same_physical destination w11)
      (bool_and (same_physical left w9) (same_physical right w10))
  | A64MultiplyX(destination, left, right) -> bool_and (same_physical destination x11)
      (bool_and (same_physical left x9) (same_physical right x10))
  | A64NegateW(destination, operand) -> bool_and (same_physical destination w11) (same_physical operand w9)
  | A64NegateX(destination, operand) -> bool_and (same_physical destination x11) (same_physical operand x9)
  | A64CompareSetW(_, destination, left, right) -> bool_and (same_physical destination w11)
      (bool_and (same_physical left w9) (same_physical right w10))
  | A64CompareSetX(_, destination, left, right) -> bool_and (same_physical destination w11)
      (bool_and (same_physical left x9) (same_physical right x10))
  | A64CallDirect(_) -> true
  | A64CallRuntime(_) -> true
}

let contains_label = fn wanted -> fn items -> match items {
  | Nil -> false
  | Cons(A64LinearLabelItem(label), remaining) -> match (same_label wanted label) {
    | true -> true
    | false -> contains_label wanted remaining
  }
  | Cons(_, remaining) -> contains_label wanted remaining
}
let duplicate_labels = fn items -> match items {
  | Nil -> false
  | Cons(A64LinearLabelItem(label), remaining) -> match (contains_label label remaining) {
    | true -> true
    | false -> duplicate_labels remaining
  }
  | Cons(_, remaining) -> duplicate_labels remaining
}
let valid_terminator_targets = fn terminator -> fn items -> match terminator {
  | A64LinearJump(target) -> contains_label target items
  | A64LinearBranch(_, when_true, when_false) -> bool_and
      (contains_label when_true items) (contains_label when_false items)
  | A64LinearReturn -> true
  | A64LinearTrap(_) -> true
  | A64LinearUnreachable -> true
}
let verify_references_from = fn all_items -> fn pending_items -> match pending_items {
  | Nil -> Ok(0)
  | Cons(A64LinearTerminatorItem(terminator), remaining) ->
    match (valid_terminator_targets terminator all_items) {
      | false -> Err("LinearIR branch targets a label that is not defined")
      | true -> verify_references_from all_items remaining
    }
  | Cons(_, remaining) -> verify_references_from all_items remaining
}
let verify_references = fn items -> verify_references_from items items

let null_environment_setup = fn instruction -> match instruction {
  | A64MoveZero(register) -> same_physical register x0
  | _ -> false
}
let source_argument_setup = fn instruction -> match instruction {
  | A64ReloadSpill(register, slot) -> bool_and (physical_number_is 1 register)
      (physical_matches_type register (frame_slot_type slot))
  | _ -> false
}
let runtime_i32_argument_setup = fn expected -> fn instruction -> match instruction {
  | A64ReloadSpill(register, slot) -> bool_and (same_physical register expected)
      (same_type (frame_slot_type slot) A64I32)
  | _ -> false
}
let valid_runtime_call_setup = fn helper -> fn previous -> fn last -> match helper {
  | A64RuntimeHostArgs -> true
  | A64RuntimeAllocate -> match last {
    | Some(A64LoadI64Literal(register, IntegerLiteral(_))) -> same_physical register x0
    | _ -> false
  }
  | A64RuntimeI32Division -> valid_i32_runtime_call_setup previous last
  | A64RuntimeI32Remainder -> valid_i32_runtime_call_setup previous last
  | A64RuntimeI64Division -> valid_i64_runtime_call_setup previous last
  | A64RuntimeI64Remainder -> valid_i64_runtime_call_setup previous last
}
let valid_i32_runtime_call_setup = fn previous -> fn last -> match last {
  | Some(instruction) -> match (runtime_i32_argument_setup w1 instruction) {
    | false -> false
    | true -> match previous {
      | Some(previous_instruction) -> runtime_i32_argument_setup w0 previous_instruction
      | None -> false
    }
  }
  | None -> false
}
let runtime_i64_argument_setup = fn expected -> fn instruction -> match instruction {
  | A64ReloadSpill(register, slot) -> bool_and (same_physical register expected)
      (same_type (frame_slot_type slot) A64I64)
  | _ -> false
}
let valid_i64_runtime_call_setup = fn previous -> fn last -> match last {
  | Some(instruction) -> match (runtime_i64_argument_setup x1 instruction) {
    | false -> false
    | true -> match previous {
      | Some(previous_instruction) -> runtime_i64_argument_setup x0 previous_instruction
      | None -> false
    }
  }
  | None -> false
}
let runtime_i32_result_setup = fn instruction -> match instruction {
  | A64SpillRegister(slot, register) -> bool_and (same_physical register w0)
      (same_type (frame_slot_type slot) A64I32)
  | _ -> false
}
let runtime_i64_result_setup = fn instruction -> match instruction {
  | A64SpillRegister(slot, register) -> bool_and (same_physical register x0)
      (same_type (frame_slot_type slot) A64I64)
  | _ -> false
}
let runtime_host_args_result_setup = fn instruction -> match instruction {
  | A64SpillRegister(slot, register) -> bool_and (same_physical register x0)
      (same_type (frame_slot_type slot) A64Pointer)
  | _ -> false
}
let runtime_allocate_result_setup = fn instruction -> match instruction {
  | A64SpillRegister(slot, register) -> bool_and (same_physical register x0)
      (same_type (frame_slot_type slot) A64Pointer)
  | _ -> false
}
let runtime_result_setup = fn helper -> fn instruction -> match helper {
  | A64RuntimeHostArgs -> runtime_host_args_result_setup instruction
  | A64RuntimeAllocate -> runtime_allocate_result_setup instruction
  | A64RuntimeI32Division -> runtime_i32_result_setup instruction
  | A64RuntimeI32Remainder -> runtime_i32_result_setup instruction
  | A64RuntimeI64Division -> runtime_i64_result_setup instruction
  | A64RuntimeI64Remainder -> runtime_i64_result_setup instruction
}
let valid_call_setup = fn previous -> fn last -> match last {
  | Some(instruction) -> match (null_environment_setup instruction) {
    | true -> true
    | false -> match (source_argument_setup instruction) {
      | false -> false
      | true -> match previous {
        | Some(previous_instruction) -> null_environment_setup previous_instruction
        | None -> false
      }
    }
  }
  | None -> false
}
let target_has_no_environment = fn target -> match target {
  | NativeTopLevelFunction(_) -> true
  | NativeModuleInitializer(_) -> true
  | NativeNestedFunction(_) -> false
}
let verify_call_sequences = fn items -> fn previous -> fn last -> match items {
  | Nil -> Ok(0)
  | Cons(A64LinearLabelItem(_), remaining) -> verify_call_sequences remaining None None
  | Cons(A64LinearTerminatorItem(_), remaining) -> verify_call_sequences remaining None None
  | Cons(A64LinearInstructionItem(A64CallDirect(target)), remaining) ->
    match (target_has_no_environment target) {
      | false -> Err("LinearIR direct call target requires an explicit closure environment")
      | true -> match (valid_call_setup previous last) {
        | false -> Err("LinearIR direct call is missing its null environment or typed argument-register setup")
        | true -> verify_call_sequences remaining last (Some(A64CallDirect(target)))
      }
    }
  | Cons(A64LinearInstructionItem(A64CallRuntime(helper)), remaining) ->
    match (valid_runtime_call_setup helper previous last) {
      | false -> Err("LinearIR runtime helper call is missing its typed argument-register setup")
      | true -> match remaining {
        | Cons(A64LinearInstructionItem(result_store), _) ->
          match (runtime_result_setup helper result_store) {
            | false -> Err("LinearIR runtime helper result uses the wrong result register or type")
            | true -> verify_call_sequences remaining last (Some(A64CallRuntime(helper)))
          }
        | _ -> Err("LinearIR runtime helper result is not spilled after the call")
      }
    }
  | Cons(A64LinearInstructionItem(instruction), remaining) ->
    verify_call_sequences remaining last (Some(instruction))
}

let closure_code_load_matches = fn history -> match history {
  | Some(A64LoadMemory(target, base, 0)) -> bool_and (same_physical target x16)
      (same_physical base x9)
  | _ -> false
}

let closure_argument_register_matches = fn register -> fn slot -> match
    (frame_slot_type slot) {
  | A64I32 -> same_physical register w1
  | A64Boolean -> same_physical register w1
  | A64I64 -> same_physical register x1
  | A64Pointer -> same_physical register x1
  | A64Unit -> false
}

let valid_closure_call_setup = fn latest -> fn previous -> fn older -> fn oldest ->
  match latest {
  | Some(A64ReloadSpill(register, argument_slot)) -> match (same_physical register x0) {
    | true -> match (frame_slot_type argument_slot) {
      | A64Pointer -> match (closure_code_load_matches previous) {
        | false -> false
        | true -> match older {
          | Some(A64ReloadSpill(base_register, closure_slot)) -> bool_and
              (same_physical base_register x9) (same_slot argument_slot closure_slot)
          | _ -> false
        }
      }
      | _ -> false
    }
    | false -> match (closure_argument_register_matches register argument_slot) {
      | false -> false
      | true -> match previous {
        | Some(A64ReloadSpill(environment_register, environment_slot)) -> match
            (same_physical environment_register x0) {
          | false -> false
          | true -> match (same_type (frame_slot_type environment_slot) A64Pointer) {
            | false -> false
            | true -> match (closure_code_load_matches older) {
              | false -> false
              | true -> match oldest {
                | Some(A64ReloadSpill(base_register, closure_slot)) -> bool_and
                    (same_physical base_register x9)
                    (same_slot environment_slot closure_slot)
                | _ -> false
              }
            }
          }
        }
        | _ -> false
      }
    }
  }
  | _ -> false
}

let verify_indirect_call_sequences = fn items -> fn latest ->
  fn previous -> fn older -> fn oldest -> match items {
  | Nil -> Ok(0)
  | Cons(A64LinearLabelItem(_), remaining) -> verify_indirect_call_sequences
      remaining None None None None
  | Cons(A64LinearTerminatorItem(_), remaining) -> verify_indirect_call_sequences
      remaining None None None None
  | Cons(A64LinearInstructionItem(A64CallIndirect(target)), remaining) -> match
      (same_physical target x16) {
    | false -> Err("LinearIR indirect call does not use the designated code-pointer register")
    | true -> match (valid_closure_call_setup latest previous older oldest) {
      | false -> Err("LinearIR closure call is missing its code, environment, or typed argument setup")
      | true -> verify_indirect_call_sequences remaining
          (Some(A64CallIndirect(target))) latest previous older
    }
  }
  | Cons(A64LinearInstructionItem(instruction), remaining) ->
    verify_indirect_call_sequences remaining (Some(instruction)) latest previous older
}

let valid_terminator_registers = fn terminator -> match terminator {
  | A64LinearBranch(condition, _, _) -> same_physical condition w9
  | _ -> true
}

let verify_item_structure = fn items -> fn values -> fn environment_slot ->
  fn active_block -> fn terminated -> match items {
  | Nil -> match (bool_and active_block terminated) {
    | true -> Ok(0)
    | false -> Err("LinearIR ends without a terminated basic or edge block")
  }
  | Cons(A64LinearLabelItem(_), remaining) -> match (bool_and active_block (bool_not terminated)) {
    | true -> Err("LinearIR label begins before the previous block terminates")
    | false -> verify_item_structure remaining values environment_slot true false
  }
  | Cons(A64LinearInstructionItem(instruction), remaining) -> match (bool_and active_block (bool_not terminated)) {
    | false -> Err("LinearIR instruction appears outside a live labeled block")
    | true -> match (valid_instruction_registers instruction values environment_slot) {
      | false -> Err("LinearIR instruction violates its spill layout or physical-register assignment")
      | true -> verify_item_structure remaining values environment_slot active_block false
    }
  }
  | Cons(A64LinearTerminatorItem(terminator), remaining) -> match (bool_and active_block (bool_not terminated)) {
    | false -> Err("LinearIR terminator appears outside a live labeled block")
    | true -> match (valid_terminator_registers terminator) {
      | false -> Err("LinearIR branch condition violates the physical-register assignment")
      | true -> verify_item_structure remaining values environment_slot active_block true
    }
  }
}

let capture_alignment = fn value_type -> match value_type {
  | A64Unit -> 1
  | A64I32 -> 4
  | A64Boolean -> 4
  | A64I64 -> 8
  | A64Pointer -> 8
}

let align_offset = fn offset -> fn alignment -> match (offset % alignment) {
  | 0 -> offset
  | remainder -> offset + (alignment - remainder)
}

let verify_capture_fields = fn fields -> fn expected_index -> fn next_offset ->
  match fields {
  | Nil -> Ok(next_offset)
  | Cons(A64CaptureField {
      index, binding: _, value_type, offset, size, alignment, span: _
    }, remaining) -> {
    let expected_alignment = capture_alignment value_type;
    let expected_size = slot_size value_type;
    let expected_offset = align_offset next_offset expected_alignment;
    let max_offset = match expected_size {
      | 0 -> 2147483647
      | 4 -> 16380
      | _ -> 32760
    };
    match (index == expected_index) {
      | false -> Err("LinearIR closure capture indexes are not contiguous")
      | true -> match (offset == expected_offset) {
        | false -> Err("LinearIR closure capture offset violates its natural layout")
        | true -> match (size == expected_size) {
          | false -> Err("LinearIR closure capture size differs from its representation")
          | true -> match (alignment == expected_alignment) {
            | false -> Err("LinearIR closure capture alignment differs from its representation")
            | true -> match (offset > max_offset) {
              | true -> Err("LinearIR closure capture offset exceeds AArch64 addressing range")
              | false -> verify_capture_fields remaining (expected_index + 1)
                  (offset + size)
            }
          }
        }
      }
    }
  }
}

let verify_closure_layout = fn (A64ClosureLayout {
  fields, environment_size, allocation_size
}) -> match (verify_capture_fields fields 0 8) {
  | Err(message) -> Err(message)
  | Ok(end_offset) -> match (end_offset > 2147483640) {
    | true -> Err("LinearIR closure layout exceeds the supported 32-bit size")
    | false -> {
      let expected_size = align_offset end_offset 8;
      match (environment_size == end_offset - 8) {
        | false -> Err("LinearIR closure environment size differs from its fields")
        | true -> match (allocation_size == expected_size) {
          | false -> Err("LinearIR closure allocation size is not 8-byte padded")
          | true -> match (allocation_size >= 8) {
            | true -> Ok(0)
            | false -> Err("LinearIR closure allocation omits its code-pointer header")
          }
        }
      }
    }
  }
}

let layout_has_captures = fn (A64ClosureLayout {
  fields, environment_size: _, allocation_size: _
}) -> match fields {
  | Nil -> false
  | _ -> true
}

let valid_environment_layout = fn capture_layout -> fn environment_slot ->
  fn values -> fn frame_size -> match environment_slot {
  | None -> match (layout_has_captures capture_layout) {
    | false -> true
    | true -> false
  }
  | Some(slot) -> match (layout_has_captures capture_layout) {
    | false -> false
    | true -> match (valid_environment_slot slot) {
      | false -> false
      | true -> match (environment_offset slot >= maximum_end values) {
        | false -> false
        | true -> environment_offset slot <= frame_size - 8
      }
    }
  }
}

let count_environment_spills = fn items -> match items {
  | Nil -> 0
  | Cons(A64LinearInstructionItem(A64SpillEnvironment(_, _)), remaining) ->
    1 + count_environment_spills remaining
  | Cons(_, remaining) -> count_environment_spills remaining
}

let environment_spill_in_block = fn wanted_slot -> fn items -> match items {
  | Nil -> false
  | Cons(A64LinearLabelItem(_), _) -> false
  | Cons(A64LinearTerminatorItem(_), _) -> false
  | Cons(A64LinearInstructionItem(A64SpillEnvironment(actual_slot, source)), remaining) ->
    match (same_environment_slot wanted_slot actual_slot) {
      | true -> same_physical source x0
      | false -> environment_spill_in_block wanted_slot remaining
    }
  | Cons(_, remaining) -> environment_spill_in_block wanted_slot remaining
}

let entry_block_spills_environment = fn entry -> fn wanted_slot -> fn items -> match items {
  | Nil -> false
  | Cons(A64LinearLabelItem(label), remaining) -> match (same_label entry label) {
    | true -> environment_spill_in_block wanted_slot remaining
    | false -> entry_block_spills_environment entry wanted_slot remaining
  }
  | Cons(_, remaining) -> entry_block_spills_environment entry wanted_slot remaining
}

let valid_environment_prologue = fn capture_layout -> fn environment_slot ->
  fn entry -> fn items -> match environment_slot {
  | None -> count_environment_spills items == 0
  | Some(slot) -> match (layout_has_captures capture_layout) {
    | false -> false
    | true -> match (count_environment_spills items == 1) {
      | false -> false
      | true -> entry_block_spills_environment entry slot items
    }
  }
}

let verify_spill_locations = fn values -> fn frame_size -> match (validate_locations values) {
  | Err(message) -> Err(message)
  | Ok(_) -> match (maximum_end values <= frame_size) {
    | true -> Ok(0)
    | false -> Err("LinearIR spill slot lies outside its stack frame")
  }
}

let verify_linear_items = fn entry -> fn values -> fn environment_slot -> fn items ->
  match (duplicate_labels items) {
  | true -> Err("LinearIR contains a duplicate block label")
  | false -> match (contains_label entry items) {
    | false -> Err("LinearIR entry label is not defined")
    | true -> match (verify_item_structure items values environment_slot false false) {
      | Err(message) -> Err(message)
      | Ok(_) -> match (verify_references items) {
        | Err(message) -> Err(message)
        | Ok(_) -> match (verify_call_sequences items None None) {
          | Err(message) -> Err(message)
          | Ok(_) -> verify_indirect_call_sequences items None None None None
        }
      }
    }
  }
}

let verify_function = fn (A64LinearFunction {
  identity: _, result_type: _, capture_layout, environment_slot,
  entry, values, frame_size, items, span: _
}) -> match (bool_and (frame_size >= 0) (frame_size % 16 == 0)) {
  | false -> Err("LinearIR frame size is not a non-negative 16-byte multiple")
  | true -> match (verify_closure_layout capture_layout) {
    | Err(message) -> Err(message)
    | Ok(_) -> match (valid_environment_layout capture_layout environment_slot
        values frame_size) {
      | false -> Err("LinearIR saved environment slot disagrees with its closure layout or frame")
      | true -> match (valid_environment_prologue capture_layout environment_slot
          entry items) {
        | false -> Err("LinearIR entry does not preserve its declared closure environment exactly once")
        | true -> match (verify_spill_locations values frame_size) {
          | Err(message) -> Err(message)
          | Ok(_) -> verify_linear_items entry values environment_slot items
        }
      }
    }
  }
}
