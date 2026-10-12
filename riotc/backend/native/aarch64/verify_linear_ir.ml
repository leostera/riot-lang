// Structural and target-convention checks for AArch64 LinearIR.
use Prelude.{List, Nil, Cons, Result, Ok, Err, Option, Some, None}
use NativeIr.Ir.{
  NativeBlockId, NativeTopLevelFunction, NativeNestedFunction,
  NativeModuleInitializer
}
use Backend.Native.Aarch64.MachineIr.{
  A64VirtualRegister, A64Label, A64Unit, A64I32, A64Boolean,
  A64Equal, A64NotEqual, A64SignedLessThan, A64SignedLessThanOrEqual,
  A64SignedGreaterThan, A64SignedGreaterThanOrEqual
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
  w0, w1, w9, w10, w11, x0
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
}
let frame_slot_value = fn (A64FrameSlot { value, offset: _, value_type: _ }) -> value
let frame_slot_offset = fn (A64FrameSlot { value: _, offset, value_type: _ }) -> offset
let frame_slot_type = fn (A64FrameSlot { value: _, offset: _, value_type }) -> value_type
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
let same_slot_offset = fn first -> fn second ->
  frame_slot_offset first == frame_slot_offset second
let same_slot = fn first -> fn second ->
  bool_and (same_register (frame_slot_value first) (frame_slot_value second))
    (bool_and (same_slot_offset first second)
      (same_type (frame_slot_type first) (frame_slot_type second)))
let same_type = fn first -> fn second -> match first {
  | A64Unit -> match second { | A64Unit -> true | _ -> false }
  | A64I32 -> match second { | A64I32 -> true | _ -> false }
  | A64Boolean -> match second { | A64Boolean -> true | _ -> false }
}

let location_end = fn location -> match location {
  | A64ValueLocation { value: _, value_type: A64Unit, slot: None } -> 0
  | A64ValueLocation { value: _, value_type: _, slot: Some(A64FrameSlot { value: _, offset, value_type: _ }) } -> offset + 4
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
let contains_slot_offset = fn wanted -> fn values -> match values {
  | Nil -> false
  | Cons(A64ValueLocation { value: _, value_type: _, slot: None }, remaining) ->
    contains_slot_offset wanted remaining
  | Cons(A64ValueLocation { value: _, value_type: _, slot: Some(slot) }, remaining) ->
    match (same_slot_offset wanted slot) {
      | true -> true
      | false -> contains_slot_offset wanted remaining
    }
}
let has_duplicate_slot_offsets = fn values -> match values {
  | Nil -> false
  | Cons(A64ValueLocation { value: _, value_type: _, slot: None }, remaining) ->
    has_duplicate_slot_offsets remaining
  | Cons(A64ValueLocation { value: _, value_type: _, slot: Some(slot) }, remaining) ->
    match (contains_slot_offset slot remaining) {
      | true -> true
      | false -> has_duplicate_slot_offsets remaining
    }
}
let validate_locations = fn values -> match (has_duplicate_values values) {
  | true -> Err("LinearIR assigns more than one location to an SSA value")
  | false -> match (has_duplicate_slot_offsets values) {
    | true -> Err("LinearIR assigns overlapping SSA spill slots")
    | false -> match (validate_location_list values) {
      | Err(message) -> Err(message)
      | Ok(_) -> Ok(0)
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
  }
}

let physical_is_w32 = fn register -> match (physical_width register) {
  | A64W32 -> true
  | A64X64 -> false
}
let physical_register_allowed = fn (A64PhysicalRegister { number, width }) -> match number {
  | 0 -> true
  | 1 -> match width { | A64W32 -> true | A64X64 -> false }
  | 9 -> match width { | A64W32 -> true | A64X64 -> false }
  | 10 -> match width { | A64W32 -> true | A64X64 -> false }
  | 11 -> match width { | A64W32 -> true | A64X64 -> false }
  | _ -> false
}
let valid_instruction_registers = fn instruction -> fn values -> match instruction {
  | A64MoveZero(register) -> match register {
    | A64PhysicalRegister { number: 0, width: A64X64 } -> true
    | _ -> false
  }
  | A64LoadI32Literal(register, _) -> bool_or (same_physical register w9) (same_physical register w10)
  | A64LoadBooleanLiteral(register, _) -> bool_or (same_physical register w9) (same_physical register w10)
  | A64ReloadSpill(register, slot) -> bool_and (physical_register_allowed register)
      (bool_and (physical_is_w32 register) (slot_is_known slot values))
  | A64SpillRegister(slot, register) -> bool_and (physical_register_allowed register)
      (bool_and (physical_is_w32 register) (slot_is_known slot values))
  | A64AddW(destination, left, right) -> bool_and (same_physical destination w11)
      (bool_and (same_physical left w9) (same_physical right w10))
  | A64SubtractW(destination, left, right) -> bool_and (same_physical destination w11)
      (bool_and (same_physical left w9) (same_physical right w10))
  | A64MultiplyW(destination, left, right) -> bool_and (same_physical destination w11)
      (bool_and (same_physical left w9) (same_physical right w10))
  | A64NegateW(destination, operand) -> bool_and (same_physical destination w11) (same_physical operand w9)
  | A64CompareSetW(_, destination, left, right) -> bool_and (same_physical destination w11)
      (bool_and (same_physical left w9) (same_physical right w10))
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
let scalar_argument_setup = fn instruction -> match instruction {
  | A64ReloadSpill(register, _) -> same_physical register w1
  | _ -> false
}
let runtime_i32_argument_setup = fn expected -> fn instruction -> match instruction {
  | A64ReloadSpill(register, slot) -> bool_and (same_physical register expected)
      (same_type (frame_slot_type slot) A64I32)
  | _ -> false
}
let valid_runtime_call_setup = fn previous -> fn last -> match last {
  | Some(instruction) -> match (runtime_i32_argument_setup w1 instruction) {
    | false -> false
    | true -> match previous {
      | Some(previous_instruction) -> runtime_i32_argument_setup w0 previous_instruction
      | None -> false
    }
  }
  | None -> false
}
let valid_call_setup = fn previous -> fn last -> match last {
  | Some(instruction) -> match (null_environment_setup instruction) {
    | true -> true
    | false -> match (scalar_argument_setup instruction) {
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
        | false -> Err("LinearIR direct call is missing its null environment or argument-register setup")
        | true -> verify_call_sequences remaining last (Some(A64CallDirect(target)))
      }
    }
  | Cons(A64LinearInstructionItem(A64CallRuntime(helper)), remaining) ->
    match (valid_runtime_call_setup previous last) {
      | false -> Err("LinearIR runtime helper call is missing its typed argument-register setup")
      | true -> verify_call_sequences remaining last (Some(A64CallRuntime(helper)))
    }
  | Cons(A64LinearInstructionItem(instruction), remaining) ->
    verify_call_sequences remaining last (Some(instruction))
}

let valid_terminator_registers = fn terminator -> match terminator {
  | A64LinearBranch(condition, _, _) -> same_physical condition w9
  | _ -> true
}

let verify_item_structure = fn items -> fn values -> fn active_block -> fn terminated -> match items {
  | Nil -> match (bool_and active_block terminated) {
    | true -> Ok(0)
    | false -> Err("LinearIR ends without a terminated basic or edge block")
  }
  | Cons(A64LinearLabelItem(_), remaining) -> match (bool_and active_block (bool_not terminated)) {
    | true -> Err("LinearIR label begins before the previous block terminates")
    | false -> verify_item_structure remaining values true false
  }
  | Cons(A64LinearInstructionItem(instruction), remaining) -> match (bool_and active_block (bool_not terminated)) {
    | false -> Err("LinearIR instruction appears outside a live labeled block")
    | true -> match (valid_instruction_registers instruction values) {
      | false -> Err("LinearIR instruction violates its spill layout or physical-register assignment")
      | true -> verify_item_structure remaining values active_block false
    }
  }
  | Cons(A64LinearTerminatorItem(terminator), remaining) -> match (bool_and active_block (bool_not terminated)) {
    | false -> Err("LinearIR terminator appears outside a live labeled block")
    | true -> match (valid_terminator_registers terminator) {
      | false -> Err("LinearIR branch condition violates the physical-register assignment")
      | true -> verify_item_structure remaining values active_block true
    }
  }
}

let verify_function = fn (A64LinearFunction {
  identity: _, result_type: _, entry, values, frame_size, items, span: _
}) -> match (bool_and (frame_size >= 0) (frame_size % 16 == 0)) {
  | false -> Err("LinearIR frame size is not a non-negative 16-byte multiple")
  | true -> match (validate_locations values) {
    | Err(message) -> Err(message)
    | Ok(_) -> match (maximum_end values <= frame_size) {
      | false -> Err("LinearIR spill slot lies outside its stack frame")
      | true -> match (duplicate_labels items) {
        | true -> Err("LinearIR contains a duplicate block label")
        | false -> match (contains_label entry items) {
          | false -> Err("LinearIR entry label is not defined")
          | true -> match (verify_item_structure items values false false) {
            | Err(message) -> Err(message)
            | Ok(_) -> match (verify_references items) {
              | Err(message) -> Err(message)
              | Ok(_) -> verify_call_sequences items None None
            }
          }
        }
      }
    }
  }
}
