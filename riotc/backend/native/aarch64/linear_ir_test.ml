// ABI, scalar/pointer spill, unit-erasure, edge-copy, and rejection fixtures.
use Prelude.{List, Nil, Cons, Result, Ok, Err, Some, None}
use String.{equal}
use Host.{write_stdout, write_stderr}
use Syntax.Ast.{ValueName, IntegerLiteral}
use Syntax.Lexer.{SourceSpan}
use Check.Inference.{BindingIdentity, ModuleValueBinding}
use NativeIr.Ir.{
  NativeTopLevelFunction, NativeBlockId, NativeNestedFunction,
  NativeNestedFunctionId, NativeEffectSet
}
use Backend.Native.Aarch64.MachineIr.{
  A64VirtualRegister, A64Label, A64Unit, A64I32, A64Boolean, A64Pointer,
  A64Function, A64ClosureLayout, A64CaptureField, A64Block, A64BlockParameter,
  A64Instruction, A64LoadUnit,
  A64LoadI32, A64LoadBoolean, A64Call, A64CallSite, A64DirectCall,
  A64HostCall, A64CallArgument, A64AddI32, A64BinaryOperands,
  A64Edge, A64Branch, A64Jump, A64ConditionalBranch, A64Return
}
use Backend.Native.Aarch64.LowerLinearIr.{lower_function}
use Backend.Native.Aarch64.VerifyLinearIr.{verify_function}
use Backend.Native.Aarch64.PrintLinearIr.{print_function}
use Backend.Native.Aarch64.LinearIr.{
  A64BasicBlockLabel, A64LinearFunction, A64LinearFailure,
  A64FrameSlot, A64EnvironmentSlot, A64ValueLocation, A64PhysicalRegister,
  A64LinearItem, A64LinearLabelItem, A64LinearInstructionItem,
  A64LinearTerminatorItem, A64LinearReturn, A64ReloadSpill,
  A64LoadMemory, A64ReloadEnvironment, A64SpillEnvironment, A64CallIndirect,
  A64SpillRegister, x0, x9, x10, x16, w0, w1, w10
}

let module_name = "linear-ir-test"
let span = SourceSpan { start_codepoint: 0, end_codepoint: 1 }
let binding = fn name -> BindingIdentity {
  source_path: module_name,
  declaration_span: span,
  name: ValueName(name),
  scope: ModuleValueBinding
}
let function_id = fn name -> NativeTopLevelFunction(binding name)
let callee_id = function_id "callee"
let nested_callee = NativeNestedFunction(NativeNestedFunctionId {
  parent: binding "parent",
  source_span: span,
  ordinal: 0
})
let empty_effects = NativeEffectSet(Nil)
let empty_capture_layout = A64ClosureLayout {
  fields: Nil, environment_size: 0, allocation_size: 8
}

let scalar_function = A64Function {
  capture_layout: empty_capture_layout,
  identity: function_id "scalar",
  result_type: A64I32,
  entry: A64Label(NativeBlockId(0)),
  blocks: Cons(A64Block {
    label: A64Label(NativeBlockId(0)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(0), value_type: A64I32, span: span
    }, Nil),
    instructions: Cons(A64Instruction {
      destination: A64VirtualRegister(1), result_type: A64I32,
      operation: A64LoadI32(IntegerLiteral("1")), effects: empty_effects, span: span
    }, Cons(A64Instruction {
      destination: A64VirtualRegister(2), result_type: A64I32,
      operation: A64AddI32(A64BinaryOperands {
        left: A64VirtualRegister(0), right: A64VirtualRegister(1)
      }), effects: empty_effects, span: span
    }, Nil)),
    terminator: A64Return(A64VirtualRegister(2)), span: span
  }, Nil),
  span: span
}

let unit_entry_function = A64Function {
  capture_layout: empty_capture_layout,
  identity: function_id "unit_entry",
  result_type: A64I32,
  entry: A64Label(NativeBlockId(0)),
  blocks: Cons(A64Block {
    label: A64Label(NativeBlockId(0)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(0), value_type: A64Unit, span: span
    }, Nil),
    instructions: Cons(A64Instruction {
      destination: A64VirtualRegister(1), result_type: A64I32,
      operation: A64LoadI32(IntegerLiteral("42")), effects: empty_effects, span: span
    }, Nil),
    terminator: A64Return(A64VirtualRegister(1)), span: span
  }, Nil),
  span: span
}

let direct_call_function = A64Function {
  capture_layout: empty_capture_layout,
  identity: function_id "caller",
  result_type: A64I32,
  entry: A64Label(NativeBlockId(0)),
  blocks: Cons(A64Block {
    label: A64Label(NativeBlockId(0)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(0), value_type: A64I32, span: span
    }, Nil),
    instructions: Cons(A64Instruction {
      destination: A64VirtualRegister(1), result_type: A64I32,
      operation: A64Call(A64CallSite {
        target: A64DirectCall(callee_id),
        arguments: Cons(A64CallArgument {
          register: A64VirtualRegister(0), value_type: A64I32
        }, Nil),
        result_type: A64I32
      }), effects: empty_effects, span: span
    }, Nil),
    terminator: A64Return(A64VirtualRegister(1)), span: span
  }, Nil),
  span: span
}

let pointer_identity_function = A64Function {
  capture_layout: empty_capture_layout,
  identity: function_id "pointer_identity",
  result_type: A64Pointer,
  entry: A64Label(NativeBlockId(0)),
  blocks: Cons(A64Block {
    label: A64Label(NativeBlockId(0)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(0), value_type: A64Pointer, span: span
    }, Nil),
    instructions: Nil,
    terminator: A64Return(A64VirtualRegister(0)), span: span
  }, Nil),
  span: span
}

let pointer_direct_call_function = A64Function {
  capture_layout: empty_capture_layout,
  identity: function_id "pointer_caller",
  result_type: A64Pointer,
  entry: A64Label(NativeBlockId(0)),
  blocks: Cons(A64Block {
    label: A64Label(NativeBlockId(0)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(0), value_type: A64Pointer, span: span
    }, Nil),
    instructions: Cons(A64Instruction {
      destination: A64VirtualRegister(1), result_type: A64Pointer,
      operation: A64Call(A64CallSite {
        target: A64DirectCall(callee_id),
        arguments: Cons(A64CallArgument {
          register: A64VirtualRegister(0), value_type: A64Pointer
        }, Nil),
        result_type: A64Pointer
      }), effects: empty_effects, span: span
    }, Nil),
    terminator: A64Return(A64VirtualRegister(1)), span: span
  }, Nil),
  span: span
}

let pointer_branch_function = A64Function {
  capture_layout: empty_capture_layout,
  identity: function_id "pointer_branch",
  result_type: A64Pointer,
  entry: A64Label(NativeBlockId(0)),
  blocks: Cons(A64Block {
    label: A64Label(NativeBlockId(0)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(0), value_type: A64Pointer, span: span
    }, Nil),
    instructions: Cons(A64Instruction {
      destination: A64VirtualRegister(1), result_type: A64Boolean,
      operation: A64LoadBoolean(true), effects: empty_effects, span: span
    }, Nil),
    terminator: A64ConditionalBranch(A64Branch {
      condition: A64VirtualRegister(1),
      when_true: A64Edge {
        target: A64Label(NativeBlockId(1)),
        arguments: Cons(A64VirtualRegister(0), Nil)
      },
      when_false: A64Edge {
        target: A64Label(NativeBlockId(2)),
        arguments: Cons(A64VirtualRegister(0), Nil)
      }
    }), span: span
  }, Cons(A64Block {
    label: A64Label(NativeBlockId(1)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(2), value_type: A64Pointer, span: span
    }, Nil),
    instructions: Nil,
    terminator: A64Return(A64VirtualRegister(2)), span: span
  }, Cons(A64Block {
    label: A64Label(NativeBlockId(2)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(3), value_type: A64Pointer, span: span
    }, Nil),
    instructions: Nil,
    terminator: A64Return(A64VirtualRegister(3)), span: span
  }, Nil))),
  span: span
}

let unit_direct_call_function = A64Function {
  capture_layout: empty_capture_layout,
  identity: function_id "unit_caller",
  result_type: A64I32,
  entry: A64Label(NativeBlockId(0)),
  blocks: Cons(A64Block {
    label: A64Label(NativeBlockId(0)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(0), value_type: A64Unit, span: span
    }, Nil),
    instructions: Cons(A64Instruction {
      destination: A64VirtualRegister(1), result_type: A64I32,
      operation: A64Call(A64CallSite {
        target: A64DirectCall(callee_id),
        arguments: Cons(A64CallArgument {
          register: A64VirtualRegister(0), value_type: A64Unit
        }, Nil),
        result_type: A64I32
      }), effects: empty_effects, span: span
    }, Nil),
    terminator: A64Return(A64VirtualRegister(1)), span: span
  }, Nil),
  span: span
}

let unit_return_function = A64Function {
  capture_layout: empty_capture_layout,
  identity: function_id "unit_return",
  result_type: A64Unit,
  entry: A64Label(NativeBlockId(0)),
  blocks: Cons(A64Block {
    label: A64Label(NativeBlockId(0)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(0), value_type: A64Unit, span: span
    }, Nil),
    instructions: Cons(A64Instruction {
      destination: A64VirtualRegister(1), result_type: A64Unit,
      operation: A64LoadUnit, effects: empty_effects, span: span
    }, Nil),
    terminator: A64Return(A64VirtualRegister(1)), span: span
  }, Nil),
  span: span
}

let branch_function = A64Function {
  capture_layout: empty_capture_layout,
  identity: function_id "branch_values",
  result_type: A64I32,
  entry: A64Label(NativeBlockId(0)),
  blocks: Cons(A64Block {
    label: A64Label(NativeBlockId(0)), parameters: Nil,
    instructions: Cons(A64Instruction {
      destination: A64VirtualRegister(0), result_type: A64Boolean,
      operation: A64LoadBoolean(true), effects: empty_effects, span: span
    }, Cons(A64Instruction {
      destination: A64VirtualRegister(1), result_type: A64I32,
      operation: A64LoadI32(IntegerLiteral("7")), effects: empty_effects, span: span
    }, Nil)),
    terminator: A64ConditionalBranch(A64Branch {
      condition: A64VirtualRegister(0),
      when_true: A64Edge {
        target: A64Label(NativeBlockId(1)),
        arguments: Cons(A64VirtualRegister(1), Nil)
      },
      when_false: A64Edge {
        target: A64Label(NativeBlockId(2)),
        arguments: Cons(A64VirtualRegister(1), Nil)
      }
    }), span: span
  }, Cons(A64Block {
    label: A64Label(NativeBlockId(1)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(2), value_type: A64I32, span: span
    }, Nil), instructions: Nil,
    terminator: A64Return(A64VirtualRegister(2)), span: span
  }, Cons(A64Block {
    label: A64Label(NativeBlockId(2)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(3), value_type: A64I32, span: span
    }, Nil), instructions: Nil,
    terminator: A64Return(A64VirtualRegister(3)), span: span
  }, Nil))),
  span: span
}

let nested_call_function = A64Function {
  capture_layout: empty_capture_layout,
  identity: function_id "nested_caller",
  result_type: A64Unit,
  entry: A64Label(NativeBlockId(0)),
  blocks: Cons(A64Block {
    label: A64Label(NativeBlockId(0)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(0), value_type: A64Unit, span: span
    }, Nil),
    instructions: Cons(A64Instruction {
      destination: A64VirtualRegister(1), result_type: A64Unit,
      operation: A64Call(A64CallSite {
        target: A64DirectCall(nested_callee),
        arguments: Cons(A64CallArgument {
          register: A64VirtualRegister(0), value_type: A64Unit
        }, Nil),
        result_type: A64Unit
      }), effects: empty_effects, span: span
    }, Nil),
    terminator: A64Return(A64VirtualRegister(1)), span: span
  }, Nil),
  span: span
}

let host_call_function = A64Function {
  capture_layout: empty_capture_layout,
  identity: function_id "host_call",
  result_type: A64Unit,
  entry: A64Label(NativeBlockId(0)),
  blocks: Cons(A64Block {
    label: A64Label(NativeBlockId(0)),
    parameters: Cons(A64BlockParameter {
      register: A64VirtualRegister(0), value_type: A64Unit, span: span
    }, Nil),
    instructions: Cons(A64Instruction {
      destination: A64VirtualRegister(1), result_type: A64Unit,
      operation: A64Call(A64CallSite {
        target: A64HostCall(binding "write"),
        arguments: Cons(A64CallArgument {
          register: A64VirtualRegister(0), value_type: A64Unit
        }, Nil),
        result_type: A64Unit
      }), effects: empty_effects, span: span
    }, Nil),
    terminator: A64Return(A64VirtualRegister(1)), span: span
  }, Nil),
  span: span
}

let check_nested_rejection = fn machine_function -> match (lower_function machine_function) {
  | Err(A64LinearFailure { reason, function: _, block: _, span: _ }) ->
    match (equal reason "nested direct call requires explicit closure-environment lowering") {
      | true -> 0
      | false -> 1
    }
  | Err(_) -> 1
  | Ok(_) -> 1
}

let invalid_frame_function = A64LinearFunction {
  capture_layout: empty_capture_layout,
  environment_slot: None,
  identity: function_id "invalid_frame",
  result_type: A64I32,
  entry: A64BasicBlockLabel(A64Label(NativeBlockId(0))),
  values: Nil,
  frame_size: 4,
  items: Nil,
  span: span
}

let invalid_capture_layout = A64ClosureLayout {
  fields: Cons(A64CaptureField {
    index: 0, binding: binding "captured", value_type: A64I32,
    offset: 12, size: 4, alignment: 4, span: span
  }, Nil),
  environment_size: 8, allocation_size: 24
}

let invalid_capture_layout_function = A64LinearFunction {
  capture_layout: invalid_capture_layout,
  environment_slot: Some(A64EnvironmentSlot { offset: 0 }),
  identity: function_id "invalid_capture_layout",
  result_type: A64I32,
  entry: A64BasicBlockLabel(A64Label(NativeBlockId(0))),
  values: Nil,
  frame_size: 16,
  items: Nil,
  span: span
}

let valid_closure_capture_layout = A64ClosureLayout {
  fields: Cons(A64CaptureField {
    index: 0, binding: binding "captured", value_type: A64I32,
    offset: 8, size: 4, alignment: 4, span: span
  }, Nil),
  environment_size: 4, allocation_size: 16
}

let closure_pointer_slot = A64FrameSlot {
  value: A64VirtualRegister(0), offset: 0, value_type: A64Pointer
}
let closure_argument_slot = A64FrameSlot {
  value: A64VirtualRegister(1), offset: 8, value_type: A64I32
}
let closure_environment_slot = A64EnvironmentSlot { offset: 16 }
let closure_block_label = A64BasicBlockLabel(A64Label(NativeBlockId(0)))

let closure_call_function = fn argument_register -> A64LinearFunction {
  capture_layout: valid_closure_capture_layout,
  environment_slot: Some(closure_environment_slot),
  identity: function_id "closure_call",
  result_type: A64I32,
  entry: closure_block_label,
  values: Cons(A64ValueLocation {
    value: A64VirtualRegister(0), value_type: A64Pointer,
    slot: Some(closure_pointer_slot)
  }, Cons(A64ValueLocation {
    value: A64VirtualRegister(1), value_type: A64I32,
    slot: Some(closure_argument_slot)
  }, Nil)),
  frame_size: 32,
  items: Cons(A64LinearLabelItem(closure_block_label),
    Cons(A64LinearInstructionItem(A64SpillEnvironment(closure_environment_slot, x0)),
    Cons(A64LinearInstructionItem(A64ReloadEnvironment(x9, closure_environment_slot)),
    Cons(A64LinearInstructionItem(A64LoadMemory(w10, x9, 8)),
    Cons(A64LinearInstructionItem(A64ReloadSpill(x9, closure_pointer_slot)),
    Cons(A64LinearInstructionItem(A64LoadMemory(x16, x9, 0)),
    Cons(A64LinearInstructionItem(A64ReloadSpill(x0, closure_pointer_slot)),
    Cons(A64LinearInstructionItem(A64ReloadSpill(argument_register, closure_argument_slot)),
    Cons(A64LinearInstructionItem(A64CallIndirect(x16)),
    Cons(A64LinearTerminatorItem(A64LinearReturn), Nil)))))))))),
  span: span
}

let invalid_overlapping_slots = A64LinearFunction {
  capture_layout: empty_capture_layout,
  environment_slot: None,
  identity: function_id "invalid_overlapping_slots",
  result_type: A64I32,
  entry: A64BasicBlockLabel(A64Label(NativeBlockId(0))),
  values: Cons(A64ValueLocation {
    value: A64VirtualRegister(0), value_type: A64Pointer,
    slot: Some(A64FrameSlot {
      value: A64VirtualRegister(0), offset: 0, value_type: A64Pointer
    })
  }, Cons(A64ValueLocation {
    value: A64VirtualRegister(1), value_type: A64I32,
    slot: Some(A64FrameSlot {
      value: A64VirtualRegister(1), offset: 4, value_type: A64I32
    })
  }, Nil)),
  frame_size: 16,
  items: Cons(A64LinearLabelItem(A64BasicBlockLabel(A64Label(NativeBlockId(0)))),
    Cons(A64LinearTerminatorItem(A64LinearReturn), Nil)),
  span: span
}

let run_and_print = fn machine_function -> match (lower_function machine_function) {
  | Err(A64LinearFailure { reason, function: _, block: _, span: _ }) -> {
    write_stderr (reason + "\\n"); 1
  }
  | Ok(linear_function) -> {
    write_stdout (print_function linear_function);
    match (verify_function linear_function) {
      | Err(message) -> { write_stderr (message + "\\n"); 1 }
      | Ok(_) -> 0
    }
  }
}
let check_rejection = fn machine_function -> match (lower_function machine_function) {
  | Err(A64LinearFailure { reason, function: _, block: _, span: _ }) ->
    match (equal reason "unsupported Host helper `write` requires explicit boundary marshalling") {
      | true -> 0
      | false -> 1
    }
  | Err(_) -> 1
  | Ok(_) -> 1
}

let check_pointer_fixtures = fn () -> match (run_and_print pointer_identity_function) {
  | 1 -> 1
  | 0 -> match (run_and_print pointer_direct_call_function) {
    | 1 -> 1
    | 0 -> run_and_print pointer_branch_function
  }
}

let main = fn () -> match (run_and_print scalar_function) {
  | 1 -> 1
  | 0 -> match (run_and_print unit_entry_function) {
    | 1 -> 1
    | 0 -> match (run_and_print direct_call_function) {
      | 1 -> 1
      | 0 -> match (run_and_print unit_direct_call_function) {
        | 1 -> 1
        | 0 -> match (run_and_print unit_return_function) {
          | 1 -> 1
          | 0 -> match (run_and_print branch_function) {
            | 1 -> 1
            | 0 -> match (check_rejection host_call_function) {
              | 1 -> 1
              | 0 -> match (check_nested_rejection nested_call_function) {
                | 1 -> 1
                | 0 -> match (verify_function invalid_frame_function) {
                  | Err(_) -> match (verify_function invalid_overlapping_slots) {
                    | Err(_) -> match (verify_function invalid_capture_layout_function) {
                      | Err(_) -> match (verify_function (closure_call_function w1)) {
                        | Ok(_) -> match (verify_function (closure_call_function w0)) {
                          | Err(_) -> check_pointer_fixtures()
                          | Ok(_) -> 1
                        }
                        | Err(_) -> 1
                      }
                      | Ok(_) -> 1
                    }
                    | Ok(_) -> 1
                  }
                  | Ok(_) -> 1
                }
              }
            }
          }
        }
      }
    }
  }
}
