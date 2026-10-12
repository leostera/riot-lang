// Lower AArch64 virtual-register MachineIR to an all-spill, physical-register LinearIR.
// Unit is erased; internal calls pass null x0 environment plus at most one typed W argument.
use Prelude.{List, Nil, Cons, Result, Ok, Err, Option, Some, None}
use String.{equal}
use NativeIr.Ir.{
  NativeBlockId, NativeTopLevelFunction, NativeNestedFunction,
  NativeModuleInitializer
}
use Syntax.Lexer.{SourceSpan}
use Syntax.Ast.{ValueName}
use Check.Inference.{BindingIdentity, ModuleValueBinding}
use NativeIr.Print.{block_id}
use Backend.Native.Aarch64.MachineIr.{
  A64VirtualRegister, A64Label, A64ValueType, A64Unit, A64I32, A64Boolean, A64Pointer,
  A64Function, A64Block, A64BlockParameter, A64Instruction, A64Operation,
  A64LoadUnit, A64LoadI32, A64LoadBoolean, A64TestUnitLiteral,
  A64TestI32Literal, A64TestBooleanLiteral, A64I32LiteralTest,
  A64BooleanLiteralTest, A64Call, A64CallSite, A64DirectCall, A64HostCall,
  A64RuntimeCall, A64RuntimeCallSite, A64RuntimeHelper, A64RuntimeHostArgs,
  A64CallArgument,
  A64AddI32, A64SubtractI32, A64MultiplyI32,
  A64CompareI32, A64EqualBoolean, A64NotEqualBoolean, A64NegateI32,
  A64BinaryOperands, A64IntegerComparison, A64Condition, A64Equal, A64NotEqual,
  A64Edge, A64Branch,
  A64Terminator, A64Jump, A64ConditionalBranch, A64Return, A64Trap,
  A64Unreachable
}
use Backend.Native.Aarch64.LinearIr.{
  A64RegisterWidth, A64W32, A64X64, A64PhysicalRegister,
  A64ValueLocation, A64FrameSlot, A64LinearLabel, A64BasicBlockLabel,
  A64TrueEdgeLabel, A64FalseEdgeLabel, A64LinearInstruction,
  A64MoveZero, A64LoadI32Literal, A64LoadBooleanLiteral, A64ReloadSpill,
  A64SpillRegister, A64AddW, A64SubtractW, A64MultiplyW, A64NegateW,
  A64CompareSetW, A64CallDirect, A64CallRuntime, A64LinearTerminator, A64LinearJump,
  A64LinearBranch, A64LinearReturn, A64LinearTrap, A64LinearUnreachable,
  A64LinearItem, A64LinearLabelItem, A64LinearInstructionItem,
  A64LinearTerminatorItem, A64LinearFunction, A64LinearFailure,
  w0, w1, w9, w10, w11, x0, x1, x9, slot_size, slot_alignment,
  align_frame, is_unit
}

type A64LocationAssignment = A64LocationAssignment(List<A64ValueLocation>, i32)
type A64BinaryOpcode = A64OpcodeAdd | A64OpcodeSubtract | A64OpcodeMultiply
type A64LoweredEdge = A64LoweredEdge(List<A64LinearItem>, A64LinearLabel)

let register_number = fn register -> match register {
  | A64VirtualRegister(number) -> number
}
let same_register = fn first -> fn second ->
  register_number first == register_number second

let location_register = fn (A64ValueLocation { value, value_type: _, slot: _ }) -> value
let same_value_type = fn first -> fn second -> match first {
  | A64Unit -> match second { | A64Unit -> true | _ -> false }
  | A64I32 -> match second { | A64I32 -> true | _ -> false }
  | A64Boolean -> match second { | A64Boolean -> true | _ -> false }
  | A64Pointer -> match second { | A64Pointer -> true | _ -> false }
}

let label_number = fn (A64Label(NativeBlockId(number))) -> number
let same_label = fn first -> fn second -> match (label_number first == label_number second) {
  | true -> true
  | false -> false
}

let block_label = fn (A64Block { label, parameters: _, instructions: _, terminator: _, span: _ }) -> label

let find_block = fn wanted -> fn blocks -> match blocks {
  | Nil -> None
  | Cons(block, remaining) -> match (same_label wanted (block_label block)) {
    | true -> Some(block)
    | false -> find_block wanted remaining
  }
}

let append_locations = fn (first: List<A64ValueLocation>) ->
  fn (second: List<A64ValueLocation>) -> match first {
  | Nil -> second
  | Cons(value, remaining) -> Cons(value, append_locations remaining second)
}
let append_linear_items = fn (first: List<A64LinearItem>) ->
  fn (second: List<A64LinearItem>) -> match first {
  | Nil -> second
  | Cons(value, remaining) -> Cons(value, append_linear_items remaining second)
}

let collect_instruction_values = fn instructions -> match instructions {
  | Nil -> Nil
  | Cons(A64Instruction { destination, result_type, operation: _, effects: _, span: _ }, remaining) ->
    Cons(A64ValueLocation { value: destination, value_type: result_type, slot: None },
      collect_instruction_values remaining)
}

let collect_parameter_values = fn parameters -> match parameters {
  | Nil -> Nil
  | Cons(A64BlockParameter { register, value_type, span: _ }, remaining) ->
    Cons(A64ValueLocation { value: register, value_type: value_type, slot: None },
      collect_parameter_values remaining)
}

let collect_block_values = fn blocks -> match blocks {
  | Nil -> Nil
  | Cons(A64Block { label: _, parameters, instructions, terminator: _, span: _ }, remaining) ->
    append_locations (collect_parameter_values parameters)
      (append_locations (collect_instruction_values instructions)
        (collect_block_values remaining))
}

let assign_locations = fn pending -> fn next_offset -> match pending {
  | Nil -> Ok(A64LocationAssignment(Nil, next_offset))
  | Cons(A64ValueLocation { value, value_type, slot: _ }, remaining) ->
    match (slot_size value_type) {
      | 0 -> match (assign_locations remaining next_offset) {
        | Err(message) -> Err(message)
        | Ok(A64LocationAssignment(rest_values, rest_offset)) -> Ok(A64LocationAssignment(
          Cons(A64ValueLocation { value: value, value_type: value_type, slot: None }, rest_values),
          rest_offset
        ))
      }
      | _ -> {
        let alignment = slot_alignment value_type;
        let offset = match (next_offset % alignment) {
          | 0 -> next_offset
          | remainder -> next_offset + (alignment - remainder)
        };
        let size = slot_size value_type;
        match (offset > 2147483647 - size) {
          | true -> Err("AArch64 spill frame exceeds the supported 32-bit size")
          | false -> {
            let slot = A64FrameSlot {
              value: value,
              offset: offset,
              value_type: value_type
            };
            match (assign_locations remaining (offset + size)) {
            | Err(message) -> Err(message)
            | Ok(A64LocationAssignment(rest_values, rest_offset)) -> Ok(A64LocationAssignment(
              Cons(A64ValueLocation {
                value: value, value_type: value_type, slot: Some(slot)
              }, rest_values),
              rest_offset
            ))
            }
          }
        }
      }
    }
}

let find_location = fn wanted -> fn locations -> match locations {
  | Nil -> None
  | Cons(location, remaining) -> match (same_register wanted (location_register location)) {
    | true -> Some(location)
    | false -> find_location wanted remaining
  }
}

let lookup_location = fn function_identity -> fn block -> fn span ->
  fn wanted -> fn locations -> match (find_location wanted locations) {
    | Some(location) -> Ok(location)
    | None -> Err(A64LinearFailure {
      function: function_identity,
      block: block,
      span: span,
      reason: "MachineIR uses a virtual register with no spill location"
    })
  }

let lookup_slot = fn function_identity -> fn block -> fn span ->
  fn wanted -> fn locations -> match (lookup_location function_identity block span wanted locations) {
    | Err(failure) -> Err(failure)
    | Ok(A64ValueLocation { value: _, value_type: A64Unit, slot: None }) ->
      Err(A64LinearFailure {
        function: function_identity, block: block, span: span,
        reason: "unit has no stack slot and was used as a physical value"
      })
    | Ok(A64ValueLocation { value: _, value_type: _, slot: Some(slot) }) -> Ok(slot)
    | Ok(_) -> Err(A64LinearFailure {
      function: function_identity, block: block, span: span,
      reason: "MachineIR value location has an inconsistent unit/slot mapping"
    })
  }

let lookup_type = fn function_identity -> fn block -> fn span ->
  fn wanted -> fn locations -> match (lookup_location function_identity block span wanted locations) {
    | Err(failure) -> Err(failure)
    | Ok(A64ValueLocation { value: _, value_type, slot: _ }) -> Ok(value_type)
  }

let add_instruction = fn instruction -> Cons(A64LinearInstructionItem(instruction), Nil)
let add_terminator = fn terminator -> Cons(A64LinearTerminatorItem(terminator), Nil)

let lower_operation = fn function_identity -> fn block -> fn span ->
  fn destination -> fn result_type -> fn operation -> fn locations -> match operation {
  | A64LoadUnit -> Ok(Nil)
  | A64LoadI32(literal) -> match (lookup_slot function_identity block span destination locations) {
    | Err(failure) -> Err(failure)
    | Ok(slot) -> Ok(append_linear_items (add_instruction(A64LoadI32Literal(w9, literal)))
        (add_instruction(A64SpillRegister(slot, w9))))
  }
  | A64LoadBoolean(value) -> match (lookup_slot function_identity block span destination locations) {
    | Err(failure) -> Err(failure)
    | Ok(slot) -> Ok(append_linear_items (add_instruction(A64LoadBooleanLiteral(w9, value)))
        (add_instruction(A64SpillRegister(slot, w9))))
  }
  | A64TestUnitLiteral(_) -> match (lookup_slot function_identity block span destination locations) {
    | Err(failure) -> Err(failure)
    | Ok(slot) -> Ok(append_linear_items (add_instruction(A64LoadBooleanLiteral(w9, true)))
        (add_instruction(A64SpillRegister(slot, w9))))
  }
  | A64TestI32Literal(A64I32LiteralTest { value, literal }) ->
    match (lookup_slot function_identity block span value locations) {
      | Err(failure) -> Err(failure)
      | Ok(left_slot) -> match (lookup_slot function_identity block span destination locations) {
        | Err(failure) -> Err(failure)
        | Ok(result_slot) -> Ok(append_linear_items (add_instruction(A64ReloadSpill(w9, left_slot)))
          (append_linear_items (add_instruction(A64LoadI32Literal(w10, literal)))
          (append_linear_items (add_instruction(A64CompareSetW(A64Equal, w11, w9, w10)))
            (add_instruction(A64SpillRegister(result_slot, w11))))))
      }
    }
  | A64TestBooleanLiteral(A64BooleanLiteralTest { value, literal }) ->
    match (lookup_slot function_identity block span value locations) {
      | Err(failure) -> Err(failure)
      | Ok(left_slot) -> match (lookup_slot function_identity block span destination locations) {
        | Err(failure) -> Err(failure)
        | Ok(result_slot) -> Ok(append_linear_items (add_instruction(A64ReloadSpill(w9, left_slot)))
          (append_linear_items (add_instruction(A64LoadBooleanLiteral(w10, literal)))
          (append_linear_items (add_instruction(A64CompareSetW(A64Equal, w11, w9, w10)))
            (add_instruction(A64SpillRegister(result_slot, w11))))))
      }
    }
  | A64Call(A64CallSite { target, arguments, result_type: call_result }) ->
    lower_call function_identity block span destination result_type target arguments
      call_result locations
  | A64RuntimeCall(A64RuntimeCallSite { helper, arguments, result_type: call_result }) ->
    match (same_value_type result_type call_result) {
      | false -> Err(A64LinearFailure {
        function: function_identity, block: block, span: span,
        reason: "runtime call result type differs from its instruction type"
      })
      | true -> match call_result {
        | A64Unit -> Err(A64LinearFailure {
          function: function_identity, block: block, span: span,
          reason: "typed i32 runtime helper must return i32"
        })
        | A64Boolean -> Err(A64LinearFailure {
          function: function_identity, block: block, span: span,
          reason: "typed i32 runtime helper must return i32"
        })
        | A64Pointer -> Err(A64LinearFailure {
          function: function_identity, block: block, span: span,
          reason: "typed i32 runtime helper must return i32"
        })
        | A64I32 -> match arguments {
          | Cons(A64CallArgument { register: left, value_type: A64I32 },
              Cons(A64CallArgument { register: right, value_type: A64I32 }, Nil)) ->
            match (lookup_slot function_identity block span left locations) {
              | Err(failure) -> Err(failure)
              | Ok(left_slot) -> match (lookup_slot function_identity block span right locations) {
                | Err(failure) -> Err(failure)
                | Ok(right_slot) -> match (lookup_slot function_identity block span destination locations) {
                  | Err(failure) -> Err(failure)
                  | Ok(result_slot) -> Ok(Cons(A64LinearInstructionItem(A64ReloadSpill(w0, left_slot)),
                    Cons(A64LinearInstructionItem(A64ReloadSpill(w1, right_slot)),
                    Cons(A64LinearInstructionItem(A64CallRuntime(helper)),
                    Cons(A64LinearInstructionItem(A64SpillRegister(result_slot, w0)), Nil)))))
                }
              }
            }
          | _ -> Err(A64LinearFailure {
            function: function_identity, block: block, span: span,
            reason: "typed i32 runtime helper requires exactly two i32 arguments"
          })
        }
      }
    }
  | A64AddI32(A64BinaryOperands { left, right }) ->
    lower_binary function_identity block span destination left right locations A64OpcodeAdd
  | A64SubtractI32(A64BinaryOperands { left, right }) ->
    lower_binary function_identity block span destination left right locations A64OpcodeSubtract
  | A64MultiplyI32(A64BinaryOperands { left, right }) ->
    lower_binary function_identity block span destination left right locations A64OpcodeMultiply
  | A64CompareI32(A64IntegerComparison { condition, left, right }) ->
    lower_comparison function_identity block span destination left right condition locations
  | A64EqualBoolean(A64BinaryOperands { left, right }) ->
    lower_comparison function_identity block span destination left right A64Equal locations
  | A64NotEqualBoolean(A64BinaryOperands { left, right }) ->
    lower_comparison function_identity block span destination left right A64NotEqual locations
  | A64NegateI32(operand) -> match (lookup_slot function_identity block span operand locations) {
    | Err(failure) -> Err(failure)
    | Ok(source_slot) -> match (lookup_slot function_identity block span destination locations) {
      | Err(failure) -> Err(failure)
      | Ok(result_slot) -> Ok(append_linear_items (add_instruction(A64ReloadSpill(w9, source_slot)))
        (append_linear_items (add_instruction(A64NegateW(w11, w9)))
          (add_instruction(A64SpillRegister(result_slot, w11)))))
    }
  }
}

let binary_instruction = fn opcode -> fn destination -> fn left -> fn right -> match opcode {
  | A64OpcodeAdd -> A64AddW(destination, left, right)
  | A64OpcodeSubtract -> A64SubtractW(destination, left, right)
  | A64OpcodeMultiply -> A64MultiplyW(destination, left, right)
}

let lower_binary = fn function_identity -> fn block -> fn span ->
  fn destination -> fn left -> fn right -> fn locations -> fn opcode ->
  match (lookup_slot function_identity block span left locations) {
    | Err(failure) -> Err(failure)
    | Ok(left_slot) -> match (lookup_slot function_identity block span right locations) {
      | Err(failure) -> Err(failure)
      | Ok(right_slot) -> match (lookup_slot function_identity block span destination locations) {
        | Err(failure) -> Err(failure)
        | Ok(result_slot) -> Ok(append_linear_items (add_instruction(A64ReloadSpill(w9, left_slot)))
          (append_linear_items (add_instruction(A64ReloadSpill(w10, right_slot)))
          (append_linear_items (add_instruction(binary_instruction opcode w11 w9 w10))
            (add_instruction(A64SpillRegister(result_slot, w11))))))
      }
    }
  }

let lower_comparison = fn function_identity -> fn block -> fn span ->
  fn destination -> fn left -> fn right -> fn condition -> fn locations ->
  match (lookup_slot function_identity block span left locations) {
    | Err(failure) -> Err(failure)
    | Ok(left_slot) -> match (lookup_slot function_identity block span right locations) {
      | Err(failure) -> Err(failure)
      | Ok(right_slot) -> match (lookup_slot function_identity block span destination locations) {
        | Err(failure) -> Err(failure)
        | Ok(result_slot) -> Ok(append_linear_items (add_instruction(A64ReloadSpill(w9, left_slot)))
          (append_linear_items (add_instruction(A64ReloadSpill(w10, right_slot)))
          (append_linear_items (add_instruction(A64CompareSetW(condition, w11, w9, w10)))
            (add_instruction(A64SpillRegister(result_slot, w11))))))
      }
    }
  }

let lower_call_argument = fn function_identity -> fn block -> fn span ->
  fn (A64CallArgument { register, value_type }) -> fn locations -> match value_type {
  | A64Unit -> Ok(Nil)
  | A64I32 -> match (lookup_slot function_identity block span register locations) {
    | Err(failure) -> Err(failure)
    | Ok(slot) -> Ok(add_instruction(A64ReloadSpill(w1, slot)))
  }
  | A64Boolean -> match (lookup_slot function_identity block span register locations) {
    | Err(failure) -> Err(failure)
    | Ok(slot) -> Ok(add_instruction(A64ReloadSpill(w1, slot)))
  }
  | A64Pointer -> match (lookup_slot function_identity block span register locations) {
    | Err(failure) -> Err(failure)
    | Ok(slot) -> Ok(add_instruction(A64ReloadSpill(x1, slot)))
  }
}

let lower_call_arguments = fn function_identity -> fn block -> fn span ->
  fn arguments -> fn locations -> match arguments {
  | Nil -> Ok(Nil)
  | Cons(argument, Nil) -> lower_call_argument function_identity block span argument locations
  | Cons(_, _) -> Err(A64LinearFailure {
    function: function_identity, block: block, span: span,
    reason: "initial AArch64 internal ABI supports at most one source argument"
  })
}

let direct_target_has_no_environment = fn target -> match target {
  | NativeTopLevelFunction(_) -> true
  | NativeModuleInitializer(_) -> true
  | NativeNestedFunction(_) -> false
}

let lower_host_args_call = fn function_identity -> fn block -> fn span ->
  fn destination -> fn instruction_type -> fn arguments -> fn call_result ->
  fn locations -> match (same_value_type instruction_type call_result) {
  | false -> Err(A64LinearFailure {
    function: function_identity, block: block, span: span,
    reason: "Host.args result type differs from its typed call signature"
  })
  | true -> match call_result {
    | A64Pointer -> match arguments {
      | Cons(A64CallArgument { register: _, value_type: A64Unit }, Nil) ->
        match (lookup_slot function_identity block span destination locations) {
          | Err(failure) -> Err(failure)
          | Ok(result_slot) -> Ok(append_linear_items
            (add_instruction(A64CallRuntime(A64RuntimeHostArgs)))
            (add_instruction(A64SpillRegister(result_slot, x0))))
        }
      | _ -> Err(A64LinearFailure {
        function: function_identity, block: block, span: span,
        reason: "Host.args helper requires one erased unit argument"
      })
    }
    | _ -> Err(A64LinearFailure {
      function: function_identity, block: block, span: span,
      reason: "Host.args helper must return an opaque pointer"
    })
  }
}

let is_host_args_binding = fn (binding: BindingIdentity) -> match binding {
  | BindingIdentity {
      source_path, declaration_span: _, name: ValueName(name), scope
    } -> match (equal source_path "tinyml/lib/host.ml") {
    | false -> false
    | true -> match (equal name "args") {
      | false -> false
      | true -> match scope {
        | ModuleValueBinding -> true
        | _ -> false
      }
    }
  }
  | _ -> false
}

let lower_call = fn function_identity -> fn block -> fn span ->
  fn destination -> fn instruction_type -> fn target -> fn arguments ->
  fn call_result -> fn locations -> match target {
  | A64HostCall(binding) -> match (is_host_args_binding binding) {
    | true -> lower_host_args_call function_identity block span destination
        instruction_type arguments call_result locations
    | false -> {
      let BindingIdentity {
        source_path: _, declaration_span: _, name: ValueName(name), scope: _
      } = binding;
      Err(A64LinearFailure {
        function: function_identity, block: block, span: span,
        reason: "unsupported Host helper `" + name
          + "` requires explicit boundary marshalling"
      })
    }
  }
  | A64DirectCall(callee) -> match (direct_target_has_no_environment callee) {
    | false -> Err(A64LinearFailure {
      function: function_identity, block: block, span: span,
      reason: "nested direct call requires explicit closure-environment lowering"
    })
    | true -> match (lower_call_arguments function_identity block span arguments locations) {
      | Err(failure) -> Err(failure)
      | Ok(argument_setup) -> match (same_value_type instruction_type call_result) {
      | false -> Err(A64LinearFailure {
        function: function_identity, block: block, span: span,
        reason: "call instruction result type differs from its typed call signature"
      })
      | true -> match (is_unit call_result) {
        | true -> Ok(append_linear_items (add_instruction(A64MoveZero(x0)))
          (append_linear_items argument_setup (add_instruction(A64CallDirect(callee)))))
        | false -> match (lookup_slot function_identity block span destination locations) {
          | Err(failure) -> Err(failure)
          | Ok(result_slot) -> Ok(append_linear_items (add_instruction(A64MoveZero(x0)))
            (append_linear_items argument_setup (append_linear_items (add_instruction(A64CallDirect(callee)))
              (add_instruction(A64SpillRegister(result_slot, match call_result {
                | A64Pointer -> x0
                | _ -> w0
              }))))))
        }
      }
    }
    }
  }
}

let lower_instruction = fn function_identity -> fn label ->
  fn (A64Instruction { destination, result_type, operation, effects: _, span }) ->
  fn locations -> match (lower_operation function_identity label
      span destination result_type operation locations) {
    | Err(failure) -> Err(failure)
    | Ok(instructions) -> Ok(instructions)
  }

let lower_instructions = fn function_identity -> fn block -> fn instructions ->
  fn locations -> match instructions {
  | Nil -> Ok(Nil)
  | Cons(instruction, remaining) -> match (lower_instruction function_identity block instruction locations) {
    | Err(failure) -> Err(failure)
    | Ok(current) -> match (lower_instructions function_identity block remaining locations) {
      | Err(failure) -> Err(failure)
      | Ok(rest) -> Ok(append_linear_items current rest)
    }
  }
}

let target_parameters = fn function_identity -> fn block -> fn span ->
  fn (A64Edge { target: A64Label(target_block), arguments: _ }) -> fn blocks ->
  match (find_block (A64Label(target_block)) blocks) {
    | Some(A64Block { parameters, label: _, instructions: _, terminator: _, span: _ }) -> Ok(parameters)
    | None -> Err(A64LinearFailure {
      function: function_identity, block: block, span: span,
      reason: "MachineIR edge targets missing " + block_id target_block
    })
  }

let lower_edge_copies = fn function_identity -> fn block -> fn span ->
  fn arguments -> fn parameters -> fn locations -> match arguments {
  | Nil -> match parameters {
    | Nil -> Ok(Nil)
    | Cons(_, _) -> Err(A64LinearFailure {
      function: function_identity, block: block, span: span,
      reason: "MachineIR edge omits target block arguments"
    })
  }
  | Cons(_, _) -> match parameters {
    | Nil -> Err(A64LinearFailure {
      function: function_identity, block: block, span: span,
      reason: "MachineIR edge supplies more arguments than the target block"
    })
    | Cons(A64BlockParameter {
        register: destination, value_type: destination_type, span: _
      }, destinations) -> match arguments {
      | Nil -> Ok(Nil)
      | Cons(source, sources) -> match (lookup_type function_identity block span source locations) {
        | Err(failure) -> Err(failure)
        | Ok(source_type) -> match (same_value_type source_type destination_type) {
          | false -> Err(A64LinearFailure {
            function: function_identity, block: block, span: span,
            reason: "MachineIR edge argument type differs from target block parameter"
          })
          | true -> match (lower_edge_copies function_identity block span sources destinations locations) {
            | Err(failure) -> Err(failure)
            | Ok(rest) -> match (is_unit source_type) {
              | true -> Ok(rest)
              | false -> match (lookup_slot function_identity block span source locations) {
                | Err(failure) -> Err(failure)
                | Ok(source_slot) -> match (lookup_slot function_identity block span destination locations) {
                  | Err(failure) -> Err(failure)
                  | Ok(destination_slot) -> {
                    let copy_register = match source_type {
                      | A64Pointer -> x9
                      | _ -> w9
                    };
                    Ok(append_linear_items
                      (add_instruction(A64ReloadSpill(copy_register, source_slot)))
                      (append_linear_items
                        (add_instruction(A64SpillRegister(destination_slot, copy_register))) rest))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

let lower_edge = fn function_identity -> fn block -> fn span ->
  fn (A64Edge { target, arguments }) -> fn blocks -> fn locations ->
  match (target_parameters function_identity block span
      (A64Edge { target: target, arguments: arguments }) blocks) {
    | Err(failure) -> Err(failure)
    | Ok(parameters) -> match (lower_edge_copies function_identity block span arguments parameters locations) {
      | Err(failure) -> Err(failure)
      | Ok(copies) -> Ok(A64LoweredEdge(copies, A64BasicBlockLabel(target)))
    }
  }

let lower_terminator = fn function_identity -> fn function_result -> fn block ->
  fn span -> fn terminator -> fn blocks -> fn locations -> match terminator {
  | A64Jump(edge) -> match (lower_edge function_identity block span edge blocks locations) {
    | Err(failure) -> Err(failure)
    | Ok(A64LoweredEdge(copies, target)) -> Ok(append_linear_items copies (add_terminator(A64LinearJump(target))))
  }
  | A64ConditionalBranch(A64Branch { condition, when_true, when_false }) ->
    match (lookup_slot function_identity block span condition locations) {
      | Err(failure) -> Err(failure)
      | Ok(condition_slot) -> match (lower_edge function_identity block span when_true blocks locations) {
        | Err(failure) -> Err(failure)
        | Ok(A64LoweredEdge(true_copies, true_target)) -> match (lower_edge function_identity block span when_false blocks locations) {
          | Err(failure) -> Err(failure)
          | Ok(A64LoweredEdge(false_copies, false_target)) -> {
            let source_label = block;
            let true_label = A64TrueEdgeLabel(source_label);
            let false_label = A64FalseEdgeLabel(source_label);
            Ok(append_linear_items (add_instruction(A64ReloadSpill(w9, condition_slot)))
              (append_linear_items (add_terminator(A64LinearBranch(w9, true_label, false_label)))
              (append_linear_items (Cons(A64LinearLabelItem(true_label), true_copies))
              (append_linear_items (add_terminator(A64LinearJump(true_target)))
              (append_linear_items (Cons(A64LinearLabelItem(false_label), false_copies))
                (add_terminator(A64LinearJump(false_target))))))))
          }
        }
      }
    }
  | A64Return(value) -> match (lookup_type function_identity block span value locations) {
    | Err(failure) -> Err(failure)
    | Ok(value_type) -> match (same_value_type function_result value_type) {
      | false -> Err(A64LinearFailure {
        function: function_identity, block: block, span: span,
        reason: "return value type differs from the function result type"
      })
      | true -> match (is_unit function_result) {
        | true -> Ok(add_terminator(A64LinearReturn))
        | false -> match (lookup_slot function_identity block span value locations) {
          | Err(failure) -> Err(failure)
          | Ok(slot) -> {
            let return_register = match function_result {
              | A64Pointer -> x0
              | _ -> w0
            };
            Ok(append_linear_items (add_instruction(A64ReloadSpill(return_register, slot)))
              (add_terminator(A64LinearReturn)))
          }
        }
      }
    }
  }
  | A64Trap(reason) -> Ok(add_terminator(A64LinearTrap(reason)))
  | A64Unreachable -> Ok(add_terminator(A64LinearUnreachable))
}

let lower_entry_parameter_setup = fn function_identity -> fn label ->
  fn span -> fn parameters -> fn locations -> match parameters {
  | Nil -> Ok(Nil)
  | Cons(A64BlockParameter { register: _, value_type: A64Unit, span: _ }, Nil) -> Ok(Nil)
  | Cons(A64BlockParameter { register, value_type: A64I32, span: _ }, Nil) ->
    match (lookup_slot function_identity label span register locations) {
      | Err(failure) -> Err(failure)
      | Ok(slot) -> Ok(add_instruction(A64SpillRegister(slot, w1)))
    }
  | Cons(A64BlockParameter { register, value_type: A64Boolean, span: _ }, Nil) ->
    match (lookup_slot function_identity label span register locations) {
      | Err(failure) -> Err(failure)
      | Ok(slot) -> Ok(add_instruction(A64SpillRegister(slot, w1)))
    }
  | Cons(A64BlockParameter { register, value_type: A64Pointer, span: _ }, Nil) ->
    match (lookup_slot function_identity label span register locations) {
      | Err(failure) -> Err(failure)
      | Ok(slot) -> Ok(add_instruction(A64SpillRegister(slot, x1)))
    }
  | Cons(_, Cons(_, _)) -> Err(A64LinearFailure {
    function: function_identity,
    block: label,
    span: span,
    reason: "initial AArch64 internal ABI supports one source argument"
  })
}

let lower_block_items = fn function_identity -> fn function_result -> fn blocks ->
  fn locations -> fn label -> fn parameter_setup -> fn instructions ->
  fn terminator -> fn span -> match (lower_instructions function_identity label instructions locations) {
    | Err(failure) -> Err(failure)
    | Ok(lowered_instructions) -> match (lower_terminator function_identity function_result
        label span terminator blocks locations) {
      | Err(failure) -> Err(failure)
      | Ok(lowered_terminator) -> Ok(append_linear_items (Cons(A64LinearLabelItem(A64BasicBlockLabel(label)),
          append_linear_items parameter_setup lowered_instructions)) lowered_terminator)
    }
  }

let lower_block = fn function_identity -> fn function_result -> fn entry -> fn blocks ->
  fn locations -> fn (A64Block { label, parameters, instructions, terminator, span }) ->
  match (same_label entry label) {
    | false -> lower_block_items function_identity function_result blocks locations
        label Nil instructions terminator span
    | true -> match (lower_entry_parameter_setup function_identity label span parameters locations) {
      | Err(failure) -> Err(failure)
      | Ok(parameter_setup) -> lower_block_items function_identity function_result blocks
          locations label parameter_setup instructions terminator span
    }
  }

let lower_blocks_from = fn function_identity -> fn function_result -> fn entry ->
  fn all_blocks -> fn pending_blocks -> fn locations -> match pending_blocks {
  | Nil -> Ok(Nil)
  | Cons(block, remaining) -> match (lower_block function_identity function_result
      entry all_blocks locations block) {
    | Err(failure) -> Err(failure)
    | Ok(current) -> match (lower_blocks_from function_identity function_result
        entry all_blocks remaining locations) {
      | Err(failure) -> Err(failure)
      | Ok(rest) -> Ok(append_linear_items current rest)
    }
  }
}
let lower_blocks = fn function_identity -> fn function_result -> fn entry -> fn blocks ->
  fn locations -> lower_blocks_from function_identity function_result entry
    blocks blocks locations

let lower_function = fn (A64Function { identity, result_type, entry, blocks, span }) ->
  match (assign_locations (collect_block_values blocks) 0) {
    | Err(message) -> match blocks {
      | Cons(A64Block { label, span: block_span, parameters: _, instructions: _, terminator: _ }, _) ->
        Err(A64LinearFailure {
          function: identity, block: label, span: block_span, reason: message
        })
      | Nil -> Err(A64LinearFailure {
        function: identity, block: entry, span: span, reason: message
      })
    }
    | Ok(A64LocationAssignment(values, next_offset)) -> match (lower_blocks identity result_type entry blocks values) {
      | Err(failure) -> Err(failure)
      | Ok(items) -> Ok(A64LinearFunction {
        identity: identity,
        result_type: result_type,
        entry: A64BasicBlockLabel(entry),
        values: values,
        frame_size: align_frame next_offset,
        items: items,
        span: span
      })
    }
  }
