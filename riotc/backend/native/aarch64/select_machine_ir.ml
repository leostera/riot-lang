// Select the ABI-free scalar/control-flow subset of verified NativeIR to AArch64 virtual registers.
use Prelude.{List, Nil, Cons, Result, Ok, Err}
use String.{equal}
use Syntax.Ast.{
  Addition, Subtraction, Multiplication, Division, Remainder, Equal,
  NotEqual, LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual
}
use Check.Inference.{
  InferenceType, AppliedType, InferredNamedType, TypeSubstitution,
  apply_substitutions
}
use NativeIr.Ir.{
  NativeFunction, NativeFunctionId, NativeBlock, NativeBlockId, NativeBlockParameter,
  NativeValueId, NativeInstruction, NativeConstant, NativeUnit, NativeInteger,
  NativeBoolean, NativePrimitiveOperation, NativePrimitiveBinary,
  NativePrimitiveNegate, NativeBinaryPrimitive, NativeNegatePrimitive,
  NativeTestValue, NativeValueTest, NativeLiteralTest, NativePatternMatchTest,
  NativeLiteralPatternTest, NativeCallOperation, NativeCall, NativeDirectCall,
  NativeClosureCall, NativeHostCall, NativeEffectSet, NativeTerminator,
  NativeJump, NativeConditionalBranch,
  NativeBranch, NativeReturn, NativeTrap, NativeUnreachable, NativeSwitchTerminator,
  NativeEdge
}
use Backend.Native.Aarch64.MachineIr.{
  A64VirtualRegister, A64Label, A64ValueType, A64Unit, A64I32, A64Boolean,
  A64Function, A64Block, A64BlockParameter, A64Instruction, A64Operation,
  A64LoadUnit, A64LoadI32, A64LoadBoolean, A64TestUnitLiteral, A64Call, A64CallSite, A64DirectCall,
  A64HostCall, A64CallArgument, A64RuntimeCall, A64RuntimeCallSite,
  A64RuntimeI32Division, A64RuntimeI32Remainder, A64AddI32, A64SubtractI32,
  A64MultiplyI32,
  A64TestI32Literal, A64TestBooleanLiteral, A64I32LiteralTest,
  A64BooleanLiteralTest, A64CompareI32, A64EqualBoolean, A64NotEqualBoolean,
  A64BinaryOperands,
  A64IntegerComparison,
  A64Equal, A64NotEqual, A64SignedLessThan, A64SignedLessThanOrEqual,
  A64SignedGreaterThan, A64SignedGreaterThanOrEqual, A64Edge,
  A64Branch, A64Terminator, A64Jump, A64ConditionalBranch, A64Return,
  A64Trap, A64Unreachable, A64NegateI32, A64SelectionFailure
}

let native_register = fn (NativeValueId(number)) -> A64VirtualRegister(number)
let native_label = fn block -> A64Label(block)

let scalar_type = fn checked_type -> fn substitutions ->
  match (apply_substitutions checked_type substitutions) {
    | AppliedType(InferredNamedType { name, arguments: Nil }) -> match name {
      | "unit" -> Ok(A64Unit)
      | "i32" -> Ok(A64I32)
      | "bool" -> Ok(A64Boolean)
      | _ -> Err("only unit, i32 and bool values have an AArch64 representation here")
    }
    | _ -> Err("only fully resolved unit, i32 and bool values are selected here")
  }

let same_scalar_type = fn first -> fn second -> match first {
  | A64Unit -> match second { | A64Unit -> true | _ -> false }
  | A64I32 -> match second { | A64I32 -> true | _ -> false }
  | A64Boolean -> match second { | A64Boolean -> true | _ -> false }
}

let is_scalar_type = fn expected -> fn checked_type -> fn substitutions ->
  match (scalar_type checked_type substitutions) {
    | Err(_) -> false
    | Ok(actual) -> same_scalar_type expected actual
  }

// Keep this helper's result concrete: module-level values remain monomorphic
// while Riotc checks the module, so callers must wrap the failure in `Err`.
let selection_failure = fn function_identity -> fn block_identity -> fn span ->
  fn reason -> A64SelectionFailure {
    function: function_identity,
    block: block_identity,
    span: span,
    reason: reason
  }

let comparison_condition = fn operator -> match operator {
  | Equal -> Ok(A64Equal)
  | NotEqual -> Ok(A64NotEqual)
  | LessThan -> Ok(A64SignedLessThan)
  | LessThanOrEqual -> Ok(A64SignedLessThanOrEqual)
  | GreaterThan -> Ok(A64SignedGreaterThan)
  | GreaterThanOrEqual -> Ok(A64SignedGreaterThanOrEqual)
  | _ -> Err("operator is not an integer comparison")
}

let select_i32_runtime_binary = fn helper -> fn operand_type -> fn result_type ->
  fn left -> fn right -> fn substitutions ->
  match (is_scalar_type A64I32 operand_type substitutions) {
    | false -> Err("runtime i32 helper requires i32 operands")
    | true -> match (is_scalar_type A64I32 result_type substitutions) {
      | false -> Err("runtime i32 helper must produce i32")
      | true -> Ok(A64RuntimeCall(A64RuntimeCallSite {
        helper: helper,
        arguments: Cons(A64CallArgument {
          register: native_register left,
          value_type: A64I32
        }, Cons(A64CallArgument {
          register: native_register right,
          value_type: A64I32
        }, Nil)),
        result_type: A64I32
      }))
    }
  }

let select_integer_binary = fn operator -> fn operand_type -> fn result_type ->
  fn left -> fn right -> fn substitutions -> match operator {
    | Addition -> match (is_scalar_type A64I32 operand_type substitutions) {
      | false -> Err("integer addition requires i32 operands")
      | true -> match (is_scalar_type A64I32 result_type substitutions) {
        | true -> Ok(A64AddI32(A64BinaryOperands {
          left: native_register left,
          right: native_register right
        }))
        | false -> Err("i32 addition must produce i32")
      }
    }
    | Subtraction -> match (is_scalar_type A64I32 operand_type substitutions) {
      | false -> Err("integer subtraction requires i32 operands")
      | true -> match (is_scalar_type A64I32 result_type substitutions) {
        | true -> Ok(A64SubtractI32(A64BinaryOperands {
          left: native_register left,
          right: native_register right
        }))
        | false -> Err("i32 subtraction must produce i32")
      }
    }
    | Multiplication -> match (is_scalar_type A64I32 operand_type substitutions) {
      | false -> Err("integer multiplication requires i32 operands")
      | true -> match (is_scalar_type A64I32 result_type substitutions) {
        | true -> Ok(A64MultiplyI32(A64BinaryOperands {
          left: native_register left,
          right: native_register right
        }))
        | false -> Err("i32 multiplication must produce i32")
      }
    }
    | Division -> select_i32_runtime_binary A64RuntimeI32Division
        operand_type result_type left right substitutions
    | Remainder -> select_i32_runtime_binary A64RuntimeI32Remainder
        operand_type result_type left right substitutions
    | _ -> match (is_scalar_type A64Boolean operand_type substitutions) {
      | true -> match operator {
        | Equal -> match (is_scalar_type A64Boolean result_type substitutions) {
          | true -> Ok(A64EqualBoolean(A64BinaryOperands {
            left: native_register left,
            right: native_register right
          }))
          | false -> Err("boolean equality must produce bool")
        }
        | NotEqual -> match (is_scalar_type A64Boolean result_type substitutions) {
          | true -> Ok(A64NotEqualBoolean(A64BinaryOperands {
            left: native_register left,
            right: native_register right
          }))
          | false -> Err("boolean inequality must produce bool")
        }
        | _ -> Err("boolean values only support equality comparisons")
      }
      | false -> match (comparison_condition operator) {
        | Err(message) -> Err(message)
        | Ok(condition) -> match (is_scalar_type A64I32 operand_type substitutions) {
          | false -> Err("integer comparison requires i32 operands")
          | true -> match (is_scalar_type A64Boolean result_type substitutions) {
            | true -> Ok(A64CompareI32(A64IntegerComparison {
              condition: condition,
              left: native_register left,
              right: native_register right
            }))
            | false -> Err("integer comparison must produce bool")
          }
        }
      }
    }
  }

let select_call_target = fn target -> match target {
  | NativeDirectCall(function) -> Ok(A64DirectCall(function))
  | NativeHostCall(binding) -> Ok(A64HostCall(binding))
  | NativeClosureCall(_) -> Err("closure calls require a selected closure representation")
}

let select_call_arguments = fn values -> fn argument_types -> fn substitutions ->
  match values {
    | Nil -> match argument_types {
      | Nil -> Ok(Nil)
      | _ -> Err("call value/type arity mismatch")
    }
    | Cons(value, remaining_values) -> match argument_types {
      | Nil -> Err("call value/type arity mismatch")
      | Cons(checked_type, remaining_types) -> match (scalar_type checked_type substitutions) {
        | Err(message) -> Err("call argument: " + message)
        | Ok(value_type) -> match (select_call_arguments remaining_values
            remaining_types substitutions) {
          | Err(message) -> Err(message)
          | Ok(rest) -> Ok(Cons(A64CallArgument {
            register: native_register value,
            value_type: value_type
          }, rest))
        }
      }
    }
  }

let select_call_site = fn target -> fn arguments -> fn argument_types ->
  fn call_result -> fn instruction_result -> fn substitutions -> {
  match (select_call_target target) {
    | Err(message) -> Err(message)
    | Ok(machine_target) -> match (select_call_arguments arguments argument_types substitutions) {
      | Err(message) -> Err(message)
      | Ok(machine_arguments) -> match (scalar_type call_result substitutions) {
        | Err(message) -> Err("call result: " + message)
        | Ok(machine_result) -> match (is_scalar_type machine_result
            instruction_result substitutions) {
          | false -> Err("call result type disagrees with its NativeIR definition")
          | true -> Ok(A64CallSite {
            target: machine_target,
            arguments: machine_arguments,
            result_type: machine_result
          })
        }
      }
    }
  }
}

// The NativeIR verifier guarantees that the tested value matches this literal's type.
let select_literal_test = fn value -> fn constant -> fn result_type ->
  fn substitutions -> match (is_scalar_type A64Boolean result_type substitutions) {
    | false -> Err("literal pattern test must produce bool")
    | true -> match constant {
      | NativeUnit -> Ok(A64TestUnitLiteral(native_register value))
      | NativeInteger(literal) -> Ok(A64TestI32Literal(A64I32LiteralTest {
        value: native_register value,
        literal: literal
      }))
      | NativeBoolean(literal) -> Ok(A64TestBooleanLiteral(A64BooleanLiteralTest {
        value: native_register value,
        literal: literal
      }))
      | _ -> Err("unit and string literal tests are outside the scalar selector")
    }
  }

let select_operation = fn operation -> fn result_type -> fn substitutions -> match operation {
  | NativeConstant(NativeUnit) -> match (is_scalar_type A64Unit result_type substitutions) {
    | true -> Ok(A64LoadUnit)
    | false -> Err("unit constant must have type unit")
  }
  | NativeConstant(NativeInteger(literal)) -> match (is_scalar_type A64I32 result_type substitutions) {
    | true -> Ok(A64LoadI32(literal))
    | false -> Err("integer constant must have type i32")
  }
  | NativeConstant(NativeBoolean(value)) -> match (is_scalar_type A64Boolean result_type substitutions) {
    | true -> Ok(A64LoadBoolean(value))
    | false -> Err("boolean constant must have type bool")
  }
  | NativeConstant(_) -> Err("string constants are outside the scalar selector")
  | NativePrimitiveOperation(NativePrimitiveBinary(NativeBinaryPrimitive {
      operator, operand_type, left, right
    })) -> select_integer_binary operator operand_type result_type left right substitutions
  | NativeTestValue(NativeValueTest {
      value, test: NativeLiteralTest(constant)
    }) -> select_literal_test value constant result_type substitutions
  | NativeTestValue(NativeValueTest {
      value, test: NativePatternMatchTest(NativeLiteralPatternTest(constant))
    }) -> select_literal_test value constant result_type substitutions
  | NativeCallOperation(NativeCall {
      target, arguments, argument_types, result_type: call_result
    }) -> match (select_call_site target arguments argument_types call_result
        result_type substitutions) {
      | Err(message) -> Err(message)
      | Ok(call_site) -> Ok(A64Call(call_site))
    }
  | NativePrimitiveOperation(NativePrimitiveNegate(NativeNegatePrimitive {
      operand_type, operand
    })) -> match (is_scalar_type A64I32 operand_type substitutions) {
    | false -> Err("integer negation requires an i32 operand")
    | true -> match (is_scalar_type A64I32 result_type substitutions) {
      | true -> Ok(A64NegateI32(native_register operand))
      | false -> Err("i32 negation must produce i32")
    }
  }
  | _ -> Err("operation is not in the AArch64 scalar selector subset")
}

let operation_is_call = fn operation -> match operation {
  | NativeCallOperation(_) -> true
  | NativePrimitiveOperation(NativePrimitiveBinary(NativeBinaryPrimitive {
      operator: Division, operand_type: _, left: _, right: _
    })) -> true
  | NativePrimitiveOperation(NativePrimitiveBinary(NativeBinaryPrimitive {
      operator: Remainder, operand_type: _, left: _, right: _
    })) -> true
  | _ -> false
}

let select_instruction = fn function_identity -> fn block_identity ->
  fn substitutions -> fn (NativeInstruction {
    result, result_type, operation, effects: NativeEffectSet(effects), span
  }) -> match (select_operation operation result_type substitutions) {
    | Err(reason) -> Err(selection_failure function_identity block_identity span reason)
    | Ok(machine_operation) -> match (operation_is_call operation) {
      | true -> match (scalar_type result_type substitutions) {
        | Err(reason) -> Err(selection_failure function_identity block_identity span reason)
        | Ok(machine_type) -> Ok(A64Instruction {
          destination: native_register result,
          result_type: machine_type,
          operation: machine_operation,
          effects: NativeEffectSet(effects),
          span: span
        })
      }
      | false -> match effects {
        | Nil -> match (scalar_type result_type substitutions) {
        | Err(reason) -> Err(selection_failure function_identity block_identity span reason)
        | Ok(machine_type) -> Ok(A64Instruction {
          destination: native_register result,
          result_type: machine_type,
          operation: machine_operation,
          effects: NativeEffectSet(Nil),
          span: span
        })
        }
        | _ -> Err(selection_failure function_identity block_identity span
            "selection would discard an explicit NativeIR effect")
      }
    }
  }

let select_block_parameters = fn function_identity -> fn block_identity ->
  fn substitutions -> fn parameters -> match parameters {
    | Nil -> Ok(Nil)
    | Cons(NativeBlockParameter { value, checked_type, span }, remaining) ->
      match (scalar_type checked_type substitutions) {
        | Err(reason) -> Err(selection_failure function_identity block_identity span reason)
        | Ok(value_type) -> match (select_block_parameters function_identity block_identity
            substitutions remaining) {
          | Err(failure) -> Err(failure)
          | Ok(rest) -> Ok(Cons(A64BlockParameter {
            register: native_register value,
            value_type: value_type,
            span: span
          }, rest))
        }
      }
  }

let select_instructions = fn function_identity -> fn block_identity ->
  fn substitutions -> fn instructions -> match instructions {
    | Nil -> Ok(Nil)
    | Cons(instruction, remaining) -> match (select_instruction function_identity
        block_identity substitutions instruction) {
      | Err(failure) -> Err(failure)
      | Ok(selected) -> match (select_instructions function_identity block_identity
          substitutions remaining) {
        | Err(failure) -> Err(failure)
        | Ok(rest) -> Ok(Cons(selected, rest))
      }
    }
  }

let select_edge = fn (NativeEdge { target, arguments }) -> A64Edge {
  target: native_label target,
  arguments: native_registers arguments
}

let native_registers = fn values -> match values {
  | Nil -> Nil
  | Cons(value, remaining) -> Cons(native_register value, native_registers remaining)
}

let select_terminator = fn function_identity -> fn block_identity ->
  fn span -> fn terminator -> match terminator {
    | NativeJump(edge) -> Ok(A64Jump(select_edge edge))
    | NativeConditionalBranch(NativeBranch { condition, when_true, when_false }) ->
      Ok(A64ConditionalBranch(A64Branch {
        condition: native_register condition,
        when_true: select_edge when_true,
        when_false: select_edge when_false
      }))
    | NativeReturn(value) -> Ok(A64Return(native_register value))
    | NativeTrap(reason) -> Ok(A64Trap(reason))
    | NativeUnreachable -> Ok(A64Unreachable)
    | NativeSwitchTerminator(_) -> Err(selection_failure function_identity block_identity span
        "switch terminators are not in the AArch64 scalar selector subset")
  }

let select_block = fn function_identity -> fn substitutions ->
  fn (NativeBlock { identity, parameters, instructions, terminator, span }) ->
  match (select_block_parameters function_identity identity substitutions parameters) {
    | Err(failure) -> Err(failure)
    | Ok(machine_parameters) -> match (select_instructions function_identity identity
        substitutions instructions) {
      | Err(failure) -> Err(failure)
      | Ok(machine_instructions) -> match (select_terminator function_identity identity
          span terminator) {
        | Err(failure) -> Err(failure)
        | Ok(machine_terminator) -> Ok(A64Block {
          label: native_label identity,
          parameters: machine_parameters,
          instructions: machine_instructions,
          terminator: machine_terminator,
          span: span
        })
      }
    }
  }

let select_blocks = fn function_identity -> fn substitutions -> fn blocks -> match blocks {
  | Nil -> Ok(Nil)
  | Cons(block, remaining) -> match (select_block function_identity substitutions block) {
    | Err(failure) -> Err(failure)
    | Ok(selected) -> match (select_blocks function_identity substitutions remaining) {
      | Err(failure) -> Err(failure)
      | Ok(rest) -> Ok(Cons(selected, rest))
    }
  }
}

// The caller must verify the NativeIR module first. This stage preserves its
// SSA IDs, CFG edges, and spans; no physical register or calling convention is chosen.
let select_function = fn (NativeFunction {
  identity, result_type, entry, blocks, span
}) -> fn substitutions -> match (scalar_type result_type substitutions) {
  | Err(reason) -> match blocks {
    | Cons(NativeBlock { identity: block, span: block_span, parameters: _, instructions: _,
        terminator: _ }, _) -> Err(selection_failure identity block block_span reason)
    | Nil -> Err(A64SelectionFailure {
      function: identity, block: NativeBlockId(0), span: span, reason: reason
    })
  }
  | Ok(machine_result_type) -> match (select_blocks identity substitutions blocks) {
    | Err(failure) -> Err(failure)
    | Ok(machine_blocks) -> Ok(A64Function {
      identity: identity,
      result_type: machine_result_type,
      entry: native_label entry,
      blocks: machine_blocks,
      span: span
    })
  }
}
