// Executable positive and negative fixtures for NativeIR verification.
use Prelude.{List, Nil, Cons, Result, Ok, Err}
use Syntax.Ast.{ValueName, IntegerLiteral, Addition, Division}
use Syntax.Lexer.{SourceSpan}
use Check.Inference.{
  InferenceType, AppliedType, InferredNamedType, InferredTuple, BindingIdentity,
  ModuleValueBinding
}
use NativeIr.Ir.{
  NativeModule, NativeGlobal, NativeFunctionId, NativeTopLevelFunction,
  NativeModuleInitializer, NativeFunction, NativeBlockId, NativeValueId,
  NativeBlock, NativeBlockParameter, NativeInstruction, NativeOperation,
  NativeConstant, NativeUnit, NativeInteger, NativeBoolean, NativeString,
  NativePrimitiveOperation, NativePrimitiveBinary, NativeBinaryPrimitive,
  NativeCreateAggregate, NativeAggregate, NativeAggregateValue, NativeTupleAggregate,
  NativeTupleField, NativeEffectSet, NativeEffect, NativeMayTrap, NativeDivisionByZero,
  NativeAllocate, NativeEdge, NativeJump, NativeConditionalBranch,
  NativeBranch, NativeReturn, NativeTrap, NativePatternFailure,
  NativeTerminator
}
use NativeIr.Verify.{verify_module}

let module_name = "native-ir-verifier-test"
let source_span = SourceSpan { start_codepoint: 0, end_codepoint: 1 }
let unit_type = AppliedType(InferredNamedType { name: "unit", arguments: Nil })
let int_type = AppliedType(InferredNamedType { name: "i32", arguments: Nil })
let bool_type = AppliedType(InferredNamedType { name: "bool", arguments: Nil })
let tuple_type = InferredTuple(Cons(int_type, Cons(bool_type, Nil)))
let initializer_id = NativeModuleInitializer(module_name)
let test_binding = BindingIdentity {
  source_path: module_name,
  declaration_span: source_span,
  name: ValueName("test_function"),
  scope: ModuleValueBinding
}

let initializer_function = NativeFunction {
  identity: initializer_id,
  result_type: unit_type,
  entry: NativeBlockId(0),
  blocks: Cons(NativeBlock {
    identity: NativeBlockId(0),
    parameters: Nil,
    instructions: Cons(NativeInstruction {
      result: NativeValueId(0),
      result_type: unit_type,
      operation: NativeConstant(NativeUnit),
      effects: NativeEffectSet(Nil),
      span: source_span
    }, Nil),
    terminator: NativeReturn(NativeValueId(0)),
    span: source_span
  }, Nil),
  span: source_span
}

let fixture_function = fn result_type -> fn entry -> fn blocks -> NativeFunction {
  identity: NativeTopLevelFunction(test_binding),
  result_type: result_type,
  entry: entry,
  blocks: blocks,
  span: source_span
}

let fixture_module = fn function -> NativeModule {
  module_identity: module_name,
  nominal_types: Nil,
  substitutions: Nil,
  globals: Nil,
  initializer: initializer_id,
  functions: Cons(initializer_function, Cons(function, Nil))
}

let instruction = fn result -> fn checked_type -> fn operation -> fn effects ->
  NativeInstruction {
    result: result,
    result_type: checked_type,
    operation: operation,
    effects: effects,
    span: source_span
  }

let block = fn id -> fn parameters -> fn instructions -> fn terminator -> NativeBlock {
  identity: NativeBlockId(id),
  parameters: parameters,
  instructions: instructions,
  terminator: terminator,
  span: source_span
}

let expect_ok = fn result -> match result {
  | Ok(true) -> true
  | _ -> false
}

let expect_error = fn result -> match result {
  | Err(_) -> true
  | _ -> false
}

let valid_join_function = fixture_function int_type (NativeBlockId(0)) (Cons(
  block 0 (Nil) (Cons(instruction (NativeValueId(0)) bool_type
    (NativeConstant(NativeBoolean(true))) (NativeEffectSet(Nil)), Nil))
    (NativeConditionalBranch(NativeBranch {
      condition: NativeValueId(0),
      when_true: NativeEdge { target: NativeBlockId(1), arguments: Nil },
      when_false: NativeEdge { target: NativeBlockId(2), arguments: Nil }
    })),
  Cons(block 1 (Nil) (Cons(instruction (NativeValueId(1)) int_type
    (NativeConstant(NativeInteger(IntegerLiteral("10")))) (NativeEffectSet(Nil)), Nil))
    (NativeJump(NativeEdge {
      target: NativeBlockId(3),
      arguments: Cons(NativeValueId(1), Nil)
    })),
  Cons(block 2 (Nil) (Cons(instruction (NativeValueId(2)) int_type
    (NativeConstant(NativeInteger(IntegerLiteral("20")))) (NativeEffectSet(Nil)), Nil))
    (NativeJump(NativeEdge {
      target: NativeBlockId(3),
      arguments: Cons(NativeValueId(2), Nil)
    })),
  Cons(block 3 (Cons(NativeBlockParameter {
      value: NativeValueId(3),
      checked_type: int_type,
      span: source_span
    }, Nil)) (Nil) (NativeReturn(NativeValueId(3))), Nil)))))

let bad_undefined_function = fixture_function int_type (NativeBlockId(0))
  (Cons(block 0 (Nil) (Nil) (NativeReturn(NativeValueId(77))), Nil))

let bad_duplicate_definition_function = fixture_function int_type (NativeBlockId(0))
  (Cons(block 0 (Nil) (Cons(
    instruction (NativeValueId(1)) int_type (NativeConstant(NativeInteger(IntegerLiteral("1"))))
      (NativeEffectSet(Nil)),
    Cons(instruction (NativeValueId(1)) int_type (NativeConstant(NativeInteger(IntegerLiteral("2"))))
      (NativeEffectSet(Nil)), Nil)
  )) (NativeReturn(NativeValueId(1))), Nil))

let bad_dominance_function = fixture_function int_type (NativeBlockId(0)) (Cons(
  block 0 (Nil) (Cons(instruction (NativeValueId(0)) bool_type
    (NativeConstant(NativeBoolean(true))) (NativeEffectSet(Nil)), Nil))
    (NativeConditionalBranch(NativeBranch {
      condition: NativeValueId(0),
      when_true: NativeEdge { target: NativeBlockId(1), arguments: Nil },
      when_false: NativeEdge { target: NativeBlockId(2), arguments: Nil }
    })),
  Cons(block 1 (Nil) (Cons(instruction (NativeValueId(1)) int_type
    (NativeConstant(NativeInteger(IntegerLiteral("10")))) (NativeEffectSet(Nil)), Nil))
    (NativeJump(NativeEdge { target: NativeBlockId(3), arguments: Nil })),
  Cons(block 2 (Nil) (Nil) (NativeReturn(NativeValueId(1))),
  Cons(block 3 (Nil) (Nil) (NativeReturn(NativeValueId(1))), Nil)))))

let bad_edge_arity_function = fixture_function int_type (NativeBlockId(0)) (Cons(
  block 0 (Nil) (Nil) (NativeJump(NativeEdge {
    target: NativeBlockId(1), arguments: Nil
  })),
  Cons(block 1 (Cons(NativeBlockParameter {
    value: NativeValueId(0), checked_type: int_type, span: source_span
  }, Nil)) (Nil) (NativeReturn(NativeValueId(0))), Nil)))

let bad_edge_type_function = fixture_function int_type (NativeBlockId(0)) (Cons(
  block 0 (Nil) (Cons(instruction (NativeValueId(0)) bool_type
    (NativeConstant(NativeBoolean(true))) (NativeEffectSet(Nil)), Nil))
    (NativeJump(NativeEdge {
      target: NativeBlockId(1), arguments: Cons(NativeValueId(0), Nil)
    })),
  Cons(block 1 (Cons(NativeBlockParameter {
    value: NativeValueId(1), checked_type: int_type, span: source_span
  }, Nil)) (Nil) (NativeReturn(NativeValueId(1))), Nil)))

let bad_return_type_function = fixture_function int_type (NativeBlockId(0)) (Cons(
  block 0 (Nil) (Cons(instruction (NativeValueId(0)) unit_type
    (NativeConstant(NativeUnit)) (NativeEffectSet(Nil)), Nil))
    (NativeReturn(NativeValueId(0))), Nil))

let division_function = fn effects -> {
  let left = instruction (NativeValueId(0)) int_type
    (NativeConstant(NativeInteger(IntegerLiteral("8")))) (NativeEffectSet(Nil));
  let right = instruction (NativeValueId(1)) int_type
    (NativeConstant(NativeInteger(IntegerLiteral("2")))) (NativeEffectSet(Nil));
  let division = instruction (NativeValueId(2)) int_type
    (NativePrimitiveOperation(NativePrimitiveBinary(NativeBinaryPrimitive {
      operator: Division,
      operand_type: int_type,
      left: NativeValueId(0),
      right: NativeValueId(1)
    }))) effects;
  let body = NativeBlock {
    identity: NativeBlockId(0),
    parameters: Nil,
    instructions: Cons(left, Cons(right, Cons(division, Nil))),
    terminator: NativeReturn(NativeValueId(2)),
    span: source_span
  };
  fixture_function int_type (NativeBlockId(0)) (Cons(body, Nil))
}

let bad_division_effect_function = division_function (NativeEffectSet(Nil))

let valid_division_effect_function = division_function
  (NativeEffectSet(Cons(NativeMayTrap(NativeDivisionByZero), Nil)))

let valid_trap_function = fixture_function unit_type (NativeBlockId(0))
  (Cons(block 0 (Nil) (Nil) (NativeTrap(NativePatternFailure)), Nil))

let tuple_aggregate_function = fn first_index -> {
  let first = instruction (NativeValueId(0)) int_type
    (NativeConstant(NativeInteger(IntegerLiteral("7")))) (NativeEffectSet(Nil));
  let second = instruction (NativeValueId(1)) bool_type
    (NativeConstant(NativeBoolean(true))) (NativeEffectSet(Nil));
  let aggregate = instruction (NativeValueId(2)) tuple_type
    (NativeCreateAggregate(NativeAggregate {
      kind: NativeTupleAggregate,
      fields: Cons(NativeAggregateValue {
        field: NativeTupleField(first_index), value: NativeValueId(0)
      }, Cons(NativeAggregateValue {
        field: NativeTupleField(1), value: NativeValueId(1)
      }, Nil))
    })) (NativeEffectSet(Cons(NativeAllocate, Nil)));
  fixture_function tuple_type (NativeBlockId(0)) (Cons(block 0 (Nil)
    (Cons(first, Cons(second, Cons(aggregate, Nil))))
    (NativeReturn(NativeValueId(2))), Nil))
}

let valid_tuple_aggregate_function = tuple_aggregate_function 0
let bad_tuple_aggregate_function = tuple_aggregate_function 1

let run_existing_tests = fn () -> both
  (expect_ok (verify_module (fixture_module valid_join_function)))
  (both (expect_error (verify_module (fixture_module bad_undefined_function)))
  (both (expect_error (verify_module (fixture_module bad_duplicate_definition_function)))
  (both (expect_error (verify_module (fixture_module bad_dominance_function)))
  (both (expect_error (verify_module (fixture_module bad_edge_arity_function)))
  (both (expect_error (verify_module (fixture_module bad_edge_type_function)))
  (both (expect_error (verify_module (fixture_module bad_return_type_function)))
  (both (expect_error (verify_module (fixture_module bad_division_effect_function)))
  (both (expect_ok (verify_module (fixture_module valid_division_effect_function)))
    (expect_ok (verify_module (fixture_module valid_trap_function)))))))))))

let bad_constant_result_type_function = fixture_function int_type (NativeBlockId(0))
  (Cons(block 0 (Nil) (Cons(instruction (NativeValueId(0)) int_type
    (NativeConstant(NativeBoolean(true))) (NativeEffectSet(Nil)), Nil))
    (NativeReturn(NativeValueId(0))), Nil))

let bad_branch_condition_function = fixture_function int_type (NativeBlockId(0)) (Cons(
  block 0 (Nil) (Cons(instruction (NativeValueId(0)) int_type
    (NativeConstant(NativeInteger(IntegerLiteral("1")))) (NativeEffectSet(Nil)), Nil))
    (NativeConditionalBranch(NativeBranch {
      condition: NativeValueId(0),
      when_true: NativeEdge { target: NativeBlockId(1), arguments: Nil },
      when_false: NativeEdge { target: NativeBlockId(2), arguments: Nil }
    })),
  Cons(block 1 (Nil) (Nil) (NativeReturn(NativeValueId(0))),
  Cons(block 2 (Nil) (Nil) (NativeReturn(NativeValueId(0))), Nil))))

let run_tests = fn () -> both (run_existing_tests ())
  (both (expect_error (verify_module (fixture_module bad_constant_result_type_function)))
  (both (expect_error (verify_module (fixture_module bad_branch_condition_function)))
  (both (expect_ok (verify_module (fixture_module valid_tuple_aggregate_function)))
    (expect_error (verify_module (fixture_module bad_tuple_aggregate_function))))))

let both = fn first -> fn second -> match first {
  | false -> false
  | true -> second
}

let main = fn () -> match (run_tests ()) {
  | true -> 0
  | false -> 1
}
