// Positive/negative fixtures for ABI-free AArch64 scalar selection.
use Prelude.{List, Nil, Cons, Result, Ok, Err}
use String.{equal}
use Syntax.Ast.{ValueName, IntegerLiteral, Addition, Division, Equal, NotEqual}
use Syntax.Lexer.{SourceSpan}
use Check.Inference.{
  InferenceType, AppliedType, InferredNamedType, BindingIdentity,
  ModuleValueBinding
}
use NativeIr.Ir.{
  NativeModule, NativeTopLevelFunction, NativeModuleInitializer,
  NativeFunction, NativeBlockId, NativeValueId, NativeBlock, NativeBlockParameter,
  NativeInstruction, NativeConstant, NativeUnit, NativeInteger,
  NativeBoolean, NativePrimitiveOperation, NativePrimitiveBinary,
  NativeBinaryPrimitive, NativeCallOperation, NativeCall, NativeDirectCall,
  NativeHostCall, NativeUnknownCall, NativeHostRead, NativeEffectSet,
  NativeEffect, NativeMayTrap,
  NativeDivisionByZero, NativeReturn, NativeConditionalBranch,
  NativeBranch, NativeEdge
}
use NativeIr.Verify.{verify_module}
use Backend.Native.Aarch64.MachineIr.{A64SelectionFailure}
use Backend.Native.Aarch64.SelectMachineIr.{select_function}
use Backend.Native.Aarch64.PrintMachineIr.{print_function}

let module_name = "aarch64-machine-ir-test"
let source_span = SourceSpan { start_codepoint: 0, end_codepoint: 1 }
let unit_type = AppliedType(InferredNamedType { name: "unit", arguments: Nil })
let int_type = AppliedType(InferredNamedType { name: "i32", arguments: Nil })
let bool_type = AppliedType(InferredNamedType { name: "bool", arguments: Nil })
let initializer_identity = NativeModuleInitializer(module_name)
let test_binding = BindingIdentity {
  source_path: module_name,
  declaration_span: source_span,
  name: ValueName("sum"),
  scope: ModuleValueBinding
}

let initializer_function = NativeFunction {
  identity: initializer_identity,
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

let binary_function = fn operator -> fn effects -> fn function_name -> NativeFunction {
  identity: NativeTopLevelFunction(BindingIdentity {
    source_path: module_name,
    declaration_span: source_span,
    name: ValueName(function_name),
    scope: ModuleValueBinding
  }),
  result_type: int_type,
  entry: NativeBlockId(0),
  blocks: Cons(NativeBlock {
    identity: NativeBlockId(0),
    parameters: Nil,
    instructions: Cons(NativeInstruction {
      result: NativeValueId(0),
      result_type: int_type,
      operation: NativeConstant(NativeInteger(IntegerLiteral("10"))),
      effects: NativeEffectSet(Nil),
      span: source_span
    }, Cons(NativeInstruction {
      result: NativeValueId(1),
      result_type: int_type,
      operation: NativeConstant(NativeInteger(IntegerLiteral("32"))),
      effects: NativeEffectSet(Nil),
      span: source_span
    }, Cons(NativeInstruction {
      result: NativeValueId(2),
      result_type: int_type,
      operation: NativePrimitiveOperation(NativePrimitiveBinary(NativeBinaryPrimitive {
        operator: operator,
        operand_type: int_type,
        left: NativeValueId(0),
        right: NativeValueId(1)
      })),
      effects: effects,
      span: source_span
    }, Nil))),
    terminator: NativeReturn(NativeValueId(2)),
    span: source_span
  }, Nil),
  span: source_span
}

let fixture_module_with_functions = fn functions -> NativeModule {
  module_identity: module_name,
  nominal_types: Nil,
  substitutions: Nil,
  globals: Nil,
  initializer: initializer_identity,
  functions: Cons(initializer_function, functions)
}

let fixture_module = fn function ->
  fixture_module_with_functions (Cons(function, Nil))

let valid_add_function = binary_function (Addition) (NativeEffectSet(Nil)) "sum"
let effectful_add_function = binary_function (Addition)
  (NativeEffectSet(Cons(NativeHostRead, Nil))) "effectful_sum"

let boolean_comparison_function = fn operator -> fn function_name -> NativeFunction {
  identity: NativeTopLevelFunction(BindingIdentity {
    source_path: module_name,
    declaration_span: source_span,
    name: ValueName(function_name),
    scope: ModuleValueBinding
  }),
  result_type: bool_type,
  entry: NativeBlockId(0),
  blocks: Cons(NativeBlock {
    identity: NativeBlockId(0),
    parameters: Nil,
    instructions: Cons(NativeInstruction {
      result: NativeValueId(0),
      result_type: bool_type,
      operation: NativeConstant(NativeBoolean(true)),
      effects: NativeEffectSet(Nil),
      span: source_span
    }, Cons(NativeInstruction {
      result: NativeValueId(1),
      result_type: bool_type,
      operation: NativeConstant(NativeBoolean(false)),
      effects: NativeEffectSet(Nil),
      span: source_span
    }, Cons(NativeInstruction {
      result: NativeValueId(2),
      result_type: bool_type,
      operation: NativePrimitiveOperation(NativePrimitiveBinary(NativeBinaryPrimitive {
        operator: operator,
        operand_type: bool_type,
        left: NativeValueId(0),
        right: NativeValueId(1)
      })),
      effects: NativeEffectSet(Nil),
      span: source_span
    }, Nil))),
    terminator: NativeReturn(NativeValueId(2)),
    span: source_span
  }, Nil),
  span: source_span
}

let boolean_equality_function = boolean_comparison_function Equal "boolean_equal"
let boolean_inequality_function = boolean_comparison_function NotEqual "boolean_not_equal"

let division_function = binary_function (Division)
  (NativeEffectSet(Cons(NativeMayTrap(NativeDivisionByZero), Nil))) "quotient"

let increment_identity = NativeTopLevelFunction(BindingIdentity {
  source_path: module_name,
  declaration_span: source_span,
  name: ValueName("increment"),
  scope: ModuleValueBinding
})

let increment_function = NativeFunction {
  identity: increment_identity,
  result_type: int_type,
  entry: NativeBlockId(0),
  blocks: Cons(NativeBlock {
    identity: NativeBlockId(0),
    parameters: Cons(NativeBlockParameter {
      value: NativeValueId(0), checked_type: int_type, span: source_span
    }, Nil),
    instructions: Nil,
    terminator: NativeReturn(NativeValueId(0)),
    span: source_span
  }, Nil),
  span: source_span
}

let direct_call_function = NativeFunction {
  identity: NativeTopLevelFunction(BindingIdentity {
    source_path: module_name,
    declaration_span: source_span,
    name: ValueName("call_increment"),
    scope: ModuleValueBinding
  }),
  result_type: int_type,
  entry: NativeBlockId(0),
  blocks: Cons(NativeBlock {
    identity: NativeBlockId(0),
    parameters: Nil,
    instructions: Cons(NativeInstruction {
      result: NativeValueId(0),
      result_type: int_type,
      operation: NativeConstant(NativeInteger(IntegerLiteral("10"))),
      effects: NativeEffectSet(Nil),
      span: source_span
    }, Cons(NativeInstruction {
      result: NativeValueId(1),
      result_type: int_type,
      operation: NativeCallOperation(NativeCall {
        target: NativeDirectCall(increment_identity),
        arguments: Cons(NativeValueId(0), Nil),
        argument_types: Cons(int_type, Nil),
        result_type: int_type
      }),
      effects: NativeEffectSet(Cons(NativeUnknownCall, Nil)),
      span: source_span
    }, Nil)),
    terminator: NativeReturn(NativeValueId(1)),
    span: source_span
  }, Nil),
  span: source_span
}

let direct_call_module = fixture_module_with_functions
  (Cons(increment_function, Cons(direct_call_function, Nil)))

let host_binding = BindingIdentity {
  source_path: "host-lib",
  declaration_span: source_span,
  name: ValueName("read_i32"),
  scope: ModuleValueBinding
}

let host_call_function = NativeFunction {
  identity: NativeTopLevelFunction(BindingIdentity {
    source_path: module_name,
    declaration_span: source_span,
    name: ValueName("call_host"),
    scope: ModuleValueBinding
  }),
  result_type: int_type,
  entry: NativeBlockId(0),
  blocks: Cons(NativeBlock {
    identity: NativeBlockId(0),
    parameters: Nil,
    instructions: Cons(NativeInstruction {
      result: NativeValueId(0),
      result_type: int_type,
      operation: NativeConstant(NativeInteger(IntegerLiteral("10"))),
      effects: NativeEffectSet(Nil),
      span: source_span
    }, Cons(NativeInstruction {
      result: NativeValueId(1),
      result_type: int_type,
      operation: NativeCallOperation(NativeCall {
        target: NativeHostCall(host_binding),
        arguments: Cons(NativeValueId(0), Nil),
        argument_types: Cons(int_type, Nil),
        result_type: int_type
      }),
      effects: NativeEffectSet(Cons(NativeHostRead, Nil)),
      span: source_span
    }, Nil)),
    terminator: NativeReturn(NativeValueId(1)),
    span: source_span
  }, Nil),
  span: source_span
}

let branch_function = NativeFunction {
  identity: NativeTopLevelFunction(BindingIdentity {
    source_path: module_name,
    declaration_span: source_span,
    name: ValueName("choose"),
    scope: ModuleValueBinding
  }),
  result_type: int_type,
  entry: NativeBlockId(0),
  blocks: Cons(NativeBlock {
    identity: NativeBlockId(0),
    parameters: Nil,
    instructions: Cons(NativeInstruction {
      result: NativeValueId(0),
      result_type: AppliedType(InferredNamedType { name: "bool", arguments: Nil }),
      operation: NativeConstant(NativeBoolean(true)),
      effects: NativeEffectSet(Nil),
      span: source_span
    }, Cons(NativeInstruction {
      result: NativeValueId(1),
      result_type: int_type,
      operation: NativeConstant(NativeInteger(IntegerLiteral("10"))),
      effects: NativeEffectSet(Nil),
      span: source_span
    }, Nil)),
    terminator: NativeConditionalBranch(NativeBranch {
      condition: NativeValueId(0),
      when_true: NativeEdge {
        target: NativeBlockId(1), arguments: Cons(NativeValueId(1), Nil)
      },
      when_false: NativeEdge {
        target: NativeBlockId(2), arguments: Cons(NativeValueId(1), Nil)
      }
    }),
    span: source_span
  }, Cons(NativeBlock {
    identity: NativeBlockId(1),
    parameters: Cons(NativeBlockParameter {
      value: NativeValueId(2), checked_type: int_type, span: source_span
    }, Nil),
    instructions: Nil,
    terminator: NativeReturn(NativeValueId(2)),
    span: source_span
  }, Cons(NativeBlock {
    identity: NativeBlockId(2),
    parameters: Cons(NativeBlockParameter {
      value: NativeValueId(3), checked_type: int_type, span: source_span
    }, Nil),
    instructions: Nil,
    terminator: NativeReturn(NativeValueId(3)),
    span: source_span
  }, Nil))),
  span: source_span
}

let selection_prints_expected_ir = fn function -> match (select_function function Nil) {
  | Err(_) -> false
  | Ok(selected) -> equal (print_function selected)
      ("function fn aarch64-machine-ir-test::sum@0 -> i32 entry .Lbb0 {\n"
        + ".Lbb0():\n"
        + "    %v0:i32 = load_i32 10\n"
        + "    %v1:i32 = load_i32 32\n"
        + "    %v2:i32 = add_w %v0, %v1\n"
        + "    return %v2\n}\n")
}

let selection_prints_boolean_equality = fn function -> match (select_function function Nil) {
  | Err(_) -> false
  | Ok(selected) -> equal (print_function selected)
      ("function fn aarch64-machine-ir-test::boolean_equal@0 -> bool entry .Lbb0 {\n"
        + ".Lbb0():\n"
        + "    %v0:bool = load_bool true\n"
        + "    %v1:bool = load_bool false\n"
        + "    %v2:bool = cmp_set.eq_bool %v0, %v1\n"
        + "    return %v2\n}\n")
}

let selection_prints_boolean_inequality = fn function -> match (select_function function Nil) {
  | Err(_) -> false
  | Ok(selected) -> equal (print_function selected)
      ("function fn aarch64-machine-ir-test::boolean_not_equal@0 -> bool entry .Lbb0 {\n"
        + ".Lbb0():\n"
        + "    %v0:bool = load_bool true\n"
        + "    %v1:bool = load_bool false\n"
        + "    %v2:bool = cmp_set.ne_bool %v0, %v1\n"
        + "    return %v2\n}\n")
}

let selection_preserves_direct_call_and_effect = fn function ->
  match (select_function function Nil) {
    | Err(_) -> false
    | Ok(selected) -> equal (print_function selected)
        ("function fn aarch64-machine-ir-test::call_increment@0 -> i32 entry .Lbb0 {\n"
          + ".Lbb0():\n"
          + "    %v0:i32 = load_i32 10\n"
          + "    %v1:i32 = call fn aarch64-machine-ir-test::increment@0(%v0:i32) -> i32 !{unknown_call}\n"
          + "    return %v1\n}\n")
  }

let selection_preserves_host_call_and_effect = fn function ->
  match (select_function function Nil) {
    | Err(_) -> false
    | Ok(selected) -> equal (print_function selected)
        ("function fn aarch64-machine-ir-test::call_host@0 -> i32 entry .Lbb0 {\n"
          + ".Lbb0():\n"
          + "    %v0:i32 = load_i32 10\n"
          + "    %v1:i32 = call host host-lib::read_i32@0(%v0:i32) -> i32 !{host_read}\n"
          + "    return %v1\n}\n")
  }

let selection_preserves_branch_labels_and_edge_arguments = fn function ->
  match (select_function function Nil) {
    | Err(_) -> false
    | Ok(selected) -> equal (print_function selected)
        ("function fn aarch64-machine-ir-test::choose@0 -> i32 entry .Lbb0 {\n"
          + ".Lbb0():\n"
          + "    %v0:bool = load_bool true\n"
          + "    %v1:i32 = load_i32 10\n"
          + "    branch %v0, .Lbb1(%v1), .Lbb2(%v1)\n"
          + ".Lbb1(%v2:i32):\n"
          + "    return %v2\n"
          + ".Lbb2(%v3:i32):\n"
          + "    return %v3\n}\n")
  }

let selection_rejects_division_without_trap_lowering = fn function ->
  match (select_function function Nil) {
    | Ok(_) -> false
    | Err(A64SelectionFailure { reason, function: _, block: _, span: _ }) ->
      equal reason "division selection awaits explicit divide-by-zero trap lowering"
  }

let selection_rejects_unmodeled_effect = fn function ->
  match (select_function function Nil) {
    | Ok(_) -> false
    | Err(A64SelectionFailure { reason, function: _, block: _, span: _ }) ->
      equal reason "selection would discard an explicit NativeIR effect"
  }

let module_verifies = fn native_module -> match (verify_module native_module) {
  | Err(_) -> false
  | Ok(_) -> true
}

let all_pass = fn tests -> match tests {
  | Nil -> true
  | Cons(false, _) -> false
  | Cons(true, remaining) -> all_pass remaining
}

let run_tests = fn () -> all_pass (Cons(
  module_verifies (fixture_module valid_add_function),
  Cons(module_verifies (fixture_module effectful_add_function),
  Cons(module_verifies (fixture_module boolean_equality_function),
  Cons(module_verifies (fixture_module boolean_inequality_function),
  Cons(module_verifies (fixture_module branch_function),
  Cons(module_verifies direct_call_module,
  Cons(module_verifies (fixture_module host_call_function),
  Cons(module_verifies (fixture_module division_function),
  Cons(selection_prints_expected_ir valid_add_function,
  Cons(selection_rejects_unmodeled_effect effectful_add_function,
  Cons(selection_prints_boolean_equality boolean_equality_function,
  Cons(selection_prints_boolean_inequality boolean_inequality_function,
  Cons(selection_preserves_direct_call_and_effect direct_call_function,
  Cons(selection_preserves_host_call_and_effect host_call_function,
  Cons(selection_preserves_branch_labels_and_edge_arguments branch_function,
  Cons(selection_rejects_division_without_trap_lowering division_function,
    Nil)))))))))))))))))

let main = fn () -> match (run_tests ()) {
  | true -> 0
  | false -> 1
}
