// Positive/negative fixtures for ABI-free AArch64 scalar selection.
use Prelude.{List, Nil, Cons, Result, Ok, Err}
use String.{equal}
use Syntax.Ast.{
  ValueName, IntegerLiteral, Addition, Subtraction, Multiplication, Division,
  Remainder, Equal, NotEqual, LessThan, LessThanOrEqual, GreaterThan,
  GreaterThanOrEqual
}
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
  NativePrimitiveNegate, NativeNegatePrimitive, NativeBinaryPrimitive,
  NativeCallOperation, NativeCall, NativeDirectCall, NativeTestValue,
  NativeValueTest, NativeLiteralTest, NativePatternMatchTest,
  NativeLiteralPatternTest,
  NativeHostCall, NativeUnknownCall, NativeHostRead, NativeEffectSet,
  NativeEffect, NativeMayTrap,
  NativeDivisionByZero, NativeRemainderByZero, NativeExplicitTrap,
  NativeJump, NativeReturn, NativeTrap, NativeUnreachable,
  NativeConditionalBranch, NativeBranch, NativeEdge
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

let binary_function = fn operator -> fn result_type -> fn effects ->
  fn function_name -> NativeFunction {
  identity: NativeTopLevelFunction(BindingIdentity {
    source_path: module_name,
    declaration_span: source_span,
    name: ValueName(function_name),
    scope: ModuleValueBinding
  }),
  result_type: result_type,
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
      result_type: result_type,
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

let no_effects = NativeEffectSet(Nil)
let valid_add_function = binary_function Addition int_type no_effects "sum"
let negation_function = NativeFunction {
  identity: NativeTopLevelFunction(BindingIdentity {
    source_path: module_name,
    declaration_span: source_span,
    name: ValueName("negate"),
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
      effects: no_effects,
      span: source_span
    }, Cons(NativeInstruction {
      result: NativeValueId(1),
      result_type: int_type,
      operation: NativePrimitiveOperation(NativePrimitiveNegate(NativeNegatePrimitive {
        operand_type: int_type,
        operand: NativeValueId(0)
      })),
      effects: no_effects,
      span: source_span
    }, Nil)),
    terminator: NativeReturn(NativeValueId(1)),
    span: source_span
  }, Nil),
  span: source_span
}
let jump_function = NativeFunction {
  identity: NativeTopLevelFunction(BindingIdentity {
    source_path: module_name,
    declaration_span: source_span,
    name: ValueName("forward"),
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
      effects: no_effects,
      span: source_span
    }, Nil),
    terminator: NativeJump(NativeEdge {
      target: NativeBlockId(1),
      arguments: Cons(NativeValueId(0), Nil)
    }),
    span: source_span
  }, Cons(NativeBlock {
    identity: NativeBlockId(1),
    parameters: Cons(NativeBlockParameter {
      value: NativeValueId(1),
      checked_type: int_type,
      span: source_span
    }, Nil),
    instructions: Nil,
    terminator: NativeReturn(NativeValueId(1)),
    span: source_span
  }, Nil)),
  span: source_span
}

let unreachable_function = NativeFunction {
  identity: NativeTopLevelFunction(BindingIdentity {
    source_path: module_name,
    declaration_span: source_span,
    name: ValueName("never_returns"),
    scope: ModuleValueBinding
  }),
  result_type: int_type,
  entry: NativeBlockId(0),
  blocks: Cons(NativeBlock {
    identity: NativeBlockId(0),
    parameters: Nil,
    instructions: Nil,
    terminator: NativeUnreachable,
    span: source_span
  }, Nil),
  span: source_span
}

let explicit_trap_function = NativeFunction {
  identity: NativeTopLevelFunction(BindingIdentity {
    source_path: module_name,
    declaration_span: source_span,
    name: ValueName("explicitly_trap"),
    scope: ModuleValueBinding
  }),
  result_type: int_type,
  entry: NativeBlockId(0),
  blocks: Cons(NativeBlock {
    identity: NativeBlockId(0),
    parameters: Nil,
    instructions: Nil,
    terminator: NativeTrap(NativeExplicitTrap),
    span: source_span
  }, Nil),
  span: source_span
}
let valid_subtract_function = binary_function Subtraction int_type
  no_effects "difference"
let valid_multiply_function = binary_function Multiplication int_type
  no_effects "product"
let valid_equal_function = binary_function Equal bool_type no_effects "integer_equal"
let valid_not_equal_function = binary_function NotEqual bool_type
  no_effects "integer_not_equal"
let valid_less_than_function = binary_function LessThan bool_type
  no_effects "integer_less_than"
let valid_less_equal_function = binary_function LessThanOrEqual bool_type
  no_effects "integer_less_equal"
let valid_greater_than_function = binary_function GreaterThan bool_type
  no_effects "integer_greater_than"
let valid_greater_equal_function = binary_function GreaterThanOrEqual bool_type
  no_effects "integer_greater_equal"
let scalar_operations_module = fixture_module_with_functions
  (Cons(valid_add_function, Cons(valid_subtract_function,
  Cons(valid_multiply_function, Cons(valid_equal_function,
  Cons(valid_not_equal_function, Cons(valid_less_than_function,
  Cons(valid_less_equal_function, Cons(valid_greater_than_function,
  Cons(valid_greater_equal_function, Cons(negation_function, Nil)))))))))))
let integer_literal_pattern_function = NativeFunction {
  identity: NativeTopLevelFunction(BindingIdentity {
    source_path: module_name,
    declaration_span: source_span,
    name: ValueName("is_ten"),
    scope: ModuleValueBinding
  }),
  result_type: bool_type,
  entry: NativeBlockId(0),
  blocks: Cons(NativeBlock {
    identity: NativeBlockId(0),
    parameters: Nil,
    instructions: Cons(NativeInstruction {
      result: NativeValueId(0),
      result_type: int_type,
      operation: NativeConstant(NativeInteger(IntegerLiteral("10"))),
      effects: no_effects,
      span: source_span
    }, Cons(NativeInstruction {
      result: NativeValueId(1),
      result_type: bool_type,
      operation: NativeTestValue(NativeValueTest {
        value: NativeValueId(0),
        test: NativeLiteralTest(NativeInteger(IntegerLiteral("10")))
      }),
      effects: no_effects,
      span: source_span
    }, Nil)),
    terminator: NativeReturn(NativeValueId(1)),
    span: source_span
  }, Nil),
  span: source_span
}

let boolean_literal_pattern_function = NativeFunction {
  identity: NativeTopLevelFunction(BindingIdentity {
    source_path: module_name,
    declaration_span: source_span,
    name: ValueName("is_true"),
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
      effects: no_effects,
      span: source_span
    }, Cons(NativeInstruction {
      result: NativeValueId(1),
      result_type: bool_type,
      operation: NativeTestValue(NativeValueTest {
        value: NativeValueId(0),
        test: NativePatternMatchTest(NativeLiteralPatternTest(NativeBoolean(true)))
      }),
      effects: no_effects,
      span: source_span
    }, Nil)),
    terminator: NativeReturn(NativeValueId(1)),
    span: source_span
  }, Nil),
  span: source_span
}

let literal_pattern_module = fixture_module_with_functions
  (Cons(integer_literal_pattern_function, Cons(boolean_literal_pattern_function, Nil)))

let effectful_add_function = binary_function Addition int_type
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

let division_function = binary_function Division int_type
  (NativeEffectSet(Cons(NativeMayTrap(NativeDivisionByZero), Nil))) "quotient"
let remainder_function = binary_function Remainder int_type
  (NativeEffectSet(Cons(NativeMayTrap(NativeRemainderByZero), Nil))) "remainder"

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

let selection_prints_negation = fn function -> match (select_function function Nil) {
  | Err(_) -> false
  | Ok(selected) -> equal (print_function selected)
      ("function fn aarch64-machine-ir-test::negate@0 -> i32 entry .Lbb0 {\n"
        + ".Lbb0():\n"
        + "    %v0:i32 = load_i32 10\n"
        + "    %v1:i32 = neg_w %v0\n"
        + "    return %v1\n}\n")
}

let selection_prints_jump_edge = fn function -> match (select_function function Nil) {
  | Err(_) -> false
  | Ok(selected) -> equal (print_function selected)
      ("function fn aarch64-machine-ir-test::forward@0 -> i32 entry .Lbb0 {\n"
        + ".Lbb0():\n"
        + "    %v0:i32 = load_i32 10\n"
        + "    jump .Lbb1(%v0)\n"
        + ".Lbb1(%v1:i32):\n"
        + "    return %v1\n}\n")
}

let selection_prints_unreachable = fn function ->
  match (select_function function Nil) {
    | Err(_) -> false
    | Ok(selected) -> equal (print_function selected)
        ("function fn aarch64-machine-ir-test::never_returns@0 -> i32 entry .Lbb0 {\n"
          + ".Lbb0():\n"
          + "    unreachable\n}\n")
  }

let selection_prints_explicit_trap = fn function -> match (select_function function Nil) {
  | Err(_) -> false
  | Ok(selected) -> equal (print_function selected)
      ("function fn aarch64-machine-ir-test::explicitly_trap@0 -> i32 entry .Lbb0 {\n"
        + ".Lbb0():\n"
        + "    trap explicit_trap\n}\n")
}

let selection_prints_binary_operation = fn function -> fn function_name ->
  fn result_type -> fn operation -> match (select_function function Nil) {
    | Err(_) -> false
    | Ok(selected) -> equal (print_function selected)
        ("function fn " + module_name + "::" + function_name + "@0 -> "
          + result_type + " entry .Lbb0 {\n"
          + ".Lbb0():\n"
          + "    %v0:i32 = load_i32 10\n"
          + "    %v1:i32 = load_i32 32\n"
          + "    %v2:" + result_type + " = " + operation + " %v0, %v1\n"
          + "    return %v2\n}\n")
  }

let selection_prints_integer_literal_pattern = fn function ->
  match (select_function function Nil) {
    | Err(_) -> false
    | Ok(selected) -> equal (print_function selected)
        ("function fn aarch64-machine-ir-test::is_ten@0 -> bool entry .Lbb0 {\n"
          + ".Lbb0():\n"
          + "    %v0:i32 = load_i32 10\n"
          + "    %v1:bool = cmp_set.eq_w %v0, #10\n"
          + "    return %v1\n}\n")
  }

let selection_prints_boolean_literal_pattern = fn function ->
  match (select_function function Nil) {
    | Err(_) -> false
    | Ok(selected) -> equal (print_function selected)
        ("function fn aarch64-machine-ir-test::is_true@0 -> bool entry .Lbb0 {\n"
          + ".Lbb0():\n"
          + "    %v0:bool = load_bool true\n"
          + "    %v1:bool = cmp_set.eq_bool %v0, true\n"
          + "    return %v1\n}\n")
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

let selection_rejects_remainder_without_trap_lowering = fn function ->
  match (select_function function Nil) {
    | Ok(_) -> false
    | Err(A64SelectionFailure { reason, function: _, block: _, span: _ }) ->
      equal reason "remainder selection awaits explicit remainder-by-zero trap lowering"
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

let run_tests = fn () -> all_pass (
  Cons(module_verifies (fixture_module explicit_trap_function),
  Cons(module_verifies (fixture_module unreachable_function),
  Cons(module_verifies (fixture_module jump_function),
  Cons(selection_prints_jump_edge jump_function,
  Cons(selection_prints_negation negation_function,
  Cons(selection_prints_explicit_trap explicit_trap_function,
  Cons(selection_prints_unreachable unreachable_function,
  Cons(module_verifies scalar_operations_module,
  Cons(module_verifies literal_pattern_module,
  Cons(module_verifies (fixture_module effectful_add_function),
  Cons(module_verifies (fixture_module boolean_equality_function),
  Cons(module_verifies (fixture_module boolean_inequality_function),
  Cons(module_verifies (fixture_module branch_function),
  Cons(module_verifies direct_call_module,
  Cons(module_verifies (fixture_module host_call_function),
  Cons(module_verifies (fixture_module division_function),
  Cons(module_verifies (fixture_module remainder_function),
  Cons(selection_prints_binary_operation valid_add_function "sum" "i32" "add_w",
  Cons(selection_prints_binary_operation valid_subtract_function "difference" "i32" "sub_w",
  Cons(selection_prints_binary_operation valid_multiply_function "product" "i32" "mul_w",
  Cons(selection_prints_binary_operation valid_equal_function "integer_equal" "bool" "cmp_set.eq_w",
  Cons(selection_prints_binary_operation valid_not_equal_function "integer_not_equal" "bool" "cmp_set.ne_w",
  Cons(selection_prints_binary_operation valid_less_than_function "integer_less_than" "bool" "cmp_set.slt_w",
  Cons(selection_prints_binary_operation valid_less_equal_function "integer_less_equal" "bool" "cmp_set.sle_w",
  Cons(selection_prints_binary_operation valid_greater_than_function "integer_greater_than" "bool" "cmp_set.sgt_w",
  Cons(selection_prints_binary_operation valid_greater_equal_function "integer_greater_equal" "bool" "cmp_set.sge_w",
  Cons(selection_prints_integer_literal_pattern integer_literal_pattern_function,
  Cons(selection_prints_boolean_literal_pattern boolean_literal_pattern_function,
  Cons(selection_rejects_unmodeled_effect effectful_add_function,
  Cons(selection_prints_boolean_equality boolean_equality_function,
  Cons(selection_prints_boolean_inequality boolean_inequality_function,
  Cons(selection_preserves_direct_call_and_effect direct_call_function,
  Cons(selection_preserves_host_call_and_effect host_call_function,
  Cons(selection_preserves_branch_labels_and_edge_arguments branch_function,
  Cons(selection_rejects_division_without_trap_lowering division_function,
  Cons(selection_rejects_remainder_without_trap_lowering remainder_function,
  Nil)))))))))))))))))))))))))))))))))))))

let main = fn () -> match (run_tests ()) {
  | true -> 0
  | false -> 1
}
