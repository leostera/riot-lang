// Deterministic text form for inspecting target-independent NativeIR.
use Prelude.{List, Nil, Cons, Option, Some, None}
use String.{length, get, slice}
use Syntax.Ast.{
  ValueName, ConstructorName, IntegerLiteral, TextLiteral, Addition, Subtraction,
  Multiplication, Division, Remainder, Equal, NotEqual, LessThan,
  LessThanOrEqual, GreaterThan, GreaterThanOrEqual
}
use Syntax.Lexer.{SourceSpan}
use Check.Inference.{
  InferenceType, FreeVariable, GenericVariable, AppliedType, InferredNamedType,
  FunctionType, InferredFunctionType, InferredTuple, InferredRecord,
  InferredRecordField, InferredVariant, InferredVariantCase, TypeSubstitution,
  TypeVisibility, TypeScheme, BindingIdentity, ModuleValueBinding,
  LocalValueBinding, ConstructorIdentity
}
use NativeIr.Ir.{
  NativeModule, NativeGlobal, NativeFunctionId, NativeTopLevelFunction,
  NativeNestedFunction, NativeNestedFunctionId, NativeModuleInitializer,
  NativeFunction, NativeBlockId, NativeValueId, NativeBlock, NativeBlockParameter,
  NativeInstruction, NativeOperation, NativeConstant, NativeUnit, NativeInteger,
  NativeBoolean, NativeString, NativePrimitive, NativePrimitiveOperation,
  NativePrimitiveBinary, NativePrimitiveNegate, NativeBinaryPrimitive,
  NativeNegatePrimitive, NativeCallOperation, NativeCall, NativeClosureCall,
  NativeDirectCall, NativeHostCall, NativeCreateClosure, NativeClosure,
  NativeCapture, NativeClosureCapture, NativeCreateAggregate, NativeAggregate, NativeAggregateValue, NativeTupleAggregate,
  NativeRecordAggregate, NativeVariantAggregate, NativeProjectAggregate,
  NativeProjection, NativeTupleField, NativeRecordField, NativeConstructorField,
  NativeTestValue, NativeValueTest, NativeTest, NativeLiteralTest,
  NativeConstructorTest, NativeTupleArityTest, NativePatternMatchTest,
  NativePatternTest, NativeAnyPatternTest, NativeLiteralPatternTest,
  NativeNestedConstructorPatternTest, NativeConstructorPatternTest,
  NativeTuplePatternTest, NativeNestedRecordPatternTest, NativeRecordPatternTest,
  NativeRecordPatternFieldTest, NativeLoadCapture, NativeLoadGlobal,
  NativeStoreGlobal, NativeGlobalStore, NativeEffectSet, NativeEffect,
  NativeMayTrap, NativeAllocate, NativeReadMutableState, NativeWriteMutableState,
  NativeHostRead, NativeHostWrite, NativeHostProcess, NativeUnknownCall,
  NativeDivisionByZero, NativeRemainderByZero, NativeArithmeticOverflow,
  NativePatternFailure, NativeExplicitTrap, NativeEdge, NativeJump,
  NativeConditionalBranch, NativeBranch, NativeSwitchTerminator, NativeSwitch,
  NativeSwitchCase, NativeReturn, NativeTrap, NativeUnreachable
}
use String.{equal}

let digits = fn number -> match number {
  | 0 -> "0" | 1 -> "1" | 2 -> "2" | 3 -> "3" | 4 -> "4"
  | 5 -> "5" | 6 -> "6" | 7 -> "7" | 8 -> "8" | _ -> "9"
}

let decimal = fn number -> match (number < 0) {
  | true -> "-" + decimal (0 - number)
  | false -> match (number < 10) {
    | true -> digits number
    | false -> decimal (number / 10) + digits (number % 10)
  }
}

let escaped_character = fn codepoint -> match codepoint {
  | 0 -> "\\0"
  | 8 -> "\\b"
  | 9 -> "\\t"
  | 10 -> "\\n"
  | 12 -> "\\f"
  | 13 -> "\\r"
  | 34 -> "\\\""
  | 92 -> "\\\\"
  | _ -> ""
}

let escape_text = fn text -> fn offset -> match (offset == length text) {
  | true -> ""
  | false -> match (get text offset) {
    | None -> ""
    | Some(codepoint) -> match (equal (escaped_character codepoint) "") {
      | true -> slice text offset (offset + 1) + escape_text text (offset + 1)
      | false -> escaped_character codepoint + escape_text text (offset + 1)
    }
  }
}

let quoted = fn text -> "\"" + escape_text text 0 + "\""
let value_name = fn (ValueName(name)) -> name
let constructor_name = fn (ConstructorName(name)) -> name
let value_id = fn (NativeValueId(number)) -> "%" + decimal number
let block_id = fn (NativeBlockId(number)) -> "bb" + decimal number

let print_binding = fn (BindingIdentity {
  source_path, declaration_span: SourceSpan { start_codepoint, end_codepoint: _ },
  name, scope
}) -> match scope {
  | ModuleValueBinding -> source_path + "::" + value_name name + "@" + decimal start_codepoint
  | LocalValueBinding -> value_name name + "@" + decimal start_codepoint
}

let print_function_id = fn identity -> match identity {
  | NativeTopLevelFunction(binding) -> "fn " + print_binding binding
  | NativeNestedFunction(NativeNestedFunctionId {
      parent, source_span: SourceSpan { start_codepoint, end_codepoint: _ }, ordinal
    }) -> "fn nested(" + print_binding parent + "," + decimal start_codepoint
      + "," + decimal ordinal + ")"
  | NativeModuleInitializer(module_path) -> "fn init(" + module_path + ")"
}

let type_list = fn types -> match types {
  | Nil -> ""
  | Cons(first, remaining) -> match remaining {
    | Nil -> print_type first
    | _ -> print_type first + ", " + type_list remaining
  }
}

let print_type = fn checked_type -> match checked_type {
  | FreeVariable(index) -> "'t" + decimal index
  | GenericVariable(index) -> "'g" + decimal index
  | AppliedType(InferredNamedType { name, arguments }) -> match arguments {
    | Nil -> name
    | _ -> name + "<" + type_list arguments + ">"
  }
  | FunctionType(InferredFunctionType { argument, result }) ->
    "(" + print_type argument + " -> " + print_type result + ")"
  | InferredTuple(items) -> "(" + type_list items + ")"
  | InferredRecord(fields) -> "{" + print_record_types fields + "}"
  | InferredVariant(cases) -> "[" + print_variant_types cases + "]"
}

let print_record_types = fn fields -> match fields {
  | Nil -> ""
  | Cons(InferredRecordField { name, field_type }, remaining) -> match remaining {
    | Nil -> value_name name + ": " + print_type field_type
    | _ -> value_name name + ": " + print_type field_type + ", " + print_record_types remaining
  }
}

let print_variant_types = fn cases -> match cases {
  | Nil -> ""
  | Cons(InferredVariantCase { name, fields }, remaining) -> match remaining {
    | Nil -> print_constructor_case name fields
    | _ -> print_constructor_case name fields + " | " + print_variant_types remaining
  }
}

let print_constructor_case = fn name -> fn fields -> match fields {
  | Nil -> constructor_name name
  | _ -> constructor_name name + "(" + type_list fields + ")"
}

let print_constant = fn constant -> match constant {
  | NativeUnit -> "unit"
  | NativeInteger(IntegerLiteral(text)) -> text
  | NativeBoolean(value) -> match value { | true -> "true" | false -> "false" }
  | NativeString(TextLiteral(text)) -> quoted text
}

let print_effect = fn effect -> match effect {
  | NativeMayTrap(NativeDivisionByZero) -> "may-trap(division-by-zero)"
  | NativeMayTrap(NativeRemainderByZero) -> "may-trap(remainder-by-zero)"
  | NativeMayTrap(NativeArithmeticOverflow) -> "may-trap(arithmetic-overflow)"
  | NativeMayTrap(NativePatternFailure) -> "may-trap(pattern-failure)"
  | NativeMayTrap(NativeExplicitTrap) -> "may-trap(explicit)"
  | NativeAllocate -> "allocate"
  | NativeReadMutableState -> "read-state"
  | NativeWriteMutableState -> "write-state"
  | NativeHostRead -> "host-read"
  | NativeHostWrite -> "host-write"
  | NativeHostProcess -> "host-process"
  | NativeUnknownCall -> "unknown-call"
}

let print_effects = fn (NativeEffectSet(effects)) -> match effects {
  | Nil -> "pure"
  | _ -> "!{" + print_effect_list effects + "}"
}

let print_effect_list = fn effects -> match effects {
  | Nil -> ""
  | Cons(effect, remaining) -> match remaining {
    | Nil -> print_effect effect
    | _ -> print_effect effect + ", " + print_effect_list remaining
  }
}

let print_constructor_identity = fn (ConstructorIdentity {
  type_identity, constructor_name: name
}) -> type_identity + "." + constructor_name name

let print_test = fn test -> match test {
  | NativeLiteralTest(constant) -> "literal " + print_constant constant
  | NativeConstructorTest(entity) -> "constructor " + print_constructor_identity entity
  | NativeTupleArityTest(arity) -> "tuple-arity " + decimal arity
  | NativePatternMatchTest(pattern) -> print_pattern_test pattern
}

let print_pattern_test = fn pattern -> match pattern {
  | NativeAnyPatternTest -> "_"
  | NativeLiteralPatternTest(constant) -> print_constant constant
  | NativeNestedConstructorPatternTest(NativeConstructorPatternTest { entity, arguments }) ->
    print_constructor_identity entity + print_pattern_test_list arguments
  | NativeTuplePatternTest(arguments) -> "(" + print_pattern_test_list arguments + ")"
  | NativeNestedRecordPatternTest(NativeRecordPatternTest { entity, fields }) ->
    print_constructor_identity entity + "{" + print_record_pattern_fields fields + "}"
}

let print_pattern_test_list = fn patterns -> match patterns {
  | Nil -> ""
  | Cons(pattern, remaining) -> match remaining {
    | Nil -> print_pattern_test pattern
    | _ -> print_pattern_test pattern + ", " + print_pattern_test_list remaining
  }
}

let print_record_pattern_fields = fn fields -> match fields {
  | Nil -> ""
  | Cons(NativeRecordPatternFieldTest { name, pattern }, remaining) -> match remaining {
    | Nil -> value_name name + " = " + print_pattern_test pattern
    | _ -> value_name name + " = " + print_pattern_test pattern
      + ", " + print_record_pattern_fields remaining
  }
}

let print_field = fn field -> match field {
  | NativeTupleField(index) -> decimal index
  | NativeRecordField(name) -> value_name name
  | NativeConstructorField(index) -> decimal index
}

let print_target = fn target -> match target {
  | NativeClosureCall(callee) -> "closure " + value_id callee
  | NativeDirectCall(identity) -> "direct " + print_function_id identity
  | NativeHostCall(binding) -> "host " + print_binding binding
}

let print_values = fn values -> match values {
  | Nil -> ""
  | Cons(value, remaining) -> match remaining {
    | Nil -> value_id value
    | _ -> value_id value + ", " + print_values remaining
  }
}

let print_aggregate_fields = fn fields -> match fields {
  | Nil -> ""
  | Cons(NativeAggregateValue { field, value }, remaining) -> match remaining {
    | Nil -> print_field field + "=" + value_id value
    | _ -> print_field field + "=" + value_id value + ", "
      + print_aggregate_fields remaining
  }
}

let print_capture = fn (NativeCapture { binding, checked_type, span: _ }) ->
  print_binding binding + ":" + print_type checked_type

let print_closure_capture = fn (NativeClosureCapture { capture, value }) ->
  print_capture capture + "=" + value_id value

let print_closure_captures = fn captures -> match captures {
  | Nil -> ""
  | Cons(capture, remaining) -> match remaining {
    | Nil -> print_closure_capture capture
    | _ -> print_closure_capture capture + ", " + print_closure_captures remaining
  }
}

let print_function_captures = fn captures -> match captures {
  | Nil -> ""
  | Cons(capture, remaining) -> match remaining {
    | Nil -> print_capture capture
    | _ -> print_capture capture + ", " + print_function_captures remaining
  }
}

let print_operation = fn operation -> match operation {
  | NativeConstant(constant) -> "const " + print_constant constant
  | NativePrimitiveOperation(NativePrimitiveBinary(NativeBinaryPrimitive {
      operator, operand_type, left, right
    })) -> "binary[" + print_type operand_type + "] " + print_operator operator
      + " " + value_id left + ", " + value_id right
  | NativePrimitiveOperation(NativePrimitiveNegate(NativeNegatePrimitive {
      operand_type, operand
    })) -> "negate[" + print_type operand_type + "] " + value_id operand
  | NativeCallOperation(NativeCall { target, arguments, argument_types, result_type }) ->
    "call " + print_target target + "(" + print_typed_values arguments argument_types
      + ") -> " + print_type result_type
  | NativeCreateClosure(NativeClosure { function, captures }) -> "closure "
    + print_function_id function + " captures [" + print_closure_captures captures + "]"
  | NativeCreateAggregate(NativeAggregate { kind, fields }) -> print_aggregate_kind kind
    + " [" + print_aggregate_fields fields + "]"
  | NativeProjectAggregate(NativeProjection { aggregate, field }) -> "project "
    + value_id aggregate + "." + print_field field
  | NativeTestValue(NativeValueTest { value, test }) -> "test " + value_id value
    + " is " + print_test test
  | NativeLoadCapture(index) -> "load-capture " + decimal index
  | NativeLoadGlobal(binding) -> "load-global " + print_binding binding
  | NativeStoreGlobal(NativeGlobalStore { binding, value }) -> "store-global "
    + print_binding binding + " <- " + value_id value
}

let print_operator = fn operator -> match operator {
  | Addition -> "+" | Subtraction -> "-" | Multiplication -> "*"
  | Division -> "/" | Remainder -> "%" | Equal -> "=="
  | NotEqual -> "!=" | LessThan -> "<" | LessThanOrEqual -> "<="
  | GreaterThan -> ">" | GreaterThanOrEqual -> ">="
}

let print_aggregate_kind = fn kind -> match kind {
  | NativeTupleAggregate -> "tuple"
  | NativeRecordAggregate(entity) -> "record " + print_constructor_identity entity
  | NativeVariantAggregate(entity) -> "variant " + print_constructor_identity entity
}

let print_typed_values = fn values -> fn types -> match values {
  | Nil -> ""
  | Cons(value, remaining_values) -> match types {
    | Nil -> value_id value
    | Cons(checked_type, remaining_types) -> match remaining_values {
      | Nil -> value_id value + ": " + print_type checked_type
      | _ -> value_id value + ": " + print_type checked_type + ", "
        + print_typed_values remaining_values remaining_types
    }
  }
}

let print_instruction = fn (NativeInstruction {
  result, result_type, operation, effects, span: SourceSpan { start_codepoint, end_codepoint }
}) -> "    " + value_id result + ": " + print_type result_type + " = "
  + print_operation operation + " " + print_effects effects
  + " @" + decimal start_codepoint + ".." + decimal end_codepoint + "\n"

let print_instructions = fn instructions -> match instructions {
  | Nil -> ""
  | Cons(instruction, remaining) -> print_instruction instruction + print_instructions remaining
}

let print_block_parameters = fn parameters -> match parameters {
  | Nil -> ""
  | Cons(NativeBlockParameter { value, checked_type, span: _ }, remaining) -> match remaining {
    | Nil -> value_id value + ": " + print_type checked_type
    | _ -> value_id value + ": " + print_type checked_type + ", "
      + print_block_parameters remaining
  }
}

let print_edge = fn (NativeEdge { target, arguments }) -> block_id target
  + "(" + print_values arguments + ")"

let print_switch_cases = fn cases -> match cases {
  | Nil -> ""
  | Cons(NativeSwitchCase { test, edge }, remaining) ->
    "  case " + print_test test + " -> " + print_edge edge + "\n"
      + print_switch_cases remaining
}

let print_terminator = fn terminator -> match terminator {
  | NativeJump(edge) -> "    jump " + print_edge edge + "\n"
  | NativeConditionalBranch(NativeBranch { condition, when_true, when_false }) ->
    "    branch " + value_id condition + " ? " + print_edge when_true
      + " : " + print_edge when_false + "\n"
  | NativeSwitchTerminator(NativeSwitch { value, cases, default }) ->
    "    switch " + value_id value + " {\n" + print_switch_cases cases
      + "  default -> " + print_edge default + "\n    }\n"
  | NativeReturn(value) -> "    return " + value_id value + "\n"
  | NativeTrap(kind) -> "    trap " + print_trap_kind kind + "\n"
  | NativeUnreachable -> "    unreachable\n"
}

let print_trap_kind = fn kind -> match kind {
  | NativeDivisionByZero -> "division-by-zero"
  | NativeRemainderByZero -> "remainder-by-zero"
  | NativeArithmeticOverflow -> "arithmetic-overflow"
  | NativePatternFailure -> "pattern-failure"
  | NativeExplicitTrap -> "explicit"
}

let print_block = fn (NativeBlock {
  identity, parameters, instructions, terminator, span: SourceSpan { start_codepoint, end_codepoint }
}) -> "  " + block_id identity + "(" + print_block_parameters parameters + ")"
  + " @" + decimal start_codepoint + ".." + decimal end_codepoint + " {\n"
  + print_instructions instructions + print_terminator terminator + "  }\n"

let print_blocks = fn blocks -> match blocks {
  | Nil -> ""
  | Cons(block, remaining) -> print_block block + print_blocks remaining
}

let print_function = fn (NativeFunction {
  identity, captures, result_type, entry, blocks, span: SourceSpan { start_codepoint, end_codepoint }
}) -> print_function_id identity + " captures [" + print_function_captures captures + "] -> "
  + print_type result_type + " entry " + block_id entry + " @" + decimal start_codepoint + ".."
  + decimal end_codepoint + " {\n" + print_blocks blocks + "}\n"

let print_functions = fn functions -> match functions {
  | Nil -> ""
  | Cons(function, remaining) -> print_function function + "\n" + print_functions remaining
}

let print_global = fn (NativeGlobal {
  binding, scheme: TypeScheme { quantified_variables: _, body },
  span: SourceSpan { start_codepoint, end_codepoint: _ }
}) -> "  global " + print_binding binding + " : " + print_type body
  + " @" + decimal start_codepoint + "\n"

let print_globals = fn globals -> match globals {
  | Nil -> ""
  | Cons(global, remaining) -> print_global global + print_globals remaining
}

let print_native_module = fn (NativeModule {
  module_identity, nominal_types: _, substitutions: _, globals,
  initializer, functions
}) -> "module " + module_identity + " {\n"
  + print_globals globals + "  initializer " + print_function_id initializer + "\n\n"
  + print_functions functions + "}\n"
