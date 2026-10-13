// Lower typed LambdaIR into target-independent SSA blocks and values.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use String.{equal}
use Syntax.Ast.{
  Addition, Division, Remainder, ValueName
}
use Syntax.Lexer.{SourceSpan}
use Check.Inference.{
  InferenceType, FreeVariable, GenericVariable, AppliedType,
  InferredNamedType, FunctionType, InferredFunctionType, InferredTuple,
  InferredRecord, InferredVariant, InferredRecordField, InferredVariantCase,
  TypeSubstitution, TypeVisibility, TypeScheme, apply_substitutions,
  BindingIdentity, BindingScope, ModuleValueBinding, LocalValueBinding,
  ConstructorIdentity
}
use Lambda.Ir.{
  LambdaModule, LambdaValueDeclaration, LambdaExpression, LambdaExpressionKind,
  LambdaConstant, LambdaUnit, LambdaInteger, LambdaBoolean, LambdaString,
  LambdaConstantExpr, LambdaValueReference, LambdaConstruct,
  LambdaConstructorExpression, LambdaFunction, LambdaFunctionExpression,
  LambdaApply, LambdaApplyExpression, LambdaLet, LambdaLetExpression,
  LambdaBinaryPrimitive, LambdaBinaryPrimitiveExpression,
  LambdaNegatePrimitive, LambdaNegatePrimitiveExpression, LambdaTuple,
  LambdaRecord, LambdaRecordExpression, LambdaRecordFieldValue, LambdaMatch,
  LambdaMatchExpression, LambdaMatchArm, LambdaBlock, LambdaPattern,
  LambdaPatternKind, LambdaBind, LambdaWildcard, LambdaConstantPattern,
  LambdaConstructPattern, LambdaConstructorPattern, LambdaTuplePattern,
  LambdaRecordPatternNode, LambdaRecordPattern, LambdaRecordPatternField
}
use NativeIr.Ir.{
  NativeModule, NativeGlobal, NativeFunctionId, NativeTopLevelFunction,
  NativeNestedFunction, NativeNestedFunctionId, NativeModuleInitializer,
  NativeBlockId, NativeValueId, NativeFunction, NativeCapture,
  NativeClosureCapture, NativeBlock, NativeBlockParameter, NativeInstruction, NativeOperation,
  NativeConstant, NativeUnit, NativeInteger, NativeBoolean, NativeString,
  NativePrimitive, NativePrimitiveOperation, NativePrimitiveBinary,
  NativePrimitiveNegate, NativeBinaryPrimitive, NativeNegatePrimitive,
  NativeCallOperation,
  NativeCall, NativeDirectCall, NativeClosureCall, NativeHostCall,
  NativeCallTarget, NativeCreateClosure,
  NativeClosure, NativeCreateAggregate, NativeAggregate, NativeAggregateValue,
  NativeAggregateKind, NativeTupleAggregate, NativeRecordAggregate, NativeVariantAggregate,
  NativeProjectAggregate, NativeProjection, NativeAggregateField,
  NativeTupleField, NativeRecordField, NativeConstructorField,
  NativeTestValue, NativeValueTest, NativeTest, NativeLiteralTest,
  NativeConstructorTest, NativeTupleArityTest, NativePatternMatchTest,
  NativePatternTest, NativeAnyPatternTest, NativeLiteralPatternTest,
  NativeNestedConstructorPatternTest, NativeConstructorPatternTest,
  NativeTuplePatternTest, NativeNestedRecordPatternTest, NativeRecordPatternTest,
  NativeRecordPatternFieldTest,
  NativeLoadCapture, NativeLoadGlobal, NativeStoreGlobal, NativeGlobalStore,
  NativeEffectSet, NativeEffect, NativeMayTrap, NativeAllocate,
  NativeReadMutableState, NativeWriteMutableState, NativeHostRead,
  NativeHostWrite, NativeHostProcess, NativeUnknownCall,
  NativeDivisionByZero, NativeRemainderByZero, NativePatternFailure,
  NativeEdge, NativeJump, NativeConditionalBranch, NativeBranch,
  NativeReturn, NativeTrap, NativeUnreachable, NativeTerminator
}

type NativeBindingValue = NativeBindingValue {
  binding: BindingIdentity,
  value: NativeValueId,
  checked_type: InferenceType
}

type NativeBlockBuilder = NativeBlockBuilder {
  identity: NativeBlockId,
  parameters: List<NativeBlockParameter>,
  reversed_instructions: List<NativeInstruction>,
  terminator: Option<NativeTerminator>,
  span: SourceSpan
}

type NativeFunctionBuilder = NativeFunctionBuilder {
  identity: NativeFunctionId,
  owner: Option<BindingIdentity>,
  direct_functions: List<BindingIdentity>,
  substitutions: List<TypeSubstitution>,
  result_type: InferenceType,
  entry: NativeBlockId,
  current: NativeBlockId,
  blocks: List<NativeBlockBuilder>,
  next_block: i32,
  next_value: i32,
  environment: List<NativeBindingValue>,
  generated_functions: List<NativeFunction>,
  span: SourceSpan
}

type NativeLoweredValue = NativeLoweredValue {
  value: NativeValueId,
  checked_type: InferenceType,
  builder: NativeFunctionBuilder
}

type NativeFunctionParts = NativeFunctionParts {
  argument: InferenceType,
  result: InferenceType
}

let same_span = fn first -> fn second -> {
  let SourceSpan { start_codepoint: first_start, end_codepoint: first_end } = first;
  let SourceSpan { start_codepoint: second_start, end_codepoint: second_end } = second;
  match (first_start == second_start) {
    | false -> false
    | true -> first_end == second_end
  }
}

let same_binding = fn first -> fn second -> {
  let BindingIdentity {
    source_path, declaration_span, name: ValueName(name), scope
  } = first;
  let BindingIdentity {
    source_path: other_path, declaration_span: other_span,
    name: ValueName(other_name), scope: other_scope
  } = second;
  match (equal source_path other_path) {
    | false -> false
    | true -> match (equal name other_name) {
      | false -> false
      | true -> match (same_span declaration_span other_span) {
        | false -> false
        | true -> match scope {
          | ModuleValueBinding -> match other_scope {
            | ModuleValueBinding -> true | LocalValueBinding -> false
          }
          | LocalValueBinding -> match other_scope {
            | ModuleValueBinding -> false | LocalValueBinding -> true
          }
        }
      }
    }
  }
}

// Host bindings are recognized by resolved module identity and name; aliases
// still retain closure semantics rather than inheriting an intrinsic mapping.
let native_host_effect_for_binding = fn (binding: BindingIdentity) ->
  match binding {
  | BindingIdentity {
      source_path, declaration_span: _, name: ValueName(name), scope
    } -> match scope {
    | LocalValueBinding -> None
    | ModuleValueBinding -> match (equal source_path "tinyml/lib/host.ml") {
      | false -> None
      | true -> match name {
        | "args" -> Some(NativeHostRead)
        | "current_directory" -> Some(NativeHostRead)
        | "runtime_directory" -> Some(NativeHostRead)
        | "read_file" -> Some(NativeHostRead)
        | "write_file" -> Some(NativeHostWrite)
        | "write_stdout" -> Some(NativeHostWrite)
        | "write_stderr" -> Some(NativeHostWrite)
        | "run_process" -> Some(NativeHostProcess)
        | _ -> None
      }
    }
  }
  | _ -> None
}

let value_id_number = fn (NativeValueId(number)) -> number
let block_id_number = fn (NativeBlockId(number)) -> number

let same_value_id = fn first -> fn second ->
  value_id_number first == value_id_number second

let same_block_id = fn first -> fn second ->
  block_id_number first == block_id_number second

let function_parts = fn checked_type -> fn substitutions ->
  match (apply_substitutions checked_type substitutions) {
    | FunctionType(InferredFunctionType { argument, result }) ->
      Ok(NativeFunctionParts { argument: argument, result: result })
    | _ -> Err("NativeIR lowering: function expression has a non-function checked type")
  }

let native_unit_type = AppliedType(InferredNamedType {
  name: "unit",
  arguments: Nil
})

let native_bool_type = AppliedType(InferredNamedType {
  name: "bool",
  arguments: Nil
})

let new_function_builder = fn identity -> fn owner -> fn direct_functions ->
  fn substitutions -> fn result_type -> fn span -> {
  let entry = NativeBlockId(0);
  let entry_block = NativeBlockBuilder {
    identity: entry,
    parameters: Nil,
    reversed_instructions: Nil,
    terminator: None,
    span: span
  };
  NativeFunctionBuilder {
    identity: identity,
    owner: owner,
    direct_functions: direct_functions,
    substitutions: substitutions,
    result_type: result_type,
    entry: entry,
    current: entry,
    blocks: Cons(entry_block, Nil),
    next_block: 1,
    next_value: 0,
    environment: Nil,
    generated_functions: Nil,
    span: span
  }
}

let block_builder_identity = fn (NativeBlockBuilder {
  identity, parameters: _, reversed_instructions: _, terminator: _, span: _
}) -> identity

let find_block_builder = fn wanted -> fn blocks -> match blocks {
  | Nil -> None
  | Cons(block, remaining) -> match (same_block_id wanted
      (block_builder_identity block)) {
    | true -> Some(block)
    | false -> find_block_builder wanted remaining
  }
}

let replace_block_builder = fn wanted -> fn replacement -> fn blocks -> match blocks {
  | Nil -> Nil
  | Cons(block, remaining) -> match (same_block_id wanted
      (block_builder_identity block)) {
    | true -> Cons(replacement, remaining)
    | false -> Cons(block, replace_block_builder wanted replacement remaining)
  }
}

let update_current_block = fn replacement -> fn (NativeFunctionBuilder {
  identity, owner, direct_functions, substitutions, result_type, entry, current, blocks, next_block,
  next_value, environment, generated_functions, span
}) -> NativeFunctionBuilder {
  identity: identity,
  owner: owner,
  direct_functions: direct_functions,
  substitutions: substitutions,
  result_type: result_type,
  entry: entry,
  current: current,
  blocks: replace_block_builder current replacement blocks,
  next_block: next_block,
  next_value: next_value,
  environment: environment,
  generated_functions: generated_functions,
  span: span
}

let fresh_value_id = fn (NativeFunctionBuilder {
  identity, owner, direct_functions: _, substitutions: _, result_type, entry, current, blocks, next_block, next_value,
  environment, generated_functions, span
}) -> NativeValueId(next_value)

let allocate_value_id = fn (NativeFunctionBuilder {
  identity, owner, direct_functions, substitutions, result_type, entry, current, blocks, next_block, next_value,
  environment, generated_functions, span
}) -> NativeFunctionBuilder {
  identity: identity,
  owner: owner,
  direct_functions: direct_functions,
  substitutions: substitutions,
  result_type: result_type,
  entry: entry,
  current: current,
  blocks: blocks,
  next_block: next_block,
  next_value: next_value + 1,
  environment: environment,
  generated_functions: generated_functions,
  span: span
}

let function_builder_current = fn (NativeFunctionBuilder {
  current, identity: _, owner: _, direct_functions: _, substitutions: _, result_type: _, entry: _, blocks: _,
  next_block: _, next_value: _, environment: _, generated_functions: _, span: _
}) -> current

let function_builder_blocks = fn (NativeFunctionBuilder {
  blocks, identity: _, owner: _, direct_functions: _, substitutions: _, result_type: _, entry: _, current: _,
  next_block: _, next_value: _, environment: _, generated_functions: _, span: _
}) -> blocks

let append_instruction = fn instruction -> fn builder ->
  match (find_block_builder (function_builder_current builder)
      (function_builder_blocks builder)) {
    | None -> Err("NativeIR lowering: current block is missing")
    | Some(NativeBlockBuilder {
        identity, parameters, reversed_instructions, terminator, span
      }) -> match terminator {
        | Some(_) -> Err("NativeIR lowering: cannot emit after a block terminator")
        | None -> {
          let updated_block = NativeBlockBuilder {
            identity: identity,
            parameters: parameters,
            reversed_instructions: Cons(instruction, reversed_instructions),
            terminator: None,
            span: span
          };
          Ok(update_current_block updated_block builder)
        }
      }
  }

let emit_instruction = fn result_type -> fn operation -> fn effects -> fn span ->
  fn builder -> {
  let result = fresh_value_id builder;
  let instruction = NativeInstruction {
    result: result,
    result_type: result_type,
    operation: operation,
    effects: effects,
    span: span
  };
  match (append_instruction instruction (allocate_value_id builder)) {
    | Err(message) -> Err(message)
    | Ok(next_builder) -> Ok(NativeLoweredValue {
      value: result,
      checked_type: result_type,
      builder: next_builder
    })
  }
}

let set_terminator = fn terminator -> fn builder ->
  match (find_block_builder (function_builder_current builder)
      (function_builder_blocks builder)) {
    | None -> Err("NativeIR lowering: current block is missing")
    | Some(NativeBlockBuilder {
        identity, parameters, reversed_instructions, terminator: existing, span
      }) -> match existing {
        | Some(_) -> Err("NativeIR lowering: block already has a terminator")
        | None -> Ok(update_current_block (NativeBlockBuilder {
          identity: identity,
          parameters: parameters,
          reversed_instructions: reversed_instructions,
          terminator: Some(terminator),
          span: span
        }) builder)
      }
  }

let create_block = fn span -> fn (NativeFunctionBuilder {
  identity: function_identity, owner, direct_functions, substitutions, result_type, entry, current: _, blocks,
  next_block, next_value, environment, generated_functions, span: function_span
}) -> {
  let block_identity = NativeBlockId(next_block);
  let block = NativeBlockBuilder {
    identity: block_identity,
    parameters: Nil,
    reversed_instructions: Nil,
    terminator: None,
    span: span
  };
  NativeFunctionBuilder {
    identity: function_identity,
    owner: owner,
    direct_functions: direct_functions,
    substitutions: substitutions,
    result_type: result_type,
    entry: entry,
    current: block_identity,
    blocks: Cons(block, blocks),
    next_block: next_block + 1,
    next_value: next_value,
    environment: environment,
    generated_functions: generated_functions,
    span: function_span
  }
}

let create_block_parameter = fn checked_type -> fn span -> fn builder -> {
  let value = fresh_value_id builder;
  let updated_builder = allocate_value_id builder;
  match (find_block_builder (function_builder_current updated_builder)
      (function_builder_blocks updated_builder)) {
    | None -> Err("NativeIR lowering: new block is missing")
    | Some(NativeBlockBuilder {
        identity, parameters, reversed_instructions, terminator, span: block_span
      }) -> match terminator {
        | Some(_) -> Err("NativeIR lowering: cannot add a parameter after termination")
        | None -> {
          let parameter = NativeBlockParameter {
            value: value,
            checked_type: checked_type,
            span: span
          };
          let updated_block = NativeBlockBuilder {
            identity: identity,
            parameters: append_native_parameters parameters (Cons(parameter, Nil)),
            reversed_instructions: reversed_instructions,
            terminator: None,
            span: block_span
          };
          Ok(NativeLoweredValue {
            value: value,
            checked_type: checked_type,
            builder: update_current_block updated_block updated_builder
          })
        }
      }
  }
}

let append_native_parameters = fn first -> fn second -> match first {
  | Nil -> second
  | Cons(parameter, remaining) ->
    Cons(parameter, append_native_parameters remaining second)
}

let select_block = fn wanted -> fn (NativeFunctionBuilder {
  identity, owner, direct_functions, substitutions, result_type, entry, current: _, blocks, next_block, next_value,
  environment, generated_functions, span
}) -> NativeFunctionBuilder {
  identity: identity,
  owner: owner,
  direct_functions: direct_functions,
  substitutions: substitutions,
  result_type: result_type,
  entry: entry,
  current: wanted,
  blocks: blocks,
  next_block: next_block,
  next_value: next_value,
  environment: environment,
  generated_functions: generated_functions,
  span: span
}

let add_binding_value = fn binding -> fn value -> fn checked_type ->
  fn (NativeFunctionBuilder {
    identity, owner, direct_functions, substitutions, result_type, entry, current, blocks, next_block, next_value,
    environment, generated_functions, span
  }) -> NativeFunctionBuilder {
    identity: identity,
    owner: owner,
    direct_functions: direct_functions,
    substitutions: substitutions,
    result_type: result_type,
    entry: entry,
    current: current,
    blocks: blocks,
    next_block: next_block,
    next_value: next_value,
    environment: Cons(NativeBindingValue {
      binding: binding,
      value: value,
      checked_type: checked_type
    }, environment),
    generated_functions: generated_functions,
    span: span
  }

let lookup_binding_value = fn wanted -> fn environment -> match environment {
  | Nil -> None
  | Cons(NativeBindingValue { binding, value, checked_type }, remaining) ->
    match (same_binding wanted binding) {
      | true -> Some(NativeBindingValue {
        binding: binding,
        value: value,
        checked_type: checked_type
      })
      | false -> lookup_binding_value wanted remaining
    }
}

let set_builder_environment = fn environment -> fn (NativeFunctionBuilder {
  identity, owner, direct_functions, substitutions, result_type, entry, current, blocks, next_block, next_value,
  environment: _, generated_functions, span
}) -> NativeFunctionBuilder {
  identity: identity,
  owner: owner,
  direct_functions: direct_functions,
  substitutions: substitutions,
  result_type: result_type,
  entry: entry,
  current: current,
  blocks: blocks,
  next_block: next_block,
  next_value: next_value,
  environment: environment,
  generated_functions: generated_functions,
  span: span
}

let set_builder_owner = fn owner -> fn (NativeFunctionBuilder {
  identity, owner: _, direct_functions, substitutions, result_type, entry, current, blocks, next_block, next_value,
  environment, generated_functions, span
}) -> NativeFunctionBuilder {
  identity: identity,
  owner: owner,
  direct_functions: direct_functions,
  substitutions: substitutions,
  result_type: result_type,
  entry: entry,
  current: current,
  blocks: blocks,
  next_block: next_block,
  next_value: next_value,
  environment: environment,
  generated_functions: generated_functions,
  span: span
}

let add_generated_function = fn generated -> fn (NativeFunctionBuilder {
  identity, owner, direct_functions, substitutions, result_type, entry, current, blocks, next_block, next_value,
  environment, generated_functions, span
}) -> NativeFunctionBuilder {
  identity: identity,
  owner: owner,
  direct_functions: direct_functions,
  substitutions: substitutions,
  result_type: result_type,
  entry: entry,
  current: current,
  blocks: blocks,
  next_block: next_block,
  next_value: next_value,
  environment: environment,
  generated_functions: append_native_functions generated generated_functions,
  span: span
}

let append_native_functions = fn first -> fn second -> match first {
  | Nil -> second
  | Cons(function, remaining) ->
    Cons(function, append_native_functions remaining second)
}

type DetachedBlock = DetachedBlock {
  identity: NativeBlockId,
  builder: NativeFunctionBuilder
}

let create_detached_block = fn span -> fn builder -> {
  let previous = function_builder_current builder;
  let created = create_block span builder;
  let identity = function_builder_current created;
  DetachedBlock {
    identity: identity,
    builder: select_block previous created
  }
}

let reverse_native_blocks = fn blocks -> fn reversed -> match blocks {
  | Nil -> reversed
  | Cons(block, remaining) -> reverse_native_blocks remaining (Cons(block, reversed))
}

let reverse_native_instructions = fn instructions -> fn reversed -> match instructions {
  | Nil -> reversed
  | Cons(instruction, remaining) ->
    reverse_native_instructions remaining (Cons(instruction, reversed))
}

let finish_block_builder = fn (NativeBlockBuilder {
  identity, parameters, reversed_instructions, terminator, span
}) -> match terminator {
  | None -> Err("NativeIR lowering: block has no terminator")
  | Some(final_terminator) -> Ok(NativeBlock {
    identity: identity,
    parameters: parameters,
    instructions: reverse_native_instructions reversed_instructions Nil,
    terminator: final_terminator,
    span: span
  })
}

let finish_native_blocks = fn builders -> match builders {
  | Nil -> Ok(Nil)
  | Cons(builder, remaining) -> match (finish_block_builder builder) {
    | Err(message) -> Err(message)
    | Ok(block) -> match (finish_native_blocks remaining) {
      | Err(message) -> Err(message)
      | Ok(blocks) -> Ok(Cons(block, blocks))
    }
  }
}

let finish_function_builder = fn captures -> fn (NativeFunctionBuilder {
  identity, result_type, entry, blocks, generated_functions: _, span,
  owner: _, direct_functions: _, substitutions: _, current: _, next_block: _, next_value: _, environment: _
}) -> match (finish_native_blocks
    (reverse_native_blocks blocks Nil)) {
  | Err(message) -> Err(message)
  | Ok(final_blocks) -> Ok(NativeFunction {
    identity: identity,
    captures: captures,
    result_type: result_type,
    entry: entry,
    blocks: final_blocks,
    span: span
  })
}

let emit_return = fn value -> fn builder ->
  set_terminator (NativeReturn(value)) builder

let lower_native_constant = fn constant -> match constant {
  | LambdaUnit -> NativeUnit
  | LambdaInteger(value) -> NativeInteger(value)
  | LambdaBoolean(value) -> NativeBoolean(value)
  | LambdaString(value) -> NativeString(value)
}

let lower_native_pattern_test = fn (LambdaPattern {
  checked_type: _, kind, span: _
}) -> match kind {
  | LambdaBind(_) -> NativeAnyPatternTest
  | LambdaWildcard -> NativeAnyPatternTest
  | LambdaConstantPattern(constant) ->
    NativeLiteralPatternTest(lower_native_constant constant)
  | LambdaConstructPattern(LambdaConstructorPattern { entity, arguments }) ->
    NativeNestedConstructorPatternTest(NativeConstructorPatternTest {
      entity: entity,
      arguments: lower_native_pattern_test_list arguments
    })
  | LambdaTuplePattern(patterns) ->
    NativeTuplePatternTest(lower_native_pattern_test_list patterns)
  | LambdaRecordPatternNode(LambdaRecordPattern { entity, fields }) ->
    NativeNestedRecordPatternTest(NativeRecordPatternTest {
      entity: entity,
      fields: lower_native_record_pattern_test_fields fields
    })
}

let lower_native_pattern_test_list = fn patterns -> match patterns {
  | Nil -> Nil
  | Cons(pattern, remaining) -> Cons(lower_native_pattern_test pattern,
    lower_native_pattern_test_list remaining)
}

let lower_native_record_pattern_test_fields = fn fields -> match fields {
  | Nil -> Nil
  | Cons(LambdaRecordPatternField { name, pattern }, remaining) ->
    Cons(NativeRecordPatternFieldTest {
      name: name,
      pattern: lower_native_pattern_test pattern
    }, lower_native_record_pattern_test_fields remaining)
}

let pattern_requires_test = fn (LambdaPattern {
  checked_type: _, kind, span: _
}) -> match kind {
  | LambdaBind(_) -> false
  | LambdaWildcard -> false
  | _ -> true
}

let emit_pattern_test = fn pattern -> fn subject -> fn span -> fn builder ->
  emit_instruction native_bool_type
    (NativeTestValue(NativeValueTest {
      value: subject,
      test: NativePatternMatchTest(lower_native_pattern_test pattern)
    })) (NativeEffectSet(Nil)) span builder

let lower_pattern_scope = fn pattern -> fn subject -> fn span -> fn builder ->
  match (pattern_requires_test pattern) {
    | false -> bind_pattern_values pattern subject builder
    | true -> {
      let DetachedBlock { identity: success_id, builder: with_success } =
        create_detached_block span builder;
      let DetachedBlock { identity: failure_id, builder: with_failure } =
        create_detached_block span with_success;
      match (emit_pattern_test pattern subject span with_failure) {
        | Err(message) -> Err(message)
        | Ok(NativeLoweredValue { value: test, checked_type: _, builder: tested }) ->
          match (set_terminator (NativeConditionalBranch(NativeBranch {
            condition: test,
            when_true: NativeEdge { target: success_id, arguments: Nil },
            when_false: NativeEdge { target: failure_id, arguments: Nil }
          })) tested) {
            | Err(message) -> Err(message)
            | Ok(branched) -> match (set_terminator
                (NativeTrap(NativePatternFailure)) (select_block failure_id branched)) {
              | Err(message) -> Err(message)
              | Ok(trapped) -> bind_pattern_values pattern subject
                  (select_block success_id trapped)
            }
          }
      }
    }
  }

let bind_pattern_values = fn (LambdaPattern {
  checked_type: checked_type, kind, span: _
}) -> fn subject -> fn builder -> match kind {
  | LambdaBind(binding) -> Ok(add_binding_value binding subject checked_type builder)
  | LambdaWildcard -> Ok(builder)
  | LambdaConstantPattern(_) -> Ok(builder)
  | LambdaTuplePattern(patterns) ->
    bind_tuple_pattern_fields patterns subject 0 builder
  | LambdaConstructPattern(LambdaConstructorPattern { entity: _, arguments }) ->
    bind_constructor_pattern_fields arguments subject 0 builder
  | LambdaRecordPatternNode(LambdaRecordPattern { entity: _, fields }) ->
    bind_record_pattern_fields fields subject builder
}

let bind_pattern_projection = fn (LambdaPattern {
  checked_type, kind, span
}) -> fn subject -> fn field -> fn builder ->
  match (emit_instruction checked_type (NativeProjectAggregate(NativeProjection {
    aggregate: subject,
    field: field
  })) (NativeEffectSet(Nil)) span builder) {
    | Err(message) -> Err(message)
    | Ok(NativeLoweredValue { value, checked_type: _, builder: next_builder }) ->
      bind_pattern_values (LambdaPattern {
        checked_type: checked_type,
        kind: kind,
        span: span
      }) value next_builder
  }

let bind_tuple_pattern_fields = fn patterns -> fn subject -> fn index -> fn builder ->
  match patterns {
    | Nil -> Ok(builder)
    | Cons(pattern, remaining) -> match (bind_pattern_projection pattern subject
        (NativeTupleField(index)) builder) {
      | Err(message) -> Err(message)
      | Ok(next_builder) -> bind_tuple_pattern_fields remaining subject
        (index + 1) next_builder
    }
  }

let bind_constructor_pattern_fields = fn patterns -> fn subject -> fn index ->
  fn builder -> match patterns {
    | Nil -> Ok(builder)
    | Cons(pattern, remaining) -> match (bind_pattern_projection pattern subject
        (NativeConstructorField(index)) builder) {
      | Err(message) -> Err(message)
      | Ok(next_builder) -> bind_constructor_pattern_fields remaining subject
        (index + 1) next_builder
    }
  }

let bind_record_pattern_fields = fn fields -> fn subject -> fn builder -> match fields {
  | Nil -> Ok(builder)
  | Cons(LambdaRecordPatternField { name, pattern }, remaining) ->
    match (bind_pattern_projection pattern subject (NativeRecordField(name)) builder) {
      | Err(message) -> Err(message)
      | Ok(next_builder) -> bind_record_pattern_fields remaining subject next_builder
    }
}

type NativeLoweredValues = NativeLoweredValues {
  values: List<NativeValueId>,
  builder: NativeFunctionBuilder
}

type NativeLoweredFields = NativeLoweredFields {
  fields: List<NativeAggregateValue>,
  builder: NativeFunctionBuilder
}

type NativeCapturedBindings = NativeCapturedBindings {
  values: List<NativeValueId>,
  builder: NativeFunctionBuilder
}

type NativeCompiledFunction = NativeCompiledFunction {
  function: NativeFunction,
  nested_functions: List<NativeFunction>,
  captures: List<NativeClosureCapture>
}

let append_native_values = fn first -> fn second -> match first {
  | Nil -> second
  | Cons(value, remaining) -> Cons(value, append_native_values remaining second)
}

let lower_expression_list = fn expressions -> fn builder -> match expressions {
  | Nil -> Ok(NativeLoweredValues { values: Nil, builder: builder })
  | Cons(expression, remaining) -> match (lower_expression expression builder) {
    | Err(message) -> Err(message)
    | Ok(NativeLoweredValue { value, checked_type: _, builder: next_builder }) ->
      match (lower_expression_list remaining next_builder) {
        | Err(message) -> Err(message)
        | Ok(NativeLoweredValues { values, builder: final_builder }) ->
          Ok(NativeLoweredValues {
            values: Cons(value, values),
            builder: final_builder
          })
      }
  }
}

let lower_record_expression_fields = fn fields -> fn builder -> match fields {
  | Nil -> Ok(NativeLoweredFields { fields: Nil, builder: builder })
  | Cons(LambdaRecordFieldValue { name, value }, remaining) ->
    match (lower_expression value builder) {
      | Err(message) -> Err(message)
      | Ok(NativeLoweredValue { value: field_value, checked_type: _, builder: next_builder }) ->
        match (lower_record_expression_fields remaining next_builder) {
          | Err(message) -> Err(message)
          | Ok(NativeLoweredFields { fields, builder: final_builder }) ->
            Ok(NativeLoweredFields {
              fields: Cons(NativeAggregateValue {
                field: NativeRecordField(name),
                value: field_value
              }, fields),
              builder: final_builder
            })
        }
    }
}

let lower_effects_for_binary = fn operator -> fn operand_type -> fn substitutions -> match operator {
  | Division -> NativeEffectSet(Cons(NativeMayTrap(NativeDivisionByZero), Nil))
  | Remainder -> NativeEffectSet(Cons(NativeMayTrap(NativeRemainderByZero), Nil))
  | Addition -> match (apply_substitutions operand_type substitutions) {
    | AppliedType(InferredNamedType { name, arguments: _ }) -> match (equal name "string") {
      | true -> NativeEffectSet(Cons(NativeAllocate, Nil))
      | false -> NativeEffectSet(Nil)
    }
    | _ -> NativeEffectSet(Nil)
  }
  | _ -> NativeEffectSet(Nil)
}

let emit_constant = fn checked_type -> fn constant -> fn span -> fn builder ->
  emit_instruction checked_type (NativeConstant(lower_native_constant constant))
    (NativeEffectSet(Nil)) span builder

let load_global = fn binding -> fn checked_type -> fn span -> fn builder ->
  emit_instruction checked_type (NativeLoadGlobal(binding))
    (NativeEffectSet(Cons(NativeReadMutableState, Nil))) span builder

let store_global = fn binding -> fn value -> fn span -> fn builder ->
  emit_instruction native_unit_type (NativeStoreGlobal(NativeGlobalStore {
    binding: binding,
    value: value
  })) (NativeEffectSet(Cons(NativeWriteMutableState, Nil))) span builder

let builder_generated_functions = fn (NativeFunctionBuilder {
  identity: _, owner: _, direct_functions: _, substitutions: _, result_type: _, entry: _, current: _, blocks: _,
  next_block: _, next_value: _, environment: _, generated_functions, span: _
}) -> generated_functions

let compile_capture_bindings = fn bindings -> fn index -> fn builder -> match bindings {
  | Nil -> Ok(NativeCapturedBindings { values: Nil, builder: builder })
  | Cons(NativeBindingValue { binding, value: _, checked_type }, remaining) ->
    match (emit_instruction checked_type (NativeLoadCapture(index))
        (NativeEffectSet(Nil)) (binding_declaration_span binding) builder) {
      | Err(message) -> Err(message)
      | Ok(NativeLoweredValue { value, checked_type: _, builder: loaded }) ->
        match (compile_capture_bindings remaining (index + 1)
            (add_binding_value binding value checked_type loaded)) {
          | Err(message) -> Err(message)
          | Ok(NativeCapturedBindings { values, builder: captured_builder }) ->
            Ok(NativeCapturedBindings {
              values: Cons(value, values),
              builder: captured_builder
            })
        }
    }
}

let binding_declaration_span = fn (BindingIdentity {
  source_path: _, declaration_span, name: _, scope: _
}) -> declaration_span

let native_capture_metadata = fn bindings -> match bindings {
  | Nil -> Nil
  | Cons(NativeBindingValue { binding, checked_type, value: _ }, remaining) ->
    Cons(NativeCapture {
      binding: binding,
      checked_type: checked_type,
      span: binding_declaration_span binding
    }, native_capture_metadata remaining)
}

let closure_capture_values = fn bindings -> match bindings {
  | Nil -> Nil
  | Cons(NativeBindingValue { binding, checked_type, value }, remaining) ->
    Cons(NativeClosureCapture {
      capture: NativeCapture {
        binding: binding,
        checked_type: checked_type,
        span: binding_declaration_span binding
      },
      value: value
    }, closure_capture_values remaining)
}

let compile_function_body = fn identity -> fn owner -> fn direct_functions -> fn substitutions ->
  fn (LambdaFunctionExpression { parameter, body }) ->
  fn checked_type -> fn span -> fn captured_bindings ->
  match (function_parts checked_type substitutions) {
    | Err(message) -> Err(message)
    | Ok(NativeFunctionParts { argument, result }) -> {
      let initial_builder = new_function_builder identity (Some(owner))
        direct_functions substitutions result span;
      match (compile_capture_bindings captured_bindings 0 initial_builder) {
        | Err(message) -> Err(message)
        | Ok(NativeCapturedBindings {
            values: _, builder: with_captures
          }) -> match (create_block_parameter argument span with_captures) {
              | Err(message) -> Err(message)
              | Ok(NativeLoweredValue {
                  value: argument_value, checked_type: _, builder: parameter_builder
                }) -> match (lower_pattern_scope parameter argument_value span
                    parameter_builder) {
                | Err(message) -> Err(message)
                | Ok(scoped_builder) -> match (lower_expression body scoped_builder) {
                  | Err(message) -> Err(message)
                  | Ok(NativeLoweredValue {
                      value: result_value, checked_type: _, builder: body_builder
                    }) -> match (emit_return result_value body_builder) {
                    | Err(message) -> Err(message)
                    | Ok(terminated_builder) -> {
                      let nested_functions = builder_generated_functions terminated_builder;
                      match (finish_function_builder
                          (native_capture_metadata captured_bindings)
                          terminated_builder) {
                        | Err(message) -> Err(message)
                        | Ok(native_function) -> Ok(NativeCompiledFunction {
                          function: native_function,
                          nested_functions: append_native_functions
                            (Cons(native_function, Nil)) nested_functions,
                          captures: closure_capture_values captured_bindings
                        })
                      }
                    }
                  }
                }
            }
      }
    }
  }
}

let compile_lambda = fn identity -> fn owner -> fn function_expression ->
  fn checked_type -> fn span -> fn captured_bindings -> fn outer_builder ->
  match (compile_function_body identity owner
      (function_builder_direct_functions outer_builder)
      (function_builder_substitutions outer_builder) function_expression
      checked_type span captured_bindings) {
    | Err(message) -> Err(message)
    | Ok(NativeCompiledFunction { function, nested_functions, captures }) ->
      match (emit_instruction checked_type (NativeCreateClosure(NativeClosure {
        function: identity,
        captures: captures
      })) (NativeEffectSet(Cons(NativeAllocate, Nil))) span
          (add_generated_function nested_functions outer_builder)) {
        | Err(message) -> Err(message)
        | Ok(closure_value) -> Ok(closure_value)
      }
  }

let function_builder_environment = fn (NativeFunctionBuilder {
  identity: _, owner: _, direct_functions: _, substitutions: _, result_type: _, entry: _, current: _, blocks: _,
  next_block: _, next_value: _, environment, generated_functions: _, span: _
}) -> environment

let function_builder_owner = fn (NativeFunctionBuilder {
  identity: _, owner, direct_functions: _, substitutions: _, result_type: _, entry: _, current: _, blocks: _,
  next_block: _, next_value: _, environment: _, generated_functions: _, span: _
}) -> owner

let function_builder_substitutions = fn (NativeFunctionBuilder {
  identity: _, owner: _, direct_functions: _, substitutions, result_type: _, entry: _, current: _, blocks: _,
  next_block: _, next_value: _, environment: _, generated_functions: _, span: _
}) -> substitutions

let function_builder_direct_functions = fn (NativeFunctionBuilder {
  identity: _, owner: _, direct_functions, substitutions: _, result_type: _, entry: _, current: _, blocks: _,
  next_block: _, next_value: _, environment: _, generated_functions: _, span: _
}) -> direct_functions

let lower_lambda_function = fn function_expression -> fn checked_type -> fn span ->
  fn builder -> match (function_builder_owner builder) {
    | None -> Err("NativeIR lowering: nested function has no owning declaration")
    | Some(owner) -> {
      let SourceSpan { start_codepoint: ordinal, end_codepoint: _ } = span;
      let identity = NativeNestedFunction(NativeNestedFunctionId {
        parent: owner,
        source_span: span,
        ordinal: ordinal
      });
      compile_lambda identity owner function_expression checked_type span
        (function_builder_environment builder) builder
    }
  }

let lower_expression_list_last = fn expressions -> fn result_type -> fn span ->
  fn builder -> match expressions {
    | Nil -> emit_constant result_type LambdaUnit span builder
    | Cons(expression, remaining) -> match (lower_expression expression builder) {
      | Err(message) -> Err(message)
      | Ok(NativeLoweredValue { value, checked_type: _, builder: next_builder }) ->
        match remaining {
          | Nil -> Ok(NativeLoweredValue {
            value: value,
            checked_type: result_type,
            builder: next_builder
          })
          | _ -> lower_expression_list_last remaining result_type span next_builder
        }
    }
  }

let function_builder_span = fn (NativeFunctionBuilder {
  identity: _, owner: _, direct_functions: _, substitutions: _, result_type: _, entry: _, current: _, blocks: _,
  next_block: _, next_value: _, environment: _, generated_functions: _, span
}) -> span

let lower_match_arms = fn arms -> fn subject -> fn result_type -> fn join_id ->
  fn base_environment -> fn span -> fn builder -> match arms {
    | Nil -> set_terminator (NativeTrap(NativePatternFailure)) builder
    | Cons(LambdaMatchArm { pattern, body }, remaining) ->
      match (pattern_requires_test pattern) {
        | false -> match (bind_pattern_values pattern subject
            (set_builder_environment base_environment builder)) {
          | Err(message) -> Err(message)
          | Ok(scoped_builder) -> match (lower_expression body scoped_builder) {
            | Err(message) -> Err(message)
            | Ok(NativeLoweredValue { value, checked_type: _, builder: body_builder }) ->
              set_terminator (NativeJump(NativeEdge {
                target: join_id,
                arguments: Cons(value, Nil)
              })) body_builder
          }
        }
        | true -> {
          let pattern_span = pattern_source_span pattern;
          let DetachedBlock { identity: body_id, builder: with_body } =
            create_detached_block pattern_span builder;
          let DetachedBlock { identity: failure_id, builder: with_failure } =
            create_detached_block pattern_span with_body;
          match (emit_pattern_test pattern subject pattern_span with_failure) {
            | Err(message) -> Err(message)
            | Ok(NativeLoweredValue { value: test, checked_type: _, builder: tested }) ->
              match (set_terminator (NativeConditionalBranch(NativeBranch {
                condition: test,
                when_true: NativeEdge { target: body_id, arguments: Nil },
                when_false: NativeEdge { target: failure_id, arguments: Nil }
              })) tested) {
                | Err(message) -> Err(message)
                | Ok(branched) -> {
                  let body_start = set_builder_environment base_environment
                    (select_block body_id branched);
                  match (bind_pattern_values pattern subject body_start) {
                    | Err(message) -> Err(message)
                    | Ok(scoped_builder) -> match (lower_expression body scoped_builder) {
                      | Err(message) -> Err(message)
                      | Ok(NativeLoweredValue {
                          value: result, checked_type: _, builder: body_builder
                        }) -> match (set_terminator (NativeJump(NativeEdge {
                          target: join_id,
                          arguments: Cons(result, Nil)
                        })) body_builder) {
                        | Err(message) -> Err(message)
                        | Ok(joined) -> lower_match_arms remaining subject result_type
                            join_id base_environment span
                            (set_builder_environment base_environment
                              (select_block failure_id joined))
                      }
                    }
                  }
                }
              }
          }
        }
      }
  }

let pattern_source_span = fn (LambdaPattern {
  checked_type: _, kind: _, span
}) -> span

let lower_match = fn arms -> fn subject -> fn result_type -> fn span -> fn builder -> {
  let base_environment = function_builder_environment builder;
  let starting_block = function_builder_current builder;
  let DetachedBlock { identity: join_id, builder: with_join } =
    create_detached_block span builder;
  match (create_block_parameter result_type span (select_block join_id with_join)) {
    | Err(message) -> Err(message)
    | Ok(NativeLoweredValue { value: join_value, checked_type: _, builder: join_parameter_builder }) -> {
      let arm_builder = select_block starting_block join_parameter_builder;
      match (lower_match_arms arms subject result_type join_id base_environment span arm_builder) {
        | Err(message) -> Err(message)
        | Ok(arms_builder) -> Ok(NativeLoweredValue {
          value: join_value,
          checked_type: result_type,
          builder: select_block join_id arms_builder
        })
      }
    }
  }
}

let expression_checked_type = fn (LambdaExpression {
  checked_type, kind: _, span: _
}) -> checked_type

let has_direct_function_binding = fn wanted -> fn candidates -> match candidates {
  | Nil -> false
  | Cons(candidate, remaining) -> match (same_binding wanted candidate) {
    | true -> true
    | false -> has_direct_function_binding wanted remaining
  }
}

let lower_direct_apply_expression = fn binding -> fn callee_type -> fn argument ->
  fn result_type -> fn span -> fn builder ->
  match (function_parts callee_type (function_builder_substitutions builder)) {
    | Err(message) -> Err(message)
    | Ok(NativeFunctionParts { argument: argument_type, result: _ }) ->
      match (lower_expression argument builder) {
        | Err(message) -> Err(message)
        | Ok(NativeLoweredValue { value: argument_value, checked_type: _, builder: argument_builder }) ->
          emit_instruction result_type (NativeCallOperation(NativeCall {
            target: NativeDirectCall(NativeTopLevelFunction(binding)),
            arguments: Cons(argument_value, Nil),
            argument_types: Cons(argument_type, Nil),
            result_type: result_type
          })) (NativeEffectSet(Cons(NativeReadMutableState,
            Cons(NativeUnknownCall, Nil)))) span argument_builder
      }
  }

let lower_closure_apply_expression = fn callee -> fn argument -> fn result_type -> fn span ->
  fn builder -> match (lower_expression callee builder) {
    | Err(message) -> Err(message)
    | Ok(NativeLoweredValue {
        value: callee_value, checked_type: _, builder: callee_builder
      }) -> match (lower_expression argument callee_builder) {
      | Err(message) -> Err(message)
      | Ok(NativeLoweredValue {
          value: argument_value, checked_type: _, builder: argument_builder
        }) -> match (function_parts (expression_checked_type callee)
            (function_builder_substitutions builder)) {
        | Err(message) -> Err(message)
        | Ok(NativeFunctionParts { argument: argument_type, result: _ }) ->
          match (emit_instruction result_type (NativeCallOperation(NativeCall {
            target: NativeClosureCall(callee_value),
            arguments: Cons(argument_value, Nil),
            argument_types: Cons(argument_type, Nil),
            result_type: result_type
          })) (NativeEffectSet(Cons(NativeUnknownCall, Nil))) span argument_builder) {
            | Err(message) -> Err(message)
            | Ok(call_value) -> Ok(call_value)
          }
      }
    }
  }

let lower_host_apply_expression = fn binding -> fn host_effect -> fn callee_type ->
  fn argument -> fn result_type -> fn span -> fn builder ->
  match (function_parts callee_type (function_builder_substitutions builder)) {
    | Err(message) -> Err(message)
    | Ok(NativeFunctionParts { argument: argument_type, result: _ }) ->
      match (lower_expression argument builder) {
        | Err(message) -> Err(message)
        | Ok(NativeLoweredValue {
            value: argument_value, checked_type: _, builder: argument_builder
          }) -> emit_instruction result_type (NativeCallOperation(NativeCall {
            target: NativeHostCall(binding),
            arguments: Cons(argument_value, Nil),
            argument_types: Cons(argument_type, Nil),
            result_type: result_type
          })) (NativeEffectSet(Cons(host_effect, Nil))) span argument_builder
      }
  }

let lower_apply_expression = fn callee -> fn argument -> fn result_type -> fn span ->
  fn builder -> match callee {
    | LambdaExpression { checked_type: callee_type,
        kind: LambdaValueReference(binding), span: _ } ->
      match (has_direct_function_binding binding
          (function_builder_direct_functions builder)) {
        | true -> lower_direct_apply_expression binding callee_type argument
            result_type span builder
        | false -> match (native_host_effect_for_binding binding) {
          | Some(host_effect) -> lower_host_apply_expression binding host_effect
              callee_type argument result_type span builder
          | None -> lower_closure_apply_expression callee argument
              result_type span builder
        }
      }
    | _ -> lower_closure_apply_expression callee argument result_type span builder
  }

let lower_binary_expression = fn operator -> fn operand_type -> fn left -> fn right ->
  fn result_type -> fn span -> fn builder -> match (lower_expression left builder) {
    | Err(message) -> Err(message)
    | Ok(NativeLoweredValue { value: left_value, checked_type: _, builder: left_builder }) ->
      match (lower_expression right left_builder) {
        | Err(message) -> Err(message)
        | Ok(NativeLoweredValue { value: right_value, checked_type: _, builder: right_builder }) ->
          emit_instruction result_type
            (NativePrimitiveOperation(NativePrimitiveBinary(NativeBinaryPrimitive {
              operator: operator,
              operand_type: operand_type,
              left: left_value,
              right: right_value
            }))) (lower_effects_for_binary operator operand_type
              (function_builder_substitutions right_builder)) span right_builder
      }
  }

let lower_negate_expression = fn operand_type -> fn operand -> fn result_type ->
  fn span -> fn builder -> match (lower_expression operand builder) {
    | Err(message) -> Err(message)
    | Ok(NativeLoweredValue { value, checked_type: _, builder: operand_builder }) ->
      emit_instruction result_type
        (NativePrimitiveOperation(NativePrimitiveNegate(NativeNegatePrimitive {
          operand_type: operand_type,
          operand: value
        }))) (NativeEffectSet(Nil)) span operand_builder
  }

let tuple_aggregate_fields = fn values -> fn index -> match values {
  | Nil -> Nil
  | Cons(value, remaining) -> Cons(NativeAggregateValue {
    field: NativeTupleField(index),
    value: value
  }, tuple_aggregate_fields remaining (index + 1))
}

let constructor_aggregate_fields = fn values -> fn index -> match values {
  | Nil -> Nil
  | Cons(value, remaining) -> Cons(NativeAggregateValue {
    field: NativeConstructorField(index),
    value: value
  }, constructor_aggregate_fields remaining (index + 1))
}

let lower_tuple_expression = fn expressions -> fn result_type -> fn span -> fn builder ->
  match (lower_expression_list expressions builder) {
    | Err(message) -> Err(message)
    | Ok(NativeLoweredValues { values, builder: values_builder }) ->
      emit_instruction result_type (NativeCreateAggregate(NativeAggregate {
        kind: NativeTupleAggregate,
        fields: tuple_aggregate_fields values 0
      })) (NativeEffectSet(Cons(NativeAllocate, Nil))) span values_builder
  }

let lower_construct_expression = fn entity -> fn expressions -> fn result_type ->
  fn span -> fn builder -> match (lower_expression_list expressions builder) {
    | Err(message) -> Err(message)
    | Ok(NativeLoweredValues { values, builder: values_builder }) ->
      emit_instruction result_type (NativeCreateAggregate(NativeAggregate {
        kind: NativeVariantAggregate(entity),
        fields: constructor_aggregate_fields values 0
      })) (NativeEffectSet(Cons(NativeAllocate, Nil))) span values_builder
  }

let lower_record_expression = fn entity -> fn fields -> fn result_type -> fn span ->
  fn builder -> match (lower_record_expression_fields fields builder) {
    | Err(message) -> Err(message)
    | Ok(NativeLoweredFields { fields, builder: fields_builder }) ->
      emit_instruction result_type (NativeCreateAggregate(NativeAggregate {
        kind: NativeRecordAggregate(entity),
        fields: fields
      })) (NativeEffectSet(Cons(NativeAllocate, Nil))) span fields_builder
  }

let lower_expression = fn (LambdaExpression { checked_type, kind, span }) -> fn builder ->
  match kind {
    | LambdaConstantExpr(constant) -> emit_constant checked_type constant span builder
    | LambdaValueReference(binding) -> match (binding_scope binding) {
      | ModuleValueBinding -> load_global binding checked_type span builder
      | LocalValueBinding -> match (lookup_binding_value binding
          (function_builder_environment builder)) {
        | None -> Err("NativeIR lowering: local binding is not in the environment")
        | Some(NativeBindingValue { value, checked_type: _, binding: _ }) ->
          Ok(NativeLoweredValue {
            value: value,
            checked_type: checked_type,
            builder: builder
          })
      }
    }
    | LambdaConstruct(LambdaConstructorExpression { entity, arguments }) ->
      lower_construct_expression entity arguments checked_type span builder
    | LambdaFunction(function_expression) ->
      lower_lambda_function function_expression checked_type span builder
    | LambdaApply(LambdaApplyExpression { callee, argument }) ->
      lower_apply_expression callee argument checked_type span builder
    | LambdaLet(LambdaLetExpression { binding, value, body }) ->
      match (lower_expression value builder) {
        | Err(message) -> Err(message)
        | Ok(NativeLoweredValue { value, checked_type: _, builder: value_builder }) ->
          match (lower_pattern_scope binding value span value_builder) {
            | Err(message) -> Err(message)
            | Ok(body_builder) -> lower_expression body body_builder
          }
      }
    | LambdaBinaryPrimitive(LambdaBinaryPrimitiveExpression {
        operator, operand_type, left, right
      }) -> lower_binary_expression operator operand_type left right checked_type span builder
    | LambdaNegatePrimitive(LambdaNegatePrimitiveExpression {
        operand_type, operand
      }) -> lower_negate_expression operand_type operand checked_type span builder
    | LambdaTuple(expressions) ->
      lower_tuple_expression expressions checked_type span builder
    | LambdaRecord(LambdaRecordExpression { entity, fields }) ->
      lower_record_expression entity fields checked_type span builder
    | LambdaMatch(LambdaMatchExpression { scrutinee, arms }) ->
      match (lower_expression scrutinee builder) {
        | Err(message) -> Err(message)
        | Ok(NativeLoweredValue { value, checked_type: _, builder: scrutinee_builder }) ->
          lower_match arms value checked_type span scrutinee_builder
      }
    | LambdaBlock(expressions) -> lower_expression_list_last expressions checked_type span builder
  }

let binding_scope = fn (BindingIdentity {
  source_path: _, declaration_span: _, name: _, scope
}) -> scope

type NativeModuleDeclarations = NativeModuleDeclarations {
  declarations: List<LambdaValueDeclaration>,
  nominal_types: List<TypeVisibility>,
  substitutions: List<TypeSubstitution>
}

let lower_native_globals = fn declarations -> match declarations {
  | Nil -> Nil
  | Cons(LambdaValueDeclaration { binding, scheme, body: _, span }, remaining) ->
    Cons(NativeGlobal {
      binding: binding,
      scheme: scheme,
      span: span
    }, lower_native_globals remaining)
}

// Binding identity is enough to bypass a closure only for a monomorphic,
// same-module lambda declaration. Aliases, imported values, and generalized
// functions retain ordinary closure-call semantics.
let direct_function_bindings = fn declarations -> match declarations {
  | Nil -> Nil
  | Cons(LambdaValueDeclaration {
      binding, body: LambdaExpression { kind: LambdaFunction(_), checked_type: _, span: _ },
      scheme: TypeScheme { quantified_variables: Nil, body: _ }, span: _
    }, remaining) -> Cons(binding, direct_function_bindings remaining)
  | Cons(_, remaining) -> direct_function_bindings remaining
}

let lower_top_level_initializer = fn binding ->
  fn (LambdaExpression { checked_type, kind, span }) -> fn builder -> match kind {
    | LambdaFunction(function_expression) -> compile_lambda
        (NativeTopLevelFunction(binding)) binding function_expression
        checked_type span Nil builder
    | _ -> lower_expression (LambdaExpression {
      checked_type: checked_type,
      kind: kind,
      span: span
    }) builder
  }

let lower_module_declarations = fn declarations -> fn builder -> match declarations {
  | Nil -> Ok(builder)
  | Cons(LambdaValueDeclaration { binding, scheme: _, body, span }, remaining) -> {
    let owned_builder = set_builder_owner (Some(binding)) builder;
    match (lower_top_level_initializer binding body owned_builder) {
      | Err(message) -> Err(message)
      | Ok(NativeLoweredValue { value, checked_type: _, builder: value_builder }) ->
        match (store_global binding value span value_builder) {
          | Err(message) -> Err(message)
          | Ok(NativeLoweredValue { value: _, checked_type: _, builder: stored_builder }) ->
            lower_module_declarations remaining stored_builder
        }
    }
  }
}

let empty_module_span = SourceSpan {
  start_codepoint: 0,
  end_codepoint: 0
}

let lower_module = fn module_identity ->
  fn (LambdaModule { nominal_types, substitutions, recursive_values }) -> {
  let module_span = match recursive_values {
    | Nil -> empty_module_span
    | Cons(LambdaValueDeclaration { span, binding: _, scheme: _, body: _ }, _) -> span
  };
  let initializer_identity = NativeModuleInitializer(module_identity);
  let direct_functions = direct_function_bindings recursive_values;
  let initial_builder = new_function_builder initializer_identity None direct_functions
    substitutions native_unit_type module_span;
  match (lower_module_declarations recursive_values initial_builder) {
    | Err(message) -> Err(message)
    | Ok(initialized_builder) -> match (emit_constant native_unit_type LambdaUnit
        module_span initialized_builder) {
      | Err(message) -> Err(message)
      | Ok(NativeLoweredValue { value: unit_value, checked_type: _, builder: unit_builder }) ->
        match (emit_return unit_value unit_builder) {
          | Err(message) -> Err(message)
          | Ok(terminated_builder) -> {
            let generated_functions = builder_generated_functions terminated_builder;
            match (finish_function_builder Nil terminated_builder) {
              | Err(message) -> Err(message)
              | Ok(initializer_function) -> Ok(NativeModule {
                module_identity: module_identity,
                nominal_types: nominal_types,
                substitutions: substitutions,
                globals: lower_native_globals recursive_values,
                initializer: initializer_identity,
                functions: append_native_functions
                  (Cons(initializer_function, Nil)) generated_functions
              })
            }
          }
        }
    }
  }
}

let binding_is_in = fn wanted -> fn bindings -> match bindings {
  | Nil -> false
  | Cons(binding, remaining) -> match (same_binding wanted binding) {
    | true -> true
    | false -> binding_is_in wanted remaining
  }
}

let find_direct_binding_by_name = fn wanted_name -> fn bindings -> match bindings {
  | Nil -> None
  | Cons(binding, remaining) -> {
    let BindingIdentity { name: ValueName(name), source_path: _,
      declaration_span: _, scope: _ } = binding;
    match (equal wanted_name name) {
      | true -> Some(binding)
      | false -> find_direct_binding_by_name wanted_name remaining
    }
  }
}

let find_top_level_lambda = fn wanted -> fn declarations -> match declarations {
  | Nil -> None
  | Cons(LambdaValueDeclaration {
      binding, scheme: _,
      body: LambdaExpression { checked_type, kind: LambdaFunction(function_expression), span },
      span: _
    }, remaining) -> match (same_binding wanted binding) {
    | true -> Some((function_expression, checked_type, span))
    | false -> find_top_level_lambda wanted remaining
  }
  | Cons(_, remaining) -> find_top_level_lambda wanted remaining
}

let native_direct_target = fn operation -> match operation {
  | NativeCallOperation(NativeCall {
      target: NativeDirectCall(NativeTopLevelFunction(binding)), arguments: _,
      argument_types: _, result_type: _
    }) -> Some(binding)
  | _ -> None
}

let native_instruction_targets = fn instructions -> match instructions {
  | Nil -> Nil
  | Cons(NativeInstruction { operation, result: _, result_type: _, effects: _, span: _ },
      remaining) -> match (native_direct_target operation) {
    | None -> native_instruction_targets remaining
    | Some(binding) -> Cons(binding, native_instruction_targets remaining)
  }
}

let append_binding_ids = fn first -> fn second -> match first {
  | Nil -> second
  | Cons(binding, remaining) -> Cons(binding, append_binding_ids remaining second)
}

let native_block_targets = fn blocks -> match blocks {
  | Nil -> Nil
  | Cons(NativeBlock { instructions, identity: _, parameters: _, terminator: _, span: _ },
      remaining) -> append_binding_ids (native_instruction_targets instructions)
      (native_block_targets remaining)
}

let native_function_targets = fn functions -> match functions {
  | Nil -> Nil
  | Cons(NativeFunction { blocks, identity: _, result_type: _, entry: _, span: _ }, remaining) ->
    append_binding_ids (native_block_targets blocks) (native_function_targets remaining)
}

let append_unseen_direct_targets = fn targets -> fn direct_functions ->
  fn compiled_bindings -> fn pending -> match targets {
  | Nil -> pending
  | Cons(binding, remaining) -> match (binding_is_in binding direct_functions) {
    | false -> append_unseen_direct_targets remaining direct_functions compiled_bindings pending
    | true -> match (binding_is_in binding compiled_bindings) {
      | true -> append_unseen_direct_targets remaining direct_functions compiled_bindings pending
      | false -> match (binding_is_in binding pending) {
        | true -> append_unseen_direct_targets remaining direct_functions compiled_bindings pending
        | false -> append_unseen_direct_targets remaining direct_functions compiled_bindings
          (Cons(binding, pending))
      }
    }
  }
}

let lower_reachable_functions = fn pending -> fn declarations ->
  fn direct_functions -> fn substitutions -> fn compiled_bindings ->
  fn accumulated_functions -> match pending {
  | Nil -> Ok(accumulated_functions)
  | Cons(binding, remaining) -> match (binding_is_in binding compiled_bindings) {
    | true -> lower_reachable_functions remaining declarations direct_functions
        substitutions compiled_bindings accumulated_functions
    | false -> match (find_top_level_lambda binding declarations) {
      | None -> Err("NativeIR: direct-call target has no top-level lambda declaration")
      | Some((function_expression, checked_type, span)) ->
        match (compile_function_body (NativeTopLevelFunction(binding)) binding
            direct_functions substitutions function_expression checked_type span Nil) {
          | Err(message) -> Err(message)
          | Ok(NativeCompiledFunction {
              function: _, nested_functions, captures: _
            }) -> {
            let next_compiled = Cons(binding, compiled_bindings);
            let next_pending = append_unseen_direct_targets
              (native_function_targets nested_functions) direct_functions
              next_compiled remaining;
            lower_reachable_functions next_pending declarations direct_functions
              substitutions next_compiled
              (append_native_functions accumulated_functions nested_functions)
          }
        }
    }
  }
}

let module_values_have_closure_only_initialization = fn declarations -> match declarations {
  | Nil -> true
  | Cons(LambdaValueDeclaration {
      binding: _, scheme: _, span: _,
      body: LambdaExpression { kind: LambdaFunction(_), checked_type: _, span: _ }
    }, remaining) -> module_values_have_closure_only_initialization remaining
  | _ -> false
}

let lower_module_for_entry = fn module_identity ->
  fn (LambdaModule { nominal_types, substitutions, recursive_values }) ->
  fn entry_name -> match (module_values_have_closure_only_initialization recursive_values) {
  | false -> Err("NativeIR: module initializer has non-closure work; direct-call graph mode cannot omit it")
  | true -> {
    let direct_functions = direct_function_bindings recursive_values;
    match (find_direct_binding_by_name entry_name direct_functions) {
      | None -> Err("NativeIR: entry is not a monomorphic top-level function")
      | Some(entry_binding) -> match (lower_reachable_functions
          (Cons(entry_binding, Nil)) recursive_values direct_functions substitutions
          Nil Nil) {
        | Err(message) -> Err(message)
        | Ok(lowered_functions) -> match (lower_module module_identity
            (LambdaModule {
              nominal_types: nominal_types,
              substitutions: substitutions,
              recursive_values: Nil
            })) {
          | Err(message) -> Err(message)
          | Ok(NativeModule {
              module_identity: _, nominal_types: _, substitutions: _, globals: _,
              initializer, functions: initializer_functions
            }) -> Ok(NativeModule {
              module_identity: module_identity,
              nominal_types: nominal_types,
              substitutions: substitutions,
              globals: lower_native_globals recursive_values,
              initializer: initializer,
              functions: append_native_functions initializer_functions lowered_functions
            })
        }
      }
    }
  }
}
