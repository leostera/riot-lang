// Structural, SSA, type, and effect checks for target-independent NativeIR.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use String.{equal}
use Syntax.Ast.{
  ValueName, ConstructorName, TypeName, TypeDeclarationData, TypeAlias,
  TypeRecord, TypeVariants, TupleVariant, TupleVariantData, RecordVariant,
  RecordVariantData, RecordTypeField, BinaryOperator, Addition, Subtraction,
  Multiplication,
  Division, Remainder, Equal, NotEqual, LessThan, LessThanOrEqual,
  GreaterThan, GreaterThanOrEqual
}
use Syntax.Lexer.{SourceSpan}
use Check.Inference.{
  InferenceType, FreeVariable, GenericVariable, AppliedType, InferredNamedType,
  FunctionType, InferredFunctionType, InferredTuple, InferredRecord, InferredVariant,
  InferredRecordField, InferredVariantCase, TypeSubstitution,
  TypeVisibility, VisibleType, VisibleTypeData, apply_substitutions,
  BindingIdentity, BindingScope, ModuleValueBinding, LocalValueBinding,
  ConstructorIdentity
}
use NativeIr.Print.{print_function_id, print_type, block_id, print_constructor_identity}
use NativeIr.Ir.{
  NativeModule, NativeGlobal, NativeFunctionId, NativeTopLevelFunction,
  NativeNestedFunction, NativeNestedFunctionId, NativeModuleInitializer,
  NativeFunction, NativeBlockId, NativeValueId, NativeBlock,
  NativeBlockParameter, NativeInstruction, NativeOperation, NativeConstant,
  NativeUnit, NativeInteger, NativeBoolean, NativeString,
  NativePrimitive, NativePrimitiveOperation, NativePrimitiveBinary,
  NativePrimitiveNegate, NativeBinaryPrimitive, NativeNegatePrimitive,
  NativeCallOperation, NativeCall, NativeClosureCall, NativeCallTarget,
  NativeDirectCall, NativeHostCall, NativeCreateClosure, NativeClosure,
  NativeCreateAggregate, NativeAggregate, NativeAggregateValue, NativeAggregateKind,
  NativeTupleAggregate, NativeRecordAggregate, NativeVariantAggregate,
  NativeProjectAggregate, NativeProjection, NativeAggregateField,
  NativeTupleField, NativeRecordField, NativeConstructorField,
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
  NativeSwitchCase, NativeReturn, NativeTrap, NativeUnreachable, NativeTerminator
}

type NativeDefinition = NativeDefinition {
  value: NativeValueId,
  block: NativeBlockId,
  position: i32,
  checked_type: InferenceType
}

type NativeDominatorSet = NativeDominatorSet {
  block: NativeBlockId,
  dominators: List<NativeBlockId>
}

type NativeDominanceContext = NativeDominanceContext {
  entry: NativeBlockId,
  blocks: List<NativeBlock>
}

type NativeFunctionLookup = NativeFunctionLookup {
  function: NativeFunction,
  found: bool
}

type NativeGenericInstantiation = NativeGenericInstantiation {
  variable: i32,
  is_free_variable: bool,
  checked_type: InferenceType
}

type NativeConstructorShape = NativeConstructorShape {
  type_identity: string,
  type_parameter_count: i32,
  field_count: i32,
  field_names: List<ValueName>
}

let both = fn first -> fn second -> match first {
  | false -> false
  | true -> second
}

let either = fn first -> fn second -> match first {
  | true -> true
  | false -> second
}

let value_id_number = fn (NativeValueId(number)) -> number
let block_id_number = fn (NativeBlockId(number)) -> number

let same_value_id = fn first -> fn second ->
  value_id_number first == value_id_number second

let same_block_id = fn first -> fn second ->
  block_id_number first == block_id_number second

let same_value_name = fn first -> fn second -> {
  let ValueName(first_name) = first;
  let ValueName(second_name) = second;
  equal first_name second_name
}

let same_constructor_name = fn first -> fn second -> {
  let ConstructorName(first_name) = first;
  let ConstructorName(second_name) = second;
  equal first_name second_name
}

let same_binding = fn first -> fn second -> {
  let BindingIdentity { source_path, declaration_span, name, scope } = first;
  let BindingIdentity {
    source_path: other_path, declaration_span: other_span,
    name: other_name, scope: other_scope
  } = second;
  match (equal source_path other_path) {
    | false -> false
    | true -> match (same_value_name name other_name) {
      | false -> false
      | true -> match (same_span declaration_span other_span) {
        | false -> false
        | true -> match scope {
          | ModuleValueBinding -> match other_scope {
            | ModuleValueBinding -> true
            | LocalValueBinding -> false
          }
          | LocalValueBinding -> match other_scope {
            | ModuleValueBinding -> false
            | LocalValueBinding -> true
          }
        }
      }
    }
  }
}

let same_span = fn first -> fn second -> {
  let SourceSpan { start_codepoint: first_start, end_codepoint: first_end } = first;
  let SourceSpan { start_codepoint: second_start, end_codepoint: second_end } = second;
  both (first_start == second_start) (first_end == second_end)
}

let same_function_id = fn first -> fn second -> match first {
  | NativeTopLevelFunction(first_binding) -> match second {
    | NativeTopLevelFunction(second_binding) -> same_binding first_binding second_binding
    | _ -> false
  }
  | NativeNestedFunction(NativeNestedFunctionId {
      parent: first_parent, source_span: first_span, ordinal: first_ordinal
    }) -> match second {
    | NativeNestedFunction(NativeNestedFunctionId {
        parent: second_parent, source_span: second_span, ordinal: second_ordinal
      }) -> both (same_binding first_parent second_parent)
        (both (same_span first_span second_span) (first_ordinal == second_ordinal))
    | _ -> false
  }
  | NativeModuleInitializer(first_path) -> match second {
    | NativeModuleInitializer(second_path) -> equal first_path second_path
    | _ -> false
  }
}

let same_type_list = fn first -> fn second -> fn substitutions -> match first {
  | Nil -> match second { | Nil -> true | _ -> false }
  | Cons(first_type, first_remaining) -> match second {
    | Nil -> false
    | Cons(second_type, second_remaining) ->
      both (same_type first_type second_type substitutions)
        (same_type_list first_remaining second_remaining substitutions)
  }
}

let same_variant_type_cases = fn first -> fn second -> fn substitutions -> match first {
  | Nil -> match second { | Nil -> true | _ -> false }
  | Cons(InferredVariantCase { name: first_name, fields: first_fields }, first_remaining) ->
    match second {
      | Nil -> false
      | Cons(InferredVariantCase { name: second_name, fields: second_fields }, second_remaining) ->
        both (same_constructor_name first_name second_name)
          (both (same_type_list first_fields second_fields substitutions)
            (same_variant_type_cases first_remaining second_remaining substitutions))
    }
}

let same_record_type_fields = fn first -> fn second -> fn substitutions -> match first {
  | Nil -> match second { | Nil -> true | _ -> false }
  | Cons(InferredRecordField { name: first_name, field_type: first_type }, first_remaining) ->
    match second {
      | Nil -> false
      | Cons(InferredRecordField { name: second_name, field_type: second_type }, second_remaining) ->
        both (same_value_name first_name second_name)
          (both (same_type first_type second_type substitutions)
            (same_record_type_fields first_remaining second_remaining substitutions))
    }
}

let same_type = fn first -> fn second -> fn substitutions ->
  match (apply_substitutions first substitutions) {
    | FreeVariable(first_id) -> match (apply_substitutions second substitutions) {
      | FreeVariable(second_id) -> first_id == second_id
      | _ -> false
    }
    | GenericVariable(first_id) -> match (apply_substitutions second substitutions) {
      | GenericVariable(second_id) -> first_id == second_id
      | _ -> false
    }
    | AppliedType(InferredNamedType { name: first_name, arguments: first_arguments }) ->
      match (apply_substitutions second substitutions) {
        | AppliedType(InferredNamedType { name: second_name, arguments: second_arguments }) ->
          both (equal first_name second_name)
            (same_type_list first_arguments second_arguments substitutions)
        | _ -> false
      }
    | FunctionType(InferredFunctionType { argument: first_argument, result: first_result }) ->
      match (apply_substitutions second substitutions) {
        | FunctionType(InferredFunctionType { argument: second_argument, result: second_result }) ->
          both (same_type first_argument second_argument substitutions)
            (same_type first_result second_result substitutions)
        | _ -> false
      }
    | InferredTuple(first_items) -> match (apply_substitutions second substitutions) {
      | InferredTuple(second_items) -> same_type_list first_items second_items substitutions
      | _ -> false
    }
    | InferredRecord(first_fields) -> match (apply_substitutions second substitutions) {
      | InferredRecord(second_fields) ->
        same_record_type_fields first_fields second_fields substitutions
      | _ -> false
    }
    | InferredVariant(first_cases) -> match (apply_substitutions second substitutions) {
      | InferredVariant(second_cases) ->
        same_variant_type_cases first_cases second_cases substitutions
      | _ -> false
    }
  }

let same_checked_type = fn first -> fn second -> fn substitutions ->
  same_type first second substitutions

let type_is_named = fn checked_type -> fn wanted_name -> fn substitutions ->
  match (apply_substitutions checked_type substitutions) {
    | AppliedType(InferredNamedType { name, arguments: Nil }) -> equal name wanted_name
    | _ -> false
  }

let contains_value = fn wanted -> fn values -> match values {
  | Nil -> false
  | Cons(value, remaining) -> either (same_value_id wanted value)
      (contains_value wanted remaining)
}

let contains_block = fn wanted -> fn blocks -> match blocks {
  | Nil -> false
  | Cons(block, remaining) -> either (same_block_id wanted block)
      (contains_block wanted remaining)
}

let add_definition = fn definition -> fn definitions ->
  match (find_definition (definition_value definition) definitions) {
    | Some(_) -> Err("NativeIR verifier: SSA value is defined more than once")
    | None -> Ok(Cons(definition, definitions))
  }

let definition_value = fn (NativeDefinition { value, block: _, position: _, checked_type: _ }) -> value

let find_definition = fn wanted -> fn definitions -> match definitions {
  | Nil -> None
  | Cons(definition, remaining) -> match (same_value_id wanted (definition_value definition)) {
    | true -> Some(definition)
    | false -> find_definition wanted remaining
  }
}

let collect_parameter_definitions = fn parameters -> fn block_id -> fn definitions ->
  fn position -> match parameters {
    | Nil -> Ok(definitions)
    | Cons(NativeBlockParameter { value, checked_type, span: _ }, remaining) ->
      match (add_definition (NativeDefinition {
        value: value, block: block_id, position: position, checked_type: checked_type
      }) definitions) {
        | Err(message) -> Err(message)
        | Ok(next_definitions) -> collect_parameter_definitions remaining block_id
            next_definitions position
      }
  }

let collect_instruction_definitions = fn instructions -> fn block_id -> fn definitions ->
  fn position -> match instructions {
    | Nil -> Ok(definitions)
    | Cons(NativeInstruction { result, result_type, operation: _, effects: _, span: _ }, remaining) ->
      match (add_definition (NativeDefinition {
        value: result, block: block_id, position: position, checked_type: result_type
      }) definitions) {
        | Err(message) -> Err(message)
        | Ok(next_definitions) -> collect_instruction_definitions remaining block_id
            next_definitions (position + 1)
      }
  }

let collect_block_definitions = fn (NativeBlock {
  identity, parameters, instructions, terminator: _, span: _
}) -> fn definitions -> match (collect_parameter_definitions
    parameters identity definitions 0) {
  | Err(message) -> Err(message)
  | Ok(parameter_definitions) -> collect_instruction_definitions
      instructions identity parameter_definitions 1
}

let collect_function_definitions = fn blocks -> fn definitions -> match blocks {
  | Nil -> Ok(definitions)
  | Cons(block, remaining) -> match (collect_block_definitions block definitions) {
    | Err(message) -> Err(message)
    | Ok(next_definitions) -> collect_function_definitions remaining next_definitions
  }
}

let find_block = fn wanted -> fn blocks -> match blocks {
  | Nil -> None
  | Cons(block, remaining) -> match (same_block_id wanted (block_identity block)) {
    | true -> Some(block)
    | false -> find_block wanted remaining
  }
}

let block_identity = fn (NativeBlock {
  identity, parameters: _, instructions: _, terminator: _, span: _
}) -> identity

let block_ids = fn blocks -> match blocks {
  | Nil -> Nil
  | Cons(block, remaining) -> Cons(block_identity block, block_ids remaining)
}

let print_block_ids = fn blocks -> match blocks {
  | Nil -> ""
  | Cons(block, remaining) -> match remaining {
    | Nil -> block_id (block_identity block)
    | _ -> block_id (block_identity block) + ", " + print_block_ids remaining
  }
}

let edge_target = fn (NativeEdge { target, arguments: _ }) -> target

let edge_targets_for_terminator = fn terminator -> match terminator {
  | NativeJump(edge) -> Cons(edge_target edge, Nil)
  | NativeConditionalBranch(NativeBranch { condition: _, when_true, when_false }) ->
    Cons(edge_target when_true, Cons(edge_target when_false, Nil))
  | NativeSwitchTerminator(NativeSwitch { value: _, cases, default }) ->
    Cons(edge_target default, switch_case_targets cases)
  | NativeReturn(_) -> Nil
  | NativeTrap(_) -> Nil
  | NativeUnreachable -> Nil
}

let switch_case_targets = fn cases -> match cases {
  | Nil -> Nil
  | Cons(NativeSwitchCase { test: _, edge }, remaining) ->
    Cons(edge_target edge, switch_case_targets remaining)
}

let block_successors = fn (NativeBlock {
  identity: _, parameters: _, instructions: _, terminator, span: _
}) -> edge_targets_for_terminator terminator

let block_list_successors = fn blocks -> match blocks {
  | Nil -> Nil
  | Cons(block, remaining) -> append_ids (block_successors block)
      (block_list_successors remaining)
}

let append_ids = fn first -> fn second -> match first {
  | Nil -> second
  | Cons(value, remaining) -> Cons(value, append_ids remaining second)
}

let reachable_from = fn pending -> fn visited -> fn blocks -> match pending {
  | Nil -> visited
  | Cons(block_id, remaining) -> match (contains_block block_id visited) {
    | true -> reachable_from remaining visited blocks
    | false -> match (find_block block_id blocks) {
      | None -> reachable_from remaining (Cons(block_id, visited)) blocks
      | Some(block) -> reachable_from
          (append_ids (block_successors block) remaining)
          (Cons(block_id, visited)) blocks
    }
  }
}

let reachable_avoiding = fn blocked -> fn pending -> fn visited -> fn blocks ->
  match pending {
    | Nil -> visited
    | Cons(block_id, remaining) -> match (same_block_id block_id blocked) {
      | true -> reachable_avoiding blocked remaining visited blocks
      | false -> match (contains_block block_id visited) {
        | true -> reachable_avoiding blocked remaining visited blocks
        | false -> match (find_block block_id blocks) {
          | None -> reachable_avoiding blocked remaining (Cons(block_id, visited)) blocks
          | Some(block) -> reachable_avoiding blocked
              (append_ids (block_successors block) remaining)
              (Cons(block_id, visited)) blocks
        }
      }
    }
  }

let definition_dominates_use = fn definition_block -> fn use_block ->
  fn (NativeDominanceContext { entry, blocks }) -> match (same_block_id definition_block entry) {
    | true -> true
    | false -> match (contains_block use_block
        (reachable_avoiding definition_block (Cons(entry, Nil)) Nil blocks)) {
      | true -> false
      | false -> true
    }
  }

let verify_all_blocks_reachable = fn blocks -> fn reachable -> match blocks {
  | Nil -> Ok(true)
  | Cons(block, remaining) -> match (contains_block (block_identity block) reachable) {
    | false -> Err("NativeIR verifier: function contains an unreachable block")
    | true -> verify_all_blocks_reachable remaining reachable
  }
}

let predecessor_ids = fn wanted -> fn blocks -> match blocks {
  | Nil -> Nil
  | Cons(block, remaining) -> match (contains_block wanted (block_successors block)) {
    | true -> Cons(block_identity block, predecessor_ids wanted remaining)
    | false -> predecessor_ids wanted remaining
  }
}

let find_dominators = fn wanted -> fn sets -> match sets {
  | Nil -> None
  | Cons(NativeDominatorSet { block, dominators }, remaining) ->
    match (same_block_id wanted block) {
      | true -> Some(dominators)
      | false -> find_dominators wanted remaining
    }
}

let is_subset_ids = fn first -> fn second -> match first {
  | Nil -> true
  | Cons(value, remaining) -> both (contains_block value second)
      (is_subset_ids remaining second)
}

let equal_id_sets = fn first -> fn second ->
  both (is_subset_ids first second) (is_subset_ids second first)

let intersect_ids = fn first -> fn others -> match first {
  | Nil -> Nil
  | Cons(value, remaining) -> match (all_sets_contain value others) {
    | true -> Cons(value, intersect_ids remaining others)
    | false -> intersect_ids remaining others
  }
}

let all_sets_contain = fn wanted -> fn sets -> match sets {
  | Nil -> true
  | Cons(values, remaining) -> both (contains_block wanted values)
      (all_sets_contain wanted remaining)
}

let dominator_lists_for = fn predecessors -> fn sets -> match predecessors {
  | Nil -> Nil
  | Cons(predecessor, remaining) -> match (find_dominators predecessor sets) {
    | None -> Nil
    | Some(dominators) -> Cons(dominators, dominator_lists_for remaining sets)
  }
}

let intersect_predecessor_dominators = fn predecessors -> fn sets ->
  match (dominator_lists_for predecessors sets) {
    | Nil -> Nil
    | Cons(first, remaining) -> intersect_ids first remaining
  }

let compute_dominator_pass = fn blocks -> fn entry -> fn block_ids -> fn old_sets ->
  match blocks {
    | Nil -> Nil
    | Cons(block, remaining) -> {
      let identity = block_identity block;
      let next_dominators = match (same_block_id identity entry) {
        | true -> Cons(entry, Nil)
        | false -> Cons(identity, intersect_predecessor_dominators
            (predecessor_ids identity blocks) old_sets)
      };
      Cons(NativeDominatorSet {
        block: identity,
        dominators: next_dominators
      }, compute_dominator_pass remaining entry block_ids old_sets)
    }
  }

let dominator_pass_is_stable = fn first -> fn second -> match first {
  | Nil -> match second { | Nil -> true | _ -> false }
  | Cons(NativeDominatorSet { block: first_block, dominators: first_dominators }, remaining) ->
    match (find_dominators first_block second) {
      | None -> false
      | Some(second_dominators) -> both
          (equal_id_sets first_dominators second_dominators)
          (dominator_pass_is_stable remaining second)
    }
}

let initial_dominator_sets = fn block_ids -> fn entry -> match block_ids {
  | Nil -> Nil
  | Cons(block, remaining) -> Cons(NativeDominatorSet {
    block: block,
    dominators: match (same_block_id block entry) {
      | true -> Cons(entry, Nil)
      | false -> block_ids
    }
  }, initial_dominator_sets remaining entry)
}

let fixed_point_dominators = fn blocks -> fn entry -> fn block_ids -> fn previous -> {
  let next = compute_dominator_pass blocks entry block_ids previous;
  match (dominator_pass_is_stable previous next) {
    | true -> next
    | false -> fixed_point_dominators blocks entry block_ids next
  }
}

let find_global = fn wanted -> fn globals -> match globals {
  | Nil -> None
  | Cons(NativeGlobal { binding, scheme: _, span: _ }, remaining) ->
    match (same_binding wanted binding) {
      | true -> Some(true)
      | false -> find_global wanted remaining
    }
}

let find_function = fn wanted -> fn functions -> match functions {
  | Nil -> None
  | Cons(function, remaining) -> match (same_function_id wanted
      (native_function_identity function)) {
    | true -> Some(function)
    | false -> find_function wanted remaining
  }
}

let native_function_identity = fn (NativeFunction {
  identity, result_type: _, entry: _, blocks: _, span: _
}) -> identity

let definition_type = fn (NativeDefinition {
  value: _, block: _, position: _, checked_type
}) -> checked_type

let definition_block = fn (NativeDefinition {
  value: _, block, position: _, checked_type: _
}) -> block

let definition_position = fn (NativeDefinition {
  value: _, block: _, position, checked_type: _
}) -> position

let verify_use = fn value -> fn use_block -> fn use_position ->
  fn definitions -> fn dominators -> match (find_definition value definitions) {
    | None -> Err("NativeIR verifier: use refers to an undefined SSA value")
    | Some(definition) -> match (same_block_id use_block (definition_block definition)) {
      | true -> match (definition_position definition < use_position) {
        | true -> Ok(true)
        | false -> Err("NativeIR verifier: SSA value is used before its definition")
      }
      | false -> match (definition_dominates_use
          (definition_block definition) use_block dominators) {
        | true -> Ok(true)
        | false -> Err("NativeIR verifier: SSA definition does not dominate its use")
      }
    }
  }

let verify_uses = fn values -> fn use_block -> fn use_position ->
  fn definitions -> fn dominators -> match values {
    | Nil -> Ok(true)
    | Cons(value, remaining) -> match (verify_use value use_block use_position
        definitions dominators) {
      | Err(message) -> Err(message)
      | Ok(_) -> verify_uses remaining use_block use_position definitions dominators
    }
  }

let append_optional_target = fn target -> fn values -> match target {
  | None -> values
  | Some(value) -> Cons(value, values)
}

let aggregate_operand_values = fn fields -> match fields {
  | Nil -> Nil
  | Cons(NativeAggregateValue { field: _, value }, remaining) ->
    Cons(value, aggregate_operand_values remaining)
}

let operation_operands = fn operation -> match operation {
  | NativeConstant(_) -> Nil
  | NativePrimitiveOperation(NativePrimitiveBinary(NativeBinaryPrimitive {
      operator: _, operand_type: _, left, right
    })) -> Cons(left, Cons(right, Nil))
  | NativePrimitiveOperation(NativePrimitiveNegate(NativeNegatePrimitive {
      operand_type: _, operand
    })) -> Cons(operand, Nil)
  | NativeCallOperation(NativeCall { target, arguments, argument_types: _, result_type: _ }) ->
    match target {
      | NativeClosureCall(callee) -> Cons(callee, arguments)
      | NativeDirectCall(_) -> arguments
      | NativeHostCall(_) -> arguments
    }
  | NativeCreateClosure(NativeClosure { function: _, captures }) -> captures
  | NativeCreateAggregate(NativeAggregate { kind: _, fields }) ->
    aggregate_operand_values fields
  | NativeProjectAggregate(NativeProjection { aggregate, field: _ }) -> Cons(aggregate, Nil)
  | NativeTestValue(NativeValueTest { value, test: _ }) -> Cons(value, Nil)
  | NativeLoadCapture(_) -> Nil
  | NativeLoadGlobal(_) -> Nil
  | NativeStoreGlobal(NativeGlobalStore { binding: _, value }) -> Cons(value, Nil)
}

let definition_checked_type = fn value -> fn definitions -> match (find_definition value definitions) {
  | None -> None
  | Some(definition) -> Some(definition_type definition)
}

let effect_set_values = fn (NativeEffectSet(effects)) -> effects

let same_trap_kind = fn first -> fn second -> match first {
  | NativeDivisionByZero -> match second { | NativeDivisionByZero -> true | _ -> false }
  | NativeRemainderByZero -> match second { | NativeRemainderByZero -> true | _ -> false }
  | NativeArithmeticOverflow -> match second { | NativeArithmeticOverflow -> true | _ -> false }
  | NativePatternFailure -> match second { | NativePatternFailure -> true | _ -> false }
  | NativeExplicitTrap -> match second { | NativeExplicitTrap -> true | _ -> false }
}

let same_effect = fn first -> fn second -> match first {
  | NativeMayTrap(kind) -> match second {
    | NativeMayTrap(other_kind) -> same_trap_kind kind other_kind
    | _ -> false
  }
  | NativeAllocate -> match second { | NativeAllocate -> true | _ -> false }
  | NativeReadMutableState -> match second { | NativeReadMutableState -> true | _ -> false }
  | NativeWriteMutableState -> match second { | NativeWriteMutableState -> true | _ -> false }
  | NativeHostRead -> match second { | NativeHostRead -> true | _ -> false }
  | NativeHostWrite -> match second { | NativeHostWrite -> true | _ -> false }
  | NativeHostProcess -> match second { | NativeHostProcess -> true | _ -> false }
  | NativeUnknownCall -> match second { | NativeUnknownCall -> true | _ -> false }
}

let effect_set_contains = fn wanted -> fn effects -> match effects {
  | Nil -> false
  | Cons(effect, remaining) -> either (same_effect wanted effect)
      (effect_set_contains wanted remaining)
}

let require_effect = fn wanted -> fn effects -> match (effect_set_contains wanted
    (effect_set_values effects)) {
  | true -> Ok(true)
  | false -> Err("NativeIR verifier: operation is missing a required effect")
}

let same_value_type = fn value -> fn expected -> fn definitions -> fn substitutions ->
  match (definition_checked_type value definitions) {
    | None -> Err("NativeIR verifier: typed operand has no definition")
    | Some(actual) -> match (same_checked_type actual expected substitutions) {
      | true -> Ok(true)
      | false -> Err("NativeIR verifier: operand type " + print_type actual
          + " does not match expected " + print_type expected)
    }
  }

let verify_edge_argument_types = fn arguments -> fn parameters -> fn definitions ->
  fn substitutions -> match arguments {
    | Nil -> match parameters {
      | Nil -> Ok(true)
      | _ -> Err("NativeIR verifier: edge argument count does not match block parameters")
    }
    | Cons(argument, remaining_arguments) -> match parameters {
      | Nil -> Err("NativeIR verifier: edge argument count does not match block parameters")
      | Cons(NativeBlockParameter { value: _, checked_type, span: _ }, remaining_parameters) ->
        match (same_value_type argument checked_type definitions substitutions) {
          | Err(message) -> Err(message)
          | Ok(_) -> verify_edge_argument_types remaining_arguments remaining_parameters
              definitions substitutions
        }
    }
  }

let verify_edge = fn (NativeEdge { target, arguments }) -> fn blocks ->
  fn use_block -> fn use_position -> fn definitions -> fn dominators ->
  fn substitutions -> match (find_block target blocks) {
    | None -> Err("NativeIR verifier: edge from " + block_id use_block
        + " targets missing " + block_id target + " among [" + print_block_ids blocks + "]")
    | Some(NativeBlock { identity: _, parameters, instructions: _, terminator: _, span: _ }) ->
      match (verify_uses arguments use_block use_position definitions dominators) {
        | Err(message) -> Err(message)
        | Ok(_) -> verify_edge_argument_types arguments parameters definitions substitutions
      }
  }

let verify_switch_cases = fn cases -> fn default -> fn blocks -> fn use_block ->
  fn use_position -> fn definitions -> fn dominators -> fn substitutions -> match cases {
    | Nil -> verify_edge default blocks use_block use_position definitions dominators substitutions
    | Cons(NativeSwitchCase { test: _, edge }, remaining) ->
      match (verify_edge edge blocks use_block use_position definitions dominators substitutions) {
        | Err(message) -> Err(message)
        | Ok(_) -> verify_switch_cases remaining default blocks use_block use_position
            definitions dominators substitutions
      }
  }

let verify_terminator = fn terminator -> fn result_type -> fn blocks ->
  fn use_block -> fn use_position -> fn definitions -> fn dominators ->
  fn substitutions -> match terminator {
    | NativeJump(edge) -> verify_edge edge blocks use_block use_position
        definitions dominators substitutions
    | NativeConditionalBranch(NativeBranch { condition, when_true, when_false }) ->
      match (same_value_type condition (AppliedType(InferredNamedType {
        name: "bool", arguments: Nil
      })) definitions substitutions) {
        | Err(message) -> Err(message)
        | Ok(_) -> match (verify_edge when_true blocks use_block use_position
            definitions dominators substitutions) {
          | Err(message) -> Err(message)
          | Ok(_) -> verify_edge when_false blocks use_block use_position
              definitions dominators substitutions
        }
      }
    | NativeSwitchTerminator(NativeSwitch { value, cases, default }) ->
      match (verify_use value use_block use_position definitions dominators) {
        | Err(message) -> Err(message)
        | Ok(_) -> verify_switch_cases cases default blocks use_block use_position
            definitions dominators substitutions
      }
    | NativeReturn(value) -> match (verify_use value use_block use_position
        definitions dominators) {
      | Err(message) -> Err(message)
      | Ok(_) -> match (same_value_type value result_type definitions substitutions) {
        | Ok(_) -> Ok(true)
        | Err(_) -> Err("NativeIR verifier: return value type does not match function result")
      }
    }
    | NativeTrap(_) -> Ok(true)
    | NativeUnreachable -> Ok(true)
  }

let verify_instruction_uses = fn instructions -> fn block -> fn position ->
  fn definitions -> fn dominators -> match instructions {
    | Nil -> Ok(true)
    | Cons(NativeInstruction {
        result: _, result_type: _, operation, effects: _, span: _
      }, remaining) -> match (verify_uses (operation_operands operation) block position
          definitions dominators) {
      | Err(message) -> Err(message)
      | Ok(_) -> verify_instruction_uses remaining block (position + 1)
          definitions dominators
    }
  }

let verify_block_terminators = fn blocks -> fn all_blocks -> fn result_type ->
  fn definitions -> fn dominators -> fn substitutions -> match blocks {
    | Nil -> Ok(true)
    | Cons(NativeBlock {
        identity, instructions, terminator, parameters: _, span: _
      }, remaining) -> match (verify_terminator terminator result_type all_blocks identity
          (instruction_count instructions + 1) definitions dominators substitutions) {
      | Err(message) -> Err(message)
      | Ok(_) -> verify_block_terminators remaining all_blocks result_type
          definitions dominators substitutions
    }
  }

let instruction_count = fn instructions -> match instructions {
  | Nil -> 0
  | Cons(_, remaining) -> 1 + instruction_count remaining
}

let verify_block_edges = fn blocks -> fn definitions -> fn dominators ->
  fn substitutions -> match blocks {
    | Nil -> Ok(true)
    | Cons(NativeBlock {
        identity, instructions, terminator: _, parameters: _, span: _
      }, remaining) -> match (verify_instruction_uses instructions identity 1 definitions dominators) {
      | Err(message) -> Err(message)
      | Ok(_) -> verify_block_edges remaining definitions dominators substitutions
    }
  }

let require_if = fn condition -> fn effect -> fn effects -> match condition {
  | false -> Ok(true)
  | true -> require_effect effect effects
}

let effects_have_host = fn effects -> {
  let items = effect_set_values effects;
  either (effect_set_contains NativeHostRead items)
    (either (effect_set_contains NativeHostWrite items)
      (effect_set_contains NativeHostProcess items))
}

let verify_operation_effects = fn operation -> fn result_type -> fn effects ->
  fn substitutions -> match operation {
    | NativeConstant(_) -> Ok(true)
    | NativePrimitiveOperation(NativePrimitiveBinary(NativeBinaryPrimitive {
        operator: Division, operand_type: _, left: _, right: _
      })) -> require_effect (NativeMayTrap(NativeDivisionByZero)) effects
    | NativePrimitiveOperation(NativePrimitiveBinary(NativeBinaryPrimitive {
        operator: Remainder, operand_type: _, left: _, right: _
      })) -> require_effect (NativeMayTrap(NativeRemainderByZero)) effects
    | NativePrimitiveOperation(NativePrimitiveBinary(NativeBinaryPrimitive {
        operator: Addition, operand_type, left: _, right: _
      })) -> require_if (type_is_named operand_type "string" substitutions)
          NativeAllocate effects
    | NativePrimitiveOperation(_) -> Ok(true)
    | NativeCallOperation(NativeCall { target: NativeHostCall(_), arguments: _,
        argument_types: _, result_type: _ }) -> match (effects_have_host effects) {
      | true -> Ok(true)
      | false -> Err("NativeIR verifier: Host call is missing a Host effect")
    }
    | NativeCallOperation(_) -> require_effect NativeUnknownCall effects
    | NativeCreateClosure(_) -> require_effect NativeAllocate effects
    | NativeCreateAggregate(_) -> require_effect NativeAllocate effects
    | NativeProjectAggregate(_) -> Ok(true)
    | NativeTestValue(_) -> Ok(true)
    | NativeLoadCapture(_) -> Ok(true)
    | NativeLoadGlobal(_) -> require_effect NativeReadMutableState effects
    | NativeStoreGlobal(_) -> require_effect NativeWriteMutableState effects
  }

let value_at = fn index -> fn values -> match values {
  | Nil -> None
  | Cons(value, remaining) -> match (index == 0) {
    | true -> Some(value)
    | false -> value_at (index - 1) remaining
  }
}

let list_length = fn values -> match values {
  | Nil -> 0
  | Cons(_, remaining) -> 1 + list_length remaining
}

let aggregate_value_count = fn fields -> match fields {
  | Nil -> 0
  | Cons(_, remaining) -> 1 + aggregate_value_count remaining
}

let inference_type_count = fn types -> match types {
  | Nil -> 0
  | Cons(_, remaining) -> 1 + inference_type_count remaining
}

let value_name_count = fn names -> match names {
  | Nil -> 0
  | Cons(_, remaining) -> 1 + value_name_count remaining
}

let record_field_type = fn name -> fn fields -> match fields {
  | Nil -> None
  | Cons(InferredRecordField { name: field_name, field_type }, remaining) ->
    match (same_value_name name field_name) {
      | true -> Some(field_type)
      | false -> record_field_type name remaining
    }
}

let type_at = fn index -> fn types -> match types {
  | Nil -> None
  | Cons(checked_type, remaining) -> match (index == 0) {
    | true -> Some(checked_type)
    | false -> type_at (index - 1) remaining
  }
}

let verify_tuple_aggregate_fields = fn fields -> fn types -> fn definitions ->
  fn substitutions -> fn index -> match fields {
    | Nil -> match types {
      | Nil -> Ok(true)
      | _ -> Err("NativeIR verifier: tuple aggregate arity mismatch")
    }
    | Cons(NativeAggregateValue { field, value }, remaining_fields) -> match types {
      | Nil -> Err("NativeIR verifier: tuple aggregate arity mismatch")
      | Cons(expected_type, remaining_types) -> match field {
        | NativeTupleField(field_index) -> match (field_index == index) {
          | false -> Err("NativeIR verifier: tuple aggregate field index is not canonical")
          | true -> match (same_value_type value expected_type definitions substitutions) {
            | Err(message) -> Err(message)
            | Ok(_) -> verify_tuple_aggregate_fields remaining_fields remaining_types
                definitions substitutions (index + 1)
          }
        }
        | _ -> Err("NativeIR verifier: tuple aggregate has a non-tuple field key")
      }
    }
  }

let function_result_type = fn (NativeFunction {
  identity: _, result_type, entry: _, blocks: _, span: _
}) -> result_type

let function_entry = fn (NativeFunction {
  identity: _, result_type: _, entry, blocks: _, span: _
}) -> entry

let function_blocks = fn (NativeFunction {
  identity: _, result_type: _, entry: _, blocks, span: _
}) -> blocks

let block_parameters = fn (NativeBlock {
  identity: _, parameters, instructions: _, terminator: _, span: _
}) -> parameters

let verify_call_argument_types = fn arguments -> fn argument_types -> fn definitions ->
  fn substitutions -> match arguments {
    | Nil -> match argument_types {
      | Nil -> Ok(true)
      | _ -> Err("NativeIR verifier: call argument type count mismatch")
    }
    | Cons(argument, remaining_arguments) -> match argument_types {
      | Nil -> Err("NativeIR verifier: call argument type count mismatch")
      | Cons(expected_type, remaining_types) -> match (same_value_type
          argument expected_type definitions substitutions) {
        | Err(message) -> Err(message)
        | Ok(_) -> verify_call_argument_types remaining_arguments remaining_types
            definitions substitutions
      }
    }
  }

let find_generic_instantiation = fn wanted -> fn is_free_variable ->
  fn instantiations -> match instantiations {
    | Nil -> None
    | Cons(NativeGenericInstantiation { variable, is_free_variable: existing_kind,
        checked_type }, remaining) -> match (wanted == variable) {
      | false -> find_generic_instantiation wanted is_free_variable remaining
      | true -> match is_free_variable {
        | false -> match existing_kind {
          | false -> Some(checked_type)
          | true -> find_generic_instantiation wanted is_free_variable remaining
        }
        | true -> match existing_kind {
          | true -> Some(checked_type)
          | false -> find_generic_instantiation wanted is_free_variable remaining
        }
      }
    }
  }

let match_instantiated_type_list = fn expected -> fn actual -> fn substitutions ->
  fn instantiations -> match expected {
    | Nil -> match actual {
      | Nil -> Ok(instantiations)
      | _ -> Err("NativeIR verifier: instantiated type arity mismatch")
    }
    | Cons(expected_type, expected_remaining) -> match actual {
      | Nil -> Err("NativeIR verifier: instantiated type arity mismatch")
      | Cons(actual_type, actual_remaining) -> match (match_instantiated_type
          expected_type actual_type substitutions instantiations) {
        | Err(message) -> Err(message)
        | Ok(next_instantiations) -> match_instantiated_type_list
            expected_remaining actual_remaining substitutions next_instantiations
      }
    }
  }

let match_instantiated_record_fields = fn expected -> fn actual -> fn substitutions ->
  fn instantiations -> match expected {
    | Nil -> match actual {
      | Nil -> Ok(instantiations)
      | _ -> Err("NativeIR verifier: instantiated record field count mismatch")
    }
    | Cons(InferredRecordField { name, field_type }, expected_remaining) -> match actual {
      | Nil -> Err("NativeIR verifier: instantiated record field count mismatch")
      | Cons(InferredRecordField { name: actual_name, field_type: actual_type }, actual_remaining) ->
        match (same_value_name name actual_name) {
          | false -> Err("NativeIR verifier: instantiated record field name mismatch")
          | true -> match (match_instantiated_type field_type actual_type
              substitutions instantiations) {
            | Err(message) -> Err(message)
            | Ok(next_instantiations) -> match_instantiated_record_fields
                expected_remaining actual_remaining substitutions next_instantiations
          }
        }
    }
  }

let match_instantiated_variant_cases = fn expected -> fn actual -> fn substitutions ->
  fn instantiations -> match expected {
    | Nil -> match actual {
      | Nil -> Ok(instantiations)
      | _ -> Err("NativeIR verifier: instantiated variant case count mismatch")
    }
    | Cons(InferredVariantCase { name, fields }, expected_remaining) -> match actual {
      | Nil -> Err("NativeIR verifier: instantiated variant case count mismatch")
      | Cons(InferredVariantCase { name: actual_name, fields: actual_fields }, actual_remaining) ->
        match (same_constructor_name name actual_name) {
          | false -> Err("NativeIR verifier: instantiated variant constructor mismatch")
          | true -> match (match_instantiated_type_list fields actual_fields
              substitutions instantiations) {
            | Err(message) -> Err(message)
            | Ok(next_instantiations) -> match_instantiated_variant_cases
                expected_remaining actual_remaining substitutions next_instantiations
          }
        }
    }
  }

let match_instantiated_type = fn expected -> fn actual -> fn substitutions ->
  fn instantiations -> match (apply_substitutions expected substitutions) {
    | GenericVariable(variable) -> match (find_generic_instantiation variable false instantiations) {
      | None -> Ok(Cons(NativeGenericInstantiation {
        variable: variable,
        is_free_variable: false,
        checked_type: apply_substitutions actual substitutions
      }, instantiations))
      | Some(previous_type) -> match (same_checked_type previous_type actual substitutions) {
        | true -> Ok(instantiations)
        | false -> Err("NativeIR verifier: generic call type is inconsistently instantiated")
      }
    }
    | FreeVariable(variable) -> match (find_generic_instantiation variable true instantiations) {
      | None -> Ok(Cons(NativeGenericInstantiation {
        variable: variable,
        is_free_variable: true,
        checked_type: apply_substitutions actual substitutions
      }, instantiations))
      | Some(previous_type) -> match (same_checked_type previous_type actual substitutions) {
        | true -> Ok(instantiations)
        | false -> Err("NativeIR verifier: generic call type is inconsistently instantiated")
      }
    }
    | AppliedType(InferredNamedType { name, arguments }) ->
      match (apply_substitutions actual substitutions) {
        | AppliedType(InferredNamedType { name: actual_name, arguments: actual_arguments }) ->
          match (equal name actual_name) {
            | false -> Err("NativeIR verifier: call type constructor mismatch")
            | true -> match_instantiated_type_list arguments actual_arguments
                substitutions instantiations
          }
        | _ -> Err("NativeIR verifier: call type constructor mismatch")
      }
    | FunctionType(InferredFunctionType { argument, result }) ->
      match (apply_substitutions actual substitutions) {
        | FunctionType(InferredFunctionType { argument: actual_argument, result: actual_result }) ->
          match (match_instantiated_type argument actual_argument substitutions instantiations) {
            | Err(message) -> Err(message)
            | Ok(next_instantiations) -> match_instantiated_type result actual_result
                substitutions next_instantiations
          }
        | _ -> Err("NativeIR verifier: call type constructor mismatch")
      }
    | InferredTuple(items) -> match (apply_substitutions actual substitutions) {
      | InferredTuple(actual_items) -> match_instantiated_type_list items actual_items
          substitutions instantiations
      | _ -> Err("NativeIR verifier: call type constructor mismatch")
    }
    | InferredRecord(fields) -> match (apply_substitutions actual substitutions) {
      | InferredRecord(actual_fields) -> match_instantiated_record_fields fields actual_fields
          substitutions instantiations
      | _ -> Err("NativeIR verifier: call type constructor mismatch")
    }
    | InferredVariant(cases) -> match (apply_substitutions actual substitutions) {
      | InferredVariant(actual_cases) -> match_instantiated_variant_cases cases actual_cases
          substitutions instantiations
      | _ -> Err("NativeIR verifier: call type constructor mismatch")
    }
  }

let verify_closure_argument_type = fn callee -> fn argument_type -> fn result_type ->
  fn definitions -> fn substitutions -> match (definition_checked_type callee definitions) {
    | None -> Err("NativeIR verifier: closure call target has no SSA definition")
    | Some(callee_type) -> match (apply_substitutions callee_type substitutions) {
      | FunctionType(InferredFunctionType { argument, result }) ->
        match (match_instantiated_type argument argument_type substitutions Nil) {
          | Err(_) -> Err("NativeIR verifier: closure call argument type mismatch (function "
              + print_type argument + ", call " + print_type argument_type + ")")
          | Ok(instantiations) -> match (match_instantiated_type result result_type
              substitutions instantiations) {
            | Err(_) -> Err("NativeIR verifier: closure call result type mismatch (function "
                + print_type result + ", call " + print_type result_type + ")")
            | Ok(_) -> Ok(true)
          }
        }
      | _ -> Err("NativeIR verifier: closure call target is not a function")
    }
  }

let verify_call_target = fn target -> fn arguments -> fn argument_types ->
  fn result_type -> fn definitions -> fn functions -> fn substitutions -> match target {
    | NativeClosureCall(callee) -> match arguments {
      | Nil -> Err("NativeIR verifier: closure call has no argument")
      | Cons(_, remaining) -> match remaining {
        | Nil -> match argument_types {
          | Nil -> Err("NativeIR verifier: closure call has no argument type")
          | Cons(argument_type, remaining_types) -> match remaining_types {
            | Nil -> verify_closure_argument_type callee argument_type
                result_type definitions substitutions
            | _ -> Err("NativeIR verifier: closure call type arity mismatch")
          }
        }
        | _ -> Err("NativeIR verifier: closure call arity mismatch")
      }
    }
    | NativeDirectCall(identity) -> match (find_function identity functions) {
      | None -> Err("NativeIR verifier: direct call target is not defined")
      | Some(target_function) -> match arguments {
        | Nil -> Err("NativeIR verifier: direct call has no argument")
        | Cons(_, remaining_arguments) -> match remaining_arguments {
          | Nil -> match argument_types {
            | Nil -> Err("NativeIR verifier: direct call has no argument type")
            | Cons(argument_type, remaining_types) -> match remaining_types {
              | Nil -> match (verify_entry_argument target_function argument_type substitutions) {
                | Err(message) -> Err(message)
                | Ok(_) -> match (same_checked_type
                    (function_result_type target_function) result_type substitutions) {
                  | true -> Ok(true)
                  | false -> Err("NativeIR verifier: direct call result type mismatch")
                }
              }
              | _ -> Err("NativeIR verifier: direct call type arity mismatch")
            }
          }
          | _ -> Err("NativeIR verifier: direct call arity mismatch")
        }
      }
    }
    | NativeHostCall(_) -> Ok(true)
  }

let verify_entry_argument = fn target_function -> fn expected_type ->
  fn substitutions -> match (find_block (function_entry target_function)
      (function_blocks target_function)) {
    | None -> Err("NativeIR verifier: called function has no entry block")
    | Some(block) -> match (block_parameters block) {
      | Nil -> Err("NativeIR verifier: called function has no entry argument")
      | Cons(NativeBlockParameter { value: _, checked_type, span: _ }, remaining) ->
        match remaining {
          | Nil -> match (same_checked_type checked_type expected_type substitutions) {
            | true -> Ok(true)
            | false -> Err("NativeIR verifier: closure function argument type mismatch")
          }
          | _ -> Err("NativeIR verifier: function entry must have exactly one argument")
        }
    }
  }

let verify_closure_signature = fn identity -> fn closure_type -> fn functions ->
  fn substitutions -> match (find_function identity functions) {
    | None -> Err("NativeIR verifier: closure code function is not defined")
    | Some(target_function) -> match (apply_substitutions closure_type substitutions) {
      | FunctionType(InferredFunctionType { argument, result }) ->
        match (same_checked_type result (function_result_type target_function) substitutions) {
          | false -> Err("NativeIR verifier: closure function result type mismatch")
          | true -> verify_entry_argument target_function argument substitutions
        }
      | _ -> Err("NativeIR verifier: closure value does not have a function type")
    }
  }

let binary_operator_is_comparison = fn operator -> match operator {
  | Equal -> true
  | NotEqual -> true
  | LessThan -> true
  | LessThanOrEqual -> true
  | GreaterThan -> true
  | GreaterThanOrEqual -> true
  | _ -> false
}

let verify_binary_result_type = fn operator -> fn operand_type -> fn result_type ->
  fn substitutions -> match operator {
    | Addition -> match (type_is_named operand_type "string" substitutions) {
      | true -> match (same_checked_type operand_type result_type substitutions) {
        | true -> Ok(true)
        | false -> Err("NativeIR verifier: string addition result type mismatch")
      }
      | false -> match (type_is_named operand_type "i32" substitutions) {
        | false -> Err("NativeIR verifier: addition has an unsupported operand type")
        | true -> match (type_is_named result_type "i32" substitutions) {
          | true -> Ok(true)
          | false -> Err("NativeIR verifier: integer addition result type mismatch")
        }
      }
    }
    | Subtraction -> verify_integer_binary_result result_type substitutions
    | Multiplication -> verify_integer_binary_result result_type substitutions
    | Division -> verify_integer_binary_result result_type substitutions
    | Remainder -> verify_integer_binary_result result_type substitutions
    | Equal -> verify_boolean_result result_type substitutions
    | NotEqual -> verify_boolean_result result_type substitutions
    | LessThan -> verify_boolean_result result_type substitutions
    | LessThanOrEqual -> verify_boolean_result result_type substitutions
    | GreaterThan -> verify_boolean_result result_type substitutions
    | GreaterThanOrEqual -> verify_boolean_result result_type substitutions
  }

let verify_integer_binary_result = fn result_type -> fn substitutions ->
  match (type_is_named result_type "i32" substitutions) {
    | true -> Ok(true)
    | false -> Err("NativeIR verifier: integer primitive has a non-i32 result")
  }

let verify_boolean_result = fn result_type -> fn substitutions ->
  match (type_is_named result_type "bool" substitutions) {
    | true -> Ok(true)
    | false -> Err("NativeIR verifier: comparison primitive has a non-bool result")
  }

let verify_capture_instruction = fn (NativeInstruction {
  result: _, result_type, operation, effects: _, span: _
}) -> fn captures -> fn definitions -> fn substitutions -> match operation {
  | NativeLoadCapture(index) -> match (index < 0) {
    | true -> Err("NativeIR verifier: capture index is negative")
    | false -> match (value_at index captures) {
      | None -> Err("NativeIR verifier: closure omits a required capture")
      | Some(value) -> same_value_type value result_type definitions substitutions
    }
  }
  | _ -> Ok(true)
}

let verify_capture_instructions = fn instructions -> fn captures ->
  fn definitions -> fn substitutions -> match instructions {
    | Nil -> Ok(true)
    | Cons(instruction, remaining) -> match (verify_capture_instruction instruction
        captures definitions substitutions) {
      | Err(message) -> Err(message)
      | Ok(_) -> verify_capture_instructions remaining captures definitions substitutions
    }
  }

let verify_function_captures = fn blocks -> fn captures -> fn definitions ->
  fn substitutions -> match blocks {
    | Nil -> Ok(true)
    | Cons(NativeBlock { identity: _, parameters: _, instructions, terminator: _, span: _ }, remaining) ->
      match (verify_capture_instructions instructions captures definitions substitutions) {
        | Err(message) -> Err(message)
        | Ok(_) -> verify_function_captures remaining captures definitions substitutions
      }
  }

let verify_projection_type = fn aggregate -> fn field -> fn result_type ->
  fn definitions -> fn substitutions -> match (definition_checked_type aggregate definitions) {
    | None -> Err("NativeIR verifier: projected aggregate has no SSA definition")
    | Some(aggregate_type) -> match (apply_substitutions aggregate_type substitutions) {
      | InferredTuple(types) -> match field {
        | NativeTupleField(index) -> match (type_at index types) {
          | None -> Err("NativeIR verifier: tuple projection index is out of range")
          | Some(expected_type) -> match (same_checked_type expected_type result_type substitutions) {
            | true -> Ok(true)
            | false -> Err("NativeIR verifier: tuple projection result type mismatch")
          }
        }
        | _ -> Err("NativeIR verifier: projection kind does not match tuple type")
      }
      | InferredRecord(fields) -> match field {
        | NativeRecordField(name) -> match (record_field_type name fields) {
          | None -> Err("NativeIR verifier: record projection field is not present")
          | Some(expected_type) -> match (same_checked_type expected_type result_type substitutions) {
            | true -> Ok(true)
            | false -> Err("NativeIR verifier: record projection result type mismatch")
          }
        }
        | _ -> Err("NativeIR verifier: projection kind does not match record type")
      }
      | _ -> Ok(true)
    }
  }

let verify_aggregate_field_keys = fn kind -> fn fields -> fn index -> match fields {
  | Nil -> Ok(true)
  | Cons(NativeAggregateValue { field, value: _ }, remaining) -> match kind {
    | NativeTupleAggregate -> match field {
      | NativeTupleField(field_index) -> match (field_index == index) {
        | true -> verify_aggregate_field_keys kind remaining (index + 1)
        | false -> Err("NativeIR verifier: tuple aggregate field index is not canonical")
      }
      | _ -> Err("NativeIR verifier: tuple aggregate has a non-tuple field key")
    }
    | NativeRecordAggregate(_) -> match field {
      | NativeRecordField(_) -> verify_aggregate_field_keys kind remaining (index + 1)
      | _ -> Err("NativeIR verifier: record aggregate has a non-record field key")
    }
    | NativeVariantAggregate(_) -> match field {
      | NativeConstructorField(field_index) -> match (field_index == index) {
        | true -> verify_aggregate_field_keys kind remaining (index + 1)
        | false -> Err("NativeIR verifier: constructor aggregate field index is not canonical")
      }
      | _ -> Err("NativeIR verifier: constructor aggregate has a non-constructor field key")
    }
  }
}

let record_field_count = fn fields -> match fields {
  | Nil -> 0
  | Cons(_, remaining) -> 1 + record_field_count remaining
}

let type_expression_count = fn types -> match types {
  | Nil -> 0
  | Cons(_, remaining) -> 1 + type_expression_count remaining
}

let type_parameter_count = fn parameters -> match parameters {
  | Nil -> 0
  | Cons(_, remaining) -> 1 + type_parameter_count remaining
}

let native_field_names = fn fields -> match fields {
  | Nil -> Nil
  | Cons(RecordTypeField { name, field_type: _ }, remaining) ->
    Cons(name, native_field_names remaining)
}

let native_record_constructor_shape = fn type_identity -> fn parameter_count -> fn fields ->
  Ok(NativeConstructorShape {
    type_identity: type_identity,
    type_parameter_count: parameter_count,
    field_count: record_field_count fields,
    field_names: native_field_names fields
  })

let find_native_variant_constructor = fn wanted -> fn type_identity ->
  fn parameter_count -> fn variants -> match variants {
    | Nil -> Err("constructor is not declared by the nominal type")
    | Cons(TupleVariant(TupleVariantData { constructor_name, fields }), remaining) ->
      match (same_constructor_name wanted constructor_name) {
        | false -> find_native_variant_constructor wanted type_identity parameter_count remaining
        | true -> Ok(NativeConstructorShape {
          type_identity: type_identity,
          type_parameter_count: parameter_count,
          field_count: type_expression_count fields,
          field_names: Nil
        })
      }
    | Cons(RecordVariant(RecordVariantData { constructor_name, fields }), remaining) ->
      match (same_constructor_name wanted constructor_name) {
        | false -> find_native_variant_constructor wanted type_identity parameter_count remaining
        | true -> native_record_constructor_shape type_identity parameter_count fields
      }
  }

let find_native_constructor_in_type = fn entity -> fn type_visibility -> {
  let ConstructorIdentity { constructor_name: wanted, type_identity: _ } = entity;
  match type_visibility {
    | VisibleType(VisibleTypeData {
        declaration: TypeDeclarationData { name, parameters, definition, span: _ },
        type_identity, visible_names: _, available_names: _
      }) -> match definition {
      | TypeAlias(_) -> Err("type aliases do not own constructors")
      | TypeRecord(fields) -> match name {
        | TypeName(type_name) -> match (same_constructor_name wanted
            (ConstructorName(type_name))) {
          | false -> Err("constructor does not belong to this record type")
          | true -> native_record_constructor_shape type_identity
              (type_parameter_count parameters) fields
        }
      }
      | TypeVariants(variants) -> find_native_variant_constructor wanted type_identity
          (type_parameter_count parameters) variants
    }
  }
}

let same_constructor_identity_type = fn (ConstructorIdentity {
  type_identity: wanted, constructor_name: _
}) -> fn actual -> equal wanted actual

let find_native_constructor = fn entity -> fn visible_types -> match visible_types {
  | Nil -> Err("NativeIR verifier: nominal type identity is not visible")
  | Cons(type_visibility, remaining) -> match type_visibility {
    | VisibleType(VisibleTypeData {
        declaration: _, type_identity, visible_names: _, available_names: _
      }) -> match (same_constructor_identity_type entity type_identity) {
      | true -> find_native_constructor_in_type entity type_visibility
      | false -> find_native_constructor entity remaining
    }
  }
}

let find_record_aggregate_value = fn wanted_name -> fn fields -> match fields {
  | Nil -> None
  | Cons(NativeAggregateValue { field: NativeRecordField(name), value }, remaining) ->
    match (same_value_name wanted_name name) {
      | true -> Some(value)
      | false -> find_record_aggregate_value wanted_name remaining
    }
  | Cons(_, remaining) -> find_record_aggregate_value wanted_name remaining
}

let verify_record_aggregate_payload = fn names -> fn fields -> match names {
  | Nil -> Ok(true)
  | Cons(name, remaining) -> match (find_record_aggregate_value name fields) {
    | None -> Err("NativeIR verifier: record aggregate is missing a declared field")
    | Some(_) -> verify_record_aggregate_payload remaining fields
  }
}

let verify_native_nominal_aggregate = fn kind -> fn entity -> fn fields ->
  fn result_type -> fn module -> fn substitutions ->
  match (find_native_constructor entity (module_nominal_types module)) {
    | Err(message) -> Err("NativeIR verifier: aggregate references unknown constructor "
      + print_constructor_identity entity + " in " + module_identity module + ": " + message)
    | Ok(NativeConstructorShape {
        type_identity, type_parameter_count, field_count, field_names
      }) -> match (apply_substitutions result_type substitutions) {
      | AppliedType(InferredNamedType { name, arguments }) -> match (equal name type_identity) {
        | false -> Err("NativeIR verifier: aggregate result type does not match its constructor")
        | true -> match (inference_type_count arguments == type_parameter_count) {
          | false -> Err("NativeIR verifier: aggregate nominal type argument count mismatch")
          | true -> match kind {
            | NativeRecordAggregate(_) -> match (aggregate_value_count fields == field_count) {
              | false -> Err("NativeIR verifier: record aggregate field count mismatch")
              | true -> match (value_name_count field_names == field_count) {
                | false -> Err("NativeIR verifier: record constructor field metadata is inconsistent")
                | true -> verify_record_aggregate_payload field_names fields
              }
            }
            | NativeVariantAggregate(_) -> match (aggregate_value_count fields == field_count) {
              | true -> Ok(true)
              | false -> Err("NativeIR verifier: constructor aggregate arity mismatch")
            }
            | NativeTupleAggregate -> Err("NativeIR verifier: tuple aggregate has no nominal constructor")
          }
        }
      }
      | _ -> Err("NativeIR verifier: nominal aggregate has a non-nominal result type")
    }
  }

let verify_instruction_operation = fn module -> fn (NativeInstruction {
  result: _, result_type, operation, effects: _, span: _
}) -> fn definitions -> fn substitutions -> match operation {
        | NativeConstant(constant) -> match constant {
          | NativeUnit -> verify_named_result result_type "unit" substitutions
          | NativeInteger(_) -> verify_named_result result_type "i32" substitutions
          | NativeBoolean(_) -> verify_named_result result_type "bool" substitutions
          | NativeString(_) -> verify_named_result result_type "string" substitutions
        }
        | NativePrimitiveOperation(NativePrimitiveBinary(NativeBinaryPrimitive {
            operator, operand_type, left, right
          })) -> match (same_value_type left operand_type definitions substitutions) {
          | Err(message) -> Err(message)
          | Ok(_) -> match (same_value_type right operand_type definitions substitutions) {
            | Err(message) -> Err(message)
            | Ok(_) -> verify_binary_result_type operator operand_type result_type substitutions
          }
        }
        | NativePrimitiveOperation(NativePrimitiveNegate(NativeNegatePrimitive {
            operand_type, operand
          })) -> match (same_value_type operand operand_type definitions substitutions) {
          | Err(message) -> Err(message)
          | Ok(_) -> match (verify_named_result operand_type "i32" substitutions) {
            | Err(message) -> Err(message)
            | Ok(_) -> verify_named_result result_type "i32" substitutions
          }
        }
        | NativeCallOperation(NativeCall {
            target, arguments, argument_types, result_type: call_result
          }) -> match (same_checked_type result_type call_result substitutions) {
          | false -> Err("NativeIR verifier: call result declaration disagrees with its SSA result")
          | true -> match (verify_call_argument_types arguments argument_types
              definitions substitutions) {
            | Err(message) -> Err(message)
            | Ok(_) -> verify_call_target target arguments argument_types call_result
                definitions (module_functions module) substitutions
          }
        }
        | NativeCreateClosure(NativeClosure { function, captures }) ->
          match (verify_closure_signature function result_type
              (module_functions module) substitutions) {
            | Err(message) -> Err(message)
            | Ok(_) -> match (find_function function (module_functions module)) {
              | None -> Err("NativeIR verifier: closure code function is not defined")
              | Some(target_function) -> match (verify_function_captures
                  (function_blocks target_function) captures definitions substitutions) {
                | Err(message) -> Err(message)
                | Ok(_) -> Ok(true)
              }
            }
          }
        | NativeCreateAggregate(NativeAggregate { kind, fields }) ->
          match (verify_aggregate_field_keys kind fields 0) {
            | Err(message) -> Err(message)
            | Ok(_) -> match kind {
              | NativeTupleAggregate -> match (apply_substitutions result_type substitutions) {
                | InferredTuple(types) -> verify_tuple_aggregate_fields fields types
                    definitions substitutions 0
                | _ -> Err("NativeIR verifier: tuple aggregate has a non-tuple result type")
              }
              | NativeRecordAggregate(entity) -> verify_native_nominal_aggregate kind
                  entity fields result_type module substitutions
              | NativeVariantAggregate(entity) -> verify_native_nominal_aggregate kind
                  entity fields result_type module substitutions
            }
          }
        | NativeProjectAggregate(NativeProjection { aggregate, field }) ->
          verify_projection_type aggregate field result_type definitions substitutions
        | NativeTestValue(_) -> verify_named_result result_type "bool" substitutions
        | NativeLoadCapture(index) -> match (index < 0) {
          | true -> Err("NativeIR verifier: capture index is negative")
          | false -> Ok(true)
        }
        | NativeLoadGlobal(binding) -> match (find_global binding (module_globals module)) {
          | Some(_) -> Ok(true)
          | None -> match (binding_source_path binding
              (module_identity module)) {
            | true -> Err("NativeIR verifier: load references an unknown local global")
            | false -> Ok(true)
          }
        }
        | NativeStoreGlobal(NativeGlobalStore { binding, value }) ->
          match (find_global binding (module_globals module)) {
            | None -> Err("NativeIR verifier: store references an unknown global")
            | Some(_) -> verify_named_result result_type "unit" substitutions
          }
      }

let verify_named_result = fn checked_type -> fn name -> fn substitutions ->
  match (type_is_named checked_type name substitutions) {
    | true -> Ok(true)
    | false -> Err("NativeIR verifier: instruction result has an unexpected type")
  }

let module_functions = fn (NativeModule {
  module_identity: _, nominal_types: _, substitutions: _, globals: _,
  initializer: _, functions
}) -> functions

let module_nominal_types = fn (NativeModule {
  module_identity: _, nominal_types, substitutions: _, globals: _,
  initializer: _, functions: _
}) -> nominal_types

let module_globals = fn (NativeModule {
  module_identity: _, nominal_types: _, substitutions: _, globals,
  initializer: _, functions: _
}) -> globals

let module_identity = fn (NativeModule {
  module_identity, nominal_types: _, substitutions: _, globals: _,
  initializer: _, functions: _
}) -> module_identity

let binding_source_path = fn (BindingIdentity {
  source_path, declaration_span: _, name: _, scope: _
}) -> fn module_identity -> equal source_path module_identity

let module_substitutions = fn (NativeModule {
  module_identity: _, nominal_types: _, substitutions, globals: _,
  initializer: _, functions: _
}) -> substitutions

let verify_instruction_list = fn module -> fn instructions -> fn definitions ->
  fn substitutions -> match instructions {
    | Nil -> Ok(true)
    | Cons(NativeInstruction { result, result_type, operation, effects, span }, remaining) -> {
      let instruction = NativeInstruction {
        result: result,
        result_type: result_type,
        operation: operation,
        effects: effects,
        span: span
      };
      match (verify_instruction_operation module instruction definitions substitutions) {
        | Err(message) -> Err(message)
        | Ok(_) -> match (verify_operation_effects operation result_type effects substitutions) {
          | Err(message) -> Err(message)
          | Ok(_) -> verify_instruction_list module remaining definitions substitutions
        }
      }
    }
  }

let verify_function_instructions = fn blocks -> fn module -> fn definitions ->
  fn substitutions -> match blocks {
    | Nil -> Ok(true)
    | Cons(NativeBlock { identity: _, parameters: _, instructions, terminator: _, span: _ }, remaining) ->
      match (verify_instruction_list module instructions definitions substitutions) {
        | Err(message) -> Err(message)
        | Ok(_) -> verify_function_instructions remaining module definitions substitutions
      }
  }

let verify_unique_block_ids = fn ids -> fn seen -> match ids {
  | Nil -> Ok(true)
  | Cons(identity, remaining) -> match (contains_block identity seen) {
    | true -> Err("NativeIR verifier: block ID is defined more than once")
    | false -> verify_unique_block_ids remaining (Cons(identity, seen))
  }
}

let verify_function = fn (NativeFunction {
  identity: _, result_type, entry, blocks, span: _
}) -> fn module -> {
  let identities = block_ids blocks;
  match (find_block entry blocks) {
    | None -> Err("NativeIR verifier: function entry block does not exist")
    | Some(_) -> match (verify_unique_block_ids identities Nil) {
      | Err(message) -> Err(message)
      | Ok(_) -> {
        let reachable = reachable_from (Cons(entry, Nil)) Nil blocks;
        match (verify_all_blocks_reachable blocks reachable) {
          | Err(message) -> Err(message)
          | Ok(_) -> {
            let dominance = NativeDominanceContext { entry: entry, blocks: blocks };
            match (collect_function_definitions blocks Nil) {
              | Err(message) -> Err(message)
              | Ok(definitions) -> match (verify_block_edges blocks definitions
                  dominance (module_substitutions module)) {
                | Err(message) -> Err(message)
                | Ok(_) -> match (verify_block_terminators blocks blocks result_type
                    definitions dominance (module_substitutions module)) {
                  | Err(message) -> Err(message)
                  | Ok(_) -> verify_function_instructions blocks module definitions
                      (module_substitutions module)
                }
              }
            }
          }
        }
      }
    }
  }
}

let global_bindings = fn globals -> match globals {
  | Nil -> Nil
  | Cons(NativeGlobal { binding, scheme: _, span: _ }, remaining) ->
    Cons(binding, global_bindings remaining)
}

let verify_unique_bindings = fn bindings -> fn seen -> match bindings {
  | Nil -> Ok(true)
  | Cons(binding, remaining) -> match (binding_seen binding seen) {
    | true -> Err("NativeIR verifier: global binding is defined more than once")
    | false -> verify_unique_bindings remaining (Cons(binding, seen))
  }
}

let binding_seen = fn wanted -> fn bindings -> match bindings {
  | Nil -> false
  | Cons(binding, remaining) -> either (same_binding wanted binding)
      (binding_seen wanted remaining)
}

let verify_functions = fn functions -> fn module -> match functions {
  | Nil -> Ok(true)
  | Cons(function, remaining) -> match (verify_function function module) {
    | Err(message) -> Err("in " + print_function_id (native_function_identity function)
        + ": " + message)
    | Ok(_) -> verify_functions remaining module
  }
}

let verify_module = fn (NativeModule {
  module_identity, nominal_types, substitutions, globals, initializer, functions
}) -> {
  let module = NativeModule {
    module_identity: module_identity,
    nominal_types: nominal_types,
    substitutions: substitutions,
    globals: globals,
    initializer: initializer,
    functions: functions
  };
  let expected_initializer = NativeModuleInitializer(module_identity);
  match (same_function_id initializer expected_initializer) {
    | false -> Err("NativeIR verifier: module initializer identity does not match module")
    | true -> match (find_function initializer functions) {
      | None -> Err("NativeIR verifier: module initializer function is missing")
      | Some(initializer_function) -> match (verify_named_result
          (function_result_type initializer_function) "unit" substitutions) {
        | Err(message) -> Err(message)
        | Ok(_) -> match (verify_unique_function_ids functions Nil) {
          | Err(message) -> Err(message)
          | Ok(_) -> match (verify_unique_bindings (global_bindings globals) Nil) {
            | Err(message) -> Err(message)
            | Ok(_) -> verify_functions functions module
          }
        }
      }
    }
  }
}

let same_function_id_seen = fn wanted -> fn functions -> match functions {
  | Nil -> false
  | Cons(function, remaining) -> either
      (same_function_id wanted (native_function_identity function))
      (same_function_id_seen wanted remaining)
}

let verify_unique_function_ids = fn functions -> fn seen -> match functions {
  | Nil -> Ok(true)
  | Cons(function, remaining) -> match (same_function_id_seen
      (native_function_identity function) seen) {
    | true -> Err("NativeIR verifier: function identity is defined more than once: "
        + print_function_id (native_function_identity function))
    | false -> verify_unique_function_ids remaining (Cons(function, seen))
  }
}
