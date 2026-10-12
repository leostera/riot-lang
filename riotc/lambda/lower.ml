// Lower checked source modules into the target-independent LambdaIR core.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use String.{equal}
use Syntax.Ast.{
  SourceModule, Declaration, ValueDeclaration, ValueDeclarationData, TypeDeclaration,
  Integer, IntegerLiteral, Boolean, StringLiteral, Unit, VariableExpr, TupleExpr,
  RecordExpr, RecordExpression, RecordExpressionField, Binary, BinaryExpression,
  BinaryOperator, Negate, Function, FunctionExpression, Apply,
  ApplicationExpression, Let, LetExpression, Match, MatchExpression, MatchArm,
  ConstructorExpr, ConstructorExpression, Ascription, AscriptionExpression, Block,
  Pattern, Variable, PatternUnit, Wildcard, PatternInteger, PatternBoolean,
  PatternString, TuplePattern, ConstructorPattern, ConstructorPatternData,
  RecordPattern, RecordPatternData, RecordPatternField, AscribedPattern,
  AscribedPatternData, ValueName, ConstructorName, expression_kind,
  expression_span, pattern_kind, pattern_span
}
use Syntax.Lexer.{SourceSpan}
use Check.Inference.{
  CheckedModuleInterface, InferredModuleInterface, TypedModule,
  ExpressionTypeAnnotation, PatternTypeAnnotation, InferenceType, TypeScheme,
  ValueScheme, SchemeBinding, BindingIdentity, BindingScope,
  ModuleValueBinding, LocalValueBinding, ResolvedValueReference,
  ConstructorIdentity, ResolvedConstructorReference,
  TypeVisibility, TypeSubstitution
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

let same_source_span = fn first -> fn second -> {
  let SourceSpan { start_codepoint: first_start, end_codepoint: first_end } = first;
  let SourceSpan { start_codepoint: second_start, end_codepoint: second_end } = second;
  match (first_start == second_start) {
    | false -> false
    | true -> first_end == second_end
  }
}

let find_expression_annotation = fn wanted_span -> fn annotations ->
  match annotations {
    | Nil -> None
    | Cons(ExpressionTypeAnnotation { span, inferred_type }, remaining) ->
      match (same_source_span wanted_span span) {
        | true -> Some(inferred_type)
        | false -> find_expression_annotation wanted_span remaining
      }
  }

let find_pattern_annotation = fn wanted_span -> fn annotations ->
  match annotations {
    | Nil -> None
    | Cons(PatternTypeAnnotation { span, inferred_type }, remaining) ->
      match (same_source_span wanted_span span) {
        | true -> Some(inferred_type)
        | false -> find_pattern_annotation wanted_span remaining
      }
  }

let expression_type = fn expression -> fn (TypedModule {
  expression_types, source_module: _, pattern_types: _, nominal_types: _,
  substitutions: _, bindings: _, value_references: _, constructor_references: _
}) -> match (find_expression_annotation (expression_span expression)
    expression_types) {
  | None -> Err("LambdaIR: expression has no checked type annotation")
  | Some(checked_type) -> Ok(checked_type)
}

let pattern_type = fn pattern -> fn (TypedModule {
  expression_types: _, source_module: _, pattern_types, nominal_types: _,
  substitutions: _, bindings: _, value_references: _, constructor_references: _
}) -> match (find_pattern_annotation (pattern_span pattern) pattern_types) {
  | None -> Err("LambdaIR: pattern has no checked type annotation")
  | Some(checked_type) -> Ok(checked_type)
}

let find_value_reference = fn wanted_span -> fn references ->
  match references {
    | Nil -> None
    | Cons(ResolvedValueReference { reference_span, binding }, remaining) ->
      match (same_source_span wanted_span reference_span) {
        | true -> Some(binding)
        | false -> find_value_reference wanted_span remaining
      }
  }

let find_constructor_reference = fn wanted_span -> fn references ->
  match references {
    | Nil -> None
    | Cons(ResolvedConstructorReference { reference_span, entity }, remaining) ->
      match (same_source_span wanted_span reference_span) {
        | true -> Some(entity)
        | false -> find_constructor_reference wanted_span remaining
      }
  }

let value_binding_for = fn expression -> fn (TypedModule {
  expression_types: _, source_module: _, pattern_types: _, nominal_types: _,
  substitutions: _, bindings: _, value_references, constructor_references: _
}) -> match (find_value_reference (expression_span expression) value_references) {
  | None -> Err("LambdaIR: variable expression has no resolved binding")
  | Some(binding) -> Ok(binding)
}

let constructor_entity_for = fn span -> fn (TypedModule {
  expression_types: _, source_module: _, pattern_types: _, nominal_types: _,
  substitutions: _, bindings: _, value_references: _, constructor_references
}) -> match (find_constructor_reference span constructor_references) {
  | None -> Err("LambdaIR: constructor has no resolved nominal entity")
  | Some(entity) -> Ok(entity)
}

let find_binding_identity = fn wanted_name -> fn wanted_span -> fn wanted_scope ->
  fn bindings -> match bindings {
    | Nil -> None
    | Cons(BindingIdentity {
        source_path, declaration_span, name: ValueName(name), scope
      }, remaining) -> match (equal wanted_name name) {
        | false -> find_binding_identity wanted_name wanted_span wanted_scope remaining
        | true -> match (same_source_span wanted_span declaration_span) {
          | false -> find_binding_identity wanted_name wanted_span wanted_scope remaining
          | true -> match scope {
            | ModuleValueBinding -> match wanted_scope {
              | ModuleValueBinding -> Some(BindingIdentity {
                source_path: source_path,
                declaration_span: declaration_span,
                name: ValueName(name),
                scope: ModuleValueBinding
              })
              | LocalValueBinding -> find_binding_identity wanted_name wanted_span
                wanted_scope remaining
            }
            | LocalValueBinding -> match wanted_scope {
              | LocalValueBinding -> Some(BindingIdentity {
                source_path: source_path,
                declaration_span: declaration_span,
                name: ValueName(name),
                scope: LocalValueBinding
              })
              | ModuleValueBinding -> find_binding_identity wanted_name wanted_span
                wanted_scope remaining
            }
          }
        }
      }
  }

let pattern_binding = fn pattern -> fn (TypedModule {
  expression_types: _, source_module: _, pattern_types: _, nominal_types: _,
  substitutions: _, bindings, value_references: _, constructor_references: _
}) -> match (pattern_kind pattern) {
  | Variable(ValueName(name)) -> match (find_binding_identity name
      (pattern_span pattern) LocalValueBinding bindings) {
    | None -> Err("LambdaIR: variable pattern has no resolved binding identity")
    | Some(binding) -> Ok(binding)
  }
  | _ -> Err("LambdaIR: requested a binding identity for a non-variable pattern")
}

let module_value_binding = fn name -> fn span -> fn (TypedModule {
  expression_types: _, source_module: _, pattern_types: _, nominal_types: _,
  substitutions: _, bindings, value_references: _, constructor_references: _
}) -> match (find_binding_identity name span ModuleValueBinding bindings) {
  | None -> Err("LambdaIR: top-level value has no resolved binding identity")
  | Some(binding) -> Ok(binding)
}

let find_value_scheme = fn wanted_name -> fn values ->
  match values {
    | Nil -> None
    | Cons(SchemeBinding(ValueName(name), scheme), remaining) ->
      match (equal wanted_name name) {
        | true -> Some(scheme)
        | false -> find_value_scheme wanted_name remaining
      }
  }

let lower_expression_list = fn expressions -> fn typed_module ->
  match expressions {
    | Nil -> Ok(Nil)
    | Cons(expression, remaining) -> match (lower_expression expression typed_module) {
      | Err(message) -> Err(message)
      | Ok(lowered_expression) -> match (lower_expression_list remaining typed_module) {
        | Err(message) -> Err(message)
        | Ok(lowered_remaining) -> Ok(Cons(lowered_expression, lowered_remaining))
      }
    }
  }

let lower_pattern_list = fn patterns -> fn typed_module ->
  match patterns {
    | Nil -> Ok(Nil)
    | Cons(pattern, remaining) -> match (lower_pattern pattern typed_module) {
      | Err(message) -> Err(message)
      | Ok(lowered_pattern) -> match (lower_pattern_list remaining typed_module) {
        | Err(message) -> Err(message)
        | Ok(lowered_remaining) -> Ok(Cons(lowered_pattern, lowered_remaining))
      }
    }
  }

let lower_record_expression_fields = fn fields -> fn typed_module ->
  match fields {
    | Nil -> Ok(Nil)
    | Cons(RecordExpressionField { name, value }, remaining) ->
      match (lower_expression value typed_module) {
        | Err(message) -> Err(message)
        | Ok(lowered_value) -> match (lower_record_expression_fields remaining typed_module) {
          | Err(message) -> Err(message)
          | Ok(lowered_remaining) -> Ok(Cons(LambdaRecordFieldValue {
            name: name,
            value: lowered_value
          }, lowered_remaining))
        }
      }
  }

let lower_record_pattern_fields = fn fields -> fn typed_module ->
  match fields {
    | Nil -> Ok(Nil)
    | Cons(RecordPatternField { name, pattern }, remaining) ->
      match (lower_pattern pattern typed_module) {
        | Err(message) -> Err(message)
        | Ok(lowered_pattern) -> match (lower_record_pattern_fields remaining typed_module) {
          | Err(message) -> Err(message)
          | Ok(lowered_remaining) -> Ok(Cons(LambdaRecordPatternField {
            name: name,
            pattern: lowered_pattern
          }, lowered_remaining))
        }
      }
  }

let lower_match_arms = fn arms -> fn typed_module ->
  match arms {
    | Nil -> Ok(Nil)
    | Cons(MatchArm { pattern, body }, remaining) ->
      match (lower_pattern pattern typed_module) {
        | Err(message) -> Err(message)
        | Ok(lowered_pattern) -> match (lower_expression body typed_module) {
          | Err(message) -> Err(message)
          | Ok(lowered_body) -> match (lower_match_arms remaining typed_module) {
            | Err(message) -> Err(message)
            | Ok(lowered_remaining) -> Ok(Cons(LambdaMatchArm {
              pattern: lowered_pattern,
              body: lowered_body
            }, lowered_remaining))
          }
        }
      }
  }

let lower_constructor_arguments = fn arguments -> fn typed_module ->
  lower_expression_list arguments typed_module

let lower_expression = fn expression -> fn typed_module ->
  match (expression_type expression typed_module) {
    | Err(message) -> Err(message)
    | Ok(checked_type) -> {
      let span = expression_span expression;
      let lowered_kind = match (expression_kind expression) {
        | Integer(literal) -> Ok(LambdaConstantExpr(LambdaInteger(literal)))
        | Boolean(value) -> Ok(LambdaConstantExpr(LambdaBoolean(value)))
        | StringLiteral(literal) -> Ok(LambdaConstantExpr(LambdaString(literal)))
        | Unit -> Ok(LambdaConstantExpr(LambdaUnit))
        | VariableExpr(_) -> match (value_binding_for expression typed_module) {
          | Err(message) -> Err(message)
          | Ok(binding) -> Ok(LambdaValueReference(binding))
        }
        | TupleExpr(items) -> match (lower_expression_list items typed_module) {
          | Err(message) -> Err(message)
          | Ok(lowered_items) -> Ok(LambdaTuple(lowered_items))
        }
        | RecordExpr(RecordExpression { constructor_name: _, fields }) ->
          match (constructor_entity_for span typed_module) {
            | Err(message) -> Err(message)
            | Ok(entity) -> match (lower_record_expression_fields fields typed_module) {
              | Err(message) -> Err(message)
              | Ok(lowered_fields) -> Ok(LambdaRecord(LambdaRecordExpression {
                entity: entity,
                fields: lowered_fields
              }))
            }
          }
        | Binary(BinaryExpression { operator, left, right }) ->
          match (expression_type left typed_module) {
            | Err(message) -> Err(message)
            | Ok(operand_type) -> match (lower_expression left typed_module) {
              | Err(message) -> Err(message)
              | Ok(lowered_left) -> match (lower_expression right typed_module) {
                | Err(message) -> Err(message)
                | Ok(lowered_right) -> Ok(LambdaBinaryPrimitive(
                  LambdaBinaryPrimitiveExpression {
                    operator: operator,
                    operand_type: operand_type,
                    left: lowered_left,
                    right: lowered_right
                  }))
              }
            }
          }
        | Negate(operand) -> match (expression_type operand typed_module) {
          | Err(message) -> Err(message)
          | Ok(operand_type) -> match (lower_expression operand typed_module) {
            | Err(message) -> Err(message)
            | Ok(lowered_operand) -> Ok(LambdaNegatePrimitive(
              LambdaNegatePrimitiveExpression {
                operand_type: operand_type,
                operand: lowered_operand
              }))
          }
        }
        | Function(FunctionExpression { parameter, body }) ->
          match (lower_pattern parameter typed_module) {
            | Err(message) -> Err(message)
            | Ok(lowered_parameter) -> match (lower_expression body typed_module) {
              | Err(message) -> Err(message)
              | Ok(lowered_body) -> Ok(LambdaFunction(LambdaFunctionExpression {
                parameter: lowered_parameter,
                body: lowered_body
              }))
            }
          }
        | Apply(ApplicationExpression { callee, argument }) ->
          match (lower_expression callee typed_module) {
            | Err(message) -> Err(message)
            | Ok(lowered_callee) -> match (lower_expression argument typed_module) {
              | Err(message) -> Err(message)
              | Ok(lowered_argument) -> Ok(LambdaApply(LambdaApplyExpression {
                callee: lowered_callee,
                argument: lowered_argument
              }))
            }
          }
        | Let(LetExpression { pattern, annotation: _, value, body }) ->
          match (lower_pattern pattern typed_module) {
            | Err(message) -> Err(message)
            | Ok(lowered_binding) -> match (lower_expression value typed_module) {
              | Err(message) -> Err(message)
              | Ok(lowered_value) -> match (lower_expression body typed_module) {
                | Err(message) -> Err(message)
                | Ok(lowered_body) -> Ok(LambdaLet(LambdaLetExpression {
                  binding: lowered_binding,
                  value: lowered_value,
                  body: lowered_body
                }))
              }
            }
          }
        | Match(MatchExpression { scrutinee, arms }) ->
          match (lower_expression scrutinee typed_module) {
            | Err(message) -> Err(message)
            | Ok(lowered_scrutinee) -> match (lower_match_arms arms typed_module) {
              | Err(message) -> Err(message)
              | Ok(lowered_arms) -> Ok(LambdaMatch(LambdaMatchExpression {
                scrutinee: lowered_scrutinee,
                arms: lowered_arms
              }))
            }
          }
        | ConstructorExpr(ConstructorExpression { constructor_name: _, arguments }) ->
          match (constructor_entity_for span typed_module) {
            | Err(message) -> Err(message)
            | Ok(entity) -> match (lower_constructor_arguments arguments typed_module) {
              | Err(message) -> Err(message)
              | Ok(lowered_arguments) -> Ok(LambdaConstruct(LambdaConstructorExpression {
                entity: entity,
                arguments: lowered_arguments
              }))
            }
          }
        | Ascription(AscriptionExpression { expression: inner, annotation: _ }) ->
          match (lower_expression inner typed_module) {
            | Err(message) -> Err(message)
            | Ok(LambdaExpression { checked_type: _, kind, span: _ }) -> Ok(kind)
          }
        | Block(items) -> match (lower_expression_list items typed_module) {
          | Err(message) -> Err(message)
          | Ok(lowered_items) -> Ok(LambdaBlock(lowered_items))
        }
      };
      match lowered_kind {
        | Err(message) -> Err(message)
        | Ok(kind) -> Ok(LambdaExpression {
          checked_type: checked_type,
          kind: kind,
          span: span
        })
      }
    }
  }

let lower_pattern = fn pattern -> fn typed_module ->
  match (pattern_type pattern typed_module) {
    | Err(message) -> Err(message)
    | Ok(checked_type) -> {
      let span = pattern_span pattern;
      let lowered_kind = match (pattern_kind pattern) {
        | Variable(_) -> match (pattern_binding pattern typed_module) {
          | Err(message) -> Err(message)
          | Ok(binding) -> Ok(LambdaBind(binding))
        }
        | PatternUnit -> Ok(LambdaConstantPattern(LambdaUnit))
        | Wildcard -> Ok(LambdaWildcard)
        | PatternInteger(literal) -> Ok(LambdaConstantPattern(LambdaInteger(literal)))
        | PatternBoolean(value) -> Ok(LambdaConstantPattern(LambdaBoolean(value)))
        | PatternString(literal) -> Ok(LambdaConstantPattern(LambdaString(literal)))
        | TuplePattern(patterns) -> match (lower_pattern_list patterns typed_module) {
          | Err(message) -> Err(message)
          | Ok(lowered_patterns) -> Ok(LambdaTuplePattern(lowered_patterns))
        }
        | ConstructorPattern(ConstructorPatternData { constructor_name: _, arguments }) ->
          match (constructor_entity_for span typed_module) {
            | Err(message) -> Err(message)
            | Ok(entity) -> match (lower_pattern_list arguments typed_module) {
              | Err(message) -> Err(message)
              | Ok(lowered_arguments) -> Ok(LambdaConstructPattern(
                LambdaConstructorPattern {
                  entity: entity,
                  arguments: lowered_arguments
                }))
            }
          }
        | RecordPattern(RecordPatternData { constructor_name: _, fields }) ->
          match (constructor_entity_for span typed_module) {
            | Err(message) -> Err(message)
            | Ok(entity) -> match (lower_record_pattern_fields fields typed_module) {
              | Err(message) -> Err(message)
              | Ok(lowered_fields) -> Ok(LambdaRecordPatternNode(LambdaRecordPattern {
                entity: entity,
                fields: lowered_fields
              }))
            }
          }
        | AscribedPattern(AscribedPatternData { pattern: inner, annotation: _ }) ->
          match (lower_pattern inner typed_module) {
            | Err(message) -> Err(message)
            | Ok(LambdaPattern { checked_type: _, kind, span: _ }) -> Ok(kind)
          }
      };
      match lowered_kind {
        | Err(message) -> Err(message)
        | Ok(kind) -> Ok(LambdaPattern {
          checked_type: checked_type,
          kind: kind,
          span: span
        })
      }
    }
  }

let lower_value_declarations = fn declarations -> fn interface -> fn typed_module ->
  match declarations {
    | Nil -> Ok(Nil)
    | Cons(ValueDeclaration(declaration), remaining) -> {
      let ValueDeclarationData { name: ValueName(name), body, span } = declaration;
      match (module_value_binding name span typed_module) {
        | Err(message) -> Err(message)
        | Ok(binding) -> match (find_value_scheme name (interface_values interface)) {
          | None -> Err("LambdaIR: top-level value has no checked type scheme")
          | Some(scheme) -> match (lower_expression body typed_module) {
            | Err(message) -> Err(message)
            | Ok(lowered_body) -> match (lower_value_declarations remaining
                interface typed_module) {
              | Err(message) -> Err(message)
              | Ok(lowered_remaining) -> Ok(Cons(LambdaValueDeclaration {
                binding: binding,
                scheme: scheme,
                body: lowered_body,
                span: span
              }, lowered_remaining))
            }
          }
        }
      }
    }
    | Cons(TypeDeclaration(_), remaining) ->
      lower_value_declarations remaining interface typed_module
  }

let interface_values = fn (InferredModuleInterface { values, types: _ }) -> values

let lower_checked_module_parts = fn interface -> fn typed_module -> {
  let TypedModule {
    source_module: SourceModule { imports: _, declarations },
    expression_types: _, pattern_types: _, nominal_types, substitutions,
    bindings: _, value_references: _, constructor_references: _
  } = typed_module;
  match (lower_value_declarations declarations interface typed_module) {
    | Err(message) -> Err(message)
    | Ok(values) -> Ok(LambdaModule {
      nominal_types: nominal_types,
      substitutions: substitutions,
      recursive_values: values
    })
  }
}

let lower_checked_module = fn checked_module -> {
  let CheckedModuleInterface {
    loaded_module: _, interface, typed_module
  } = checked_module;
  lower_checked_module_parts interface typed_module
}
