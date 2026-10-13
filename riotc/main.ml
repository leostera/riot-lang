// RiotML compiler orchestration and command-line entry point.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use Host.{
  args, current_directory, runtime_directory, read_file, write_file, write_stderr,
  write_stdout, run_process
}
use Syntax.Ast.{
  ValueName, SourceModule, Declaration, ValueDeclaration, ValueDeclarationData,
  RecordExpression, RecordExpressionField, Binary, BinaryExpression, Addition,
  TypeDeclaration,
  Function, FunctionExpression, Apply, ApplicationExpression, Let, LetExpression,
  Match, MatchExpression, MatchArm, ConstructorExpr, ConstructorExpression,
  Negate,
  Ascription, AscriptionExpression, Block, TupleExpr, RecordExpr, expression_kind
}
use Syntax.Lexer.{SourceSpan, lex, UnterminatedString, UnterminatedEscape}
use Check.Inference.{
  CheckedModuleInterface, InferredModuleInterface, TypedModule,
  SchemeBinding, BindingIdentity, ModuleValueBinding, LocalValueBinding,
  InferredNamedType, AppliedType, apply_substitutions,
  infer_source_module, validate_native_entry_module
}
use Codegen.C.Emit.{emit_module_program}
use Lambda.Ir.{LambdaModule}
use Lambda.Lower.{lower_checked_module, expression_type}
use NativeIr.Lower.{lower_module, lower_module_for_entry}
use NativeIr.Verify.{verify_module}
use NativeIr.Ir.{
  NativeModule, NativeGlobal, NativeFunction, NativeFunctionId,
  NativeTopLevelFunction, NativeNestedFunction, NativeNestedFunctionId,
  NativeModuleInitializer, NativeBlock, NativeBlockId,
  NativeInstruction, NativeOperation, NativeCallOperation, NativeCall,
  NativeDirectCall, NativeCreateClosure, NativeClosure, NativeStoreGlobal,
  NativeConstant, NativeUnit, NativeReturn
}
use NativeIr.Print.{print_native_module, print_function_id}
use Backend.Native.Aarch64.SelectMachineIr.{select_function}
use Backend.Native.Aarch64.PrintMachineIr.{print_function, print_failure}
use Backend.Native.Aarch64.LowerLinearIr.{lower_function}
use Backend.Native.Aarch64.VerifyLinearIr.{verify_function}
use Backend.Native.Aarch64.PrintLinearIr.{print_linear_failure, print_linear_function}
use Backend.Native.Aarch64.LinearIr.{A64LinearFunction}
use Backend.Native.Aarch64.Emit.{emit_program}
use Modules.{
  LoadedModule, load_source_graph, module_source_pairs,
  directory_of, join_path
}
use String.{equal, get}

type SourcePath = SourcePath(string)
type ExecutablePath = ExecutablePath(string)

type NativeCompileRequest = NativeCompileRequest {
  source_path: SourcePath,
  output_path: ExecutablePath
}

type CompilationStage =
  | SourceRead
  | SourceParsing
  | SourceTypechecking
  | LambdaLowering
  | NativeIRLowering
  | COutputWrite
  | ClangLaunch
  | ClangCompilation

type CompileError = CompileError {
  stage: CompilationStage,
  message: string,
  source_span: Option<SourceSpan>
}

let compile_error = fn stage -> fn message -> fn source_span -> CompileError {
  stage: stage,
  message: message,
  source_span: source_span
}

let clang_arguments = fn runtime_directory -> fn c_file -> fn executable ->
  Cons("-std=gnu11", Cons(
    "-I", Cons(runtime_directory, Cons(
      c_file, Cons(join_path runtime_directory "runtime.c", Cons(
        "-o", Cons(executable, Nil)
      ))
    ))
  ))

let run_clang = fn runtime_directory -> fn c_file -> fn executable ->
  match (run_process "clang" (clang_arguments runtime_directory c_file executable)) {
    | Err(message) -> Err(compile_error (ClangLaunch)
      ("cannot run Clang: " + message) None)
    | Ok(0) -> Ok(0)
    | Ok(_) -> Err(compile_error (ClangCompilation)
      "Clang failed with a non-zero exit status" None)
  }

let check_loaded_modules = fn (modules: List<LoadedModule>) ->
  fn (checked_modules: List<CheckedModuleInterface>) ->
  match modules {
    | Nil -> Ok(checked_modules)
    | Cons(LoadedModule { module_path, source_path, source_module }, remaining) -> {
      let loaded_module = LoadedModule {
        module_path: module_path,
        source_path: source_path,
        source_module: source_module
      };
      match (infer_source_module loaded_module checked_modules) {
        | Err(message) -> Err("type error in " + source_path + ": " + message)
        | Ok(checked_module) -> check_loaded_modules remaining
          (Cons(checked_module, checked_modules))
      }
    }
  }

let lower_checked_modules = fn checked_modules -> match checked_modules {
  | Nil -> Ok(Nil)
  | Cons(CheckedModuleInterface {
      loaded_module: LoadedModule { module_path, source_path, source_module },
      interface, typed_module
    }, remaining) -> {
    let checked_module = CheckedModuleInterface {
      loaded_module: LoadedModule {
        module_path: module_path,
        source_path: source_path,
        source_module: source_module
      },
      interface: interface,
      typed_module: typed_module
    };
    match (lower_checked_module checked_module) {
      | Err(message) -> Err(message)
      | Ok(lambda_module) -> match (lower_checked_modules remaining) {
        | Err(message) -> Err(message)
        | Ok(lambda_modules) -> Ok(Cons((lambda_module: LambdaModule), lambda_modules))
      }
    }
  }
}

let verify_native_modules = fn checked_modules -> fn lambda_modules ->
  match checked_modules {
    | Nil -> match lambda_modules {
      | Nil -> Ok(Nil)
      | _ -> Err("NativeIR lowering: checked module count mismatch")
    }
    | Cons(CheckedModuleInterface {
        loaded_module: LoadedModule { module_path: _, source_path, source_module: _ },
        interface: _, typed_module: _
      }, remaining_checked) -> match lambda_modules {
      | Nil -> Err("NativeIR lowering: checked module count mismatch")
      | Cons(lambda_module, remaining_lambda) -> match (lower_module source_path lambda_module) {
        | Err(message) -> Err("NativeIR lowering failed for " + source_path + ": " + message)
        | Ok(native_module) -> match (verify_module native_module) {
          | Err(message) -> Err("NativeIR verification failed for " + source_path + ": " + message)
          | Ok(_) -> match (verify_native_modules remaining_checked remaining_lambda) {
            | Err(message) -> Err(message)
            | Ok(modules) -> Ok(Cons(native_module, modules))
          }
        }
      }
    }
  }

let find_root_checked_module = fn (root_path: string) ->
  fn (checked_modules: List<CheckedModuleInterface>) ->
  match checked_modules {
    | Nil -> None
    | Cons(CheckedModuleInterface {
        loaded_module: LoadedModule { module_path, source_path, source_module },
        interface,
        typed_module
      }, remaining) -> match (equal root_path source_path) {
      | true -> Some(CheckedModuleInterface {
        loaded_module: LoadedModule {
          module_path: module_path,
          source_path: source_path,
          source_module: source_module
        },
        interface: interface,
        typed_module: typed_module
      })
      | false -> find_root_checked_module root_path remaining
    }
  }

// Keep the C baseline independent; the `native-ir` command owns LambdaIR/NativeIR validation.
let c_checked_type_name = fn expression -> fn typed_module -> {
  let TypedModule { source_module: _, expression_types: _, pattern_types: _,
    nominal_types: _, substitutions, bindings: _, value_references: _,
    constructor_references: _ } = typed_module;
  match (expression_type expression typed_module) {
    | Err(message) -> Err(message)
    | Ok(inferred_type) -> match (apply_substitutions inferred_type substitutions) {
      | AppliedType(InferredNamedType { name, arguments: Nil }) -> Ok(name)
      | _ -> Err("C backend: integer operation has no concrete operand type")
    }
  }
}

let validate_c_operand_type = fn operation -> fn allow_string ->
  fn expression -> fn typed_module -> match (c_checked_type_name expression typed_module) {
    | Err(message) -> Err(message)
    | Ok("i32") -> Ok(true)
    | Ok("string") -> match allow_string {
      | true -> Ok(true)
      | false -> Err("C backend does not support string " + operation)
    }
    | Ok(name) -> Err("C backend does not support " + operation + " on `" + name + "`")
  }

let validate_c_binary = fn operator -> fn left -> fn typed_module ->
  match operator {
    | Addition -> validate_c_operand_type "addition" true left typed_module
    | _ -> validate_c_operand_type "integer arithmetic" false left typed_module
  }

let validate_c_expressions = fn expressions -> fn typed_module -> match expressions {
  | Nil -> Ok(true)
  | Cons(expression, remaining) -> match (validate_c_expression expression typed_module) {
    | Err(message) -> Err(message)
    | Ok(_) -> validate_c_expressions remaining typed_module
  }
}

let validate_c_record_fields = fn fields -> fn typed_module -> match fields {
  | Nil -> Ok(true)
  | Cons(RecordExpressionField { name: _, value }, remaining) ->
    match (validate_c_expression value typed_module) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_c_record_fields remaining typed_module
    }
}

let validate_c_match_arms = fn arms -> fn typed_module -> match arms {
  | Nil -> Ok(true)
  | Cons(MatchArm { pattern: _, body }, remaining) ->
    match (validate_c_expression body typed_module) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_c_match_arms remaining typed_module
    }
}

let validate_c_expression = fn expression -> fn typed_module ->
  match (expression_kind expression) {
    | Binary(BinaryExpression { operator, left, right }) -> match
        (validate_c_binary operator left typed_module) {
      | Err(message) -> Err(message)
      | Ok(_) -> match (validate_c_expression left typed_module) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_c_expression right typed_module
      }
    }
    | Negate(operand) -> match (validate_c_operand_type "negation" false operand typed_module) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_c_expression operand typed_module
    }
    | TupleExpr(items) -> validate_c_expressions items typed_module
    | RecordExpr(RecordExpression { constructor_name: _, fields }) ->
      validate_c_record_fields fields typed_module
    | Function(FunctionExpression { parameter: _, body }) ->
      validate_c_expression body typed_module
    | Apply(ApplicationExpression { callee, argument }) -> match
        (validate_c_expression callee typed_module) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_c_expression argument typed_module
    }
    | Let(LetExpression { pattern: _, annotation: _, value, body }) -> match
        (validate_c_expression value typed_module) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_c_expression body typed_module
    }
    | Match(MatchExpression { scrutinee, arms }) -> match
        (validate_c_expression scrutinee typed_module) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_c_match_arms arms typed_module
    }
    | ConstructorExpr(ConstructorExpression { constructor_name: _, arguments }) ->
      validate_c_expressions arguments typed_module
    | Ascription(AscriptionExpression { expression: inner, annotation: _ }) ->
      validate_c_expression inner typed_module
    | Block(items) -> validate_c_expressions items typed_module
    | _ -> Ok(true)
  }

let validate_c_declarations = fn declarations -> fn typed_module -> match declarations {
  | Nil -> Ok(true)
  | Cons(ValueDeclaration(ValueDeclarationData { name: _, body, span: _ }), remaining) ->
    match (validate_c_expression body typed_module) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_c_declarations remaining typed_module
    }
  | Cons(TypeDeclaration(_), remaining) -> validate_c_declarations remaining typed_module
}

let validate_c_backend_modules = fn checked_modules -> match checked_modules {
  | Nil -> Ok(true)
  | Cons(CheckedModuleInterface {
      loaded_module: _, interface: _, typed_module
    }, remaining) -> {
      let TypedModule {
        source_module: SourceModule { imports: _, declarations },
        expression_types: _, pattern_types: _, nominal_types: _, substitutions: _,
        bindings: _, value_references: _, constructor_references: _
      } = typed_module;
      match (validate_c_declarations declarations typed_module) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_c_backend_modules remaining
      }
    }
}

let compile_module_graph_unchecked = fn (source_file: string) ->
  match (load_source_graph source_file
    (Cons(directory_of source_file, Nil): List<string>)) {
    | Err(message) -> Err(compile_error (SourceParsing) message None)
    | Ok(modules) -> match (check_loaded_modules modules Nil) {
      | Err(message) -> Err(compile_error (SourceTypechecking) message None)
      | Ok(checked_modules) -> match (validate_c_backend_modules checked_modules) {
        | Err(message) -> Err(compile_error (SourceTypechecking) message None)
        | Ok(_) -> match (find_root_checked_module source_file checked_modules) {
        | None -> Err(compile_error (SourceParsing)
          "source graph has no entry module" None)
        | Some(CheckedModuleInterface {
            loaded_module: LoadedModule { source_module, source_path: _, module_path: _ },
            interface, typed_module: _
          }) -> match (validate_native_entry_module source_module interface) {
          | Err(message) -> Err(compile_error (SourceTypechecking) message None)
          | Ok(_) -> Ok(emit_module_program source_file
            (runtime_include_directory source_file) (module_source_pairs modules))
        }
        }
      }
    }
  }

let compile_module_graph = fn (source_file: string) ->
  match (read_file source_file) {
    | Err(message) -> Err(compile_error (SourceRead)
      ("cannot read " + source_file + ": " + message) None)
    | Ok(source) -> match (lex source) {
      | Err(UnterminatedString(span)) -> Err(compile_error (SourceParsing)
        "unterminated string literal" (Some(span)))
      | Err(UnterminatedEscape(span)) -> Err(compile_error (SourceParsing)
        "unterminated escape in string literal" (Some(span)))
      | Ok(_) -> compile_module_graph_unchecked source_file
    }
  }

let path_is_absolute = fn path ->
  match (get path 0) {
    | Some(47) -> true
    | _ -> false
  }

let absolute_runtime_directory = fn source_file -> {
  let source_directory = directory_of source_file;
  match (path_is_absolute source_directory) {
    | true -> source_directory
    | false -> match (equal source_directory ".") {
      | true -> current_directory ()
      | false -> current_directory () + "/" + source_directory
    }
  }
}

let runtime_directory_has_runtime = fn directory ->
  match (read_file (join_path directory "runtime.c")) {
    | Ok(_) -> true
    | Err(_) -> false
  }

let runtime_include_directory = fn source_file -> {
  let embedded_directory = runtime_directory ();
  let current_directory_runtime = join_path (current_directory ()) "riotc";
  let source_directory_runtime = absolute_runtime_directory source_file;
  match (runtime_directory_has_runtime embedded_directory) {
    | true -> embedded_directory
    | false -> match (runtime_directory_has_runtime current_directory_runtime) {
      | true -> current_directory_runtime
      | false -> match (runtime_directory_has_runtime source_directory_runtime) {
        | true -> source_directory_runtime
        | false -> "riotc"
      }
    }
  }
}

let compile_file = fn (NativeCompileRequest {
  source_path: SourcePath(source_file),
  output_path: ExecutablePath(executable)
}) ->
  match (compile_module_graph source_file) {
    | Err(error) -> Err(error)
    | Ok(c_source) -> {
      let c_file = executable + ".c";
      match (write_file c_file c_source) {
        | Err(message) -> Err(compile_error (COutputWrite)
          ("cannot write C output: " + message) None)
        | Ok(_) -> run_clang (runtime_include_directory source_file) c_file executable
      }
    }
  }

let compilation_stage_name = fn stage ->
  match stage {
    | SourceRead -> "source read"
    | SourceParsing -> "source parsing"
    | SourceTypechecking -> "typechecking"
    | LambdaLowering -> "LambdaIR lowering"
    | NativeIRLowering -> "NativeIR lowering and verification"
    | COutputWrite -> "C output"
    | ClangLaunch -> "Clang launch"
    | ClangCompilation -> "Clang compilation"
  }

let decimal_digit = fn value ->
  match value {
    | 0 -> "0"
    | 1 -> "1"
    | 2 -> "2"
    | 3 -> "3"
    | 4 -> "4"
    | 5 -> "5"
    | 6 -> "6"
    | 7 -> "7"
    | 8 -> "8"
    | _ -> "9"
  }

let decimal_text = fn value ->
  match (value < 10) {
    | true -> decimal_digit value
    | false -> decimal_text (value / 10) + decimal_digit (value % 10)
  }

let source_span_suffix = fn source_span ->
  match source_span {
    | None -> ""
    | Some(SourceSpan { start_codepoint, end_codepoint }) ->
      " at codepoints " + decimal_text start_codepoint + ".." + decimal_text end_codepoint
  }

let report_error = fn (CompileError { stage, message, source_span }) -> {
  write_stderr ("riotml: " + compilation_stage_name stage + ": " + message
    + source_span_suffix source_span + "\n");
  1
}

let native_ir_modules_for_file = fn source_file -> match (read_file source_file) {
  | Err(message) -> Err(compile_error (SourceRead)
      ("cannot read " + source_file + ": " + message) None)
  | Ok(source) -> match (lex source) {
    | Err(UnterminatedString(span)) -> Err(compile_error (SourceParsing)
      "unterminated string literal" (Some(span)))
    | Err(UnterminatedEscape(span)) -> Err(compile_error (SourceParsing)
      "unterminated escape in string literal" (Some(span)))
    | Ok(_) -> match (load_source_graph source_file
        (Cons(directory_of source_file, Nil))) {
      | Err(message) -> Err(compile_error (SourceParsing) message None)
      | Ok(modules) -> match (check_loaded_modules modules Nil) {
        | Err(message) -> Err(compile_error (SourceTypechecking) message None)
        | Ok(checked_modules) -> match (lower_checked_modules checked_modules) {
          | Err(message) -> Err(compile_error (LambdaLowering) message None)
          | Ok(lambda_modules) -> match (verify_native_modules checked_modules lambda_modules) {
            | Err(message) -> Err(compile_error (NativeIRLowering) message None)
            | Ok(native_modules) -> Ok(native_modules)
          }
        }
      }
    }
  }
}

let module_binding_span = fn (BindingIdentity {
  source_path: _, declaration_span, name: _, scope: _
}) -> declaration_span

let find_module_value_binding = fn wanted_name -> fn bindings -> match bindings {
  | Nil -> None
  | Cons(binding, remaining) -> match binding {
    | BindingIdentity {
        source_path: _, declaration_span: _, name: ValueName(name),
        scope: ModuleValueBinding
      } -> match (equal wanted_name name) {
      | true -> Some(binding)
      | false -> find_module_value_binding wanted_name remaining
    }
    | _ -> find_module_value_binding wanted_name remaining
  }
}

let native_globals_from_interface_values = fn values -> fn bindings -> match values {
  | Nil -> Ok(Nil)
  | Cons(SchemeBinding(ValueName(name), scheme), remaining) ->
    match (find_module_value_binding name bindings) {
      | None -> Err("NativeIR: checked module interface has no binding for `" + name + "`")
      | Some(binding) -> match (native_globals_from_interface_values remaining bindings) {
        | Err(message) -> Err(message)
        | Ok(rest) -> Ok(Cons(NativeGlobal {
          binding: binding,
          scheme: scheme,
          span: module_binding_span binding
        }, rest))
      }
    }
}

let append_native_globals = fn (first: List<NativeGlobal>) ->
  fn (second: List<NativeGlobal>) -> match first {
  | Nil -> second
  | Cons(global, remaining) ->
    Cons(global, append_native_globals remaining second)
}

let native_globals_from_checked_modules = fn modules -> match modules {
  | Nil -> Ok(Nil)
  | Cons(CheckedModuleInterface {
      loaded_module: _, interface: InferredModuleInterface { values, types: _ },
      typed_module: TypedModule {
        source_module: _, expression_types: _, pattern_types: _, nominal_types: _,
        substitutions: _, bindings, value_references: _, constructor_references: _
      }
    }, remaining) -> match (native_globals_from_interface_values values bindings) {
    | Err(message) -> Err(message)
    | Ok(module_globals) -> match (native_globals_from_checked_modules remaining) {
      | Err(message) -> Err(message)
      | Ok(rest) -> Ok(append_native_globals module_globals rest)
    }
  }
}

// NativeCodegen compiles one root-module function set. Typecheck every import,
// but Lambda-lower and build NativeIR only for the root; interfaces supply the
// verified global signatures for referenced dependencies.
let native_ir_root_modules_for_file = fn source_file -> match (read_file source_file) {
  | Err(message) -> Err(compile_error (SourceRead)
      ("cannot read " + source_file + ": " + message) None)
  | Ok(source) -> match (lex source) {
    | Err(UnterminatedString(span)) -> Err(compile_error (SourceParsing)
      "unterminated string literal" (Some(span)))
    | Err(UnterminatedEscape(span)) -> Err(compile_error (SourceParsing)
      "unterminated escape in string literal" (Some(span)))
    | Ok(_) -> match (load_source_graph source_file
        (Cons(directory_of source_file, Nil))) {
      | Err(message) -> Err(compile_error (SourceParsing) message None)
      | Ok(modules) -> match (check_loaded_modules modules Nil) {
        | Err(message) -> Err(compile_error (SourceTypechecking) message None)
        | Ok(checked_modules) -> match (find_root_checked_module source_file checked_modules) {
          | None -> Err(compile_error (LambdaLowering)
            "source graph has no root checked module" None)
          | Some(checked_module) -> match (lower_checked_module checked_module) {
            | Err(message) -> Err(compile_error (LambdaLowering)
              ("LambdaIR lowering failed for " + source_file + ": " + message) None)
            | Ok(lambda_module) -> match (lower_module source_file lambda_module) {
              | Err(message) -> Err(compile_error (NativeIRLowering)
                ("NativeIR lowering failed for " + source_file + ": " + message) None)
              | Ok(root_module) -> match (native_globals_from_checked_modules checked_modules) {
                | Err(message) -> Err(compile_error (NativeIRLowering) message None)
                | Ok(globals) -> {
                  let NativeModule {
                    module_identity, nominal_types, substitutions, globals: _,
                    initializer, functions
                  } = root_module;
                  let native_module = NativeModule {
                    module_identity: module_identity,
                    nominal_types: nominal_types,
                    substitutions: substitutions,
                    globals: globals,
                    initializer: initializer,
                    functions: functions
                  };
                  match (verify_module native_module) {
                    | Err(message) -> Err(compile_error (NativeIRLowering)
                      ("NativeIR verification failed for " + source_file + ": " + message) None)
                    | Ok(_) -> Ok(Cons(native_module, Nil))
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

let native_ir_root_graph_for_entry_file = fn source_file -> fn entry_name ->
  match (read_file source_file) {
    | Err(message) -> Err(compile_error (SourceRead)
        ("cannot read " + source_file + ": " + message) None)
    | Ok(source) -> match (lex source) {
      | Err(UnterminatedString(span)) -> Err(compile_error (SourceParsing)
        "unterminated string literal" (Some(span)))
      | Err(UnterminatedEscape(span)) -> Err(compile_error (SourceParsing)
        "unterminated escape in string literal" (Some(span)))
      | Ok(_) -> match (load_source_graph source_file
          (Cons(directory_of source_file, Nil))) {
        | Err(message) -> Err(compile_error (SourceParsing) message None)
        | Ok(modules) -> match (check_loaded_modules modules Nil) {
          | Err(message) -> Err(compile_error (SourceTypechecking) message None)
          | Ok(checked_modules) -> match (find_root_checked_module source_file checked_modules) {
            | None -> Err(compile_error (LambdaLowering)
              "source graph has no root checked module" None)
            | Some(checked_module) -> match (lower_checked_module checked_module) {
              | Err(message) -> Err(compile_error (LambdaLowering)
                ("LambdaIR lowering failed for " + source_file + ": " + message) None)
              | Ok(lambda_module) -> match (lower_module_for_entry source_file
                  lambda_module entry_name) {
                | Err(message) -> Err(compile_error (NativeIRLowering)
                  ("NativeIR graph lowering failed for " + source_file + ": " + message) None)
                | Ok(root_module) -> match (native_globals_from_checked_modules checked_modules) {
                  | Err(message) -> Err(compile_error (NativeIRLowering) message None)
                  | Ok(globals) -> {
                    let NativeModule {
                      module_identity, nominal_types, substitutions, globals: _,
                      initializer, functions
                    } = root_module;
                    let native_module = NativeModule {
                      module_identity: module_identity,
                      nominal_types: nominal_types,
                      substitutions: substitutions,
                      globals: globals,
                      initializer: initializer,
                      functions: functions
                    };
                    match (verify_module native_module) {
                      | Err(message) -> Err(compile_error (NativeIRLowering)
                        ("NativeIR verification failed for " + source_file + ": " + message) None)
                      | Ok(_) -> Ok(Cons(native_module, Nil))
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

let print_native_modules = fn modules -> match modules {
  | Nil -> ""
  | Cons(native_module, remaining) ->
    print_native_module native_module + "\n" + print_native_modules remaining
}

let run_native_ir_command = fn source_file ->
  match (native_ir_modules_for_file source_file) {
    | Err(error) -> report_error error
    | Ok(native_modules) -> {
      write_stdout (print_native_modules native_modules);
      0
    }
  }

let find_native_module = fn wanted -> fn modules -> match modules {
  | Nil -> None
  | Cons(native_module, remaining) -> {
    let NativeModule {
      module_identity, nominal_types: _, substitutions: _, globals: _,
      initializer: _, functions: _
    } = native_module;
    match (equal wanted module_identity) {
      | true -> Some(native_module)
      | false -> find_native_module wanted remaining
    }
  }
}

let find_top_level_function = fn wanted_name -> fn functions -> match functions {
  | Nil -> None
  | Cons(function, remaining) -> {
    let NativeFunction {
      identity, result_type: _, entry: _, blocks: _, span: _
    } = function;
    match identity {
      | NativeTopLevelFunction(BindingIdentity {
          name: ValueName(name), source_path: _, declaration_span: _, scope: _
        }) -> match (equal wanted_name name) {
        | true -> Some(function)
        | false -> find_top_level_function wanted_name remaining
      }
      | _ -> find_top_level_function wanted_name remaining
    }
  }
}

let same_source_span = fn first -> fn second -> {
  let SourceSpan { start_codepoint: first_start, end_codepoint: first_end } = first;
  let SourceSpan { start_codepoint: second_start, end_codepoint: second_end } = second;
  match (first_start == second_start) {
  | false -> false
  | true -> first_end == second_end
  }
}

let same_binding_scope = fn first -> fn second -> match first {
  | ModuleValueBinding -> match second {
    | ModuleValueBinding -> true
    | _ -> false
  }
  | LocalValueBinding -> match second {
    | LocalValueBinding -> true
    | _ -> false
  }
}

let same_binding_identity = fn first -> fn second -> {
  let BindingIdentity {
    source_path: first_path, declaration_span: first_span,
    name: ValueName(first_name), scope: first_scope
  } = first;
  let BindingIdentity {
    source_path: second_path, declaration_span: second_span,
    name: ValueName(second_name), scope: second_scope
  } = second;
  match (equal first_path second_path) {
  | false -> false
  | true -> match (equal first_name second_name) {
    | false -> false
    | true -> match (same_source_span first_span second_span) {
      | false -> false
      | true -> same_binding_scope first_scope second_scope
    }
  }
}
}

let same_native_function_id = fn first -> fn second -> match first {
  | NativeTopLevelFunction(first_binding) -> match second {
    | NativeTopLevelFunction(second_binding) ->
      same_binding_identity first_binding second_binding
    | _ -> false
  }
  | NativeNestedFunction(NativeNestedFunctionId {
      parent: first_parent, source_span: first_span, ordinal: first_ordinal
    }) -> match second {
    | NativeNestedFunction(NativeNestedFunctionId {
        parent: second_parent, source_span: second_span, ordinal: second_ordinal
      }) -> match (same_binding_identity first_parent second_parent) {
      | false -> false
      | true -> match (same_source_span first_span second_span) {
        | false -> false
        | true -> first_ordinal == second_ordinal
      }
    }
    | _ -> false
  }
  | NativeModuleInitializer(first_path) -> match second {
    | NativeModuleInitializer(second_path) -> equal first_path second_path
    | _ -> false
  }
}

let find_native_function_by_id = fn wanted -> fn functions -> match functions {
  | Nil -> None
  | Cons(function, remaining) -> {
    let NativeFunction { identity, result_type: _, entry: _, blocks: _, span: _ } = function;
    match (same_native_function_id wanted identity) {
      | true -> Some(function)
      | false -> find_native_function_by_id wanted remaining
    }
  }
}

let native_function_id_is_in_functions = fn wanted -> fn functions -> match functions {
  | Nil -> false
  | Cons(function, remaining) -> {
    let NativeFunction { identity, result_type: _, entry: _, blocks: _, span: _ } = function;
    match (same_native_function_id wanted identity) {
      | true -> true
      | false -> native_function_id_is_in_functions wanted remaining
    }
  }
}

let append_native_function_ids = fn first -> fn second -> match first {
  | Nil -> second
  | Cons(identity, remaining) ->
    Cons(identity, append_native_function_ids remaining second)
}

let direct_call_targets_in_instructions = fn instructions -> match instructions {
  | Nil -> Nil
  | Cons(NativeInstruction {
      result: _, result_type: _,
      operation: NativeCallOperation(NativeCall {
        target: NativeDirectCall(callee), arguments: _, argument_types: _, result_type: _
      }), effects: _, span: _
    }, remaining) -> Cons(callee, direct_call_targets_in_instructions remaining)
  | Cons(NativeInstruction { result: _, result_type: _, operation: _, effects: _, span: _ },
      remaining) -> direct_call_targets_in_instructions remaining
}

let direct_call_targets_in_blocks = fn blocks -> match blocks {
  | Nil -> Nil
  | Cons(NativeBlock {
      identity: _, parameters: _, instructions, terminator: _, span: _
    }, remaining) -> append_native_function_ids
      (direct_call_targets_in_instructions instructions)
      (direct_call_targets_in_blocks remaining)
}

let direct_call_targets_in_function = fn (NativeFunction {
  identity: _, result_type: _, entry: _, blocks, span: _
}) -> direct_call_targets_in_blocks blocks

let collect_native_direct_call_graph = fn pending -> fn selected -> fn available ->
  match pending {
    | Nil -> Ok(selected)
    | Cons(wanted, remaining) -> match (native_function_id_is_in_functions wanted selected) {
      | true -> collect_native_direct_call_graph remaining selected available
      | false -> match (find_native_function_by_id wanted available) {
        | None -> Err("direct-call target is absent from the root NativeIR function set: "
          + print_function_id wanted)
        | Some(function) -> {
          let targets = direct_call_targets_in_function function;
          collect_native_direct_call_graph
            (append_native_function_ids targets remaining)
            (Cons(function, selected)) available
        }
      }
    }
  }

let lower_native_function_set = fn functions -> fn substitutions -> match functions {
  | Nil -> Ok(Nil)
  | Cons(function, remaining) -> match (select_function function substitutions) {
    | Err(failure) -> Err(print_failure failure)
    | Ok(machine_function) -> match (lower_function machine_function) {
      | Err(failure) -> Err(print_linear_failure failure)
      | Ok(linear_function) -> match (verify_function linear_function) {
        | Err(reason) -> Err("AArch64 LinearIR verification failed: " + reason)
        | Ok(_) -> match (lower_native_function_set remaining substitutions) {
          | Err(message) -> Err(message)
          | Ok(rest) -> Ok(Cons(linear_function, rest))
        }
      }
    }
  }
}

let initializer_operation_is_closure_only = fn operation -> match operation {
  | NativeCreateClosure(NativeClosure { function: _, captures: Nil }) -> true
  | NativeStoreGlobal(_) -> true
  | NativeConstant(NativeUnit) -> true
  | _ -> false
}

let initializer_instructions_are_closure_only = fn instructions -> match instructions {
  | Nil -> true
  | Cons(NativeInstruction {
      result: _, result_type: _, operation, effects: _, span: _
    }, remaining) -> match (initializer_operation_is_closure_only operation) {
    | false -> false
    | true -> initializer_instructions_are_closure_only remaining
  }
}

let initializer_terminator_is_return = fn terminator -> match terminator {
  | NativeReturn(_) -> true
  | _ -> false
}

let same_native_block_id = fn first -> fn second -> {
  let NativeBlockId(first_id) = first;
  let NativeBlockId(second_id) = second;
  first_id == second_id
}

let module_initializer_is_closure_only = fn initializer -> fn functions ->
  match (find_native_function_by_id initializer functions) {
    | None -> false
    | Some(NativeFunction {
        identity: _, result_type: _, entry, blocks, span: _
      }) -> match blocks {
      | Cons(NativeBlock {
          identity, parameters: Nil, instructions, terminator, span: _
        }, Nil) -> match (same_native_block_id entry identity) {
        | false -> false
        | true -> match (initializer_terminator_is_return terminator) {
          | false -> false
          | true -> initializer_instructions_are_closure_only instructions
        }
      }
      | _ -> false
    }
  }

let run_aarch64_mir_command = fn source_file -> fn function_name ->
  match (native_ir_root_modules_for_file source_file) {
    | Err(error) -> report_error error
    | Ok(native_modules) -> match (find_native_module source_file native_modules) {
      | None -> {
        write_stderr ("riotml: AArch64 selection found no module for " + source_file + "\n");
        1
      }
      | Some(NativeModule {
          module_identity: _, nominal_types: _, substitutions, globals: _,
          initializer: _, functions
        }) -> match (find_top_level_function function_name functions) {
        | None -> {
          write_stderr ("riotml: AArch64 selection found no top-level function `"
            + function_name + "` in " + source_file + "\n");
          1
        }
        | Some(function) -> match (select_function function substitutions) {
          | Err(failure) -> {
            write_stderr ("riotml: " + print_failure failure + "\n");
            1
          }
          | Ok(machine_function) -> {
            write_stdout (print_function machine_function);
            0
          }
        }
      }
    }
  }

let run_aarch64_linear_ir_command = fn source_file -> fn function_name ->
  match (native_ir_root_modules_for_file source_file) {
    | Err(error) -> report_error error
    | Ok(native_modules) -> match (find_native_module source_file native_modules) {
      | None -> {
        write_stderr ("riotml: AArch64 linearization found no module for " + source_file + "\n");
        1
      }
      | Some(NativeModule {
          module_identity: _, nominal_types: _, substitutions, globals: _,
          initializer: _, functions
        }) -> match (find_top_level_function function_name functions) {
        | None -> {
          write_stderr ("riotml: AArch64 linearization found no top-level function `"
            + function_name + "` in " + source_file + "\n");
          1
        }
        | Some(function) -> match (select_function function substitutions) {
          | Err(failure) -> {
            write_stderr ("riotml: " + print_failure failure + "\n");
            1
          }
          | Ok(machine_function) -> match (lower_function machine_function) {
            | Err(failure) -> {
              write_stderr ("riotml: " + print_linear_failure failure + "\n");
              1
            }
            | Ok(linear_function) -> match (verify_function linear_function) {
              | Err(reason) -> {
                write_stderr ("riotml: AArch64 LinearIR verification failed: " + reason + "\n");
                1
              }
              | Ok(_) -> {
                write_stdout (print_linear_function linear_function);
                0
              }
            }
          }
        }
      }
    }
  }

let lower_named_linear_function = fn function_name -> fn substitutions -> fn functions ->
  match (find_top_level_function function_name functions) {
    | None -> Err("no top-level function named `" + function_name + "`")
    | Some(function) -> match (select_function function substitutions) {
      | Err(failure) -> Err(print_failure failure)
      | Ok(machine_function) -> match (lower_function machine_function) {
        | Err(failure) -> Err(print_linear_failure failure)
        | Ok(linear_function) -> match (verify_function linear_function) {
          | Err(reason) -> Err("AArch64 LinearIR verification failed: " + reason)
          | Ok(_) -> Ok(linear_function)
        }
      }
    }
  }

let remove_native_output = fn output ->
  match (run_process "/bin/rm" (Cons("-f", Cons("--", Cons(output, Nil))))) {
    | Err(message) -> Err("cannot remove stale output: " + message)
    | Ok(0) -> Ok(())
    | Ok(_) -> Err("cannot remove stale output")
  }

let prepare_native_outputs = fn output -> fn assembly_path ->
  match (remove_native_output output) {
    | Err(message) -> Err(message)
    | Ok(_) -> remove_native_output assembly_path
  }

let fail_native_link = fn output -> fn message -> {
  let cleanup = remove_native_output output;
  match cleanup {
    | Ok(_) -> {
      write_stderr ("riotml: AArch64 Codegen: " + message + "\n");
      1
    }
    | Err(cleanup_error) -> {
      write_stderr ("riotml: AArch64 Codegen: " + message + "; " + cleanup_error + "\n");
      1
    }
  }
}

let native_linker_arguments = fn runtime_directory -> fn assembly_path -> fn output ->
  Cons("-target", Cons("arm64-apple-macosx26.0", Cons("-I", Cons(runtime_directory,
    Cons(assembly_path, Cons(join_path runtime_directory "runtime.c",
      Cons("-o", Cons(output, Nil))))))))

let run_native_linker = fn runtime_directory -> fn assembly_path -> fn output ->
  match (run_process "clang" (native_linker_arguments runtime_directory assembly_path output)) {
    | Err(message) -> fail_native_link output ("cannot run Clang: " + message)
    | Ok(0) -> 0
    | Ok(_) -> fail_native_link output "Clang assembler/linker failed"
  }

let run_aarch64_compile_function_command = fn source_file -> fn function_name -> fn output -> {
  let assembly_path = output + ".aarch64.s";
  match (equal source_file output) {
    | true -> {
      write_stderr "riotml: AArch64 Codegen: source and executable paths must differ\n";
      1
    }
    | false -> match (equal source_file assembly_path) {
      | true -> {
        write_stderr "riotml: AArch64 Codegen: source and assembly paths must differ\n";
        1
      }
      | false -> match (prepare_native_outputs output assembly_path) {
        | Err(message) -> {
          write_stderr ("riotml: AArch64 Codegen: " + message + "\n");
          1
        }
        | Ok(_) -> match (native_ir_root_modules_for_file source_file) {
          | Err(error) -> report_error error
          | Ok(native_modules) -> match (find_native_module source_file native_modules) {
            | None -> {
              write_stderr ("riotml: AArch64 Codegen found no module for " + source_file + "\n");
              1
            }
            | Some(NativeModule {
                module_identity: _, nominal_types: _, substitutions, globals: _,
                initializer: _, functions
              }) -> match (lower_named_linear_function function_name substitutions functions) {
              | Err(message) -> {
                write_stderr ("riotml: AArch64 Codegen: " + message + "\n");
                1
              }
              | Ok(linear_function) -> {
                let A64LinearFunction {
                  identity, result_type: _, entry: _, values: _, frame_size: _,
                  items: _, span: _
                } = linear_function;
                match (emit_program (Cons(linear_function, Nil)) identity) {
                  | Err(message) -> {
                    write_stderr ("riotml: AArch64 Codegen: " + message + "\n");
                    1
                  }
                  | Ok(assembly) -> match (write_file assembly_path assembly) {
                    | Err(message) -> {
                      write_stderr ("riotml: AArch64 Codegen: cannot write assembly: "
                        + message + "\n");
                      1
                    }
                    | Ok(_) -> run_native_linker (runtime_include_directory source_file)
                      assembly_path output
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

// Compile a selected same-module direct-call graph. This deliberately omits the
// module initializer only when it consists solely of closure stores; selected
// functions must not require those globals or any unsupported closure path.
let run_aarch64_compile_direct_graph_command = fn source_file ->
  fn function_name -> fn output -> {
  let assembly_path = output + ".aarch64.s";
  match (equal source_file output) {
    | true -> {
      write_stderr "riotml: AArch64 Codegen: source and executable paths must differ\n";
      1
    }
    | false -> match (equal source_file assembly_path) {
      | true -> {
        write_stderr "riotml: AArch64 Codegen: source and assembly paths must differ\n";
        1
      }
      | false -> match (prepare_native_outputs output assembly_path) {
        | Err(message) -> {
          write_stderr ("riotml: AArch64 Codegen: " + message + "\n");
          1
        }
        | Ok(_) -> match (native_ir_root_graph_for_entry_file source_file function_name) {
          | Err(error) -> report_error error
          | Ok(native_modules) -> match (find_native_module source_file native_modules) {
            | None -> {
              write_stderr ("riotml: AArch64 Codegen found no module for " + source_file + "\n");
              1
            }
            | Some(NativeModule {
                module_identity: _, nominal_types: _, substitutions, globals: _,
                initializer, functions
              }) -> match (module_initializer_is_closure_only initializer functions) {
              | false -> {
                write_stderr ("riotml: AArch64 Codegen: module initializer for "
                  + source_file + " has non-closure work; direct-call graph mode cannot omit it\n");
                1
              }
              | true -> match (find_top_level_function function_name functions) {
                | None -> {
                  write_stderr ("riotml: AArch64 Codegen found no top-level function `"
                    + function_name + "` in " + source_file + "\n");
                  1
                }
                | Some(entry_function) -> {
                  let NativeFunction {
                    identity: entry_identity, result_type: _, entry: _, blocks: _, span: _
                  } = entry_function;
                  match (collect_native_direct_call_graph
                      (Cons(entry_identity, Nil)) Nil functions) {
                    | Err(message) -> {
                      write_stderr ("riotml: AArch64 Codegen: " + message + "\n");
                      1
                    }
                    | Ok(native_functions) -> match (lower_native_function_set
                        native_functions substitutions) {
                      | Err(message) -> {
                        write_stderr ("riotml: AArch64 Codegen: " + message + "\n");
                        1
                      }
                      | Ok(linear_functions) -> match (emit_program
                          linear_functions entry_identity) {
                        | Err(message) -> {
                          write_stderr ("riotml: AArch64 Codegen: " + message + "\n");
                          1
                        }
                        | Ok(assembly) -> match (write_file assembly_path assembly) {
                          | Err(message) -> {
                            write_stderr ("riotml: AArch64 Codegen: cannot write assembly: "
                              + message + "\n");
                            1
                          }
                          | Ok(_) -> run_native_linker
                            (runtime_include_directory source_file) assembly_path output
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
    }
  }
}

let parse_compile_request = fn arguments ->
  match arguments {
    | Cons("compile", Cons(source, Cons("-o", Cons(output, Nil)))) ->
      Ok(NativeCompileRequest {
        source_path: SourcePath(source),
        output_path: ExecutablePath(output)
      })
    | _ -> Err("usage: riotml compile SOURCE -o OUTPUT | native-ir SOURCE | aarch64-mir SOURCE FUNCTION | aarch64-linear-ir SOURCE FUNCTION | aarch64-compile-function SOURCE FUNCTION -o OUTPUT | aarch64-compile-direct-graph SOURCE ENTRY -o OUTPUT")
  }

let main = fn () -> match (args ()) {
  | Cons("native-ir", Cons(source_file, Nil)) -> run_native_ir_command source_file
  | Cons("aarch64-compile-function", Cons(source_file,
      Cons(function_name, Cons("-o", Cons(output, Nil))))) ->
    run_aarch64_compile_function_command source_file function_name output
  | Cons("aarch64-compile-direct-graph", Cons(source_file,
      Cons(function_name, Cons("-o", Cons(output, Nil))))) ->
    run_aarch64_compile_direct_graph_command source_file function_name output
  | Cons("aarch64-mir", Cons(source_file, Cons(function_name, Nil))) ->
    run_aarch64_mir_command source_file function_name
  | Cons("aarch64-linear-ir", Cons(source_file, Cons(function_name, Nil))) ->
    run_aarch64_linear_ir_command source_file function_name
  | arguments -> match (parse_compile_request arguments) {
    | Err(message) -> {
      write_stderr ("riotml: " + message + "\n");
      1
    }
    | Ok(request) -> match (compile_file request) {
      | Ok(status) -> status
      | Err(error) -> report_error error
    }
  }
}
