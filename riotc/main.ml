// RiotML compiler orchestration and command-line entry point.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use Host.{
  args, current_directory, runtime_directory, read_file, write_file, write_stderr,
  write_stdout, run_process
}
use Syntax.Lexer.{SourceSpan, lex, UnterminatedString, UnterminatedEscape}
use Check.Inference.{
  CheckedModuleInterface, infer_source_module, validate_native_entry_module
}
use Codegen.C.Emit.{emit_module_program}
use Lambda.Ir.{LambdaModule}
use Lambda.Lower.{lower_checked_module}
use NativeIr.Lower.{lower_module}
use NativeIr.Verify.{verify_module}
use NativeIr.Ir.{NativeModule}
use NativeIr.Print.{print_native_module}
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
let compile_module_graph_unchecked = fn (source_file: string) ->
  match (load_source_graph source_file
    (Cons(directory_of source_file, Nil): List<string>)) {
    | Err(message) -> Err(compile_error (SourceParsing) message None)
    | Ok(modules) -> match (check_loaded_modules modules Nil) {
      | Err(message) -> Err(compile_error (SourceTypechecking) message None)
      | Ok(checked_modules) -> match (find_root_checked_module source_file checked_modules) {
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

let parse_compile_request = fn arguments ->
  match arguments {
    | Cons("compile", Cons(source, Cons("-o", Cons(output, Nil)))) ->
      Ok(NativeCompileRequest {
        source_path: SourcePath(source),
        output_path: ExecutablePath(output)
      })
    | _ -> Err("usage: riotml compile SOURCE -o OUTPUT")
  }

let main = fn () -> match (args ()) {
  | Cons("native-ir", Cons(source_file, Nil)) -> run_native_ir_command source_file
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
