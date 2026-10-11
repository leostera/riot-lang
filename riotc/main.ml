// RiotML compiler orchestration and command-line entry point.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use Host.{args, read_file, write_file, write_stderr, run_process}
use Syntax.Parser.{parse_program, parse_error_message, parse_error_span}
use Syntax.Lexer.{SourceSpan, lex, UnterminatedString, UnterminatedEscape}
use Syntax.Ast.{
  SourceModule, ModulePath, Import, ImportedName, MemberName, ValueName,
  ValueDeclaration, ValueDeclarationData, TypeDeclaration, Declaration
}
use Check.Typer.{
  ValueBinding, check_program, check_source_module_with_bindings,
  validate_native_entry_module_with_bindings
}
use Lambda.Lower.{lower_program}
use Codegen.C.Emit.{emit_program, emit_module_program}
use Modules.{load_source_units, module_path_relative, directory_of, join_path}
use String.{equal, length, slice}

type SourcePath = SourcePath(string)
type ExecutablePath = ExecutablePath(string)

type NativeCompileRequest = NativeCompileRequest {
  source_path: SourcePath,
  output_path: ExecutablePath
}

type CheckedModule = CheckedModule {
  source_unit: (string, SourceModule),
  value_bindings: List<ValueBinding>
}

type CompilationStage =
  | SourceRead
  | SourceParsing
  | SourceTypechecking
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

let compile_source = fn source ->
  match (parse_program source) {
    | Err(error) -> Err(compile_error (SourceParsing)
      (parse_error_message error) (parse_error_span error))
    | Ok(parsed_program) -> match (check_program parsed_program) {
      | Err(message) -> Err(compile_error (SourceTypechecking) message None)
      | Ok(checked_program) ->
        Ok(emit_program (lower_program checked_program))
    }
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

let append_value_bindings = fn (first: List<ValueBinding>) -> fn (second: List<ValueBinding>) ->
  match first {
    | Nil -> second
    | Cons(binding, remaining) -> Cons(binding, append_value_bindings remaining second)
  }

let module_declares_value = fn (wanted_name: string) -> fn (declarations: List<Declaration>) ->
  match declarations {
    | Nil -> false
    | Cons(ValueDeclaration(ValueDeclarationData {
        name: ValueName(name), body: _
      }), remaining) -> match (equal wanted_name name) {
      | true -> true
      | false -> module_declares_value wanted_name remaining
    }
    | Cons(TypeDeclaration(_), remaining) -> module_declares_value wanted_name remaining
  }

let find_value_binding = fn (wanted_name: string) -> fn (bindings: List<ValueBinding>) ->
  match bindings {
    | Nil -> None
    | Cons(ValueBinding { name: ValueName(name), value_type }, remaining) ->
      match (equal wanted_name name) {
        | true -> Some(ValueBinding { name: ValueName(name), value_type: value_type })
        | false -> find_value_binding wanted_name remaining
      }
  }

let selected_import_value_bindings = fn (declarations: List<Declaration>) ->
  fn (bindings: List<ValueBinding>) -> fn (names: List<ImportedName>) ->
  match names {
    | Nil -> Nil
    | Cons(ImportedName(MemberName(name)), remaining) ->
      match (module_declares_value name declarations) {
        | false -> selected_import_value_bindings declarations bindings remaining
        | true -> match (find_value_binding name bindings) {
          | None -> selected_import_value_bindings declarations bindings remaining
          | Some(binding) -> Cons(binding,
            selected_import_value_bindings declarations bindings remaining)
        }
      }
  }

let path_ends_with = fn path -> fn suffix ->
  match (length path < length suffix) {
    | true -> false
    | false -> equal (slice path (length path - length suffix) (length path)) suffix
  }

let source_path_matches_module = fn source_path -> fn (module_path: ModulePath) -> {
  let relative_path = module_path_relative module_path;
  match (equal source_path relative_path) {
    | true -> true
    | false -> path_ends_with source_path ("/" + relative_path)
  }
}

let find_checked_module = fn (wanted_path: ModulePath) ->
  fn (checked_modules: List<CheckedModule>) ->
  match checked_modules {
    | Nil -> None
    | Cons(CheckedModule {
        source_unit: (source_path, source_module),
        value_bindings
      }, remaining) -> match (source_path_matches_module source_path wanted_path) {
      | true -> Some(CheckedModule {
        source_unit: (source_path, source_module),
        value_bindings: value_bindings
      })
      | false -> find_checked_module wanted_path remaining
    }
  }

let imported_bindings_for_imports = fn (imports: List<Import>) ->
  fn (checked_modules: List<CheckedModule>) ->
  match imports {
    | Nil -> Nil
    | Cons(Import { module_path, selected_names }, remaining) -> {
      let imported = match (find_checked_module module_path checked_modules) {
        | None -> Nil
        | Some(CheckedModule {
            source_unit: (_, SourceModule { imports: _, declarations }),
            value_bindings
          }) -> selected_import_value_bindings declarations value_bindings selected_names
      };
      append_value_bindings imported
        (imported_bindings_for_imports remaining checked_modules)
    }
  }

let imported_bindings_for_module = fn (SourceModule { imports, declarations: _ }) ->
  fn (checked_modules: List<CheckedModule>) ->
  imported_bindings_for_imports imports checked_modules

let check_loaded_modules = fn (modules: List<(string, SourceModule)>) ->
  fn (checked_modules: List<CheckedModule>) ->
  match modules {
    | Nil -> Ok(checked_modules)
    | Cons((source_path, source_module), remaining) -> {
      let imported_bindings = imported_bindings_for_module source_module checked_modules;
      match (check_source_module_with_bindings source_module imported_bindings) {
        | Err(message) -> Err("type error in " + source_path + ": " + message)
        | Ok(value_bindings) -> check_loaded_modules remaining
          (Cons(CheckedModule {
            source_unit: (source_path, source_module),
            value_bindings: value_bindings
          }, checked_modules))
      }
    }
  }

let find_root_checked_module = fn (root_path: string) ->
  fn (checked_modules: List<CheckedModule>) ->
  match checked_modules {
    | Nil -> None
    | Cons(CheckedModule {
        source_unit: (source_path, source_module),
        value_bindings
      }, remaining) -> match (equal root_path source_path) {
      | true -> Some(CheckedModule {
        source_unit: (source_path, source_module),
        value_bindings: value_bindings
      })
      | false -> find_root_checked_module root_path remaining
    }
  }

let compile_module_graph_unchecked = fn (source_file: string) ->
  match (load_source_units source_file
    (Cons(directory_of source_file, Nil): List<string>)) {
    | Err(message) -> Err(compile_error (SourceParsing) message None)
    | Ok(modules) -> match (check_loaded_modules modules Nil) {
      | Err(message) -> Err(compile_error (SourceTypechecking) message None)
      | Ok(checked_modules) -> match (find_root_checked_module source_file checked_modules) {
        | None -> Err(compile_error (SourceParsing)
          "source graph has no entry module" None)
        | Some(CheckedModule {
            source_unit: (_, source_module), value_bindings: _
          }) -> {
          let imported_bindings = imported_bindings_for_module source_module checked_modules;
          match (validate_native_entry_module_with_bindings source_module imported_bindings) {
            | Err(message) -> Err(compile_error (SourceTypechecking) message None)
            | Ok(_) -> Ok(emit_module_program source_file modules)
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

let runtime_include_directory = fn source_file ->
  match (read_file (join_path (directory_of source_file) "runtime.c")) {
    | Ok(_) -> directory_of source_file
    | Err(_) -> "riotc"
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

let parse_compile_request = fn arguments ->
  match arguments {
    | Cons("compile", Cons(source, Cons("-o", Cons(output, Nil)))) ->
      Ok(NativeCompileRequest {
        source_path: SourcePath(source),
        output_path: ExecutablePath(output)
      })
    | _ -> Err("usage: riotml compile SOURCE -o OUTPUT")
  }

let main = fn () ->
  match (parse_compile_request (args ())) {
    | Err(message) -> {
      write_stderr ("riotml: " + message + "\n");
      1
    }
    | Ok(request) -> match (compile_file request) {
      | Ok(status) -> status
      | Err(error) -> report_error error
    }
  }
