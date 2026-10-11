// RiotML compiler orchestration and command-line entry point.
use Prelude.{List, Nil, Cons, Result, Ok, Err}
use Host.{args, read_file, write_file, write_stderr, run_process}
use Syntax.Parser.{parse_program}
use Check.Typer.{check_program}
use Lambda.Lower.{lower_program}
use Codegen.C.Emit.{emit_program}

let compile_source = fn source ->
  match (parse_program source) {
    | Err(message) -> Err(message)
    | Ok(parsed) -> match (check_program parsed) {
      | Err(message) -> Err(message)
      | Ok(checked) -> Ok(emit_program (lower_program checked))
    }
  }

let compile_file = fn (source_path, output_path) ->
  match (read_file source_path) {
    | Err(message) -> Err("cannot read " + source_path + ": " + message)
    | Ok(source) -> match (compile_source source) {
      | Err(message) -> Err(message)
      | Ok(c_source) -> match (write_file (output_path + ".c") c_source) {
        | Err(message) -> Err("cannot write C output: " + message)
        | Ok(_) -> {
          let compiler_args = Cons("-std=gnu11", Cons("-I", Cons("riotc", Cons(output_path + ".c", Cons("riotc/runtime.c", Cons("-o", Cons(output_path, Nil)))))));
          match (run_process "clang" compiler_args) {
            | Err(message) -> Err("cannot run Clang: " + message)
            | Ok(0) -> Ok(0)
            | Ok(_) -> Err("Clang failed with a non-zero exit status")
          }
        }
      }
    }
  }

let report_error = fn message -> {
  let _ = write_stderr ("riotml: " + message + "\n");
  1
}

let main = fn _ ->
  match (args ()) {
    | Cons("compile", Cons(source_path, Cons("-o", Cons(output_path, Nil)))) ->
      match (compile_file (source_path, output_path)) {
        | Ok(status) -> status
        | Err(message) -> report_error message
      }
    | _ -> report_error "usage: riotml compile SOURCE -o OUTPUT"
  }
