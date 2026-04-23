open Std
open Std.Collections

type format_error =
  | Cannot_build_cst of Syn.build_cst_error
  | Cannot_parse of Syn.Diagnostic.t Vector.t
  | Cannot_lower of string

let parse2_diagnostics_to_string = fun diagnostics ->
  let count = Vector.length diagnostics in
  if count = 0 then
    "parse2 diagnostics prevented formatting"
  else
    let first = Vector.get_unchecked diagnostics ~at:0 |> Syn.Diagnostic.to_string in
    if count = 1 then
      first
    else
      first ^ " (+" ^ Int.to_string (count - 1) ^ " more)"

let format_error_to_string = function
  | Cannot_build_cst (Syn.Parse_diagnostics diagnostics) -> (
      match diagnostics with
      | [] -> "parse diagnostics prevented CST construction"
      | first :: rest ->
          let message = Syn.Diagnostic.to_string first in
          if rest = [] then
            message
          else
            message ^ " (+" ^ Int.to_string (List.length rest) ^ " more)"
    )
  | Cannot_build_cst (Syn.Cst_builder_error err) ->
      let context =
        match err.context with
        | [] -> ""
        | context -> " [" ^ String.concat " > " context ^ "]"
      in
      err.message ^ context
  | Cannot_parse diagnostics ->
      parse2_diagnostics_to_string diagnostics
  | Cannot_lower err ->
      err

let finalize_rendered_output = fun rendered ->
  if String.length rendered = 0 || String.ends_with ~suffix:"\n" rendered then
    rendered
  else
    rendered ^ "\n"

let source_slice = fun source ->
  match IO.IoVec.IoSlice.from_string source with
  | Ok slice -> slice
  | Error error -> panic ("failed to create parser source slice: " ^ Kernel.IO.Error.message error)

let parse_source = fun ~filename source -> Syn.parse2 ~filename (source_slice source)

let format = fun (result: Syn.Parser2.parse_result) ->
  yield ();
  let diagnostics = result.Syn.Parser2.diagnostics in
  if Vector.length diagnostics > 0 then
    Error (Cannot_parse diagnostics)
  else
    let source_file = Syn.Ast2.SourceFile.make result.Syn.Parser2.tree in
    match Lower2.source_file source_file with
    | Error err -> Error (Cannot_lower (Lower2.error_to_string err))
    | Ok rendered ->
        yield ();
        let rendered = Solver.solve ~width:100 rendered |> Printer.to_string in
        yield ();
        Ok (finalize_rendered_output rendered)

let format_source = fun ~filename source -> parse_source ~filename source |> format

let format2 = format
