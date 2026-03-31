open Std
open Std.Collections

type format_error =
  | Cannot_build_cst of Syn.build_cst_error
  | Cannot_lower of string

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
  | Cannot_lower err ->
      err

let finalize_rendered_output = fun rendered ->
    if String.length rendered = 0 || String.ends_with ~suffix:"\n" rendered then
      rendered
    else
      rendered ^ "\n"

let format = fun (result: Syn.Parser.parse_result) ->
    yield ();
    match Syn.build_cst result with
    | Error err -> Error (Cannot_build_cst err)
    | Ok source_file ->
        yield ();
        (
          match Lower.source_file source_file with
          | Error err -> Error (Cannot_lower (Lower.error_to_string err))
          | Ok rendered ->
              yield ();
              let rendered = Solver.solve ~width:100 rendered |> Printer.to_string in
              yield ();
              Ok (finalize_rendered_output rendered)
        )
