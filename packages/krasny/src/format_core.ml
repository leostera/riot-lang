open Std
open Std.Collections

type format_error =
  | Cannot_build_cst of Syn.build_cst_error

let format_error_to_string = function
  | Cannot_build_cst (Syn.Parse_diagnostics diagnostics) -> (
      match diagnostics with
      | [] -> "parse diagnostics prevented CST construction"
      | first :: rest ->
          let message = Syn.Diagnostic.to_string first in
          if rest = [] then
            message
          else
            message ^ " (+" ^ Int.to_string (List.length rest) ^ " more)")
  | Cannot_build_cst (Syn.Cst_builder_error err) ->
      let context =
        match err.context with
        | [] -> ""
        | context -> " [" ^ String.concat " > " context ^ "]"
      in
      err.message ^ context

let format (result : Syn.Parser.parse_result) =
  yield ();
  match Syn.build_cst result with
  | Error err ->
      Error (Cannot_build_cst err)
  | Ok source_file ->
      let original_source = Source.source_of_result result in
      yield ();
      Ok
        (match Lower.source_file ~source:original_source source_file with
        | Some rendered ->
            yield ();
            let rendered = Solver.solve ~width:100 rendered |> Printer.to_string in
            yield ();
            if String.ends_with ~suffix:"\n" original_source
               && String.ends_with ~suffix:"\n" rendered
            then
              rendered
            else if String.ends_with ~suffix:"\n" original_source then
              rendered ^ "\n"
            else rendered
        | None -> original_source)
