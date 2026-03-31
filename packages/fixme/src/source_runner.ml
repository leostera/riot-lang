open Std

type result = {
  tree: Rule.green_tree;
  diagnostics: Diagnostic.t list;
  parse_diagnostics: Syn.Diagnostic.t list;
}

let parse ?filename source : Syn.Parser.parse_result =
  match filename with
  | Some filename -> Syn.parse ~filename source
  | None -> Syn.parse_implementation source

let lint_diagnostics = fun ~rules ?filename (parse_result: Syn.Parser.parse_result) ->
    if List.length parse_result.diagnostics > 0 then
      []
    else
      match Syn.build_cst parse_result with
      | Error _ -> []
      | Ok cst ->
          let red_tree = Syn.Ceibo.Red.new_root parse_result.tree in
          let file_path =
            match filename with
            | Some filename -> Path.to_string filename
            | None -> "<stdin>"
          in
          let ctx = Rule.{file_path; cst} in
          rules |> List.concat_map
            (fun rule ->
              Rule.run rule ctx red_tree)

let run = fun ~rules ?filename source ->
    let parse_result = parse ?filename source in
    {
      tree = parse_result.tree;
      diagnostics = lint_diagnostics ~rules ?filename parse_result;
      parse_diagnostics = parse_result.diagnostics;

    }

let run_rule = fun ~rule ?filename source -> run ~rules:[ rule ] ?filename source

let has_parse_errors = fun result -> List.length result.parse_diagnostics > 0

let has_errors = fun result ->
    List.exists (fun diag -> Diagnostic.severity diag = Diagnostic.Error) result.diagnostics

let safe_fixes = fun result ->
    List.filter_map Diagnostic.fix result.diagnostics

let can_apply_safe_fixes = fun result ->
    not (has_parse_errors result) && not (has_errors result) && List.length (safe_fixes result) > 0

let apply_safe_fixes = fun ~source result ->
    let fixes = safe_fixes result in
    if has_parse_errors result || has_errors result || List.length fixes = 0 then
      Ok None
    else
      match Fix.apply_fixes ~source fixes with
      | Error _ as err -> err
      | Ok updated_source ->
          if String.equal updated_source source then
            Ok None
          else
            Ok (Some (updated_source, fixes))
