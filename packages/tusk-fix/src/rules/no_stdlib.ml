open Std
open Std.Collections

let rule_id = "no-stdlib"
let rule_name = "No OCaml Stdlib"

let rule_description =
  "Detects usage of OCaml's stdlib modules which should be replaced with Std"

let forbidden_modules =
  [ "Stdlib"; "Pervasives"; "Unix"; "Sys" ]

let replacement_for = function
  | "Stdlib" | "Pervasives" -> Some "Std"
  | _ -> None

let make_message text =
  match replacement_for text with
  | Some replacement ->
      "Direct usage of " ^ text ^ " is discouraged. Use " ^ replacement
      ^ " instead."
  | None ->
      "Direct usage of " ^ text
      ^ " is discouraged. Use package-owned Riot abstractions instead."

let make_suggestion text =
  match replacement_for text with
  | Some replacement -> Some ("Replace " ^ text ^ " with " ^ replacement)
  | None -> None

let make_fix token replacement =
  Fix.make
    ~title:
      ("Replace " ^ Syn.Ceibo.Red.SyntaxToken.text token ^ " with "
     ^ replacement)
    ~edits:
      [
        Fix.make_text_edit ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
          ~new_text:replacement;
      ]

let make_diagnostic token =
  let text = Syn.Ceibo.Red.SyntaxToken.text token in
  let suggestion = make_suggestion text in
  let fix = replacement_for text |> Option.map (make_fix token) in
  Diagnostic.make ~severity:Warning ~message:(make_message text)
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~rule_id ?suggestion ?fix ()

let dedupe_diagnostics diagnostics =
  let seen = HashMap.create () in
  List.filter
    (fun diag ->
      let span = Diagnostic.span diag in
      let key =
        Diagnostic.rule_id diag ^ ":" ^ Int.to_string span.start ^ ":"
        ^ Int.to_string span.end_
      in
      match HashMap.get seen key with
      | Some _ -> false
      | None ->
          ignore (HashMap.insert seen key true);
          true)
    diagnostics

let check_tree (_ctx : Rule.context) red_root =
  let open Syn.Ceibo.Red in
  let open Syn.SyntaxKind in
  let open_stmts = Traversal.find_by_kind OPEN_STMT red_root in
  let path_exprs = Traversal.find_by_kind PATH_EXPR red_root in
  let field_access_exprs = Traversal.find_by_kind FIELD_ACCESS_EXPR red_root in
  let module_paths = Traversal.find_by_kind MODULE_PATH red_root in
  let module_type_paths = Traversal.find_by_kind MODULE_TYPE_PATH red_root in
  let type_constructors = Traversal.find_by_kind TYPE_CONSTR red_root in

  let diagnostic_for_first_token node =
    match Traversal.first_non_trivia_token node with
    | Some token ->
        let text = SyntaxToken.text token in
        if List.mem text forbidden_modules then
          Some (make_diagnostic token)
        else None
    | None -> None
  in

  let diagnostic_for_open_stmt node =
    let non_trivia_children =
      SyntaxNode.children node
      |> Array.to_list
      |> List.filter (function
           | Token token -> not (Traversal.is_trivia (SyntaxToken.kind token))
           | Node _ -> true)
    in
    match non_trivia_children with
    | _open_kw :: Node module_path :: _ ->
        diagnostic_for_first_token module_path
    | _open_kw :: Token token :: _ ->
        let text = SyntaxToken.text token in
        if List.mem text forbidden_modules then
          Some (make_diagnostic token)
        else None
    | _ -> None
  in

  let diagnostic_for_field_access node =
    match Traversal.first_non_trivia_child node with
    | Some (Node receiver) -> diagnostic_for_first_token receiver
    | Some (Token token) ->
        let text = SyntaxToken.text token in
        if List.mem text forbidden_modules then
          Some (make_diagnostic token)
        else None
    | None -> None
  in

  dedupe_diagnostics
    (List.concat
       [
         List.filter_map diagnostic_for_open_stmt open_stmts;
         List.filter_map diagnostic_for_first_token path_exprs;
         List.filter_map diagnostic_for_field_access field_access_exprs;
         List.filter_map diagnostic_for_first_token module_paths;
         List.filter_map diagnostic_for_first_token module_type_paths;
         List.filter_map diagnostic_for_first_token type_constructors;
       ])

let make () =
  Rule.make ~id:rule_id ~name:rule_name ~description:rule_description ~run:check_tree ()
