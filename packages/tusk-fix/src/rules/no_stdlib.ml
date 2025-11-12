open Std
open Std.Collections

let rule_id = "no-stdlib"
let rule_name = "No OCaml Stdlib"

let rule_description =
  "Detects usage of OCaml's stdlib modules which should be replaced with Std"

let forbidden_modules =
  [ "Stdlib"; "Pervasives"; "Unix"; "Sys"; "Hashtbl"; "Queue"; "Stack"; "Set"; "Map" ]

let make_message text =
  "Direct usage of " ^ text ^ " is discouraged. Use Std equivalents instead."

let make_suggestion text = "Replace " ^ text ^ " with Std module"

let check_tree _ctx red_root =
  let open Syn.Ceibo.Red in
  let open Syn.SyntaxKind in
  let open_stmts = Traversal.find_by_kind OPEN_STMT red_root in
  let path_exprs = Traversal.find_by_kind PATH_EXPR red_root in
  let field_access_exprs = Traversal.find_by_kind FIELD_ACCESS_EXPR red_root in
  
  let check_open_stmt node =
    Array.find_map
      (function
        | Token t ->
            let text = SyntaxToken.text t in
            if not (Traversal.is_trivia (SyntaxToken.kind t)) && List.mem text forbidden_modules then
              Some (Diagnostic.make ~severity:Warning ~message:(make_message text)
                     ~span:(SyntaxToken.span t) ~rule_id ~suggestion:(make_suggestion text) ())
            else None
        | _ -> None)
      (SyntaxNode.children node)
  in
  
  let check_path_expr node =
    match Traversal.first_non_trivia_token node with
    | Some t ->
        let text = SyntaxToken.text t in
        if List.mem text forbidden_modules then
          Some (Diagnostic.make ~severity:Warning ~message:(make_message text)
                 ~span:(SyntaxToken.span t) ~rule_id ~suggestion:(make_suggestion text) ())
        else None
    | None -> None
  in
  
  let check_field_access_expr node =
    match Traversal.first_non_trivia_child node with
    | Some (Node ident_node) when SyntaxNode.kind ident_node = IDENT_EXPR -> (
        match Traversal.first_non_trivia_token ident_node with
        | Some t ->
            let text = SyntaxToken.text t in
            if List.mem text forbidden_modules then
              Some (Diagnostic.make ~severity:Warning ~message:(make_message text)
                     ~span:(SyntaxToken.span t) ~rule_id ~suggestion:(make_suggestion text) ())
            else None
        | None -> None)
    | _ -> None
  in
  
  List.concat
    [
      List.filter_map check_open_stmt open_stmts;
      List.filter_map check_path_expr path_exprs;
      List.filter_map check_field_access_expr field_access_exprs;
    ]

let make () =
  Rule.make ~id:rule_id ~name:rule_name ~description:rule_description ~run:check_tree ()
