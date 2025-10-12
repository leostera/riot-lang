open Std

let rule_id = "no-stdlib"
let rule_name = "No OCaml Stdlib"

let rule_description =
  "Detects usage of OCaml's stdlib modules (Stdlib, List, String, etc.) which \
   should be replaced with Std equivalents"

let forbidden_modules =
  [
    "Stdlib";
    "Pervasives";
    "Unix";
    "Sys";
    "Hashtbl";
    "Queue";
    "Stack";
    "Set";
    "Map";
  ]

let check_tree _ctx red_root =
  let diagnostics = ref [] in
  let open Syn.Ceibo.Red in
  let open Syn.SyntaxKind in
  (* Helper: find first non-trivia child *)
  let first_non_trivia_child node =
    let children = SyntaxNode.children node in
    Array.find_opt
      (function
        | Token t ->
            let kind = SyntaxToken.kind t in
            kind <> WHITESPACE && kind <> COMMENT && kind <> DOCSTRING
        | Node _ -> true)
      children
  in
  let rec traverse elem =
    match elem with
    | Node n ->
        let kind = SyntaxNode.kind n in
        (match kind with
        | OPEN_STMT -> check_open_stmt n
        | PATH_EXPR -> check_path_expr n
        | FIELD_ACCESS_EXPR -> check_field_access_expr n
        (* Skip TYPE_CONSTR - type definitions are often wrappers around stdlib types *)
        (* | IDENT_EXPR -> check_ident_expr n *)
        | _ -> ());
        Array.iter traverse (SyntaxNode.children n)
    | Token _ -> ()
  and check_open_stmt node =
    Array.iter
      (function
        | Token t ->
            let kind = SyntaxToken.kind t in
            let text = SyntaxToken.text t in
            if kind <> WHITESPACE && kind <> COMMENT && kind <> DOCSTRING
               && List.mem text forbidden_modules
            then add_diagnostic text (SyntaxToken.span t)
        | _ -> ())
      (SyntaxNode.children node)
  and check_path_expr node =
    match first_non_trivia_child node with
    | Some (Token t) ->
        let text = SyntaxToken.text t in
        if List.mem text forbidden_modules then
          add_diagnostic text (SyntaxToken.span t)
    | _ -> ()
  and check_field_access_expr node =
    (* Check first child which should be the module name (Hashtbl in Hashtbl.create) *)
    match first_non_trivia_child node with
    | Some (Node ident_node) when SyntaxNode.kind ident_node = IDENT_EXPR -> (
        match first_non_trivia_child ident_node with
        | Some (Token t) ->
            let text = SyntaxToken.text t in
            if List.mem text forbidden_modules then
              add_diagnostic text (SyntaxToken.span t)
        | _ -> ())
    | _ -> ()
  and check_ident_expr node =
    match first_non_trivia_child node with
    | Some (Token t) ->
        let text = SyntaxToken.text t in
        if List.mem text forbidden_modules then
          add_diagnostic text (SyntaxToken.span t)
    | Some (Node nested) when SyntaxNode.kind nested = IDENT_EXPR ->
        check_ident_expr nested
    | _ -> ()
  and add_diagnostic text span =
    let diag =
      Diagnostic.make ~severity:Warning
        ~message:
          (format
             "Direct usage of %s is discouraged. Use Std equivalents instead."
             text)
        ~span ~rule_id
        ~suggestion:(format "Replace %s with Std module" text)
        ()
    in
    diagnostics := diag :: !diagnostics
  in
  traverse (Node red_root);
  !diagnostics

let make () =
  Rule.make ~id:rule_id ~name:rule_name ~description:rule_description
    ~run:check_tree ()
