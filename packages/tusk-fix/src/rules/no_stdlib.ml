open Std
open Std.Collections

let rule_id = "no-stdlib"
let rule_name = "No OCaml Stdlib"

let rule_description =
  "Detects usage of OCaml's stdlib modules which should be replaced with Std"

let forbidden_modules =
  [ "Stdlib"; "Pervasives"; "Unix"; "Sys"; "Hashtbl"; "Queue"; "Stack"; "Set"; "Map" ]

let replacement_for = function
  | "Stdlib" | "Pervasives" -> Some "Std"
  | "Hashtbl" -> Some "Std.Collections.HashMap"
  | "Queue" -> Some "Std.Collections.Queue"
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

let is_std_collections_qualified tokens index =
  let token_text_at idx =
    let rec loop idx = function
      | [] -> ""
      | text :: _ when idx = 0 -> text
      | _ :: rest -> loop (idx - 1) rest
    in
    if idx < 0 then "" else loop idx tokens
  in
  index >= 4
  && String.equal (token_text_at (index - 4)) "Std"
  && String.equal (token_text_at (index - 3)) "."
  && String.equal (token_text_at (index - 2)) "Collections"
  && String.equal (token_text_at (index - 1)) "."

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

let path_is_boundary_owned file_path =
  let path = Path.to_string file_path in
  String.contains path "/packages/kernel/"
  || String.contains path "/packages/miniriot/"
  || String.starts_with ~prefix:"packages/kernel/" path
  || String.starts_with ~prefix:"packages/miniriot/" path

let check_tree ({ Rule.file_path } : Rule.context) red_root =
  if path_is_boundary_owned file_path then
    []
  else
    let open Syn.Ceibo.Red in
    let open Syn.SyntaxKind in
    let open_stmts = Traversal.find_by_kind OPEN_STMT red_root in
    let path_exprs = Traversal.find_by_kind PATH_EXPR red_root in
    let field_access_exprs = Traversal.find_by_kind FIELD_ACCESS_EXPR red_root in
    let module_paths = Traversal.find_by_kind MODULE_PATH red_root in
    let module_type_paths = Traversal.find_by_kind MODULE_TYPE_PATH red_root in
    let type_constructors = Traversal.find_by_kind TYPE_CONSTR red_root in

    let check_open_stmt node =
      Array.find_map
        (function
          | Token t ->
              let text = SyntaxToken.text t in
              if
                not (Traversal.is_trivia (SyntaxToken.kind t))
                && List.mem text forbidden_modules
              then Some (make_diagnostic t)
              else None
          | _ -> None)
        (SyntaxNode.children node)
    in

    let check_first_token node =
      match Traversal.first_non_trivia_token node with
      | Some t ->
          let text = SyntaxToken.text t in
          if List.mem text forbidden_modules then
            Some (make_diagnostic t)
          else None
      | None -> None
    in

    let check_type_constr node =
      let tokens =
        SyntaxNode.children node
        |> Array.to_list
        |> List.filter_map (function
             | Token t when not (Traversal.is_trivia (SyntaxToken.kind t)) ->
                 Some (t, SyntaxToken.text t)
             | _ -> None)
      in
      tokens
      |> List.mapi (fun index (token, text) -> (index, token, text))
      |> List.find_map (fun (index, token, text) ->
             if
               List.mem text forbidden_modules
               && not
                    (is_std_collections_qualified
                       (List.map (fun (_, text) -> text) tokens)
                       index)
             then Some (make_diagnostic token)
             else None)
    in

    let check_field_access_expr node =
      match Traversal.first_non_trivia_child node with
      | Some (Node ident_node) when SyntaxNode.kind ident_node = IDENT_EXPR -> (
          match Traversal.first_non_trivia_token ident_node with
          | Some t ->
              let text = SyntaxToken.text t in
              if List.mem text forbidden_modules then
                Some (make_diagnostic t)
              else None
          | None -> None)
      | _ -> None
    in

    dedupe_diagnostics
      (List.concat
         [
           List.filter_map check_open_stmt open_stmts;
           List.filter_map check_first_token path_exprs;
           List.filter_map check_field_access_expr field_access_exprs;
           List.filter_map check_first_token module_paths;
           List.filter_map check_first_token module_type_paths;
           List.filter_map check_type_constr type_constructors;
         ])

let make () =
  Rule.make ~id:rule_id ~name:rule_name ~description:rule_description ~run:check_tree ()
