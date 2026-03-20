open Std
open Std.Collections

module Api = Tusk_fix_api

let package_name = "std"
let package_rule_id = package_name ^ ":no-stdlib"

let unix_code =
  Api.Diagnostic_code.
    {
      package_name;
      local_id = "f0001";
      rule_id = package_rule_id;
      title = "Direct Unix usage";
      body = Api.Diagnostic_code.body DirectUnixUsage;
      message = Api.Diagnostic_code.message DirectUnixUsage;
    }

let sys_code =
  Api.Diagnostic_code.
    {
      package_name;
      local_id = "f0002";
      rule_id = package_rule_id;
      title = "Direct Sys usage";
      body = Api.Diagnostic_code.body DirectSysUsage;
      message = Api.Diagnostic_code.message DirectSysUsage;
    }

let stdlib_code =
  Api.Diagnostic_code.
    {
      package_name;
      local_id = "f0003";
      rule_id = package_rule_id;
      title = "Direct Stdlib usage";
      body = Api.Diagnostic_code.body DirectStdlibUsage;
      message = Api.Diagnostic_code.message DirectStdlibUsage;
    }

let pervasives_code =
  Api.Diagnostic_code.
    {
      package_name;
      local_id = "f0004";
      rule_id = package_rule_id;
      title = "Direct Pervasives usage";
      body = Api.Diagnostic_code.body DirectPervasivesUsage;
      message = Api.Diagnostic_code.message DirectPervasivesUsage;
    }

let diagnostic_codes () = [ unix_code; sys_code; stdlib_code; pervasives_code ]

let forbidden_modules = [ "Stdlib"; "Pervasives"; "Unix"; "Sys" ]

let replacement_for = function
  | "Stdlib" | "Pervasives" -> Some "Std"
  | _ -> None

let package_code_for_module = function
  | "Unix" -> Some unix_code
  | "Sys" -> Some sys_code
  | "Stdlib" -> Some stdlib_code
  | "Pervasives" -> Some pervasives_code
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
  Api.Fix.make
    ~title:
      ("Replace " ^ Syn.Ceibo.Red.SyntaxToken.text token ^ " with "
     ^ replacement)
    ~edits:
      [
        Api.Fix.make_text_edit ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
          ~new_text:replacement;
      ]

let make_diagnostic token =
  let text = Syn.Ceibo.Red.SyntaxToken.text token in
  let suggestion = make_suggestion text in
  let fix = replacement_for text |> Option.map (make_fix token) in
  let kind =
    match package_code_for_module text with
    | Some entry ->
        Api.Diagnostic.Known (Api.Diagnostic_code.PackageProvided entry)
    | None ->
        Api.Diagnostic.Generic { rule_id = package_rule_id; message = make_message text }
  in
  Api.Diagnostic.make ~severity:Warning ~kind
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ?suggestion ?fix ()

let dedupe_diagnostics diagnostics =
  let seen = HashMap.create () in
  List.filter
    (fun diag ->
      let span = Api.Diagnostic.span diag in
      let key =
        Api.Diagnostic.rule_id diag ^ ":" ^ Int.to_string span.start ^ ":"
        ^ Int.to_string span.end_
      in
      match HashMap.get seen key with
      | Some _ -> false
      | None ->
          ignore (HashMap.insert seen key true);
          true)
    diagnostics

let check_tree (_ctx : Api.Rule.context) red_root =
  let open Syn.Ceibo.Red in
  let open Syn.SyntaxKind in
  let open_stmts = Api.Traversal.find_by_kind OPEN_STMT red_root in
  let path_exprs = Api.Traversal.find_by_kind PATH_EXPR red_root in
  let field_access_exprs = Api.Traversal.find_by_kind FIELD_ACCESS_EXPR red_root in
  let module_paths = Api.Traversal.find_by_kind MODULE_PATH red_root in
  let module_type_paths = Api.Traversal.find_by_kind MODULE_TYPE_PATH red_root in
  let type_constructors = Api.Traversal.find_by_kind TYPE_CONSTR red_root in

  let diagnostic_for_first_token node =
    match Api.Traversal.first_non_trivia_token node with
    | Some token ->
        let text = SyntaxToken.text token in
        if List.mem text forbidden_modules then Some (make_diagnostic token)
        else None
    | None -> None
  in

  let diagnostic_for_open_stmt node =
    let non_trivia_children =
      SyntaxNode.children node
      |> Array.to_list
      |> List.filter (function
           | Token token -> not (Api.Traversal.is_trivia (SyntaxToken.kind token))
           | Node _ -> true)
    in
    match non_trivia_children with
    | _open_kw :: Node module_path :: _ -> diagnostic_for_first_token module_path
    | _open_kw :: Token token :: _ ->
        let text = SyntaxToken.text token in
        if List.mem text forbidden_modules then Some (make_diagnostic token)
        else None
    | _ -> None
  in

  let diagnostic_for_field_access node =
    match Api.Traversal.first_non_trivia_child node with
    | Some (Node receiver) -> diagnostic_for_first_token receiver
    | Some (Token token) ->
        let text = SyntaxToken.text token in
        if List.mem text forbidden_modules then Some (make_diagnostic token)
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

let rule () =
  Api.Rule.make ~id:package_rule_id ~name:"No OCaml Stdlib"
    ~description:
      "Detects direct Stdlib, Unix, Sys, and Pervasives usage from the Std package boundary"
    ~run:check_tree ()
