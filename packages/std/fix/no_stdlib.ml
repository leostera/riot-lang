open Std
open Std.Collections

module Api = Tusk_fix_api

let package_name = "std"
let package_rule_id = package_name ^ ":no-stdlib"

let unix_body =
  {|
Direct calls into Unix bypass Riot's scheduling and portability boundaries.

Why this rule exists:
- Riot code runs on top of a cooperative actor runtime.
- A blocking Unix call can stall the scheduler and delay unrelated actors.
- Direct Unix usage also hard-codes platform details into packages that should stay platform-agnostic.

What to do instead:
- Prefer package-owned Riot abstractions when they exist.
- Push true OS boundaries down into the packages that are supposed to own them, like kernel.
- If you really need a Unix boundary, introduce it deliberately instead of sprinkling Unix calls through application code.
|}

let sys_body =
  {|
Direct Sys usage reaches into process-global runtime state instead of going through Riot-owned boundaries.

Why this rule exists:
- Sys exposes host and runtime details directly from OCaml.
- That makes portability and policy decisions leak into packages that should not own them.
- It also makes it harder to keep behavior consistent across the ecosystem.

What to do instead:
- Prefer Riot wrappers for system information and runtime behavior.
- Keep process-global and platform-global logic in boundary-owning packages.
|}

let stdlib_body =
  {|
Code outside the runtime boundary should go through Riot's Std layer instead of referencing Stdlib directly.

Why this rule exists:
- Riot is trying to provide a coherent programming stack, not just a pile of packages.
- Routing code through Std gives the ecosystem one owned surface instead of ad hoc direct references into Stdlib.
- That leaves room for better defaults, portability adjustments, and package-wide conventions.

What to do instead:
- Replace Stdlib references with Std when the Riot surface already owns that API.
- If Std does not yet expose something important, that is usually a signal to extend Std deliberately rather than bypass it forever.
|}

let pervasives_body =
  {|
Pervasives is the historical pre-Stdlib module and should not appear in modern Riot code.

Why this rule exists:
- Pervasives is legacy OCaml surface area.
- Riot code should point at the current owned surface, not historic compatibility layers.

What to do instead:
- Replace direct Pervasives references with Std.
|}

let unix_explanation =
  Api.Explanation.
    {
      code = "std:f0001";
      rule_id = package_rule_id;
      title = "Direct Unix usage";
      body = unix_body;
      message =
        "Direct usage of Unix is discouraged. Use package-owned Riot abstractions instead.";
    }

let sys_explanation =
  Api.Explanation.
    {
      code = "std:f0002";
      rule_id = package_rule_id;
      title = "Direct Sys usage";
      body = sys_body;
      message =
        "Direct usage of Sys is discouraged. Use package-owned Riot abstractions instead.";
    }

let stdlib_explanation =
  Api.Explanation.
    {
      code = "std:f0003";
      rule_id = package_rule_id;
      title = "Direct Stdlib usage";
      body = stdlib_body;
      message = "Direct usage of Stdlib is discouraged. Use Std instead.";
    }

let pervasives_explanation =
  Api.Explanation.
    {
      code = "std:f0004";
      rule_id = package_rule_id;
      title = "Direct Pervasives usage";
      body = pervasives_body;
      message = "Direct usage of Pervasives is discouraged. Use Std instead.";
    }

let explanations () =
  [ unix_explanation; sys_explanation; stdlib_explanation; pervasives_explanation ]

let forbidden_modules = [ "Stdlib"; "Pervasives"; "Unix"; "Sys" ]

let replacement_for = function
  | "Stdlib" | "Pervasives" -> Some "Std"
  | _ -> None

let explanation_for_module = function
  | "Unix" -> Some unix_explanation
  | "Sys" -> Some sys_explanation
  | "Stdlib" -> Some stdlib_explanation
  | "Pervasives" -> Some pervasives_explanation
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
    match explanation_for_module text with
    | Some entry ->
        Api.Diagnostic.Known
          {
            code = entry.code;
            rule_id = entry.rule_id;
            message = entry.message;
          }
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
    ~explain:
      "The std package should not reference Stdlib, Unix, Sys, or Pervasives directly. Use Std or boundary-owning Riot packages instead."
    ~run:check_tree ()
