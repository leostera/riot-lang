open Std
open Std.Collections

let iter_fold = fun fold value ~fn ->
  fold
    value
    ~init:()
    ~fn:(fun item () ->
      fn item;
      Syn.Ast.Continue ())


module Api = Fixme

let package_name = "std"

let package_rule_id = Api.Rule_id.of_string (package_name ^ ":no-stdlib")

let rule_description =
  "Detect direct Stdlib, Unix, Sys, and Pervasives usage from the Std package boundary"

let unix_body =
  {|
Direct calls into Unix bypass Riot's scheduling and portability boundaries.

In Riot, a blocking Unix call is not just a local implementation detail. It can
stall the cooperative scheduler, delay unrelated actors, and leak host-specific
behavior into packages that are supposed to stay portable.

Keep real operating-system boundaries down in packages that are meant to own
them, like `kernel`. Everywhere else, prefer the Riot-owned abstraction that
already exists, or add one deliberately if the boundary is genuinely missing.
|}

let sys_body =
  {|
Direct Sys usage reaches into process-global runtime state instead of going through Riot-owned boundaries.

`Sys` exposes host and runtime details directly from OCaml. Once those calls
spread through ordinary packages, portability decisions and process-global
policy leak into places that should not own them.

Prefer Riot wrappers for system information and runtime behavior. If something
truly belongs at the process or platform boundary, keep it in a package that is
explicitly responsible for that boundary instead of reaching for `Sys`
everywhere.
|}

let stdlib_body =
  {|
Code outside the runtime boundary should go through Riot's Std layer instead of referencing Stdlib directly.

Riot is trying to offer one coherent standard surface, not a mixture of
package-local conventions plus ad hoc direct references into `Stdlib`.
Routing code through `Std` gives the ecosystem one owned API surface and leaves
room for shared defaults, portability adjustments, and package-wide style.

When `Std` already owns an API, use it. When it does not, that is usually a
signal to extend `Std` deliberately instead of bypassing it forever.
|}

let pervasives_body =
  {|
Pervasives is the historical pre-Stdlib module and should not appear in modern Riot code.

`Pervasives` is legacy OCaml surface area. Riot code should point at the
current owned surface, not an old compatibility layer that survives mostly for
historical reasons.

Replace direct `Pervasives` references with `Std`.
|}

let rule_explain = String.concat "\n\n" [ unix_body; sys_body; stdlib_body; pervasives_body; ]

let explanation =
  Api.Explanation.{ rule_id = package_rule_id; body = rule_explain; message = rule_description }

let explanations = fun () -> [ explanation ]

let forbidden_modules = [ "Stdlib"; "Pervasives"; "Unix"; "Sys"; ]

let replacement_for = function
  | "Stdlib"
  | "Pervasives" -> Some "Std"
  | _ -> None

let make_message = fun text ->
  match replacement_for text with
  | Some replacement ->
      "Direct usage of " ^ text ^ " is discouraged. Use " ^ replacement ^ " instead."
  | None ->
      "Direct usage of " ^ text ^ " is discouraged. Use package-owned Riot abstractions instead."

let make_suggestion = fun text ->
  match replacement_for text with
  | Some replacement -> Some ("Replace " ^ text ^ " with " ^ replacement)
  | None -> None

let span_of_ast_token = fun token ->
  Syn.Ceibo.Span.make
    ~start:(Syn.Ast.Token.span_start token)
    ~end_:(Syn.Ast.Token.span_end token)

let make_fix = fun token replacement ->
  Api.Fix.make
    ~title:("Replace " ^ Syn.Ast.Token.text token ^ " with " ^ replacement)
    ~operations:[ Api.Fix.replace_token_with_text ~target:token ~text:replacement ]

let make_diagnostic = fun token ->
  let text = Syn.Ast.Token.text token in
  let suggestion = make_suggestion text in
  let fix =
    replacement_for text
    |> Option.map (make_fix token)
  in
  let kind = Api.Diagnostic.Known { rule_id = package_rule_id; message = make_message text } in
  Api.Diagnostic.make ~severity:Warning ~kind ~span:(span_of_ast_token token) ?suggestion ?fix ()

let dedupe_diagnostics = fun diagnostics ->
  let seen = HashMap.create () in
  List.filter
    (fun diag ->
      let span = Api.Diagnostic.span diag in
      let key =
        Api.Rule_id.to_string (Api.Diagnostic.rule_id diag)
        ^ ":"
        ^ Int.to_string span.start
        ^ ":"
        ^ Int.to_string span.end_
      in
      match HashMap.get seen key with
      | Some _ -> false
      | None ->
          ignore (HashMap.insert seen key true);
          true)
    diagnostics

let check_tree = fun (_ctx: Api.Rule.context) red_root ->
  let module Ast = Syn.Ast in
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
        let text = Ast.Token.text token in
        if List.mem text forbidden_modules then
          Some (make_diagnostic token)
        else
          None
    | None -> None
  in
  let non_trivia_children node =
    let children = Vector.with_capacity ~size:(Ast.Node.child_count node) in
    iter_fold Ast.Node.fold_child
      node
      ~fn:(fun child ->
        match child with
        | Syn.SyntaxTree.Node id ->
            let child_node = ({ tree = node.Ast.tree; id }: Ast.Node.t) in
            if not (Api.Traversal.is_trivia (Ast.Node.kind child_node)) then
              Vector.push children ~value:(Api.Traversal.Node child_node)
        | Syn.SyntaxTree.Token id ->
            let token = ({ tree = node.Ast.tree; id }: Ast.Token.t) in
            if not (Api.Traversal.is_trivia (Ast.Token.kind token)) then
              Vector.push children ~value:(Api.Traversal.Token token)
        | Syn.SyntaxTree.Missing _ -> ());
    Vector.to_array children
    |> Array.to_list
  in
  let diagnostic_for_open_stmt node =
    match non_trivia_children node with
    | _open_kw :: (Api.Traversal.Node module_path) :: _ -> diagnostic_for_first_token module_path
    | _open_kw :: (Api.Traversal.Token token) :: _ ->
        let text = Ast.Token.text token in
        if List.mem text forbidden_modules then
          Some (make_diagnostic token)
        else
          None
    | _ -> None
  in
  let diagnostic_for_field_access node =
    match Api.Traversal.first_non_trivia_child node with
    | Some (Api.Traversal.Node receiver) -> diagnostic_for_first_token receiver
    | Some (Api.Traversal.Token token) ->
        let text = Ast.Token.text token in
        if List.mem text forbidden_modules then
          Some (make_diagnostic token)
        else
          None
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

let rule = fun () ->
  Api.Rule.make
    ~id:package_rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
