open Std
open Std.Collections

module Api = Fixme
module Ast = Syn.Ast

let package_name = "std"

let package_rule_id = Api.Rule_id.from_string (package_name ^ ":no-stdlib")

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

let replacement_for = fun __tmp1 ->
  match __tmp1 with
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
  Syn.Span.make
    ~start:(Ast.Token.span_start token)
    ~end_:(Ast.Token.span_end token)

let make_fix = fun token replacement ->
  Api.Fix.make
    ~title:("Replace " ^ Ast.Token.text token ^ " with " ^ replacement)
    ~operations:[ Api.Fix.replace_token_with_text ~target:token ~text:replacement ]

let make_diagnostic = fun token ->
  let text = Ast.Token.text token in
  let suggestion = make_suggestion text in
  let fix =
    replacement_for text
    |> Option.map ~fn:(make_fix token)
  in
  let kind = Api.Diagnostic.Known { rule_id = package_rule_id; message = make_message text } in
  Api.Diagnostic.make ~severity:Warning ~kind ~span:(span_of_ast_token token) ?suggestion ?fix ()

let dedupe_diagnostics = fun diagnostics ->
  let seen = HashMap.create () in
  List.filter
    diagnostics
    ~fn:(fun diag ->
      let span = Api.Diagnostic.span diag in
      let key =
        Api.Rule_id.to_string (Api.Diagnostic.rule_id diag)
        ^ ":"
        ^ Int.to_string span.start
        ^ ":"
        ^ Int.to_string span.end_
      in
      match HashMap.get seen ~key with
      | Some _ -> false
      | None ->
          ignore (HashMap.insert seen ~key ~value:true);
          true)

let check_tree = fun (_ctx: Api.Rule.context) red_root ->
  let diagnostic_for_ident = fun ident ->
    match Ast.Ident.first_segment ident with
    | Some token when List.contains forbidden_modules ~value:(Ast.Token.text token) ->
        Some (make_diagnostic token)
    | Some _
    | None -> None
  in
  dedupe_diagnostics
    (
      Api.Traversal.find_nodes
        (fun node -> Option.is_some (Ast.cast_result_to_option (Ast.Ident.cast node)))
        red_root
      |> List.filter_map
        ~fn:(fun node ->
          match Ast.cast_result_to_option (Ast.Ident.cast node) with
          | Some ident -> diagnostic_for_ident ident
          | None -> None)
    )

let rule = fun () ->
  Api.Rule.make
    ~id:package_rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
