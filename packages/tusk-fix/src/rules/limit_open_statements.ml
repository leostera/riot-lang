open Std
open Std.Collections

let rule_id = "limit-open-statements"
let rule_name = "Limit Open Statements"
let rule_code = "F0126"

let rule_description =
  "Prefer no more than two open statements per file"

let rule_message =
  "Too many open statements make names harder to track; prefer explicit qualification."

let rule_explain =
  {|
Prefer no more than two open statements per file.

Why this rule exists:
- Several open statements in one file make it harder to tell where names come from.
- They also increase the risk of shadowing or accidental API collisions.
- Past a small number, explicit qualification is usually easier to audit than another file-wide open.

What to do instead:
- Keep only the most valuable opens.
- Prefer Module.value and Module.Type for everything else.
- If the scope is small, use a narrow local open instead of a file-wide one.

Examples:
  Better:
    open Std
    open Http

    let response = Http.Response.ok

  Avoid:
    open Std
    open Http
    open Json
    open Uri
|}

let open_statements source_file =
  Syn.Cst.SourceFile.items source_file
  |> List.filter_map (function
       | Syn.Cst.Item.OpenStatement stmt -> Some stmt
       | _ -> None)

let diagnostic_for_open_count opens =
  if List.length opens <= 2 then
    None
  else
    match List.drop 2 opens with
    | third_open :: _ ->
        Some
          (Diagnostic.make ~severity:Warning
             ~kind:
               (Diagnostic.Known
                  { code = rule_code; rule_id; message = rule_message })
             ~span:
               (Syn.Cst.OpenStatement.syntax_node third_open
               |> Syn.Ceibo.Red.SyntaxNode.span)
             ~suggestion:
               "Keep only the most useful opens and qualify the remaining names."
             ())
    | [] -> None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      open_statements source_file
      |> diagnostic_for_open_count
      |> Option.to_list

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
