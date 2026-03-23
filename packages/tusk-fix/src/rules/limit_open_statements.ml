open Std
open Std.Collections

let rule_id = "limit-open-statements"
let rule_description =
  "Prefer no more than two open statements per file"

let rule_explain =
  {|
Each file-wide `open` saves a little typing, but it also hides where names come from.
Once a file stacks several of them, readers have to keep a mental import table in
their head before they can tell whether `Response`, `parse`, or `empty` is local or
imported.

Past a small number, the convenience stops paying for the ambiguity. The remaining
modules are usually better referenced explicitly as `Http.Response`, `Json.decode`,
or `Uri.of_string`. If the scope is genuinely tiny, a local open is easier to audit
than another file-wide one.

Two well-chosen opens usually remain readable. A pile of them usually means the file
has become too implicit.
|}

let open_statements source_file =
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items
      |> List.filter_map (function
           | Syn.Cst.StructureItem.OpenStatement stmt -> Some stmt
           | _ -> None)
  | Syn.Cst.Interface { items; _ } ->
      items
      |> List.filter_map (function
           | Syn.Cst.SignatureItem.OpenStatement stmt -> Some stmt
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
                  { rule_id; message = rule_description })
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
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
