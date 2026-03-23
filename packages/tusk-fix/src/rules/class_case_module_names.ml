open Std
open Std.Collections

let rule_id = "class-case-module-names"
let rule_description =
  "Module names should use ClassCase instead of underscores"

let rule_explain =
  {|
Module names should use ClassCase without underscores.

Why this rule exists:
- Mixed styles like Foo_bar are harder to scan than either FooBar or foo_bar.
- Riot uses ClassCase for modules and snake_case for values. Mixing the two in one identifier makes the boundary blurry.

Examples:
  Bad:    module Foo_bar = struct ... end
  Better: module FooBar = struct ... end
|}

let contains_underscore text =
  String.exists (fun ch -> ch = '_') text

let starts_upper text =
  String.length text > 0
  && let ch = String.get text 0 in
     ch >= 'A' && ch <= 'Z'

let capitalize_piece piece =
  if String.equal piece "" then
    ""
  else
    let first = String.get piece 0 |> Char.uppercase_ascii |> String.make 1 in
    let rest =
      if String.length piece = 1 then
        ""
      else
        String.sub piece 1 (String.length piece - 1)
    in
    first ^ rest

let to_class_case text =
  text
  |> String.split_on_char '_'
  |> List.filter (fun piece -> not (String.equal piece ""))
  |> List.map capitalize_piece
  |> String.concat ""

let should_flag_module_name text =
  contains_underscore text || not (starts_upper text)

let make_diagnostic token =
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  let replacement = to_class_case original in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:("Rename " ^ original ^ " to " ^ replacement)
    ()

let diagnostic_for_module_decl decl =
  let name = Syn.Cst.ModuleDeclaration.name decl in
  if should_flag_module_name name then
    let token =
      Syn.Cst.ModuleDeclaration.module_name_token decl
      |> Syn.Cst.Token.syntax_token
    in
    Some (make_diagnostic token)
  else
    None

let diagnostic_for_module_type_decl decl =
  let name = Syn.Cst.ModuleTypeDeclaration.name decl in
  if should_flag_module_name name then
    let token =
      Syn.Cst.ModuleTypeDeclaration.module_type_name_token decl
      |> Syn.Cst.Token.syntax_token
    in
    Some (make_diagnostic token)
  else
    None

let diagnostics_for_recursive_module_decl decl =
  Syn.Cst.RecursiveModuleDeclaration.declarations decl
  |> List.filter_map diagnostic_for_module_decl

let diagnostics_for_items source_file =
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items
      |> List.concat_map (function
           | Syn.Cst.StructureItem.ModuleDeclaration decl ->
               Option.to_list (diagnostic_for_module_decl decl)
           | Syn.Cst.StructureItem.RecursiveModuleDeclaration decl ->
               diagnostics_for_recursive_module_decl decl
           | Syn.Cst.StructureItem.ModuleTypeDeclaration decl ->
               Option.to_list (diagnostic_for_module_type_decl decl)
           | _ -> [])
  | Syn.Cst.Interface { items; _ } ->
      items
      |> List.concat_map (function
           | Syn.Cst.SignatureItem.ModuleDeclaration decl ->
               Option.to_list (diagnostic_for_module_decl decl)
           | Syn.Cst.SignatureItem.RecursiveModuleDeclaration decl ->
               diagnostics_for_recursive_module_decl decl
           | Syn.Cst.SignatureItem.ModuleTypeDeclaration decl ->
               Option.to_list (diagnostic_for_module_type_decl decl)
           | _ -> [])

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file -> diagnostics_for_items source_file

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
