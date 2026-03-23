open Std
open Std.Collections

let rule_id = "prefer-t-for-single-type-modules"
let rule_description =
  "Modules with a single type definition should usually call it t"

let rule_explain =
  {|
When a module has one obvious main type, naming it `t` makes the surrounding code read
naturally. `User.t`, `Decoder.t`, and `Cache.t` are standard OCaml shapes that
communicate "the primary type owned by this module" without forcing the name to repeat
itself.

If that same module writes `type user`, call sites end up with `User.user`, which is
usually redundant. The module name has already supplied the context.

This rule is intentionally narrow. It only applies when the module clearly exposes one
primary type. Modules with several important types should name them explicitly.
|}

let is_trivia kind =
  let open Syn.SyntaxKind in
  kind = WHITESPACE || kind = COMMENT || kind = DOCSTRING

let direct_non_trivia_nodes node =
  Syn.Ceibo.Red.SyntaxNode.children node
  |> Array.to_list
  |> List.filter_map (function
       | Syn.Ceibo.Red.Node child
         when not (is_trivia (Syn.Ceibo.Red.SyntaxNode.kind child)) ->
           Some child
       | _ ->
           None)

let first_non_trivia_token node =
  Syn.Ceibo.Red.SyntaxNode.children node
  |> Array.to_list
  |> List.find_map (function
       | Syn.Ceibo.Red.Token token
         when not (is_trivia (Syn.Ceibo.Red.SyntaxToken.kind token)) ->
           Some token
       | _ ->
           None)

let type_name_token_from_decl_node node =
  direct_non_trivia_nodes node
  |> List.find_map (fun child ->
         match Syn.Ceibo.Red.SyntaxNode.kind child with
         | Syn.SyntaxKind.MODULE_PATH ->
             first_non_trivia_token child
         | _ ->
             None)

let single_type_decl_token item_nodes =
  match
    item_nodes
    |> List.filter (fun node ->
           Syn.Ceibo.Red.SyntaxNode.kind node = Syn.SyntaxKind.TYPE_DECL)
  with
  | [ type_decl ] ->
      type_name_token_from_decl_node type_decl
  | _ ->
      None

let signature_item_nodes signature_syntax_node =
  direct_non_trivia_nodes signature_syntax_node

let make_diagnostic token =
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:("Rename " ^ original ^ " to t")
    ()

let diagnostic_for_module_decl decl =
  match Syn.Cst.ModuleDeclaration.module_expression decl with
  | Some (Syn.Cst.ModuleExpression.Structure { item_syntax_nodes; _ }) -> (
      match single_type_decl_token item_syntax_nodes with
      | Some token when Syn.Ceibo.Red.SyntaxToken.text token != "t" ->
          Some (make_diagnostic token)
      | _ ->
          None)
  | _ ->
      None

let diagnostic_for_module_type_decl decl =
  match Syn.Cst.ModuleTypeDeclaration.module_type decl with
  | Some (Syn.Cst.ModuleType.Signature { signature_syntax_node; _ }) -> (
      match signature_item_nodes signature_syntax_node |> single_type_decl_token with
      | Some token when Syn.Ceibo.Red.SyntaxToken.text token != "t" ->
          Some (make_diagnostic token)
      | _ ->
          None)
  | _ ->
      None

let diagnostics_for_items source_file =
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items
      |> List.filter_map (function
           | Syn.Cst.StructureItem.ModuleDeclaration decl ->
               diagnostic_for_module_decl decl
           | Syn.Cst.StructureItem.ModuleTypeDeclaration decl ->
               diagnostic_for_module_type_decl decl
           | _ ->
               None)
  | Syn.Cst.Interface { items; _ } ->
      items
      |> List.filter_map (function
           | Syn.Cst.SignatureItem.ModuleDeclaration decl ->
               diagnostic_for_module_decl decl
           | Syn.Cst.SignatureItem.ModuleTypeDeclaration decl ->
               diagnostic_for_module_type_decl decl
           | _ ->
               None)

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:(fun ctx _red_root ->
      match ctx.cst with
      | None ->
          []
      | Some source_file ->
          diagnostics_for_items source_file)
    ()
