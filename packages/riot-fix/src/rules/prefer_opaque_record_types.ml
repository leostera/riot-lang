open Std
open Std.Collections

let rule_id = "prefer-opaque-record-types"

let rule_description = "Interfaces that already expose accessor functions should usually keep record types opaque"

let rule_explain = {|
If an interface already exposes accessor functions such as `name : t -> string`, then
publishing the record fields as well usually buys very little. Callers now have two
ways to access the same information, but the module loses freedom to rearrange or
validate its internal representation later.

Making the type opaque keeps the public API focused on the operations the module
actually wants to support. The existing accessors remain available, and the module
keeps the option to rename fields, add derived data, or enforce invariants without
breaking callers.

This rule fires when the field-level representation is already redundant with the
function surface the module has chosen to expose.
|}

let rec strip_type_wrappers = function
  | Syn.Cst.CoreType.Parenthesized { inner; _ }
  | Syn.Cst.CoreType.Attribute { type_=inner; _ }
  | Syn.Cst.CoreType.Alias { type_=inner; _ } -> strip_type_wrappers inner
  | type_ -> type_

let is_t_core_type = fun type_ ->
  match strip_type_wrappers type_ with
  | Syn.Cst.CoreType.Constr { constructor_path; arguments=[]; _ } -> (
      match Syn.Cst.Ident.name constructor_path with
      | Some "t" -> true
      | _ -> false
    )
  | _ -> false

let rec first_arrow_parameter_type = fun type_ ->
  match strip_type_wrappers type_ with
  | Syn.Cst.CoreType.Arrow { parameter_type; _ } -> Some parameter_type
  | Syn.Cst.CoreType.Poly { body; _ } -> first_arrow_parameter_type body
  | _ -> None

let is_accessor_of_t = fun ({ type_; _ }: Syn.Cst.value_declaration) ->
  match first_arrow_parameter_type type_ with
  | Some parameter_type -> is_t_core_type parameter_type
  | None -> false

let make_diagnostic = fun (decl: Syn.Cst.TypeDeclaration.t) accessor_names ->
  let accessor_summary =
    match accessor_names with
    | [] -> "exposed accessors"
    | [ name ] -> "the `" ^ name ^ "` accessor"
    | first :: second :: _ -> "accessors like `" ^ first ^ "` and `" ^ second ^ "`"
  in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.TypeDeclaration.syntax_node decl))
    ~suggestion:("Make `t` opaque in the interface and keep " ^ accessor_summary ^ " as the public surface")
    ()

let diagnostic_for_type_declaration = fun value_declarations decl ->
  match Syn.Cst.TypeDeclaration.type_name decl |> Syn.Cst.Ident.name with
  | Some "t" -> (
      match Syn.Cst.TypeDeclaration.type_definition decl with
      | Syn.Cst.TypeDefinition.Record { fields; _ } ->
          let field_names = fields |> List.map ~fn:Syn.Cst.RecordField.name in
          let accessor_names = value_declarations
          |> List.filter ~fn:is_accessor_of_t
          |> List.map
            ~fn:(fun (value_decl: Syn.Cst.value_declaration) ->
              value_decl.name_tokens |> List.map ~fn:Syn.Cst.Token.text |> String.concat "")
          |> List.filter ~fn:(fun name -> List.contains field_names ~value:name) in
          if List.length accessor_names > 0 then
            Some (make_diagnostic decl accessor_names)
          else
            None
      | _ -> None
    )
  | _ -> None

let check_tree = fun (ctx: Rule.context) _red_root ->
  if not (String.ends_with ~suffix:".mli" ctx.file_path) then
    []
  else
    let source_file = ctx.cst in
    (
      match Syn.Cst.SourceFile.signature_items source_file with
      | None -> []
      | Some items ->
          let value_declarations =
            items
            |> List.filter_map
              ~fn:(
                function
                | Syn.Cst.SignatureItem.ValueDeclaration decl -> Some decl
                | _ -> None
              )
          in
          items |> List.filter_map
            ~fn:(
              function
              | Syn.Cst.SignatureItem.TypeDeclaration decl -> diagnostic_for_type_declaration
                value_declarations
                decl
              | _ -> None
            )
    )

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
