open Std
open Std.Collections

module A = Syn.Ast
module Visitor = Syn.Visitor
module Ser = Serde.Ser

let ser_list = fun encode -> Ser.contramap Vector.from_list (Ser.list encode)

module Item = struct
  module Ident = struct
    type t = string list
  end

  type include_mode =
    | Structure
    | Signature

  type functor_arg = {
    name: string option;
    ascription: t list;
  }

  and bound_module = {
    name: string;
    ascription: t list;
  }

  and t =
    | Use of Ident.t
    | Open of t
    | Include of include_mode * t
    | Module of {
        name: string;
        signature: t list;
        body: t list;
      }
    | ModuleAlias of { name: string; target: t }
    | Functor of {
        name: string;
        args: functor_arg list;
        body: t list;
      }
    | ModuleType of {
        name: string;
        body: t list;
      }
    | FunctorApply of { callee: t; argument: t }
    | Constraint of {
        expr: t;
        signature: t list;
      }
    | Typeof of t
    | WithConstraint of {
        base: t;
        constraints: t list;
      }
    | BindModules of {
        modules: bound_module list;
        scope: t list;
      }
    | Scope of t list

  type include_payload = {
    include_mode: include_mode;
    include_expr: t;
  }

  type module_payload = {
    module_name: string;
    module_signature: t list;
    module_body: t list;
  }

  type module_alias_payload = { module_alias_name: string; module_alias_target: t }

  type functor_payload = {
    functor_name: string;
    functor_args: functor_arg list;
    functor_body: t list;
  }

  type module_type_payload = {
    module_type_name: string;
    module_type_body: t list;
  }

  type functor_apply_payload = { functor_apply_callee: t; functor_apply_argument: t }

  type constraint_payload = {
    constraint_expr: t;
    constraint_signature: t list;
  }

  type with_constraint_payload = {
    with_base: t;
    with_constraints: t list;
  }

  type bind_modules_payload = {
    bind_modules_modules: bound_module list;
    bind_modules_scope: t list;
  }

  let include_mode_serializer =
    Ser.variant
      [
        Ser.Variant.unit
          "Structure"
          (fun __tmp1 ->
            match __tmp1 with
            | Structure -> true
            | Signature -> false);
        Ser.Variant.unit
          "Signature"
          (fun __tmp1 ->
            match __tmp1 with
            | Signature -> true
            | Structure -> false);
      ]

  let ident_serializer = ser_list Ser.string

  let rec serializer = {
    Ser.run =
      (fun backend state item ->
        let item_list_serializer = ser_list serializer in
        let functor_arg_serializer =
          Ser.record
            (
              Ser.fields
                [
                  Ser.field "name" (Ser.option Ser.string) (fun (arg: functor_arg) -> arg.name);
                  Ser.field
                    "ascription"
                    item_list_serializer
                    (fun (arg: functor_arg) -> arg.ascription);
                ]
            )
        in
        let bound_module_serializer =
          Ser.record
            (
              Ser.fields
                [
                  Ser.field "name" Ser.string (fun (module_: bound_module) -> module_.name);
                  Ser.field
                    "ascription"
                    item_list_serializer
                    (fun (module_: bound_module) -> module_.ascription);
                ]
            )
        in
        let include_payload_serializer =
          Ser.record
            (
              Ser.fields
                [
                  Ser.field
                    "mode"
                    include_mode_serializer
                    (fun (payload: include_payload) -> payload.include_mode);
                  Ser.field
                    "expr"
                    serializer
                    (fun (payload: include_payload) -> payload.include_expr);
                ]
            )
        in
        let module_payload_serializer =
          Ser.record
            (
              Ser.fields
                [
                  Ser.field "name" Ser.string (fun (payload: module_payload) -> payload.module_name);
                  Ser.field
                    "signature"
                    item_list_serializer
                    (fun (payload: module_payload) -> payload.module_signature);
                  Ser.field
                    "body"
                    item_list_serializer
                    (fun (payload: module_payload) -> payload.module_body);
                ]
            )
        in
        let module_alias_payload_serializer =
          Ser.record
            (
              Ser.fields
                [
                  Ser.field
                    "name"
                    Ser.string
                    (fun (payload: module_alias_payload) -> payload.module_alias_name);
                  Ser.field
                    "target"
                    serializer
                    (fun (payload: module_alias_payload) -> payload.module_alias_target);
                ]
            )
        in
        let functor_payload_serializer =
          Ser.record
            (
              Ser.fields
                [
                  Ser.field
                    "name"
                    Ser.string
                    (fun (payload: functor_payload) -> payload.functor_name);
                  Ser.field
                    "args"
                    (ser_list functor_arg_serializer)
                    (fun (payload: functor_payload) -> payload.functor_args);
                  Ser.field
                    "body"
                    item_list_serializer
                    (fun (payload: functor_payload) -> payload.functor_body);
                ]
            )
        in
        let module_type_payload_serializer =
          Ser.record
            (
              Ser.fields
                [
                  Ser.field
                    "name"
                    Ser.string
                    (fun (payload: module_type_payload) -> payload.module_type_name);
                  Ser.field
                    "body"
                    item_list_serializer
                    (fun (payload: module_type_payload) -> payload.module_type_body);
                ]
            )
        in
        let functor_apply_payload_serializer =
          Ser.record
            (
              Ser.fields
                [
                  Ser.field
                    "callee"
                    serializer
                    (fun (payload: functor_apply_payload) -> payload.functor_apply_callee);
                  Ser.field
                    "argument"
                    serializer
                    (fun (payload: functor_apply_payload) -> payload.functor_apply_argument);
                ]
            )
        in
        let constraint_payload_serializer =
          Ser.record
            (
              Ser.fields
                [
                  Ser.field
                    "expr"
                    serializer
                    (fun (payload: constraint_payload) -> payload.constraint_expr);
                  Ser.field
                    "signature"
                    item_list_serializer
                    (fun (payload: constraint_payload) -> payload.constraint_signature);
                ]
            )
        in
        let with_constraint_payload_serializer =
          Ser.record
            (
              Ser.fields
                [
                  Ser.field
                    "base"
                    serializer
                    (fun (payload: with_constraint_payload) -> payload.with_base);
                  Ser.field
                    "constraints"
                    item_list_serializer
                    (fun (payload: with_constraint_payload) -> payload.with_constraints);
                ]
            )
        in
        let bind_modules_payload_serializer =
          Ser.record
            (
              Ser.fields
                [
                  Ser.field
                    "modules"
                    (ser_list bound_module_serializer)
                    (fun (payload: bind_modules_payload) -> payload.bind_modules_modules);
                  Ser.field
                    "scope"
                    item_list_serializer
                    (fun (payload: bind_modules_payload) -> payload.bind_modules_scope);
                ]
            )
        in
        let encode =
          Ser.variant
            [
              Ser.Variant.newtype
                "Use"
                ident_serializer
                (fun __tmp1 ->
                  match __tmp1 with
                  | Use ident -> Some ident
                  | _ -> None);
              Ser.Variant.newtype
                "Open"
                serializer
                (fun __tmp1 ->
                  match __tmp1 with
                  | Open expr -> Some expr
                  | _ -> None);
              Ser.Variant.newtype
                "Include"
                include_payload_serializer
                (fun __tmp1 ->
                  match __tmp1 with
                  | Include (include_mode, include_expr) -> Some { include_mode; include_expr }
                  | _ -> None);
              Ser.Variant.newtype
                "Module"
                module_payload_serializer
                (fun __tmp1 ->
                  match __tmp1 with
                  | Module { name; signature; body } ->
                      Some { module_name = name; module_signature = signature; module_body = body }
                  | _ -> None);
              Ser.Variant.newtype
                "ModuleAlias"
                module_alias_payload_serializer
                (fun __tmp1 ->
                  match __tmp1 with
                  | ModuleAlias { name; target } ->
                      Some { module_alias_name = name; module_alias_target = target }
                  | _ -> None);
              Ser.Variant.newtype
                "Functor"
                functor_payload_serializer
                (fun __tmp1 ->
                  match __tmp1 with
                  | Functor { name; args; body } ->
                      Some { functor_name = name; functor_args = args; functor_body = body }
                  | _ -> None);
              Ser.Variant.newtype
                "ModuleType"
                module_type_payload_serializer
                (fun __tmp1 ->
                  match __tmp1 with
                  | ModuleType { name; body } ->
                      Some { module_type_name = name; module_type_body = body }
                  | _ -> None);
              Ser.Variant.newtype
                "FunctorApply"
                functor_apply_payload_serializer
                (fun __tmp1 ->
                  match __tmp1 with
                  | FunctorApply { callee; argument } ->
                      Some { functor_apply_callee = callee; functor_apply_argument = argument }
                  | _ -> None);
              Ser.Variant.newtype
                "Constraint"
                constraint_payload_serializer
                (fun __tmp1 ->
                  match __tmp1 with
                  | Constraint { expr; signature } ->
                      Some { constraint_expr = expr; constraint_signature = signature }
                  | _ -> None);
              Ser.Variant.newtype
                "Typeof"
                serializer
                (fun __tmp1 ->
                  match __tmp1 with
                  | Typeof expr -> Some expr
                  | _ -> None);
              Ser.Variant.newtype
                "WithConstraint"
                with_constraint_payload_serializer
                (fun __tmp1 ->
                  match __tmp1 with
                  | WithConstraint { base; constraints } ->
                      Some { with_base = base; with_constraints = constraints }
                  | _ -> None);
              Ser.Variant.newtype
                "BindModules"
                bind_modules_payload_serializer
                (fun __tmp1 ->
                  match __tmp1 with
                  | BindModules { modules; scope } ->
                      Some { bind_modules_modules = modules; bind_modules_scope = scope }
                  | _ -> None);
              Ser.Variant.newtype
                "Scope"
                item_list_serializer
                (fun __tmp1 ->
                  match __tmp1 with
                  | Scope body -> Some body
                  | _ -> None);
            ]
        in
        encode.run backend state item);
  }
end

type source_kind =
  | Implementation
  | Interface

type source_summary = {
  source: Path.t;
  source_hash: Crypto.hash;
  module_path: string list option;
  kind: source_kind;
  items: Item.t list;
}

type parse_error =
  | Parse_diagnostics of Syn.Diagnostic.t list

let source_kind_serializer =
  Ser.variant
    [
      Ser.Variant.unit
        "Implementation"
        (fun __tmp1 ->
          match __tmp1 with
          | Implementation -> true
          | Interface -> false);
      Ser.Variant.unit
        "Interface"
        (fun __tmp1 ->
          match __tmp1 with
          | Interface -> true
          | Implementation -> false);
    ]

let source_summary_serializer =
  Ser.record
    (
      Ser.fields
        [
          Ser.field
            "source"
            (Ser.contramap Path.to_string Ser.string)
            (fun (summary: source_summary) -> summary.source);
          Ser.field
            "source_hash"
            (Ser.contramap Crypto.Digest.hex Ser.string)
            (fun (summary: source_summary) -> summary.source_hash);
          Ser.field
            "module_path"
            (Ser.option (ser_list Ser.string))
            (fun (summary: source_summary) -> summary.module_path);
          Ser.field "kind" source_kind_serializer (fun (summary: source_summary) -> summary.kind);
          Ser.field
            "items"
            (ser_list Item.serializer)
            (fun (summary: source_summary) -> summary.items);
        ]
    )

let is_uppercase_ascii = fun ch -> ch >= 'A' && ch <= 'Z'

let is_module_head = fun segment ->
  String.length segment > 0 && is_uppercase_ascii (String.get_unchecked segment ~at:0)

let drop_last = fun __tmp1 ->
  match __tmp1 with
  | []
  | [ _ ] -> []
  | items ->
      let rec loop acc = fun __tmp1 ->
        match __tmp1 with
        | []
        | [ _ ] -> List.reverse acc
        | head :: tail -> loop (head :: acc) tail
      in
      loop [] items

let sorted_unique_strings = fun values ->
  List.unique
    (List.sort values ~compare:String.compare)
    ~compare:String.compare

let token_text = A.Token.text

let ident_segments = fun ident ->
  A.Ident.fold_segment ident ~init:[] ~fn:(fun token acc -> A.Continue (token_text token :: acc))
  |> List.reverse

let ident_segments_if_module_head = fun ident ->
  match A.Ident.first_segment ident with
  | Some token when is_module_head (token_text token) -> Some (ident_segments ident)
  | Some _
  | None -> None

let ident_parent_segments = fun ident ->
  match A.Ident.first_segment ident with
  | Some token when is_module_head (token_text token) -> (
      let reversed =
        A.Ident.fold_segment
          ident
          ~init:[]
          ~fn:(fun token acc -> A.Continue (token_text token :: acc))
      in
      match reversed with
      | []
      | [ _ ] -> None
      | _last :: parent_reversed -> Some (List.reverse parent_reversed)
    )
  | Some _
  | None -> None

let ident_name = fun ident ->
  A.Ident.last_segment ident
  |> Option.map ~fn:token_text

let ident_module_use = fun ident ->
  match ident_segments_if_module_head ident with
  | Some segments -> [ Item.Use segments ]
  | None -> []

let ident_parent_use = fun ident ->
  match ident_parent_segments ident with
  | Some segments -> [ Item.Use segments ]
  | None -> []

let ident_module_open = fun ident ->
  match ident_segments_if_module_head ident with
  | Some segments -> [ Item.Open (Item.Use segments) ]
  | None -> []

let implicit_module_open = fun segments ->
  match segments with
  | head :: _ when is_module_head head -> [ Item.Open (Item.Use segments) ]
  | _ -> []

let ident_module_include = fun mode ident ->
  match ident_segments_if_module_head ident with
  | Some segments ->
      [
        Item.Include (mode, Item.Use segments);
      ]
  | None -> []

let prepend_all = fun values acc ->
  List.fold_left
    values
    ~init:acc
    ~fn:(fun acc value -> value :: acc)

let scoped_items = fun items ->
  match items with
  | [] -> []
  | _ -> [ Item.Scope items ]

let vector_items = fun vector ~fn ->
  let items = ref [] in
  Vector.for_each vector ~fn:(fun item -> items := prepend_all (fn item) !items);
  List.reverse !items

type container =
  | Structure_container
  | Signature_container

type collect_ctx = {
  items: Item.t list;
  container: container;
  container_restores: (int * container) list;
}

let empty_collect_ctx = { items = []; container = Structure_container; container_restores = [] }

let add_item = fun ctx item -> { ctx with items = item :: ctx.items }

let add_items = fun ctx items -> {
  ctx with
  items = List.fold_left items ~init:ctx.items ~fn:(fun acc item -> item :: acc);
}

let with_visitor_ctx = fun visitor fn -> Visitor.with_ctx visitor (fn (Visitor.ctx visitor))

let continue = fun visitor -> (visitor, Visitor.Continue)

let skip = fun visitor -> (visitor, Visitor.Skip_subtree)

let push_container = fun ctx (node: A.Node.t) container ->
  {
    ctx with
    container;
    container_restores = (node.id, ctx.container) :: ctx.container_restores;
  }

let restore_container = fun ctx (node: A.Node.t) ->
  match ctx.container_restores with
  | (id, container) :: rest when Int.equal id node.id ->
      { ctx with container; container_restores = rest }
  | _ -> ctx

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"expected dependency analyzer source slice"

let first_module_expr = fun node ->
  A.Node.fold_child_node
    node
    ~init:None
    ~fn:(fun child _ ->
      match A.ModuleExpr.cast child with
      | A.Node module_expr -> A.Return (Some module_expr)
      | A.Unknown _
      | A.Error _ -> A.Continue None)

let first_module_type_expr = fun node ->
  A.Node.fold_child_node
    node
    ~init:None
    ~fn:(fun child _ ->
      match A.ModuleTypeExpr.cast child with
      | A.Node module_type -> A.Return (Some module_type)
      | A.Unknown _
      | A.Error _ -> A.Continue None)

let child_token_at = fun node index ->
  match A.Node.child_at node index with
  | Some (Syn.SyntaxTree.Token id) -> Some ({ tree = node.A.tree; id }: A.Token.t)
  | Some (Syn.SyntaxTree.Node _)
  | Some (Syn.SyntaxTree.Missing _)
  | None -> None

let child_node_at = fun node index ->
  match A.Node.child_at node index with
  | Some (Syn.SyntaxTree.Node id) -> Some ({ tree = node.A.tree; id }: A.Node.t)
  | Some (Syn.SyntaxTree.Token _)
  | Some (Syn.SyntaxTree.Missing _)
  | None -> None

let child_token_kind_is = fun node index kind ->
  match child_token_at node index with
  | Some token -> Syn.SyntaxKind.(A.Token.kind token = kind)
  | None -> false

let find_token = fun node start stop kind ->
  let rec loop index =
    if index >= stop then
      None
    else if child_token_kind_is node index kind then
      Some index
    else
      loop (index + 1)
  in
  loop start

let is_module_expr_kind = fun __tmp1 ->
  match __tmp1 with
  | Syn.SyntaxKind.MODULE_EXPR
  | Syn.SyntaxKind.PATH_MODULE_EXPR
  | Syn.SyntaxKind.STRUCT_MODULE_EXPR
  | Syn.SyntaxKind.FUNCTOR_MODULE_EXPR
  | Syn.SyntaxKind.APPLY_MODULE_EXPR
  | Syn.SyntaxKind.CONSTRAINT_MODULE_EXPR
  | Syn.SyntaxKind.PAREN_MODULE_EXPR
  | Syn.SyntaxKind.OPAQUE_MODULE_EXPR -> true
  | _ -> false

let is_module_type_kind = fun __tmp1 ->
  match __tmp1 with
  | Syn.SyntaxKind.MODULE_TYPE_EXPR
  | Syn.SyntaxKind.PATH_MODULE_TYPE
  | Syn.SyntaxKind.SIGNATURE_MODULE_TYPE
  | Syn.SyntaxKind.TYPEOF_MODULE_TYPE
  | Syn.SyntaxKind.FUNCTOR_MODULE_TYPE
  | Syn.SyntaxKind.WITH_MODULE_TYPE
  | Syn.SyntaxKind.PAREN_MODULE_TYPE
  | Syn.SyntaxKind.OPAQUE_MODULE_TYPE -> true
  | _ -> false

let first_module_expr_after = fun node start ->
  let count = A.Node.child_count node in
  let rec loop index =
    if index >= count then
      None
    else
      match child_node_at node index with
      | Some child -> (
          match A.ModuleExpr.cast child with
          | A.Node module_expr -> Some module_expr
          | A.Unknown _
          | A.Error _ -> loop (index + 1)
        )
      | None -> loop (index + 1)
  in
  loop start

let first_module_type_expr_after = fun node start ->
  let count = A.Node.child_count node in
  let rec loop index =
    if index >= count then
      None
    else
      match child_node_at node index with
      | Some child -> (
          match A.ModuleTypeExpr.cast child with
          | A.Node module_type -> Some module_type
          | A.Unknown _
          | A.Error _ -> loop (index + 1)
        )
      | None -> loop (index + 1)
  in
  loop start

let rec functor_arg_of_node_range = fun node start stop ->
  let colon_index = find_token node (start + 1) stop Syn.SyntaxKind.COLON in
  let name_stop = Option.unwrap_or colon_index ~default:stop in
  let name =
    A.Ident.from_child_range_option node ~start_index:(start + 1) ~stop_index:name_stop
    |> Option.and_then ~fn:ident_name
  in
  let ascription =
    match colon_index with
    | Some colon_index -> (
        match A.Ident.from_child_range_option node ~start_index:(colon_index + 1) ~stop_index:stop with
        | Some ident -> ident_module_use ident
        | None -> (
            match first_module_type_expr_after node (colon_index + 1) with
            | Some module_type -> [ item_of_module_type_expr module_type ]
            | None -> []
          )
      )
    | None -> []
  in
  ({ Item.name; ascription }: Item.functor_arg)

and functor_parts = fun node ~body_kind ->
  let count = A.Node.child_count node in
  let rec find_close index =
    if index >= count then
      count
    else if child_token_kind_is node index Syn.SyntaxKind.RPAREN then
      index
    else
      find_close (index + 1)
  in
  let rec loop index args =
    if index >= count then
      (List.reverse args, [])
    else if child_token_kind_is node index Syn.SyntaxKind.ARROW then
      let body =
        match body_kind with
        | `ModuleExpr -> (
            match first_module_expr_after node (index + 1) with
            | Some module_expr -> items_of_module_expr_declaration_body module_expr
            | None -> []
          )
        | `ModuleType -> (
            match first_module_type_expr_after node (index + 1) with
            | Some module_type -> [ item_of_module_type_expr module_type ]
            | None -> []
          )
      in
      (List.reverse args, body)
    else if child_token_kind_is node index Syn.SyntaxKind.LPAREN then
      let close_index = find_close (index + 1) in
      let arg = functor_arg_of_node_range node index close_index in
      loop (close_index + 1) (arg :: args)
    else
      loop (index + 1) args
  in
  loop 0 []

and bound_modules_of_functor_args = fun args ->
  List.fold_left
    args
    ~init:[]
    ~fn:(fun modules (arg: Item.functor_arg) ->
      match arg.name with
      | Some name -> ({ Item.name; ascription = arg.ascription }: Item.bound_module) :: modules
      | None -> modules)
  |> List.reverse

and bind_functor_scope = fun args scope ->
  match bound_modules_of_functor_args args with
  | [] -> Item.Scope scope
  | modules -> Item.BindModules { modules; scope }

and bind_modules_items = fun modules scope ->
  match modules with
  | [] -> scoped_items scope
  | _ -> [ Item.BindModules { modules; scope } ]

and items_of_source_file = fun source_file ->
  match A.SourceFile.view source_file with
  | A.SourceFile.Implementation impl -> (Implementation, items_of_implementation impl)
  | A.SourceFile.Interface intf -> (Interface, items_of_interface intf)

and items_of_implementation = fun impl ->
  A.Implementation.fold_item
    impl
    ~init:[]
    ~fn:(fun item items -> A.Continue (prepend_all (items_of_structure_item item) items))
  |> List.reverse

and items_of_interface = fun intf ->
  A.Interface.fold_item
    intf
    ~init:[]
    ~fn:(fun item items -> A.Continue (prepend_all (items_of_signature_item item) items))
  |> List.reverse

and items_of_structure_item = fun item ->
  match A.StructureItem.view item with
  | A.StructureItem.Module decl -> items_of_module_declaration Structure_container decl
  | A.StructureItem.ModuleType decl -> items_of_module_type_declaration decl
  | A.StructureItem.Open decl -> items_of_open_declaration decl
  | A.StructureItem.Include decl -> items_of_include_declaration Item.Structure decl
  | _ -> items_of_node ~container:Structure_container (A.StructureItem.as_node item)

and items_of_signature_item = fun item ->
  match A.SignatureItem.view item with
  | A.SignatureItem.Value decl -> items_of_value_declaration decl
  | A.SignatureItem.Module decl -> items_of_module_declaration Signature_container decl
  | A.SignatureItem.ModuleType decl -> items_of_module_type_declaration decl
  | A.SignatureItem.Open decl -> items_of_open_declaration decl
  | A.SignatureItem.Include decl -> items_of_include_declaration Item.Signature decl
  | A.SignatureItem.External decl -> items_of_external_declaration decl
  | _ -> items_of_node ~container:Signature_container (A.SignatureItem.as_node item)

and items_of_open_declaration = fun decl ->
  match A.OpenDeclaration.ident decl with
  | Some ident -> ident_module_open ident
  | None ->
      let node = A.OpenDeclaration.as_node decl in
      (
        match first_module_expr node with
        | Some module_expr -> [ Item.Open (item_of_module_expr module_expr) ]
        | None -> (
            match first_module_type_expr node with
            | Some module_type -> [ Item.Open (item_of_module_type_expr module_type) ]
            | None -> []
          )
      )

and items_of_include_declaration = fun mode decl ->
  match A.IncludeDeclaration.body_ident decl with
  | Some ident -> ident_module_include mode ident
  | None -> (
      match A.IncludeDeclaration.body_node decl with
      | Some node -> (
          match A.ModuleExpr.cast node with
          | A.Node module_expr ->
              [
                Item.Include (mode, item_of_module_expr module_expr);
              ]
          | A.Unknown _
          | A.Error _ -> (
              match A.ModuleTypeExpr.cast node with
              | A.Node module_type ->
                  [
                    Item.Include (mode, item_of_module_type_expr module_type);
                  ]
              | A.Unknown _
              | A.Error _ -> []
            )
        )
      | None -> []
    )

and items_of_module_type_declaration = fun decl ->
  match A.ModuleTypeDeclaration.name decl with
  | None -> (
      match A.ModuleTypeDeclaration.body decl with
      | A.ModuleTypeDeclaration.Manifest { body } -> [ item_of_module_type_expr body ]
      | A.ModuleTypeDeclaration.Abstract -> []
      | A.ModuleTypeDeclaration.Unsupported { body } -> (
          match body with
          | Some node -> items_of_node ~container:Signature_container node
          | None -> []
        )
    )
  | Some ident -> (
      match ident_name ident with
      | None -> []
      | Some name ->
          let body =
            match A.ModuleTypeDeclaration.body decl with
            | A.ModuleTypeDeclaration.Manifest { body } -> [ item_of_module_type_expr body ]
            | A.ModuleTypeDeclaration.Abstract -> []
            | A.ModuleTypeDeclaration.Unsupported { body } -> (
                match body with
                | Some node -> items_of_node ~container:Signature_container node
                | None -> []
              )
          in
          [ Item.ModuleType { name; body } ]
    )

and items_of_value_declaration = fun decl ->
  match A.ValueDeclaration.view decl with
  | A.ValueDeclaration.Value { annotation; _ } -> items_of_type_expr annotation
  | A.ValueDeclaration.Unknown node -> items_of_node ~container:Signature_container node

and items_of_external_declaration = fun decl ->
  match A.ExternalDeclaration.view decl with
  | A.ExternalDeclaration.External { annotation; _ } -> items_of_type_expr annotation
  | A.ExternalDeclaration.Unknown node -> items_of_node ~container:Signature_container node

and items_of_module_declaration = fun container decl ->
  if A.ModuleDeclaration.is_recursive decl then
    let prebound =
      A.ModuleDeclaration.fold_members
        decl
        []
        (fun items member ->
          match A.ModuleDeclaration.Member.name member with
          | Some ident -> (
              match ident_name ident with
              | Some name -> Item.Module { name; signature = []; body = [] } :: items
              | None -> items
            )
          | None -> items)
      |> List.reverse
    in
    let rhs_items =
      A.ModuleDeclaration.fold_members
        decl
        []
        (fun items member ->
          prepend_all
            (items_of_recursive_module_member_rhs container member)
            items)
      |> List.reverse
    in
    match rhs_items with
    | [] -> prebound
    | _ -> prebound @ [ Item.Scope rhs_items ]
  else
    A.ModuleDeclaration.fold_members
      decl
      []
      (fun items member -> prepend_all (items_of_module_member container member) items)
    |> List.reverse

and functor_args_of_member = fun member ->
  A.ModuleDeclaration.Member.fold_functor_parameter
    member
    ~init:[]
    ~fn:(fun parameter args ->
      let name =
        match parameter.A.ModuleDeclaration.Member.name with
        | Some ident -> ident_name ident
        | None -> None
      in
      let ascription =
        match parameter.A.ModuleDeclaration.Member.annotation with
        | Some ident -> ident_module_use ident
        | None -> []
      in
      A.Continue (({ Item.name = name; ascription }: Item.functor_arg) :: args))
  |> List.reverse

and items_of_functor_parameters = fun member ->
  functor_args_of_member member
  |> List.fold_left
    ~init:[]
    ~fn:(fun items (arg: Item.functor_arg) ->
      prepend_all
        arg.Item.ascription
        (
          match arg.name with
          | Some name -> Item.Module { name; signature = []; body = [] } :: items
          | None -> items
        ))
  |> List.reverse

and items_of_recursive_module_member_rhs = fun container member ->
  let prefix = items_of_functor_parameters member in
  let annotation_items =
    match A.ModuleDeclaration.Member.module_type member with
    | Some node -> (
        match A.ModuleTypeExpr.cast node with
        | A.Node module_type -> items_of_module_type_expr module_type
        | A.Unknown _
        | A.Error _ -> items_of_node ~container:Signature_container node
      )
    | None -> []
  in
  let body_items =
    match A.ModuleDeclaration.Member.module_expr member with
    | Some node -> (
        match A.ModuleExpr.cast node with
        | A.Node module_expr -> items_of_module_expr_declaration_body module_expr
        | A.Unknown _
        | A.Error _ -> items_of_node ~container node
      )
    | None -> (
        match A.ModuleDeclaration.Member.module_type member with
        | Some node -> (
            match A.ModuleTypeExpr.cast node with
            | A.Node module_type -> items_of_module_type_expr module_type
            | A.Unknown _
            | A.Error _ -> items_of_node ~container:Signature_container node
          )
        | None -> []
      )
  in
  (prefix @ annotation_items) @ body_items

and module_item_with_prefix = fun name prefix item ->
  match prefix with
  | [] -> item
  | _ -> Item.Module { name; signature = []; body = prefix @ [ item ] }

and items_of_module_member = fun container member ->
  match A.ModuleDeclaration.Member.name member with
  | None ->
      A.ModuleDeclaration.Member.fold_child_node
        member
        ~init:[]
        ~fn:(fun node items -> A.Continue (prepend_all (items_of_node ~container node) items))
      |> List.reverse
  | Some ident -> (
      match ident_name ident with
      | None -> []
      | Some name ->
          let functor_args = functor_args_of_member member in
          let annotation_items =
            match A.ModuleDeclaration.Member.module_type member with
            | Some node -> (
                match A.ModuleTypeExpr.cast node with
                | A.Node module_type -> [ item_of_module_type_expr module_type ]
                | A.Unknown _
                | A.Error _ -> items_of_node ~container:Signature_container node
              )
            | None -> []
          in
          let make_module body = [ Item.Module { name; signature = annotation_items; body } ] in
          let make_functor body = [ Item.Functor { name; args = functor_args; body } ] in
          let make_declaration body =
            match functor_args with
            | [] -> make_module body
            | _ -> make_functor (annotation_items @ body)
          in
          match A.ModuleDeclaration.Member.module_expr member with
          | Some node -> (
              match A.ModuleExpr.cast node with
              | A.Node module_expr -> (
                  match (functor_args, A.ModuleExpr.view module_expr) with
                  | ([], A.ModuleExpr.Ident { ident }) ->
                      [ Item.ModuleAlias { name; target = Item.Use (ident_segments ident) }; ]
                  | ([], A.ModuleExpr.Functor { body }) ->
                      let (args, body) = functor_parts body ~body_kind:`ModuleExpr in
                      [ Item.Functor { name; args; body } ]
                  | ([], _) -> make_module (items_of_module_expr_declaration_body module_expr)
                  | (_, _) ->
                      make_functor
                        (annotation_items @ items_of_module_expr_declaration_body module_expr)
                )
              | A.Unknown _
              | A.Error _ -> make_declaration (items_of_node ~container:Structure_container node)
            )
          | None -> (
              match A.ModuleDeclaration.Member.module_type member with
              | Some node -> (
                  match A.ModuleTypeExpr.cast node with
                  | A.Node module_type -> make_declaration [ item_of_module_type_expr module_type ]
                  | A.Unknown _
                  | A.Error _ -> make_declaration []
                )
              | None -> make_declaration []
            )
    )

and item_of_module_expr = fun module_expr ->
  match A.ModuleExpr.view module_expr with
  | A.ModuleExpr.Ident { ident } -> Item.Use (ident_segments ident)
  | A.ModuleExpr.Structure _ -> Item.Scope (items_of_module_expr_body module_expr)
  | A.ModuleExpr.Constraint { expr; ascription; body } ->
      let expr =
        match expr with
        | Some expr -> item_of_module_expr expr
        | None -> Item.Scope (items_of_node ~container:Structure_container body)
      in
      let signature =
        match ascription with
        | Some module_type -> [ item_of_module_type_expr module_type ]
        | None -> []
      in
      Item.Constraint { expr; signature }
  | A.ModuleExpr.Apply { callee; argument; body } ->
      let fallback = Item.Scope (items_of_node ~container:Structure_container body) in
      let callee =
        match callee with
        | Some callee -> item_of_module_expr callee
        | None -> fallback
      in
      let argument =
        match argument with
        | Some argument -> item_of_module_expr argument
        | None -> fallback
      in
      Item.FunctorApply { callee; argument }
  | A.ModuleExpr.Functor { body } ->
      let (args, body) = functor_parts body ~body_kind:`ModuleExpr in
      bind_functor_scope args body
  | A.ModuleExpr.Opaque body
  | A.ModuleExpr.Error body
  | A.ModuleExpr.Unknown body -> Item.Scope (items_of_node ~container:Structure_container body)

and items_of_module_expr_declaration_body = fun module_expr ->
  match A.ModuleExpr.view module_expr with
  | A.ModuleExpr.Structure _ -> items_of_module_expr_body module_expr
  | _ -> [ item_of_module_expr module_expr ]

and items_of_module_expr_body = fun module_expr ->
  match A.ModuleExpr.view module_expr with
  | A.ModuleExpr.Ident { ident } -> ident_module_use ident
  | A.ModuleExpr.Structure _ ->
      A.ModuleExpr.fold_structure_item
        module_expr
        ~init:[]
        ~fn:(fun item items -> A.Continue (prepend_all (items_of_structure_item item) items))
      |> List.reverse
  | A.ModuleExpr.Constraint { expr; ascription; body } ->
      let expr_items =
        match expr with
        | Some expr -> items_of_module_expr_body expr
        | None -> items_of_node ~container:Structure_container body
      in
      let annotation_items =
        match ascription with
        | Some module_type -> items_of_module_type_expr module_type
        | None -> []
      in
      annotation_items @ expr_items
  | A.ModuleExpr.Apply { callee; argument; body } ->
      let _ = body in
      let callee_items =
        match callee with
        | Some callee -> items_of_module_expr_body callee
        | None -> []
      in
      let argument_items =
        match argument with
        | Some argument -> items_of_module_expr_body argument
        | None -> []
      in
      callee_items @ argument_items
  | A.ModuleExpr.Functor { body } ->
      let (args, body) = functor_parts body ~body_kind:`ModuleExpr in
      [ bind_functor_scope args body ]
  | A.ModuleExpr.Opaque body
  | A.ModuleExpr.Error body
  | A.ModuleExpr.Unknown body -> items_of_node ~container:Structure_container body

and item_of_module_type_expr = fun module_type ->
  match A.ModuleTypeExpr.view module_type with
  | A.ModuleTypeExpr.Ident { ident } -> Item.Use (ident_segments ident)
  | A.ModuleTypeExpr.Signature _ -> Item.Scope (items_of_module_type_body module_type)
  | A.ModuleTypeExpr.With { base; body; _ } ->
      let base =
        match base with
        | Some base -> item_of_module_type_expr base
        | None -> Item.Scope (items_of_node ~container:Signature_container body)
      in
      let constraints =
        A.Node.fold_child_node
          body
          ~init:[]
          ~fn:(fun child items ->
            match A.ModuleTypeConstraint.cast child with
            | A.Node constraint_ ->
                A.Continue (prepend_all (items_of_module_type_constraint constraint_) items)
            | A.Unknown _
            | A.Error _ -> A.Continue items)
        |> List.reverse
      in
      Item.WithConstraint { base; constraints }
  | A.ModuleTypeExpr.Typeof { body = Some body } -> Item.Typeof (item_of_module_expr body)
  | A.ModuleTypeExpr.Typeof { body = None } -> Item.Scope []
  | A.ModuleTypeExpr.Functor { body } ->
      let (args, body) = functor_parts body ~body_kind:`ModuleType in
      bind_functor_scope args body
  | A.ModuleTypeExpr.Error body
  | A.ModuleTypeExpr.Unknown body -> Item.Scope (items_of_node ~container:Signature_container body)

and items_of_module_type_body = fun module_type ->
  A.ModuleTypeExpr.fold_signature_item
    module_type
    ~init:[]
    ~fn:(fun item items -> A.Continue (prepend_all (items_of_signature_item item) items))
  |> List.reverse

and items_of_module_type_expr = fun module_type ->
  match A.ModuleTypeExpr.view module_type with
  | A.ModuleTypeExpr.Ident { ident } -> [ item_of_module_type_expr module_type ]
  | A.ModuleTypeExpr.Signature _ -> items_of_module_type_body module_type
  | A.ModuleTypeExpr.With { base; body; _ } -> (
      let base_items =
        match base with
        | Some base -> [ item_of_module_type_expr base ]
        | None -> []
      in
      let constraint_items =
        A.Node.fold_child_node
          body
          ~init:[]
          ~fn:(fun child items ->
            match A.ModuleTypeConstraint.cast child with
            | A.Node constraint_ ->
                A.Continue (prepend_all (items_of_module_type_constraint constraint_) items)
            | A.Unknown _
            | A.Error _ -> A.Continue items)
        |> List.reverse
      in
      match (base, constraint_items) with
      | (None, []) -> items_of_node ~container:Signature_container body
      | _ -> base_items @ constraint_items
    )
  | A.ModuleTypeExpr.Typeof { body = Some body } -> items_of_module_expr_body body
  | A.ModuleTypeExpr.Typeof { body = None } -> []
  | A.ModuleTypeExpr.Functor _ -> [ item_of_module_type_expr module_type ]
  | A.ModuleTypeExpr.Error body
  | A.ModuleTypeExpr.Unknown body -> items_of_node ~container:Signature_container body

and items_of_module_type_constraint = fun constraint_ ->
  match A.ModuleTypeConstraint.view constraint_ with
  | A.ModuleTypeConstraint.Type { body; _ } -> items_of_type_expr body
  | A.ModuleTypeConstraint.Module { body; _ } -> items_of_node ~container:Signature_container body
  | A.ModuleTypeConstraint.Unknown node -> items_of_node ~container:Signature_container node

and items_of_let_module_expr = fun let_module ->
  match A.LetModuleExpr.name let_module with
  | None -> (
      match A.LetModuleExpr.body let_module with
      | Some body -> items_of_expr body
      | None -> []
    )
  | Some ident -> (
      match ident_name ident with
      | None -> (
          match A.LetModuleExpr.body let_module with
          | Some body -> items_of_expr body
          | None -> []
        )
      | Some name ->
          let module_items =
            match A.LetModuleExpr.module_body_node let_module with
            | Some node -> (
                match A.ModuleExpr.cast node with
                | A.Node module_expr -> (
                    match A.ModuleExpr.view module_expr with
                    | A.ModuleExpr.Ident { ident } ->
                        [ Item.ModuleAlias { name; target = Item.Use (ident_segments ident) }; ]
                    | _ ->
                        [
                          Item.Module {
                            name;
                            signature = [];
                            body = items_of_module_expr_declaration_body module_expr;
                          };
                        ]
                  )
                | A.Unknown _
                | A.Error _ ->
                    [
                      Item.Module {
                        name;
                        signature = [];
                        body = items_of_node ~container:Structure_container node;
                      };
                    ]
              )
            | None -> (
                match A.LetModuleExpr.module_body_ident let_module with
                | Some ident ->
                    [ Item.ModuleAlias { name; target = Item.Use (ident_segments ident) }; ]
                | None -> [ Item.Module { name; signature = []; body = [] } ]
              )
          in
          let body_items =
            match A.LetModuleExpr.body let_module with
            | Some body -> items_of_expr body
            | None -> []
          in
          scoped_items (module_items @ body_items)
    )

and items_of_type_expr = fun type_expr ->
  match A.TypeExpr.view type_expr with
  | A.TypeExpr.Ident { ident } -> ident_parent_use ident
  | A.TypeExpr.Apply { ident; args } ->
      ident_parent_use ident @ vector_items args ~fn:items_of_type_expr
  | A.TypeExpr.Arrow { arg; ret; _ } -> items_of_type_expr arg @ items_of_type_expr ret
  | A.TypeExpr.Forall { body; _ } -> items_of_type_expr body
  | A.TypeExpr.Alias { typ; _ } -> items_of_type_expr typ
  | A.TypeExpr.Tuple { parts } -> vector_items parts ~fn:items_of_type_expr
  | A.TypeExpr.Var _
  | A.TypeExpr.Wildcard -> []
  | A.TypeExpr.Error node
  | A.TypeExpr.Unknown node -> items_of_type_expr_node node

and items_of_type_expr_node = fun node ->
  match A.VariantType.cast node with
  | A.Node variant_type -> items_of_variant_type variant_type
  | A.Unknown _
  | A.Error _ -> (
      match A.RecordType.cast node with
      | A.Node record_type -> items_of_record_type record_type
      | A.Unknown _
      | A.Error _ -> items_of_node ~container:Signature_container node
    )

and items_of_record_type = fun record_type ->
  A.RecordType.fold_field
    record_type
    ~init:[]
    ~fn:(fun field items ->
      match A.RecordField.view field with
      | A.RecordField.Field { annotation; _ } ->
          A.Continue (prepend_all (items_of_type_expr annotation) items)
      | A.RecordField.Unknown node ->
          A.Continue (prepend_all (items_of_node ~container:Signature_container node) items))
  |> List.reverse

and items_of_variant_payload = fun __tmp1 ->
  match __tmp1 with
  | A.VariantConstructor.TypeExpr type_expr -> items_of_type_expr type_expr
  | A.VariantConstructor.Record record_type -> items_of_record_type record_type

and items_of_variant_constructor = fun constructor ->
  match A.VariantConstructor.view constructor with
  | A.VariantConstructor.Constructor { rhs; _ } -> (
      match rhs with
      | A.VariantConstructor.Plain -> []
      | A.VariantConstructor.Payload { payload; _ } -> items_of_variant_payload payload
      | A.VariantConstructor.Gadt { record_payload; result; _ } ->
          (
            match record_payload with
            | Some record_type -> items_of_record_type record_type
            | None -> []
          ) @ items_of_type_expr result
    )
  | A.VariantConstructor.Unknown node -> items_of_node ~container:Signature_container node

and items_of_variant_type = fun variant_type ->
  let inherited_items =
    A.VariantType.fold_inherited_type
      variant_type
      ~init:[]
      ~fn:(fun inherited items -> A.Continue (prepend_all (items_of_type_expr inherited) items))
    |> List.reverse
  in
  let constructor_items =
    A.VariantType.fold_constructor
      variant_type
      ~init:[]
      ~fn:(fun constructor items ->
        A.Continue (prepend_all (items_of_variant_constructor constructor) items))
    |> List.reverse
  in
  inherited_items @ constructor_items

and items_of_pattern = fun pattern ->
  match A.cast_result_to_option (A.LocalOpenPattern.cast pattern) with
  | Some local_open -> (
      match A.LocalOpenPattern.view local_open with
      | A.LocalOpenPattern.Delimited { module_ident; pattern; _ } ->
          [ Item.Scope (ident_module_open module_ident @ items_of_pattern pattern); ]
      | A.LocalOpenPattern.Unknown node -> items_of_node ~container:Structure_container node
    )
  | None -> (
      match A.Pattern.view pattern with
      | A.Pattern.Unit
      | A.Pattern.Wildcard
      | A.Pattern.Literal _ -> []
      | A.Pattern.Ident { ident } -> ident_parent_use ident
      | A.Pattern.Constructor { constructor; payload } ->
          ident_parent_use constructor @ (
            match payload with
            | Some payload -> items_of_pattern payload
            | None -> []
          )
      | A.Pattern.Tuple { parts }
      | A.Pattern.List { items = parts }
      | A.Pattern.Array { items = parts } -> vector_items parts ~fn:items_of_pattern
      | A.Pattern.Record { fields; _ } ->
          vector_items
            fields
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | A.RecordPatternField { ident; pattern; _ } ->
                  ident_parent_use ident @ (
                    match pattern with
                    | Some pattern -> items_of_pattern pattern
                    | None -> []
                  )
              | A.UnknownRecordPatternField { node } -> items_of_pattern node)
      | A.Pattern.PolyVariant { payload; _ } -> (
          match payload with
          | Some payload -> items_of_pattern payload
          | None -> []
        )
      | A.Pattern.FirstClassModule _ -> []
      | A.Pattern.Interval { left; right }
      | A.Pattern.Or { left; right }
      | A.Pattern.Cons { head = left; tail = right } ->
          items_of_pattern left @ items_of_pattern right
      | A.Pattern.Constraint { pattern; annotation } ->
          items_of_pattern pattern @ items_of_type_expr annotation
      | A.Pattern.Alias { pattern; alias } -> items_of_pattern pattern @ items_of_pattern alias
      | A.Pattern.Lazy { pattern }
      | A.Pattern.Exception { pattern } -> items_of_pattern pattern
      | A.Pattern.Error node
      | A.Pattern.Unknown node -> items_of_node ~container:Structure_container node
    )

and bound_modules_of_pattern = fun pattern ->
  match A.cast_result_to_option (A.LocalOpenPattern.cast pattern) with
  | Some local_open -> (
      match A.LocalOpenPattern.view local_open with
      | A.LocalOpenPattern.Delimited { pattern; _ } -> bound_modules_of_pattern pattern
      | A.LocalOpenPattern.Unknown _ -> []
    )
  | None -> (
      match A.Pattern.view pattern with
      | A.Pattern.FirstClassModule { binder; ascription_ident; _ } -> (
          match ident_name binder with
          | Some name when is_module_head name ->
              let ascription =
                match ascription_ident with
                | Some ident -> ident_module_use ident
                | None -> []
              in
              [ { Item.name = name; ascription } ]
          | Some _
          | None -> []
        )
      | A.Pattern.Tuple { parts }
      | A.Pattern.List { items = parts }
      | A.Pattern.Array { items = parts } -> vector_items parts ~fn:bound_modules_of_pattern
      | A.Pattern.Record { fields; _ } ->
          vector_items
            fields
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | A.RecordPatternField { pattern = Some pattern; _ } ->
                  bound_modules_of_pattern pattern
              | A.RecordPatternField { pattern = None; _ }
              | A.UnknownRecordPatternField _ -> [])
      | A.Pattern.Constructor { payload = Some payload; _ }
      | A.Pattern.PolyVariant { payload = Some payload; _ }
      | A.Pattern.Constraint { pattern = payload; _ }
      | A.Pattern.Alias { pattern = payload; _ }
      | A.Pattern.Lazy { pattern = payload }
      | A.Pattern.Exception { pattern = payload } -> bound_modules_of_pattern payload
      | A.Pattern.Interval { left; right }
      | A.Pattern.Or { left; right }
      | A.Pattern.Cons { head = left; tail = right } ->
          bound_modules_of_pattern left @ bound_modules_of_pattern right
      | A.Pattern.Unit
      | A.Pattern.Wildcard
      | A.Pattern.Literal _
      | A.Pattern.Ident _
      | A.Pattern.Constructor { payload = None; _ }
      | A.Pattern.PolyVariant { payload = None; _ }
      | A.Pattern.Error _
      | A.Pattern.Unknown _ -> []
    )

and items_of_parameter = fun parameter ->
  match A.Parameter.view parameter with
  | A.Parameter.Param { label; pattern } ->
      let label_items =
        match label with
        | A.Parameter.Optional { default = Some default; _ } -> items_of_expr default
        | A.Parameter.NoLabel
        | A.Parameter.Labeled _
        | A.Parameter.Optional { default = None; _ } -> []
      in
      label_items @ (
        match pattern with
        | Some pattern -> items_of_pattern pattern
        | None -> []
      )
  | A.Parameter.Unknown node -> items_of_node ~container:Structure_container node

and bound_modules_of_parameter = fun parameter ->
  match A.Parameter.view parameter with
  | A.Parameter.Param { pattern = Some pattern; _ } -> bound_modules_of_pattern pattern
  | A.Parameter.Param { pattern = None; _ }
  | A.Parameter.Unknown _ -> []

and items_of_parameters = fun parameters -> vector_items parameters ~fn:items_of_parameter

and bound_modules_of_parameters = fun parameters ->
  vector_items
    parameters
    ~fn:bound_modules_of_parameter

and items_of_match_case = fun match_case ->
  match A.MatchCase.view match_case with
  | A.MatchCase.Case { pattern; guard; body } ->
      let scope =
        (
          items_of_pattern pattern @ (
            match guard with
            | Some guard -> items_of_expr guard
            | None -> []
          )
        ) @ items_of_expr body
      in
      let modules = bound_modules_of_pattern pattern in
      bind_modules_items modules scope
  | A.MatchCase.Unknown node -> items_of_node ~container:Structure_container node

and items_of_match_cases = fun expr ->
  A.Expr.fold_match_case
    expr
    ~init:[]
    ~fn:(fun match_case items -> A.Continue (prepend_all (items_of_match_case match_case) items))
  |> List.reverse

and items_of_fun_body = fun expr body ->
  match body with
  | A.Expr.Body_expr body -> items_of_expr body
  | A.Expr.Body_cases _ -> items_of_match_cases expr

and items_of_let_binding = fun binding ->
  match A.LetBinding.view binding with
  | A.LetBinding.Binding { pattern; body } ->
      let parameter_items =
        A.LetBinding.fold_parameter
          binding
          ~init:[]
          ~fn:(fun parameter items -> A.Continue (prepend_all (items_of_parameter parameter) items))
        |> List.reverse
      in
      let annotation_items =
        (
          match A.LetBinding.type_annotation binding with
          | Some annotation -> items_of_type_expr annotation
          | None -> []
        ) @ (
          match A.LetBinding.return_type_annotation binding with
          | Some annotation -> items_of_type_expr annotation
          | None -> []
        )
      in
      annotation_items
      @ scoped_items ((items_of_pattern pattern @ parameter_items) @ items_of_expr body)
  | A.LetBinding.Unknown node -> items_of_node ~container:Structure_container node

and items_of_expr = fun expr ->
  match A.Expr.view expr with
  | A.Expr.Fun { parameters; return_annotation; body } ->
      let scope =
        (
          items_of_parameters parameters @ (
            match return_annotation with
            | Some annotation -> items_of_type_expr annotation
            | None -> []
          )
        ) @ items_of_fun_body expr body
      in
      let modules = bound_modules_of_parameters parameters in
      bind_modules_items modules scope
  | A.Expr.Match { scrutinee; _ } -> items_of_expr scrutinee @ items_of_match_cases expr
  | A.Expr.Try { body; _ } -> items_of_expr body @ items_of_match_cases expr
  | A.Expr.Sequence { left; right } ->
      items_of_expr left @ (
        match right with
        | Some right -> items_of_expr right
        | None -> []
      )
  | A.Expr.For {
      pattern;
      start_;
      stop;
      body;
    } ->
      items_of_expr start_
      @ items_of_expr stop
      @ scoped_items (items_of_pattern pattern @ items_of_expr body)
  | _ -> items_of_node ~container:Structure_container (A.Expr.as_node expr)

and enter_structure_item = fun visitor item ->
  let node = A.StructureItem.as_node item in
  continue (with_visitor_ctx visitor (fun ctx -> push_container ctx node Structure_container))

and enter_signature_item = fun visitor item ->
  let node = A.SignatureItem.as_node item in
  continue (with_visitor_ctx visitor (fun ctx -> push_container ctx node Signature_container))

and enter_module_declaration = fun visitor decl ->
  skip
    (with_visitor_ctx
      visitor
      (fun ctx -> add_items ctx (items_of_module_declaration ctx.container decl)))

and enter_module_type_declaration = fun visitor decl ->
  skip
    (with_visitor_ctx visitor (fun ctx -> add_items ctx (items_of_module_type_declaration decl)))

and enter_open_declaration = fun visitor decl ->
  skip
    (with_visitor_ctx visitor (fun ctx -> add_items ctx (items_of_open_declaration decl)))

and enter_include_declaration = fun visitor decl ->
  skip
    (
      with_visitor_ctx
        visitor
        (fun ctx ->
          let mode =
            match ctx.container with
            | Structure_container -> Item.Structure
            | Signature_container -> Item.Signature
          in
          add_items ctx (items_of_include_declaration mode decl))
    )

and enter_let_binding = fun visitor binding ->
  skip
    (with_visitor_ctx visitor (fun ctx -> add_items ctx (items_of_let_binding binding)))

and enter_expr = fun visitor expr ->
  let visitor =
    with_visitor_ctx
      visitor
      (fun ctx ->
        match A.Expr.view expr with
        | A.Expr.Ident { ident } -> add_items ctx (ident_parent_use ident)
        | A.Expr.Constructor { constructor; _ } -> add_items ctx (ident_parent_use constructor)
        | A.Expr.FieldAccess { field; _ } -> add_items ctx (ident_parent_use field)
        | A.Expr.Record { fields; _ } ->
            Vector.to_array fields
            |> Array.fold_left
              ~init:ctx
              ~fn:(fun ctx __tmp1 ->
                match __tmp1 with
                | A.RecordExprField { ident; _ } -> add_items ctx (ident_parent_use ident)
                | A.UnknownRecordExprField _ -> ctx)
        | _ -> ctx)
  in
  let visitor =
    match A.cast_result_to_option (A.FirstClassModuleExpr.cast expr) with
    | Some first_class ->
        with_visitor_ctx
          visitor
          (fun ctx ->
            let ctx =
              match A.FirstClassModuleExpr.module_ident first_class with
              | Some ident -> add_items ctx (ident_module_use ident)
              | None -> ctx
            in
            match A.FirstClassModuleExpr.ascription_ident first_class with
            | Some ident -> add_items ctx (ident_parent_use ident)
            | None -> ctx)
    | None -> visitor
  in
  match A.Expr.view expr with
  | A.Expr.LocalOpen _ -> (
      match A.cast_result_to_option (A.LocalOpenExpr.cast expr) with
      | Some local_open -> (
          match A.LocalOpenExpr.view local_open with
          | A.LocalOpenExpr.LetOpen { module_ident; body; _ }
          | A.LocalOpenExpr.Delimited { module_ident; body; _ } ->
              skip
                (with_visitor_ctx
                  visitor
                  (fun ctx ->
                    add_item
                      ctx
                      (Item.Scope (ident_module_open module_ident @ items_of_expr body))))
          | A.LocalOpenExpr.Unknown _ -> continue visitor
        )
      | None -> continue visitor
    )
  | A.Expr.LetModule _ -> (
      match A.cast_result_to_option (A.LetModuleExpr.cast expr) with
      | Some let_module ->
          skip
            (with_visitor_ctx
              visitor
              (fun ctx -> add_items ctx (items_of_let_module_expr let_module)))
      | None -> continue visitor
    )
  | A.Expr.Fun _
  | A.Expr.Match _
  | A.Expr.Try _
  | A.Expr.For _ -> skip (with_visitor_ctx visitor (fun ctx -> add_items ctx (items_of_expr expr)))
  | _ -> continue visitor

and enter_pattern = fun visitor pattern ->
  let visitor =
    with_visitor_ctx
      visitor
      (fun ctx ->
        match A.Pattern.view pattern with
        | A.Pattern.Constructor { constructor; _ }
        | A.Pattern.Ident { ident = constructor } -> add_items ctx (ident_parent_use constructor)
        | A.Pattern.Record { fields; _ } ->
            Vector.to_array fields
            |> Array.fold_left
              ~init:ctx
              ~fn:(fun ctx __tmp1 ->
                match __tmp1 with
                | A.RecordPatternField { ident; _ } -> add_items ctx (ident_parent_use ident)
                | A.UnknownRecordPatternField _ -> ctx)
        | _ -> ctx)
  in
  match A.cast_result_to_option (A.LocalOpenPattern.cast pattern) with
  | Some local_open -> (
      match A.LocalOpenPattern.view local_open with
      | A.LocalOpenPattern.Delimited { module_ident; _ } ->
          continue
            (with_visitor_ctx visitor (fun ctx -> add_items ctx (ident_module_open module_ident)))
      | A.LocalOpenPattern.Unknown _ -> continue visitor
    )
  | None -> continue visitor

and enter_type_expr = fun visitor type_expr ->
  continue
    (
      with_visitor_ctx
        visitor
        (fun ctx ->
          match A.TypeExpr.view type_expr with
          | A.TypeExpr.Ident { ident }
          | A.TypeExpr.Apply { ident; _ } -> add_items ctx (ident_parent_use ident)
          | _ -> ctx)
    )

and enter_node = fun visitor node ->
  let kind = A.Node.kind node in
  if is_module_expr_kind kind then
    match A.cast_result_to_option (A.ModuleExpr.cast node) with
    | Some module_expr -> (
        match A.ModuleExpr.view module_expr with
        | A.ModuleExpr.Ident { ident } ->
            skip (with_visitor_ctx visitor (fun ctx -> add_items ctx (ident_module_use ident)))
        | _ -> continue visitor
      )
    | None -> continue visitor
  else if is_module_type_kind kind then (
    match A.cast_result_to_option (A.ModuleTypeExpr.cast node) with
    | Some module_type -> (
        match A.ModuleTypeExpr.view module_type with
        | A.ModuleTypeExpr.Error _
        | A.ModuleTypeExpr.Unknown _ -> continue visitor
        | _ ->
            skip
              (with_visitor_ctx
                visitor
                (fun ctx -> add_items ctx (items_of_module_type_expr module_type)))
      )
    | None -> continue visitor
  ) else
    continue visitor

and leave_node = fun visitor node ->
  with_visitor_ctx
    visitor
    (fun ctx -> restore_container ctx node)

and items_of_node = fun ~container node ->
  let hooks = {
    Visitor.empty_hooks with
    enter_node = Some enter_node;
    leave_node = Some leave_node;
    enter_structure_item = Some enter_structure_item;
    enter_signature_item = Some enter_signature_item;
    enter_module_declaration = Some enter_module_declaration;
    enter_module_type_declaration = Some enter_module_type_declaration;
    enter_open_declaration = Some enter_open_declaration;
    enter_include_declaration = Some enter_include_declaration;
    enter_let_binding = Some enter_let_binding;
    enter_expr = Some enter_expr;
    enter_pattern = Some enter_pattern;
    enter_type_expr = Some enter_type_expr;
  }
  in
  let visitor = Visitor.make ~ctx:{ empty_collect_ctx with container } ~hooks in
  let visitor = Visitor.visit_node visitor node in
  Visitor.ctx visitor
  |> fun ctx -> List.reverse ctx.items

let analyze = fun ?(implicit_opens = []) ?module_path ~source ~source_hash result ->
  if Int.(Vector.length result.Syn.Parser.diagnostics != 0) then
    Error (
      Parse_diagnostics (
        Vector.iter result.Syn.Parser.diagnostics
        |> Iter.Iterator.to_list
      )
    )
  else
    let source_file = A.SourceFile.make result.Syn.Parser.tree in
    let (kind, body_items) = items_of_source_file source_file in
    let implicit_open_items =
      List.fold_left
        implicit_opens
        ~init:[]
        ~fn:(fun items module_path -> prepend_all (implicit_module_open module_path) items)
      |> List.reverse
    in
    Ok {
      source;
      source_hash;
      module_path;
      kind;
      items = implicit_open_items @ body_items;
    }

let collect = analyze
