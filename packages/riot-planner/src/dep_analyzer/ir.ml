open Std
open Std.Collections

module A = Syn.Ast
module Ser = Serde.Ser

let ser_list = fun encode -> Ser.contramap Vector.from_list (Ser.list encode)

module Item = struct
  module Ident = struct
    module Slice = IO.IoVec.IoSlice

    type segment =
      | Text of string
      | Slice of {
          source: Slice.t;
          start: int;
          len: int;
        }

    type t = segment list

    let is_uppercase_ascii = fun __tmp1 ->
      match __tmp1 with
      | 'A' .. 'Z' -> true
      | _ -> false

    let of_string = fun value -> Text value

    let of_strings = fun values -> List.map values ~fn:of_string

    let of_token = fun (token: A.Token.t) ->
      let start = A.Token.span_start token in
      let end_ = A.Token.span_end token in
      Slice {
        source = token.A.tree.Syn.SyntaxTree.source;
        start;
        len = end_ - start;
      }

    let segment_length = fun __tmp1 ->
      match __tmp1 with
      | Text text -> String.length text
      | Slice { len; _ } -> len

    let segment_get_unchecked = fun segment ~at ->
      match segment with
      | Text text -> String.get_unchecked text ~at
      | Slice { source; start; _ } -> Slice.get_unchecked source ~at:(start + at)

    let token_is_module_head = fun (token: A.Token.t) ->
      let start = A.Token.span_start token in
      let end_ = A.Token.span_end token in
      start != end_
      && is_uppercase_ascii (Slice.get_unchecked token.A.tree.Syn.SyntaxTree.source ~at:start)

    let segment_is_module_head = fun segment ->
      segment_length segment != 0 && is_uppercase_ascii (segment_get_unchecked segment ~at:0)

    let is_module_path = fun __tmp1 ->
      match __tmp1 with
      | head :: _ -> segment_is_module_head head
      | [] -> false

    let length = List.length

    let segment_to_string = fun segment ->
      match segment with
      | Text text -> text
      | Slice { source; start; len } ->
          String.init ~len ~fn:(fun index -> Slice.get_unchecked source ~at:(start + index))

    let to_strings = fun ident -> List.map ident ~fn:segment_to_string
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
    | ImplicitOpen of t
    | Include of include_mode * t
    | Module of {
        name: string;
        signature: t list;
        body: t list;
      }
    | ModuleAlias of {
        name: string;
        target: t;
      }
    | Functor of {
        name: string;
        args: functor_arg list;
        body: t list;
      }
    | ModuleType of {
        name: string;
        body: t list;
      }
    | FunctorApply of {
        callee: t;
        argument: t;
      }
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

  type module_alias_payload = {
    module_alias_name: string;
    module_alias_target: t;
  }

  type functor_payload = {
    functor_name: string;
    functor_args: functor_arg list;
    functor_body: t list;
  }

  type module_type_payload = {
    module_type_name: string;
    module_type_body: t list;
  }

  type functor_apply_payload = {
    functor_apply_callee: t;
    functor_apply_argument: t;
  }

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
    Ser.variant [
      Ser.Variant.unit "Structure" (fun __tmp1 ->
        match __tmp1 with
        | Structure -> true
        | Signature -> false);
      Ser.Variant.unit "Signature" (fun __tmp1 ->
        match __tmp1 with
        | Signature -> true
        | Structure -> false);
    ]

  let ident_serializer = Ser.contramap Ident.to_strings (ser_list Ser.string)

  let rec serializer =
    {
      Ser.run = (fun backend state item ->
        let item_list_serializer = ser_list serializer in
        let functor_arg_serializer =
          Ser.record
            (
              Ser.fields [
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
              Ser.fields [
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
              Ser.fields [
                Ser.field
                  "mode"
                  include_mode_serializer
                  (fun (payload: include_payload) -> payload.include_mode);
                Ser.field "expr" serializer (fun (payload: include_payload) -> payload.include_expr);
              ]
            )
        in
        let module_payload_serializer =
          Ser.record
            (
              Ser.fields [
                Ser.field "name" Ser.string (fun (payload: module_payload) -> payload.module_name);
                Ser.field
                  "signature"
                  item_list_serializer
                  (fun (payload: module_payload) -> payload.module_signature);
                Ser.field "body" item_list_serializer (fun (payload: module_payload) -> payload.module_body);
              ]
            )
        in
        let module_alias_payload_serializer =
          Ser.record
            (
              Ser.fields [
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
              Ser.fields [
                Ser.field "name" Ser.string (fun (payload: functor_payload) -> payload.functor_name);
                Ser.field
                  "args"
                  (ser_list functor_arg_serializer)
                  (fun (payload: functor_payload) -> payload.functor_args);
                Ser.field "body" item_list_serializer (fun (payload: functor_payload) -> payload.functor_body);
              ]
            )
        in
        let module_type_payload_serializer =
          Ser.record
            (
              Ser.fields [
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
              Ser.fields [
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
              Ser.fields [
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
              Ser.fields [
                Ser.field "base" serializer (fun (payload: with_constraint_payload) -> payload.with_base);
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
              Ser.fields [
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
          Ser.variant [
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
              "ImplicitOpen"
              serializer
              (fun __tmp1 ->
                match __tmp1 with
                | ImplicitOpen expr -> Some expr
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
                    Some {
                      module_name = name;
                      module_signature = signature;
                      module_body = body;
                    }
                | _ -> None);
            Ser.Variant.newtype
              "ModuleAlias"
              module_alias_payload_serializer
              (fun __tmp1 ->
                match __tmp1 with
                | ModuleAlias { name; target } ->
                    Some {
                      module_alias_name = name;
                      module_alias_target = target;
                    }
                | _ -> None);
            Ser.Variant.newtype
              "Functor"
              functor_payload_serializer
              (fun __tmp1 ->
                match __tmp1 with
                | Functor { name; args; body } ->
                    Some {
                      functor_name = name;
                      functor_args = args;
                      functor_body = body;
                    }
                | _ -> None);
            Ser.Variant.newtype
              "ModuleType"
              module_type_payload_serializer
              (fun __tmp1 ->
                match __tmp1 with
                | ModuleType { name; body } ->
                    Some {
                      module_type_name = name;
                      module_type_body = body;
                    }
                | _ -> None);
            Ser.Variant.newtype
              "FunctorApply"
              functor_apply_payload_serializer
              (fun __tmp1 ->
                match __tmp1 with
                | FunctorApply { callee; argument } ->
                    Some {
                      functor_apply_callee = callee;
                      functor_apply_argument = argument;
                    }
                | _ -> None);
            Ser.Variant.newtype
              "Constraint"
              constraint_payload_serializer
              (fun __tmp1 ->
                match __tmp1 with
                | Constraint { expr; signature } ->
                    Some {
                      constraint_expr = expr;
                      constraint_signature = signature;
                    }
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
                    Some {
                      with_base = base;
                      with_constraints = constraints;
                    }
                | _ -> None);
            Ser.Variant.newtype
              "BindModules"
              bind_modules_payload_serializer
              (fun __tmp1 ->
                match __tmp1 with
                | BindModules { modules; scope } ->
                    Some {
                      bind_modules_modules = modules;
                      bind_modules_scope = scope;
                    }
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
  Ser.variant [
    Ser.Variant.unit "Implementation" (fun __tmp1 ->
      match __tmp1 with
      | Implementation -> true
      | Interface -> false);
    Ser.Variant.unit "Interface" (fun __tmp1 ->
      match __tmp1 with
      | Interface -> true
      | Implementation -> false);
  ]

let source_summary_serializer =
  Ser.record
    (
      Ser.fields [
        Ser.field "source" (Ser.contramap Path.to_string Ser.string) (fun (summary: source_summary) -> summary.source);
        Ser.field
          "source_hash"
          (Ser.contramap Crypto.Digest.hex Ser.string)
          (fun (summary: source_summary) -> summary.source_hash);
        Ser.field
          "module_path"
          (Ser.option (ser_list Ser.string))
          (fun (summary: source_summary) -> summary.module_path);
        Ser.field "kind" source_kind_serializer (fun (summary: source_summary) -> summary.kind);
        Ser.field "items" (ser_list Item.serializer) (fun (summary: source_summary) -> summary.items);
      ]
    )

let is_uppercase_ascii = fun __tmp1 ->
  match __tmp1 with
  | 'A' .. 'Z' -> true
  | _ -> false

let is_module_head = fun segment ->
  String.length segment != 0 && is_uppercase_ascii (String.get_unchecked segment ~at:0)

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

let rec ident_segments_reversed = fun ident acc ->
  match ident with
  | A.Ident.Bare token -> Item.Ident.of_token token :: acc
  | A.Ident.Qualified (token, rest) ->
      ident_segments_reversed rest (Item.Ident.of_token token :: acc)

let ident_segments = fun ident ->
  ident_segments_reversed ident []
  |> List.reverse

let ident_segments_if_module_head = fun ident ->
  match A.Ident.first_segment ident with
  | Some token when Item.Ident.token_is_module_head token -> Some (ident_segments ident)
  | Some _
  | None -> None

let ident_parent_segments = fun ident ->
  match A.Ident.first_segment ident with
  | Some token when Item.Ident.token_is_module_head token -> (
      let reversed = ident_segments_reversed ident [] in
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

let item_of_module_type_ident = fun ident ->
  match ident_parent_use ident with
  | [ item ] -> item
  | [] -> Item.Scope []
  | items -> Item.Scope items

let syntax_node = fun (node: A.Node.t) -> Syn.SyntaxTree.node node.A.tree node.A.id

let child_count = fun node ->
  let syntax = syntax_node node in
  syntax.Syn.SyntaxTree.child_count

let child_at_unchecked = fun (node: A.Node.t) (syntax: Syn.SyntaxTree.node) index ->
  Syn.SyntaxTree.child node.A.tree (syntax.Syn.SyntaxTree.first_child + index)

let direct_child_nodes = fun node ~matches ~init ~fn ->
  let syntax = syntax_node node in
  let count = syntax.Syn.SyntaxTree.child_count in
  let rec loop index acc =
    if index = count then
      acc
    else
      match child_at_unchecked node syntax index with
      | Syn.SyntaxTree.Node id ->
          let child_syntax = Syn.SyntaxTree.node node.A.tree id in
          let acc =
            if matches child_syntax.Syn.SyntaxTree.kind then
              fn ({ tree = node.A.tree; id }: A.Node.t) acc
            else
              acc
          in
          loop (index + 1) acc
      | Syn.SyntaxTree.Token _
      | Syn.SyntaxTree.Missing _ -> loop (index + 1) acc
  in
  loop 0 init

let path_segments = fun node ->
  let syntax = syntax_node node in
  let count = syntax.Syn.SyntaxTree.child_count in
  let rec loop index segments =
    if index = count then
      List.reverse segments
    else
      match child_at_unchecked node syntax index with
      | Syn.SyntaxTree.Token id ->
          let token = ({ tree = node.A.tree; id }: A.Token.t) in
          if Syn.SyntaxKind.(A.Token.kind token = IDENT) then
            loop (index + 1) (Item.Ident.of_token token :: segments)
          else
            loop (index + 1) segments
      | Syn.SyntaxTree.Node _
      | Syn.SyntaxTree.Missing _ -> loop (index + 1) segments
  in
  loop 0 []

let path_segments_reversed = fun node ->
  let syntax = syntax_node node in
  let count = syntax.Syn.SyntaxTree.child_count in
  let rec loop index segments =
    if index = count then
      segments
    else
      match child_at_unchecked node syntax index with
      | Syn.SyntaxTree.Token id ->
          let token = ({ tree = node.A.tree; id }: A.Token.t) in
          if Syn.SyntaxKind.(A.Token.kind token = IDENT) then
            loop (index + 1) (Item.Ident.of_token token :: segments)
          else
            loop (index + 1) segments
      | Syn.SyntaxTree.Node _
      | Syn.SyntaxTree.Missing _ -> loop (index + 1) segments
  in
  loop 0 []

let path_module_use = fun segments ->
  match segments with
  | _ when Item.Ident.is_module_path segments -> [ Item.Use segments ]
  | _ -> []

let path_parent_use = fun segments ->
  match drop_last segments with
  | parent when Item.Ident.is_module_path parent -> [ Item.Use parent ]
  | _ -> []

let path_module_use_node = fun node ->
  match path_segments_reversed node with
  | [] -> []
  | reversed ->
      let segments = List.reverse reversed in
      (
        match segments with
        | _ when Item.Ident.is_module_path segments -> [ Item.Use segments ]
        | _ -> []
      )

let path_parent_use_node = fun node ->
  match path_segments_reversed node with
  | []
  | [ _ ] -> []
  | _last :: parent_reversed ->
      let parent = List.reverse parent_reversed in
      (
        match parent with
        | _ when Item.Ident.is_module_path parent -> [ Item.Use parent ]
        | _ -> []
      )

let ident_module_open = fun ident ->
  match ident_segments_if_module_head ident with
  | Some segments -> [ Item.Open (Item.Use segments) ]
  | None -> []

let implicit_module_open = fun segments ->
  match segments with
  | head :: _ when is_module_head head ->
      [ Item.ImplicitOpen (Item.Use (Item.Ident.of_strings segments)) ]
  | _ -> []

let ident_module_include = fun mode ident ->
  match mode with
  | Item.Structure -> (
      match ident_segments_if_module_head ident with
      | Some segments -> [ Item.Include (mode, Item.Use segments) ]
      | None -> []
    )
  | Item.Signature -> (
      match ident_segments_if_module_head ident with
      | Some _ -> [ Item.Include (mode, Item.Scope (ident_parent_use ident)) ]
      | None -> []
    )

let rec item_is_pure_use = fun __tmp1 ->
  match __tmp1 with
  | Item.Use _ -> true
  | Item.Scope items -> List.all items ~fn:item_is_pure_use
  | _ -> false

let rec prepend_pure_uses = fun item acc ->
  match item with
  | Item.Use _ -> item :: acc
  | Item.Scope items ->
      List.fold_left items ~init:acc ~fn:(fun acc item -> prepend_pure_uses item acc)
  | _ -> acc

let flatten_pure_uses = fun items ->
  List.fold_left items ~init:[] ~fn:(fun acc item -> prepend_pure_uses item acc)
  |> List.reverse

let prepend_all = fun values acc ->
  List.fold_left values ~init:acc ~fn:(fun acc value -> value :: acc)

let singleton_item = fun item ->
  match item with
  | Item.Scope [] -> []
  | _ -> [ item ]

let scoped_items = fun items ->
  match items with
  | [] -> []
  | _ when List.all items ~fn:item_is_pure_use -> flatten_pure_uses items
  | _ -> [ Item.Scope items ]

let vector_items = fun vector ~fn ->
  let items = ref [] in
  Vector.for_each
    vector
    ~fn:(fun item -> items := prepend_all (fn item) !items);
  List.reverse !items

type container =
  | Structure_container
  | Signature_container

let first_module_expr = fun node ->
  let syntax = syntax_node node in
  let count = syntax.Syn.SyntaxTree.child_count in
  let rec loop index =
    if index = count then
      None
    else
      match child_at_unchecked node syntax index with
      | Syn.SyntaxTree.Node id -> (
          let child = ({ tree = node.A.tree; id }: A.Node.t) in
          match A.ModuleExpr.cast child with
          | A.Node module_expr -> Some module_expr
          | A.Unknown _
          | A.Error _ -> loop (index + 1)
        )
      | Syn.SyntaxTree.Token _
      | Syn.SyntaxTree.Missing _ -> loop (index + 1)
  in
  loop 0

let first_module_type_expr = fun node ->
  let syntax = syntax_node node in
  let count = syntax.Syn.SyntaxTree.child_count in
  let rec loop index =
    if index = count then
      None
    else
      match child_at_unchecked node syntax index with
      | Syn.SyntaxTree.Node id -> (
          let child = ({ tree = node.A.tree; id }: A.Node.t) in
          match A.ModuleTypeExpr.cast child with
          | A.Node module_type -> Some module_type
          | A.Unknown _
          | A.Error _ -> loop (index + 1)
        )
      | Syn.SyntaxTree.Token _
      | Syn.SyntaxTree.Missing _ -> loop (index + 1)
  in
  loop 0

let child_token_at = fun node index ->
  let syntax = syntax_node node in
  match child_at_unchecked node syntax index with
  | Syn.SyntaxTree.Token id -> Some ({ tree = node.A.tree; id }: A.Token.t)
  | Syn.SyntaxTree.Node _
  | Syn.SyntaxTree.Missing _ -> None

let child_node_at = fun node index ->
  let syntax = syntax_node node in
  match child_at_unchecked node syntax index with
  | Syn.SyntaxTree.Node id -> Some ({ tree = node.A.tree; id }: A.Node.t)
  | Syn.SyntaxTree.Token _
  | Syn.SyntaxTree.Missing _ -> None

let child_token_kind_is = fun node index kind ->
  match child_token_at node index with
  | Some token -> Syn.SyntaxKind.(A.Token.kind token = kind)
  | None -> false

let find_token = fun node start stop kind ->
  let rec loop index =
    if index = stop then
      None
    else if child_token_kind_is node index kind then
      Some index
    else
      loop (index + 1)
  in
  loop start

let direct_path_between = fun node start stop ->
  let rec loop index expect_ident acc =
    if index = stop then
      if not (List.is_empty acc) && not expect_ident then
        Some (List.reverse acc)
      else
        None
    else
      match child_token_at node index with
      | Some token when expect_ident && Syn.SyntaxKind.(A.Token.kind token = IDENT) ->
          loop (index + 1) false (Item.Ident.of_token token :: acc)
      | Some token when (not expect_ident) && Syn.SyntaxKind.(A.Token.kind token = DOT) ->
          loop (index + 1) true acc
      | _ -> None
  in
  loop start true []

let direct_module_refs_between = fun node start stop ->
  let rec collect_path index acc =
    if index = stop then
      (index, List.reverse acc)
    else
      match child_token_at node index with
      | Some token when Syn.SyntaxKind.(A.Token.kind token = IDENT) ->
          collect_path (index + 1) (Item.Ident.of_token token :: acc)
      | Some token when Syn.SyntaxKind.(A.Token.kind token = DOT) ->
          collect_path (index + 1) acc
      | _ -> (index, List.reverse acc)
  in
  let rec scan index items =
    if index = stop then
      List.reverse items
    else
      match child_token_at node index with
      | Some token when Syn.SyntaxKind.(A.Token.kind token = IDENT) ->
          let (next, segments) = collect_path index [] in
          let items =
            match segments with
            | _ when Item.Ident.is_module_path segments -> prepend_all [ Item.Use segments ] items
            | _ -> items
          in
          scan next items
      | _ -> scan (index + 1) items
  in
  scan start []

let direct_module_accesses_between = fun node start stop ->
  let rec collect_path index acc saw_dot =
    if index = stop then
      (index, List.reverse acc, saw_dot)
    else
      match child_token_at node index with
      | Some token when Syn.SyntaxKind.(A.Token.kind token = IDENT) ->
          collect_path (index + 1) (Item.Ident.of_token token :: acc) saw_dot
      | Some token when Syn.SyntaxKind.(A.Token.kind token = DOT) ->
          collect_path (index + 1) acc true
      | _ -> (index, List.reverse acc, saw_dot)
  in
  let rec scan index items =
    if index = stop then
      List.reverse items
    else
      match child_token_at node index with
      | Some token when Syn.SyntaxKind.(A.Token.kind token = IDENT) ->
          let (next, segments, saw_dot) = collect_path index [] false in
          let items =
            match segments with
            | _ when saw_dot && Item.Ident.is_module_path segments ->
                prepend_all (path_parent_use segments) items
            | _ -> items
          in
          scan next items
      | _ -> scan (index + 1) items
  in
  scan start []

let direct_type_payload_paths = fun node ->
  let count = A.Node.child_count node in
  let rec collect_path index acc =
    if index = count then
      (index, List.reverse acc)
    else
      match child_token_at node index with
      | Some token when Syn.SyntaxKind.(A.Token.kind token = IDENT) ->
          collect_path (index + 1) (Item.Ident.of_token token :: acc)
      | Some token when Syn.SyntaxKind.(A.Token.kind token = DOT) ->
          collect_path (index + 1) acc
      | _ -> (index, List.reverse acc)
  in
  let rec scan active index items =
    if index = count then
      List.reverse items
    else
      match child_token_at node index with
      | Some token when Syn.SyntaxKind.(A.Token.kind token = OF_KW || A.Token.kind token = COLON) ->
          scan true (index + 1) items
      | Some token when Syn.SyntaxKind.(A.Token.kind token = PIPE || A.Token.kind token = AND_KW) ->
          scan false (index + 1) items
      | Some token when active && Syn.SyntaxKind.(A.Token.kind token = IDENT) ->
          let (next, segments) = collect_path index [] in
          scan active next (prepend_all (path_parent_use segments) items)
      | _ -> scan active (index + 1) items
  in
  scan false 0 []

let direct_type_extension_path = fun node ->
  let count = A.Node.child_count node in
  let rec find_plus index =
    if index = count then
      None
    else if child_token_kind_is node index Syn.SyntaxKind.PLUS then
      Some index
    else
      find_plus (index + 1)
  in
  let rec collect_before_plus index plus_index acc items =
    if index = plus_index then
      prepend_all (path_parent_use (List.reverse acc)) items
    else
      match child_token_at node index with
      | Some token when Syn.SyntaxKind.(A.Token.kind token = IDENT) ->
          collect_before_plus (index + 1) plus_index (Item.Ident.of_token token :: acc) items
      | Some token when Syn.SyntaxKind.(A.Token.kind token = DOT) ->
          collect_before_plus (index + 1) plus_index acc items
      | _ -> collect_before_plus (index + 1) plus_index [] items
  in
  match find_plus 0 with
  | Some plus_index -> collect_before_plus 0 plus_index [] [] |> List.reverse
  | None -> []

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

let is_type_expr_kind = fun __tmp1 ->
  match __tmp1 with
  | Syn.SyntaxKind.TYPE_EXPR
  | Syn.SyntaxKind.PATH_TYPE
  | Syn.SyntaxKind.VAR_TYPE
  | Syn.SyntaxKind.WILDCARD_TYPE
  | Syn.SyntaxKind.ARROW_TYPE
  | Syn.SyntaxKind.POLY_TYPE
  | Syn.SyntaxKind.LABELED_TYPE
  | Syn.SyntaxKind.TUPLE_TYPE
  | Syn.SyntaxKind.APPLY_TYPE
  | Syn.SyntaxKind.PAREN_TYPE
  | Syn.SyntaxKind.OPAQUE_TYPE
  | Syn.SyntaxKind.VARIANT_TYPE -> true
  | _ -> false

let is_pattern_kind = fun __tmp1 ->
  match __tmp1 with
  | Syn.SyntaxKind.WILDCARD_PATTERN
  | Syn.SyntaxKind.PATH_PATTERN
  | Syn.SyntaxKind.CONSTRUCT_PATTERN
  | Syn.SyntaxKind.LITERAL_PATTERN
  | Syn.SyntaxKind.PAREN_PATTERN
  | Syn.SyntaxKind.TUPLE_PATTERN
  | Syn.SyntaxKind.LIST_PATTERN
  | Syn.SyntaxKind.ARRAY_PATTERN
  | Syn.SyntaxKind.RECORD_PATTERN
  | Syn.SyntaxKind.POLY_VARIANT_PATTERN
  | Syn.SyntaxKind.EXTENSION_PATTERN
  | Syn.SyntaxKind.ATTRIBUTE_PATTERN
  | Syn.SyntaxKind.LOCAL_OPEN_PATTERN
  | Syn.SyntaxKind.LOCALLY_ABSTRACT_TYPE_PATTERN
  | Syn.SyntaxKind.FIRST_CLASS_MODULE_PATTERN
  | Syn.SyntaxKind.INTERVAL_PATTERN
  | Syn.SyntaxKind.CONSTRAINT_PATTERN
  | Syn.SyntaxKind.ALIAS_PATTERN
  | Syn.SyntaxKind.OR_PATTERN
  | Syn.SyntaxKind.CONS_PATTERN
  | Syn.SyntaxKind.LAZY_PATTERN
  | Syn.SyntaxKind.EXCEPTION_PATTERN -> true
  | _ -> false

let is_parameter_kind = fun __tmp1 ->
  match __tmp1 with
  | Syn.SyntaxKind.LABELED_PARAM
  | Syn.SyntaxKind.OPTIONAL_PARAM
  | Syn.SyntaxKind.OPTIONAL_PARAM_DEFAULT -> true
  | _ -> false

let is_parameter_node_kind = fun kind -> is_parameter_kind kind || is_pattern_kind kind

let node_kind_is = fun node kind -> Syn.SyntaxKind.(A.Node.kind node = kind)

let first_child_node_matching = fun node ~matches ->
  let syntax = syntax_node node in
  let count = syntax.Syn.SyntaxTree.child_count in
  let rec loop index =
    if index = count then
      None
    else
      match child_at_unchecked node syntax index with
      | Syn.SyntaxTree.Node id ->
          let child_syntax = Syn.SyntaxTree.node node.A.tree id in
          if matches child_syntax.Syn.SyntaxTree.kind then
            Some ({ tree = node.A.tree; id }: A.Node.t)
          else
            loop (index + 1)
      | Syn.SyntaxTree.Token _
      | Syn.SyntaxTree.Missing _ -> loop (index + 1)
  in
  loop 0

let nth_child_node_matching = fun node target ~matches ->
  let syntax = syntax_node node in
  let count = syntax.Syn.SyntaxTree.child_count in
  let rec loop index seen =
    if index = count then
      None
    else
      match child_at_unchecked node syntax index with
      | Syn.SyntaxTree.Node id ->
          let child_syntax = Syn.SyntaxTree.node node.A.tree id in
          if matches child_syntax.Syn.SyntaxTree.kind then
            if seen = target then
              Some ({ tree = node.A.tree; id }: A.Node.t)
            else
              loop (index + 1) (seen + 1)
          else
            loop (index + 1) seen
      | Syn.SyntaxTree.Token _
      | Syn.SyntaxTree.Missing _ -> loop (index + 1) seen
  in
  loop 0 0

let first_child_token_matching = fun node ~matches ->
  let syntax = syntax_node node in
  let count = syntax.Syn.SyntaxTree.child_count in
  let rec loop index =
    if index = count then
      None
    else
      match child_at_unchecked node syntax index with
      | Syn.SyntaxTree.Token id ->
          let token = ({ tree = node.A.tree; id }: A.Token.t) in
          if matches (A.Token.kind token) then
            Some token
          else
            loop (index + 1)
      | Syn.SyntaxTree.Node _
      | Syn.SyntaxTree.Missing _ -> loop (index + 1)
  in
  loop 0

let first_pattern_child = fun node -> first_child_node_matching node ~matches:is_pattern_kind

let node_colon_has_leading_whitespace = fun node ->
  match first_child_token_matching node ~matches:(fun kind -> Syn.SyntaxKind.(kind = COLON)) with
  | Some colon -> A.Token.has_leading_whitespace colon
  | None -> false

let add_parameter_node = fun node acc ->
  match A.Parameter.cast node with
  | A.Node parameter -> parameter :: acc
  | A.Unknown _
  | A.Error _ -> acc

let rec prepend_parameter_node = fun node acc ->
  match A.Node.kind node with
  | kind when is_parameter_kind kind ->
      let acc = add_parameter_node node acc in
      if node_colon_has_leading_whitespace node then
        acc
      else
        (
          match first_pattern_child node with
          | Some pattern when node_kind_is pattern Syn.SyntaxKind.CONSTRUCT_PATTERN -> (
              match nth_child_node_matching pattern 1 ~matches:is_parameter_node_kind with
              | Some rest -> prepend_parameter_node rest acc
              | None -> acc
            )
          | Some _
          | None -> acc
        )
  | Syn.SyntaxKind.CONSTRUCT_PATTERN ->
      direct_child_nodes
        node
        ~matches:is_parameter_node_kind
        ~init:acc
        ~fn:(fun child acc -> prepend_parameter_node child acc)
  | Syn.SyntaxKind.CONSTRAINT_PATTERN -> (
      match first_pattern_child node with
      | Some pattern -> prepend_parameter_node pattern acc
      | None -> add_parameter_node node acc
    )
  | _ -> add_parameter_node node acc

let let_binding_parameters = fun binding ->
  let (_, parameters) =
    direct_child_nodes
      (A.LetBinding.as_node binding)
      ~matches:is_parameter_node_kind
      ~init:(false, [])
      ~fn:(fun node (seen_first, parameters) ->
        if seen_first then
          (true, prepend_parameter_node node parameters)
        else
          (true, parameters))
  in
  List.reverse parameters

let first_module_expr_after = fun node start ->
  let count = A.Node.child_count node in
  let rec loop index =
    if index = count then
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
    if index = count then
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

type module_member = {
  node: A.Node.t;
}

let fold_module_members = fun decl init fn ->
  let node = A.ModuleDeclaration.as_node decl in
  let syntax = syntax_node node in
  let count = syntax.Syn.SyntaxTree.child_count in
  let rec loop index saw_member acc =
    if index >= count then
      if saw_member then
        acc
      else
        fn acc { node }
    else
      match child_at_unchecked node syntax index with
      | Syn.SyntaxTree.Node id ->
          let child_syntax = Syn.SyntaxTree.node node.A.tree id in
          if Syn.SyntaxKind.(child_syntax.Syn.SyntaxTree.kind = MODULE_DECL_MEMBER) then
            loop (index + 1) true (fn acc { node = { tree = node.A.tree; id } })
          else
            loop (index + 1) saw_member acc
      | Syn.SyntaxTree.Token _
      | Syn.SyntaxTree.Missing _ -> loop (index + 1) saw_member acc
  in
  loop 0 false init

let module_member_name = fun member ->
  first_child_token_matching
    member.node
    ~matches:(fun kind -> Syn.SyntaxKind.(kind = IDENT || kind = UNDERSCORE))
  |> Option.map ~fn:(fun token -> A.Ident.Bare token)

let module_member_find_node = fun member ~matches ->
  let syntax = syntax_node member.node in
  let count = syntax.Syn.SyntaxTree.child_count in
  let rec loop index =
    if index >= count then
      None
    else
      match child_at_unchecked member.node syntax index with
      | Syn.SyntaxTree.Node id ->
          let child_syntax = Syn.SyntaxTree.node member.node.A.tree id in
          if matches child_syntax.Syn.SyntaxTree.kind then
            Some ({ tree = member.node.A.tree; id }: A.Node.t)
          else
            loop (index + 1)
      | Syn.SyntaxTree.Token _
      | Syn.SyntaxTree.Missing _ -> loop (index + 1)
  in
  loop 0

let module_member_first_specific_module_expr = fun node ->
  let kind = A.Node.kind node in
  if Syn.SyntaxKind.(kind = MODULE_EXPR) then
    first_child_node_matching node ~matches:is_module_expr_kind
  else if is_module_expr_kind kind then
    Some node
  else
    None

let module_member_first_specific_module_type = fun node ->
  let kind = A.Node.kind node in
  if Syn.SyntaxKind.(kind = MODULE_TYPE_EXPR) then
    first_child_node_matching node ~matches:is_module_type_kind
  else if is_module_type_kind kind then
    Some node
  else
    None

let module_member_module_expr = fun member ->
  match module_member_find_node
    member
    ~matches:(fun kind -> is_module_expr_kind kind || Syn.SyntaxKind.(kind = MODULE_EXPR))
  with
  | Some node -> module_member_first_specific_module_expr node
  | None -> None

let module_member_module_type = fun member ->
  match module_member_find_node
    member
    ~matches:(fun kind -> is_module_type_kind kind || Syn.SyntaxKind.(kind = MODULE_TYPE_EXPR))
  with
  | Some node -> module_member_first_specific_module_type node
  | None -> None

let module_member_functor_args = fun member ->
  let count = A.Node.child_count member.node in
  let rec find_close index =
    if index >= count then
      count
    else if child_token_kind_is member.node index Syn.SyntaxKind.RPAREN then
      index
    else
      find_close (index + 1)
  in
  let rec find_colon index stop =
    if index >= stop then
      None
    else if child_token_kind_is member.node index Syn.SyntaxKind.COLON then
      Some index
    else
      find_colon (index + 1) stop
  in
  let parameter_at start =
    let stop = find_close (start + 1) in
    let colon_index = find_colon (start + 1) stop in
    let name_stop = Option.unwrap_or colon_index ~default:stop in
    let name =
      A.Ident.from_child_range_option member.node ~start_index:(start + 1) ~stop_index:name_stop
      |> Option.and_then ~fn:ident_name
    in
    let ascription =
      match colon_index with
      | None -> []
      | Some colon_index -> (
          match A.Ident.from_child_range_option member.node ~start_index:(colon_index + 1) ~stop_index:stop with
          | Some ident -> ident_parent_use ident
          | None -> []
        )
    in
    ({ Item.name; ascription }: Item.functor_arg)
  in
  let rec loop index args =
    if index >= count then
      List.reverse args
    else if child_token_kind_is member.node index Syn.SyntaxKind.LPAREN then
      let close_index = find_close (index + 1) in
      loop (close_index + 1) (parameter_at index :: args)
    else
      loop (index + 1) args
  in
  loop 0 []

let fold_module_member_child_nodes = fun member ~init ~fn ->
  let syntax = syntax_node member.node in
  let count = syntax.Syn.SyntaxTree.child_count in
  let rec loop index acc =
    if index >= count then
      acc
    else
      match child_at_unchecked member.node syntax index with
      | Syn.SyntaxTree.Node id ->
          let child = ({ tree = member.node.A.tree; id }: A.Node.t) in
          loop (index + 1) (fn child acc)
      | Syn.SyntaxTree.Token _
      | Syn.SyntaxTree.Missing _ -> loop (index + 1) acc
  in
  loop 0 init

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
        | Some ident -> ident_parent_use ident
        | None -> (
            match first_module_type_expr_after node (colon_index + 1) with
            | Some module_type -> singleton_item (item_of_module_type_expr module_type)
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
            | Some module_type -> singleton_item (item_of_module_type_expr module_type)
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
  | A.SourceFile.Implementation impl ->
      (Implementation, items_of_implementation impl)
  | A.SourceFile.Interface intf -> (Interface, items_of_interface intf)

and items_of_implementation = fun impl ->
  direct_child_nodes
    (A.Implementation.as_node impl)
    ~matches:(fun kind -> Syn.SyntaxKind.(kind = STRUCTURE_ITEM))
    ~init:[]
    ~fn:(fun node items ->
      match A.StructureItem.cast node with
      | A.Node item -> prepend_all (items_of_structure_item item) items
      | A.Unknown _
      | A.Error _ -> items)
  |> List.reverse

and items_of_interface = fun intf ->
  direct_child_nodes
    (A.Interface.as_node intf)
    ~matches:(fun kind -> Syn.SyntaxKind.(kind = SIGNATURE_ITEM))
    ~init:[]
    ~fn:(fun node items ->
      match A.SignatureItem.cast node with
      | A.Node item -> prepend_all (items_of_signature_item item) items
      | A.Unknown _
      | A.Error _ -> items)
  |> List.reverse

and items_of_structure_item = fun item ->
  match A.StructureItem.view item with
  | A.StructureItem.Module decl -> items_of_module_declaration Structure_container decl
  | A.StructureItem.ModuleType decl -> items_of_module_type_declaration decl
  | A.StructureItem.Open decl -> items_of_open_declaration decl
  | A.StructureItem.Include decl -> items_of_include_declaration Item.Structure decl
  | _ -> items_of_child_nodes ~container:Structure_container (A.StructureItem.as_node item)

and items_of_signature_item = fun item ->
  match A.SignatureItem.view item with
  | A.SignatureItem.Value decl -> items_of_value_declaration decl
  | A.SignatureItem.Module decl -> items_of_module_declaration Signature_container decl
  | A.SignatureItem.ModuleType decl -> items_of_module_type_declaration decl
  | A.SignatureItem.Open decl -> items_of_open_declaration decl
  | A.SignatureItem.Include decl -> items_of_include_declaration Item.Signature decl
  | A.SignatureItem.External decl -> items_of_external_declaration decl
  | _ -> items_of_child_nodes ~container:Signature_container (A.SignatureItem.as_node item)

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
      let node = A.IncludeDeclaration.as_node decl in
      let module_expr_include () =
        match first_module_expr node with
        | Some module_expr -> [ Item.Include (mode, item_of_module_expr module_expr) ]
        | None -> []
      in
      let module_type_include () =
        match first_module_type_expr node with
        | Some module_type -> [ Item.Include (mode, item_of_module_type_expr module_type) ]
        | None -> []
      in
      let raw_include () =
        let count = A.Node.child_count node in
        match direct_path_between node 1 count with
        | Some segments -> [ Item.Include (mode, Item.Use segments) ]
        | None -> (
            match direct_module_refs_between node 1 count with
            | [] -> []
            | items -> [ Item.Include (mode, Item.Scope items) ]
          )
      in
      match mode with
      | Item.Structure -> (
          match module_expr_include () with
          | [] -> (
              match module_type_include () with
              | [] -> raw_include ()
              | items -> items
            )
          | items -> items
        )
      | Item.Signature -> (
          match module_type_include () with
          | [] -> (
              match module_expr_include () with
              | [] -> raw_include ()
              | items -> items
            )
          | items -> items
        )
    )

and items_of_module_type_declaration = fun decl ->
  match A.ModuleTypeDeclaration.name decl with
  | None -> (
      match A.ModuleTypeDeclaration.body decl with
      | A.ModuleTypeDeclaration.Manifest { body } -> singleton_item (item_of_module_type_expr body)
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
            | A.ModuleTypeDeclaration.Manifest { body } -> singleton_item (item_of_module_type_expr body)
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
      fold_module_members
        decl
        []
        (fun items member ->
          match module_member_name member with
          | Some ident -> (
              match ident_name ident with
              | Some name -> Item.Module { name; signature = []; body = [] } :: items
              | None -> items
            )
          | None -> items)
      |> List.reverse
    in
    let rhs_items =
      fold_module_members
        decl
        []
        (fun items member ->
          prepend_all (items_of_recursive_module_member_rhs container member) items)
      |> List.reverse
    in
    match rhs_items with
    | [] -> prebound
    | _ -> prebound @ [ Item.Scope rhs_items ]
  else
    fold_module_members
      decl
      []
      (fun items member -> prepend_all (items_of_module_member container member) items)
    |> List.reverse

and functor_args_of_member = fun member -> module_member_functor_args member

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
    match module_member_module_type member with
    | Some node -> (
        match A.ModuleTypeExpr.cast node with
        | A.Node module_type -> items_of_module_type_expr module_type
        | A.Unknown _
        | A.Error _ -> items_of_node ~container:Signature_container node
      )
    | None -> []
  in
  let body_items =
    match module_member_module_expr member with
    | Some node -> (
        match A.ModuleExpr.cast node with
              | A.Node module_expr -> items_of_module_expr_declaration_body module_expr
        | A.Unknown _
        | A.Error _ -> items_of_node ~container node
      )
    | None -> (
        match module_member_module_type member with
        | Some node -> (
            match A.ModuleTypeExpr.cast node with
            | A.Node module_type -> items_of_module_type_expr module_type
            | A.Unknown _
            | A.Error _ -> items_of_node ~container:Signature_container node
          )
        | None -> []
      )
  in
  prefix @ annotation_items @ body_items

and module_item_with_prefix = fun name prefix item ->
  match prefix with
  | [] -> item
    | _ -> Item.Module { name; signature = []; body = prefix @ [ item ] }

and items_of_module_member = fun container member ->
  match module_member_name member with
  | None ->
      fold_module_member_child_nodes
        member
        ~init:[]
        ~fn:(fun node items ->
          prepend_all (items_of_node ~container node) items)
      |> List.reverse
  | Some ident -> (
      match ident_name ident with
      | None -> []
      | Some name ->
          let functor_args = functor_args_of_member member in
          let annotation_items =
            match module_member_module_type member with
            | Some node -> (
                match A.ModuleTypeExpr.cast node with
                | A.Node module_type -> singleton_item (item_of_module_type_expr module_type)
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
          match module_member_module_expr member with
          | Some node -> (
              match A.ModuleExpr.cast node with
              | A.Node module_expr -> (
                  match (functor_args, A.ModuleExpr.view module_expr) with
                  | ([], A.ModuleExpr.Ident { ident }) -> [
                      Item.ModuleAlias {
                        name;
                        target = Item.Use (ident_segments ident);
                      };
                    ]
                  | ([], A.ModuleExpr.Functor { body }) ->
                      let (args, body) = functor_parts body ~body_kind:`ModuleExpr in
                      [ Item.Functor { name; args; body } ]
                  | ([], _) -> make_module (items_of_module_expr_declaration_body module_expr)
                  | (_, _) ->
                      make_functor
                        (annotation_items @ items_of_module_expr_declaration_body module_expr)
                )
              | A.Unknown _
              | A.Error _ ->
                  make_declaration (items_of_node ~container:Structure_container node)
            )
              | None -> (
                  match module_member_module_type member with
                  | Some _node -> make_declaration []
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
        | Some module_type -> singleton_item (item_of_module_type_expr module_type)
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
  | A.ModuleExpr.Structure { body } ->
      direct_child_nodes
        body
        ~matches:(fun kind -> Syn.SyntaxKind.(kind = STRUCTURE_ITEM))
        ~init:[]
        ~fn:(fun node items ->
          match A.StructureItem.cast node with
          | A.Node item -> prepend_all (items_of_structure_item item) items
          | A.Unknown _
          | A.Error _ -> items)
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
  | A.ModuleTypeExpr.Ident { ident } -> item_of_module_type_ident ident
  | A.ModuleTypeExpr.Signature _ -> Item.Scope (items_of_module_type_body module_type)
  | A.ModuleTypeExpr.With { base; body; _ } ->
      let base =
        match base with
        | Some base -> item_of_module_type_expr base
        | None -> Item.Scope (items_of_node ~container:Signature_container body)
      in
      let constraints =
        direct_child_nodes
          body
          ~matches:(fun kind ->
            Syn.SyntaxKind.(kind = WITH_TYPE_CONSTRAINT || kind = WITH_MODULE_CONSTRAINT))
          ~init:[]
          ~fn:(fun child items ->
            match A.ModuleTypeConstraint.cast child with
            | A.Node constraint_ ->
                prepend_all (items_of_module_type_constraint constraint_) items
            | A.Unknown _
            | A.Error _ -> items)
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
  match A.ModuleTypeExpr.view module_type with
  | A.ModuleTypeExpr.Signature { body } ->
      direct_child_nodes
        body
        ~matches:(fun kind -> Syn.SyntaxKind.(kind = SIGNATURE_ITEM))
        ~init:[]
        ~fn:(fun node items ->
          match A.SignatureItem.cast node with
          | A.Node item -> prepend_all (items_of_signature_item item) items
          | A.Unknown _
          | A.Error _ -> items)
      |> List.reverse
  | _ -> []

and items_of_module_type_expr = fun module_type ->
  match A.ModuleTypeExpr.view module_type with
  | A.ModuleTypeExpr.Ident { ident } -> ident_parent_use ident
  | A.ModuleTypeExpr.Signature _ -> items_of_module_type_body module_type
  | A.ModuleTypeExpr.With { base; body; _ } -> (
      let base_items =
        match base with
        | Some base -> singleton_item (item_of_module_type_expr base)
        | None -> []
      in
      let constraint_items =
        direct_child_nodes
          body
          ~matches:(fun kind ->
            Syn.SyntaxKind.(kind = WITH_TYPE_CONSTRAINT || kind = WITH_MODULE_CONSTRAINT))
          ~init:[]
          ~fn:(fun child items ->
            match A.ModuleTypeConstraint.cast child with
            | A.Node constraint_ ->
                prepend_all (items_of_module_type_constraint constraint_) items
            | A.Unknown _
            | A.Error _ -> items)
        |> List.reverse
      in
      match (base, constraint_items) with
      | (None, []) -> items_of_node ~container:Signature_container body
      | _ -> base_items @ constraint_items
    )
  | A.ModuleTypeExpr.Typeof { body = Some body } -> items_of_module_expr_body body
  | A.ModuleTypeExpr.Typeof { body = None } -> []
  | A.ModuleTypeExpr.Functor _ -> singleton_item (item_of_module_type_expr module_type)
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
                    | A.ModuleExpr.Ident { ident } -> [
                        Item.ModuleAlias {
                          name;
                          target = Item.Use (ident_segments ident);
                        };
                      ]
                    | _ -> [
                        Item.Module {
                          name;
                          signature = [];
                          body = items_of_module_expr_declaration_body module_expr;
                        };
                      ]
                  )
                | A.Unknown _
                | A.Error _ -> [
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
                    [
                      Item.ModuleAlias {
                        name;
                        target = Item.Use (ident_segments ident);
                      };
                    ]
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
  items_of_node ~container:Signature_container (A.TypeExpr.as_node type_expr)

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
  direct_child_nodes
    (A.RecordType.as_node record_type)
    ~matches:(fun kind -> Syn.SyntaxKind.(kind = RECORD_FIELD))
    ~init:[]
    ~fn:(fun node items ->
      match A.RecordField.cast node with
      | A.Node field -> (
      match A.RecordField.view field with
      | A.RecordField.Field { annotation; _ } ->
          prepend_all (items_of_type_expr annotation) items
      | A.RecordField.Unknown node ->
          prepend_all (items_of_node ~container:Signature_container node) items
        )
      | A.Unknown _
      | A.Error _ -> items)
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
      | A.VariantConstructor.Gadt {
          record_payload;
          result;
          _;
        } ->
          (
            match record_payload with
            | Some record_type -> items_of_record_type record_type
            | None -> []
          ) @ items_of_type_expr result
    )
  | A.VariantConstructor.Unknown node -> items_of_node ~container:Signature_container node

and items_of_variant_type = fun variant_type ->
  let inherited_items =
    direct_child_nodes
      (A.VariantType.as_node variant_type)
      ~matches:is_type_expr_kind
      ~init:[]
      ~fn:(fun node items ->
        match A.TypeExpr.cast node with
        | A.Node inherited -> prepend_all (items_of_type_expr inherited) items
        | A.Unknown _
        | A.Error _ -> items)
    |> List.reverse
  in
  let constructor_items =
    direct_child_nodes
      (A.VariantType.as_node variant_type)
      ~matches:(fun kind -> Syn.SyntaxKind.(kind = VARIANT_CONSTRUCTOR))
      ~init:[]
      ~fn:(fun node items ->
        match A.VariantConstructor.cast node with
        | A.Node constructor -> prepend_all (items_of_variant_constructor constructor) items
        | A.Unknown _
        | A.Error _ -> items)
    |> List.reverse
  in
  inherited_items @ constructor_items

and items_of_pattern = fun pattern ->
  match A.cast_result_to_option (A.LocalOpenPattern.cast pattern) with
  | Some local_open -> (
      match A.LocalOpenPattern.view local_open with
      | A.LocalOpenPattern.Delimited { module_ident; pattern; _ } ->
          [
            Item.Scope (ident_module_open module_ident @ items_of_pattern pattern);
          ]
      | A.LocalOpenPattern.Unknown node -> items_of_node ~container:Structure_container node
    )
  | None -> (
      match A.Pattern.view pattern with
      | A.Pattern.Unit
      | A.Pattern.Wildcard
      | A.Pattern.Literal _ -> []
      | A.Pattern.Ident { ident } -> ident_parent_use ident
      | A.Pattern.Constructor { constructor; payload } ->
          ident_parent_use constructor
          @ (
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
                  ident_parent_use ident
                  @ (
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
      | A.Pattern.FirstClassModule { ascription_ident; _ } -> (
          match ascription_ident with
          | Some ident -> ident_parent_use ident
          | None -> []
        )
      | A.Pattern.Interval { left; right }
      | A.Pattern.Or { left; right }
      | A.Pattern.Cons { head = left; tail = right } ->
          items_of_pattern left @ items_of_pattern right
      | A.Pattern.Constraint { pattern; annotation } ->
          items_of_pattern pattern @ items_of_type_expr annotation
      | A.Pattern.Alias { pattern; alias } ->
          items_of_pattern pattern @ items_of_pattern alias
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
                | Some ident -> ident_parent_use ident
                | None -> []
              in
              [ { Item.name = name; ascription } ]
          | Some _
          | None -> []
        )
      | A.Pattern.Tuple { parts }
      | A.Pattern.List { items = parts }
      | A.Pattern.Array { items = parts } ->
          vector_items parts ~fn:bound_modules_of_pattern
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
      | A.Pattern.Unknown _ -> bound_modules_of_pattern_node (A.Pattern.as_node pattern)
    )

and bound_modules_of_pattern_node = fun node ->
  let fold_children () =
    direct_child_nodes
      node
      ~matches:(fun _ -> true)
      ~init:[]
      ~fn:(fun child modules -> prepend_all (bound_modules_of_pattern_node child) modules)
    |> List.reverse
  in
  match A.Node.kind node with
  | Syn.SyntaxKind.FIRST_CLASS_MODULE_PATTERN ->
      let count = A.Node.child_count node in
      let rec find_binding index seen_module =
        if index = count then
          None
        else
          match child_token_at node index with
          | Some token when Syn.SyntaxKind.(A.Token.kind token = MODULE_KW) ->
              find_binding (index + 1) true
          | Some token when seen_module && Syn.SyntaxKind.(A.Token.kind token = IDENT) ->
              Some (token_text token)
          | Some token when seen_module && Syn.SyntaxKind.(A.Token.kind token = UNDERSCORE) ->
              None
          | _ -> find_binding (index + 1) seen_module
      in
      (
        match find_binding 0 false with
        | Some name when is_module_head name -> [ ({ Item.name; ascription = [] }: Item.bound_module) ]
        | Some _
        | None -> []
      )
  | _ -> fold_children ()

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
      label_items
      @ (
        match pattern with
        | Some pattern -> items_of_pattern pattern
        | None -> []
      )
  | A.Parameter.Unknown node -> items_of_node ~container:Structure_container node

and bound_modules_of_parameter = fun parameter ->
  let modules =
    match A.Parameter.view parameter with
    | A.Parameter.Param { pattern = Some pattern; _ } -> bound_modules_of_pattern pattern
    | A.Parameter.Param { pattern = None; _ }
    | A.Parameter.Unknown _ -> []
  in
  if List.is_empty modules then
    bound_modules_of_pattern_node (A.Parameter.as_node parameter)
  else
    modules

and items_of_parameters = fun parameters -> vector_items parameters ~fn:items_of_parameter

and bound_modules_of_parameters = fun parameters ->
  vector_items parameters ~fn:bound_modules_of_parameter

and items_of_match_case = fun match_case ->
  match A.MatchCase.view match_case with
  | A.MatchCase.Case { pattern; guard; body } ->
      let pattern_items = items_of_pattern pattern in
      let scope =
        (
          match guard with
          | Some guard -> items_of_expr guard
          | None -> []
        )
        @ items_of_expr body
      in
      let modules = bound_modules_of_pattern pattern in
      pattern_items @ bind_modules_items modules scope
  | A.MatchCase.Unknown node -> items_of_node ~container:Structure_container node

and items_of_match_cases = fun expr ->
  direct_child_nodes
    (A.Expr.as_node expr)
    ~matches:(fun kind -> Syn.SyntaxKind.(kind = MATCH_CASE))
    ~init:[]
    ~fn:(fun node items ->
      match A.MatchCase.cast node with
      | A.Node match_case -> prepend_all (items_of_match_case match_case) items
      | A.Unknown _
      | A.Error _ -> items)
  |> List.reverse

and items_of_fun_body = fun expr body ->
  match body with
  | A.Expr.Body_expr body -> items_of_expr body
  | A.Expr.Body_cases _ -> items_of_match_cases expr

and items_of_let_binding = fun binding ->
  match A.LetBinding.view binding with
  | A.LetBinding.Binding { pattern; body } ->
      let parameters = let_binding_parameters binding in
      let parameter_items =
        List.fold_left
          parameters
          ~init:[]
          ~fn:(fun items parameter -> prepend_all (items_of_parameter parameter) items)
        |> List.reverse
      in
      let parameter_modules =
        List.fold_left
          parameters
          ~init:[]
          ~fn:(fun modules parameter -> prepend_all (bound_modules_of_parameter parameter) modules)
        |> List.reverse
      in
      let annotation_items =
        (
          match A.LetBinding.type_annotation binding with
          | Some annotation -> items_of_type_expr annotation
          | None -> []
        )
        @ (
          match A.LetBinding.return_type_annotation binding with
          | Some annotation -> items_of_type_expr annotation
          | None -> []
        )
      in
      let modules = bound_modules_of_pattern pattern @ parameter_modules in
      annotation_items
      @ items_of_pattern pattern
      @ parameter_items
      @ bind_modules_items modules (items_of_expr body)
  | A.LetBinding.Unknown node -> items_of_node ~container:Structure_container node

and items_of_record_expr_fields = fun fields ->
  vector_items
    fields
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | A.RecordExprField { ident; value; _ } ->
          ident_parent_use ident
          @ (
            match value with
            | Some value -> items_of_expr value
            | None -> []
          )
      | A.UnknownRecordExprField { node } -> items_of_node ~container:Structure_container (A.RecordExprField.as_node node))

and items_of_first_class_module_expr = fun expr ->
  match A.cast_result_to_option (A.FirstClassModuleExpr.cast expr) with
  | None -> []
  | Some first_class ->
      (
        match A.FirstClassModuleExpr.module_ident first_class with
        | Some ident -> ident_module_use ident
        | None -> []
      )
      @ (
        match A.FirstClassModuleExpr.ascription_ident first_class with
        | Some ident -> ident_parent_use ident
        | None -> []
      )

and items_of_expr = fun expr ->
  match A.Expr.kind expr with
  | Syn.SyntaxKind.FIRST_CLASS_MODULE_EXPR -> items_of_first_class_module_expr expr
  | Syn.SyntaxKind.LOCAL_OPEN_EXPR -> (
      match A.cast_result_to_option (A.LocalOpenExpr.cast expr) with
      | Some local_open -> (
          match A.LocalOpenExpr.view local_open with
          | A.LocalOpenExpr.LetOpen { module_ident; body; _ }
          | A.LocalOpenExpr.Delimited { module_ident; body; _ } ->
              [ Item.Scope (ident_module_open module_ident @ items_of_expr body) ]
          | A.LocalOpenExpr.Unknown node -> items_of_node ~container:Structure_container node)
      | None -> items_of_node ~container:Structure_container (A.Expr.as_node expr)
    )
  | Syn.SyntaxKind.LET_MODULE_EXPR -> (
      match A.cast_result_to_option (A.LetModuleExpr.cast expr) with
      | Some let_module -> items_of_let_module_expr let_module
      | None -> items_of_node ~container:Structure_container (A.Expr.as_node expr)
    )
  | Syn.SyntaxKind.FUN_EXPR -> (
      match A.Expr.view expr with
      | A.Expr.Fun { parameters; return_annotation; body } ->
          let parameter_items = items_of_parameters parameters in
          let return_items =
            match return_annotation with
            | Some annotation -> items_of_type_expr annotation
            | None -> []
          in
          let modules = bound_modules_of_parameters parameters in
          parameter_items @ return_items @ bind_modules_items modules (items_of_fun_body expr body)
      | _ -> items_of_node ~container:Structure_container (A.Expr.as_node expr)
    )
  | _ -> items_of_node ~container:Structure_container (A.Expr.as_node expr)

and items_of_node = fun ~container node ->
  match A.Node.kind node with
  | Syn.SyntaxKind.PATH_EXPR
  | Syn.SyntaxKind.PATH_PATTERN
  | Syn.SyntaxKind.PATH_TYPE
  | Syn.SyntaxKind.PATH_MODULE_TYPE
  | Syn.SyntaxKind.FIELD_ACCESS_EXPR ->
      path_parent_use_node node
  | Syn.SyntaxKind.PATH_MODULE_EXPR -> path_module_use_node node
  | Syn.SyntaxKind.TYPE_DECL ->
      direct_type_extension_path node
      @ direct_type_payload_paths node
      @ items_of_child_nodes ~container node
  | Syn.SyntaxKind.TYPE_EXTENSION_DECL ->
      direct_type_extension_path node
      @ direct_type_payload_paths node
      @ items_of_child_nodes ~container node
  | Syn.SyntaxKind.OPAQUE_TYPE ->
      direct_module_accesses_between node 0 (A.Node.child_count node)
  | Syn.SyntaxKind.MODULE_DECL
  | Syn.SyntaxKind.MODULE_TYPE_DECL
  | Syn.SyntaxKind.OPEN_DECL
  | Syn.SyntaxKind.INCLUDE_DECL
  | Syn.SyntaxKind.LET_BINDING
  | Syn.SyntaxKind.LET_MODULE_EXPR
  | Syn.SyntaxKind.FUN_EXPR
  | Syn.SyntaxKind.MATCH_CASE
  | Syn.SyntaxKind.LOCAL_OPEN_EXPR
  | Syn.SyntaxKind.LOCAL_OPEN_PATTERN ->
      items_of_known_node ~container node
  | kind when is_module_expr_kind kind -> (
      match A.ModuleExpr.cast node with
      | A.Node module_expr -> (
          match A.ModuleExpr.view module_expr with
          | A.ModuleExpr.Ident { ident } -> ident_module_use ident
          | A.ModuleExpr.Structure _
          | A.ModuleExpr.Functor _
          | A.ModuleExpr.Apply _
          | A.ModuleExpr.Constraint _
          | A.ModuleExpr.Opaque _
          | A.ModuleExpr.Error _
          | A.ModuleExpr.Unknown _ -> items_of_child_nodes ~container node)
      | A.Unknown _
      | A.Error _ -> items_of_child_nodes ~container node)
  | kind when is_module_type_kind kind -> (
      match A.ModuleTypeExpr.cast node with
      | A.Node module_type -> (
          match A.ModuleTypeExpr.view module_type with
          | A.ModuleTypeExpr.Error _
          | A.ModuleTypeExpr.Unknown _ -> items_of_child_nodes ~container node
          | _ -> items_of_module_type_expr module_type)
      | A.Unknown _
      | A.Error _ -> items_of_child_nodes ~container node)
  | _ -> items_of_child_nodes ~container node

and items_of_child_nodes = fun ~container node ->
  let syntax = syntax_node node in
  let count = syntax.Syn.SyntaxTree.child_count in
  let rec loop index items =
    if index = count then
      List.reverse items
    else
      match child_at_unchecked node syntax index with
      | Syn.SyntaxTree.Node id ->
          let child = ({ tree = node.A.tree; id }: A.Node.t) in
          loop (index + 1) (prepend_all (items_of_node ~container child) items)
      | Syn.SyntaxTree.Token _
      | Syn.SyntaxTree.Missing _ -> loop (index + 1) items
  in
  loop 0 []

and items_of_known_node = fun ~container node ->
  match A.Node.kind node with
  | Syn.SyntaxKind.MODULE_DECL -> (
      match A.ModuleDeclaration.cast node with
      | A.Node decl -> items_of_module_declaration container decl
      | A.Unknown _
      | A.Error _ -> items_of_child_nodes ~container node)
  | Syn.SyntaxKind.MODULE_TYPE_DECL -> (
      match A.ModuleTypeDeclaration.cast node with
      | A.Node decl -> items_of_module_type_declaration decl
      | A.Unknown _
      | A.Error _ -> items_of_child_nodes ~container node)
  | Syn.SyntaxKind.OPEN_DECL -> (
      match A.OpenDeclaration.cast node with
      | A.Node decl -> items_of_open_declaration decl
      | A.Unknown _
      | A.Error _ -> items_of_child_nodes ~container node)
  | Syn.SyntaxKind.INCLUDE_DECL -> (
      match A.IncludeDeclaration.cast node with
      | A.Node decl ->
          let mode =
            match container with
            | Structure_container -> Item.Structure
            | Signature_container -> Item.Signature
          in
          items_of_include_declaration mode decl
      | A.Unknown _
      | A.Error _ -> items_of_child_nodes ~container node)
  | Syn.SyntaxKind.LET_BINDING -> (
      match A.LetBinding.cast node with
      | A.Node binding -> items_of_let_binding binding
      | A.Unknown _
      | A.Error _ -> items_of_child_nodes ~container node)
  | Syn.SyntaxKind.LET_MODULE_EXPR -> (
      match A.Expr.cast node with
      | A.Node expr -> (
          match A.cast_result_to_option (A.LetModuleExpr.cast expr) with
          | Some let_module -> items_of_let_module_expr let_module
          | None -> items_of_child_nodes ~container node)
      | A.Unknown _
      | A.Error _ -> items_of_child_nodes ~container node)
  | Syn.SyntaxKind.FUN_EXPR
  | Syn.SyntaxKind.LOCAL_OPEN_EXPR -> (
      match A.Expr.cast node with
      | A.Node expr -> items_of_expr expr
      | A.Unknown _
      | A.Error _ -> items_of_child_nodes ~container node)
  | Syn.SyntaxKind.MATCH_CASE -> (
      match A.MatchCase.cast node with
      | A.Node match_case -> items_of_match_case match_case
      | A.Unknown _
      | A.Error _ -> items_of_child_nodes ~container node)
  | Syn.SyntaxKind.LOCAL_OPEN_PATTERN -> (
      match A.Pattern.cast node with
      | A.Node pattern -> items_of_pattern pattern
      | A.Unknown _
      | A.Error _ -> items_of_child_nodes ~container node)
  | _ -> items_of_child_nodes ~container node

and items_of_child_node = fun ~container node ->
  match A.StructureItem.cast node with
  | A.Node item -> items_of_structure_item item
  | A.Unknown _
  | A.Error _ -> (
      match A.SignatureItem.cast node with
      | A.Node item -> items_of_signature_item item
      | A.Unknown _
      | A.Error _ -> (
          match A.ModuleDeclaration.cast node with
          | A.Node decl -> items_of_module_declaration container decl
          | A.Unknown _
          | A.Error _ -> (
              match A.ModuleTypeDeclaration.cast node with
              | A.Node decl -> items_of_module_type_declaration decl
              | A.Unknown _
              | A.Error _ -> (
                  match A.OpenDeclaration.cast node with
                  | A.Node decl -> items_of_open_declaration decl
                  | A.Unknown _
                  | A.Error _ -> (
                      match A.IncludeDeclaration.cast node with
                      | A.Node decl ->
                          let mode =
                            match container with
                            | Structure_container -> Item.Structure
                            | Signature_container -> Item.Signature
                          in
                          items_of_include_declaration mode decl
                      | A.Unknown _
                      | A.Error _ -> items_of_non_decl_node ~container node)))))

and items_of_non_decl_node = fun ~container node ->
  match A.LetBinding.cast node with
  | A.Node binding -> items_of_let_binding binding
  | A.Unknown _
  | A.Error _ -> (
      match A.Expr.cast node with
      | A.Node expr -> items_of_expr expr
      | A.Unknown _
      | A.Error _ -> (
          match A.Pattern.cast node with
          | A.Node pattern -> items_of_pattern pattern
          | A.Unknown _
          | A.Error _ -> (
              match A.TypeExpr.cast node with
              | A.Node type_expr -> items_of_type_expr type_expr
              | A.Unknown _
              | A.Error _ -> (
                  match A.ModuleExpr.cast node with
                  | A.Node module_expr -> (
                      match A.ModuleExpr.view module_expr with
                      | A.ModuleExpr.Ident { ident } -> ident_module_use ident
                      | A.ModuleExpr.Structure _
                      | A.ModuleExpr.Functor _
                      | A.ModuleExpr.Apply _
                      | A.ModuleExpr.Constraint _
                      | A.ModuleExpr.Opaque _
                      | A.ModuleExpr.Error _
                      | A.ModuleExpr.Unknown _ -> items_of_node ~container node)
                  | A.Unknown _
                  | A.Error _ -> (
                      match A.ModuleTypeExpr.cast node with
                      | A.Node module_type -> (
                          match A.ModuleTypeExpr.view module_type with
                          | A.ModuleTypeExpr.Error _
                          | A.ModuleTypeExpr.Unknown _ -> items_of_node ~container node
                          | _ -> items_of_module_type_expr module_type)
                      | A.Unknown _
                      | A.Error _ -> items_of_leaf_node ~container node)))))

and items_of_leaf_node = fun ~container node ->
  match A.VariantType.cast node with
  | A.Node variant_type -> items_of_variant_type variant_type
  | A.Unknown _
  | A.Error _ -> (
      match A.RecordType.cast node with
      | A.Node record_type -> items_of_record_type record_type
      | A.Unknown _
      | A.Error _ -> (
          match A.VariantConstructor.cast node with
          | A.Node constructor -> items_of_variant_constructor constructor
          | A.Unknown _
          | A.Error _ -> (
              match A.RecordField.cast node with
              | A.Node field -> (
                  match A.RecordField.view field with
                  | A.RecordField.Field { annotation; _ } -> items_of_type_expr annotation
                  | A.RecordField.Unknown node -> items_of_node ~container:Signature_container node)
              | A.Unknown _
              | A.Error _ -> (
                  match A.ExprItem.cast node with
                  | A.Node item -> (
                      match A.ExprItem.expr item with
                      | Some expr -> items_of_expr expr
                      | None -> [])
                  | A.Unknown _
                  | A.Error _ -> items_of_node ~container node))))

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
