open Std
open Std.Collections

module Ast = Syn.Ast
module SyntaxKind = Syn.SyntaxKind
module SyntaxTree = Syn.SyntaxTree
module SurfacePath = Model.Surface_path
module BindingId = Model.Binding_id
module EntityId = Model.Entity_id

type ty =
  | TInt
  | TBool
  | TChar
  | TString
  | TFloat
  | TUnit
  | TList of ty
  | TOption of ty
  | TTuple of ty list
  | TArrow of ty * ty
  | TVar of tyvar_cell

and tyvar_cell = { mutable var: tvar }

and tvar =
  | Unbound of int * int
  | Link of ty
  | Generic of int

type binding = {
  binding_id: BindingId.t;
  entity_id: EntityId.t;
  ty: ty;
}

type env = binding list

type state = {
  mutable next_tyvar: int;
  mutable next_binding_stamp: int;
  mutable diagnostics: Diagnostics.Diagnostic.t list;
}

let raw_at = fun tree index -> Vector.get_unchecked tree.SyntaxTree.raw_tokens ~at:index

let span_from_raw = fun (span: Ceibo.Span.t) ->
  Syn.Ceibo.Span.make
    ~start:span.Ceibo.Span.start
    ~end_:span.Ceibo.Span.end_

let span_of_raw_range = fun tree ~raw_lo ~raw_hi ->
  if Int.(raw_hi <= raw_lo) then
    Syn.Ceibo.Span.make ~start:0 ~end_:0
  else
    let first = raw_at tree raw_lo in
    let last = raw_at tree (Int.sub raw_hi 1) in
    Syn.Ceibo.Span.make
      ~start:first.Syn.RawToken.span.Ceibo.Span.start
      ~end_:last.Syn.RawToken.span.Ceibo.Span.end_

let span_of_node = fun (node: Ast.Node.t) ->
  let syntax_node = SyntaxTree.node node.tree node.id in
  match Ast.Node.first_descendant_token node with
  | Some first ->
      let first_leaf = SyntaxTree.token first.tree first.id in
      let first_raw = raw_at node.tree first_leaf.SyntaxTree.body_raw in
      let last_raw = raw_at node.tree (Int.sub syntax_node.SyntaxTree.raw_hi 1) in
      Syn.Ceibo.Span.make
        ~start:first_raw.Syn.RawToken.span.Ceibo.Span.start
        ~end_:last_raw.Syn.RawToken.span.Ceibo.Span.end_
  | None -> span_of_raw_range node.tree ~raw_lo:syntax_node.raw_lo ~raw_hi:syntax_node.raw_hi

let span_start = fun span -> span.Syn.Ceibo.Span.start

let span_end = fun span -> span.Syn.Ceibo.Span.end_

let span_of_children = fun tree ~child_count ~child_at ->
  let first = ref None in
  let last = ref None in
  let remember span =
    (
      match !first with
      | Some _ -> ()
      | None -> first := Some (span_start span)
    );
    last := Some (span_end span)
  in
  let span_of_child = function
    | SyntaxTree.Token id ->
        let leaf = SyntaxTree.token tree id in
        let raw = raw_at tree leaf.SyntaxTree.body_raw in
        remember (span_from_raw raw.Syn.RawToken.span)
    | SyntaxTree.Node id -> remember (span_of_node { Ast.tree; id })
    | SyntaxTree.Missing missing ->
        remember (Syn.Ceibo.Span.make ~start:missing.SyntaxTree.offset ~end_:missing.offset)
  in
  let rec loop index =
    if Int.(index < child_count) then (
      (
        match child_at index with
        | Some child -> span_of_child child
        | None -> ()
      );
      loop (Int.add index 1)
    )
  in
  loop 0;
  match (!first, !last) with
  | (Some start, Some end_) -> Syn.Ceibo.Span.make ~start ~end_
  | _ -> Syn.Ceibo.Span.make ~start:0 ~end_:0

let span_of_type_member = fun member ->
  let declaration = Ast.TypeDeclaration.Member.declaration member in
  span_of_children
    declaration.tree
    ~child_count:(Ast.TypeDeclaration.Member.child_count member)
    ~child_at:(Ast.TypeDeclaration.Member.child_at member)

let span_of_type_declaration = fun declaration ->
  let span = ref None in
  let remember next =
    span := Some (
      match !span with
      | None -> next
      | Some current ->
          Syn.Ceibo.Span.make
            ~start:(Int.min (span_start current) (span_start next))
            ~end_:(Int.max (span_end current) (span_end next))
    )
  in
  Ast.TypeDeclaration.for_each_member
    declaration
    ~fn:(fun member -> remember (span_of_type_member member));
  match !span with
  | Some span -> span
  | None -> span_of_node declaration

let unsupported_syntax = fun node summary ->
  Diagnostics.Diagnostic.UnsupportedSyntax {
    span = span_of_node node;
    kind = Ast.Node.kind node;
    summary;
  }

let unsupported_syntax_with_span = fun ~span ~kind summary ->
  Diagnostics.Diagnostic.UnsupportedSyntax { span; kind; summary }

let unsupported_type = fun node summary ->
  Diagnostics.Diagnostic.UnsupportedType { span = span_of_node node; summary }

let add_diagnostic = fun state diagnostic -> state.diagnostics <- diagnostic :: state.diagnostics

let make_state = fun ~next_binding_stamp -> { next_tyvar = 0; next_binding_stamp; diagnostics = [] }

let fresh_tyvar = fun state ~level ->
  let id = state.next_tyvar in
  state.next_tyvar <- Int.add state.next_tyvar 1;
  TVar {
    var = Unbound (id, level);
  }

let fresh_binding_id = fun state ~name ->
  let stamp = state.next_binding_stamp in
  state.next_binding_stamp <- Int.add stamp 1;
  BindingId.local ~stamp ~name

let make_binding = fun state ~name ~ty ->
  let binding_id = fresh_binding_id state ~name in
  let entity_id = EntityId.resolved ~binding_id ~surface_path:name in
  { binding_id; entity_id; ty }

let rec prune = fun ty ->
  match ty with
  | TVar ({ var = Link linked_ty } as cell) ->
      let linked_ty = prune linked_ty in
      cell.var <- Link linked_ty;
      linked_ty
  | ty -> ty

let rec string_of_ty = fun ty ->
  match prune ty with
  | TInt -> "int"
  | TBool -> "bool"
  | TChar -> "char"
  | TString -> "string"
  | TFloat -> "float"
  | TUnit -> "unit"
  | TList element -> string_of_ty element ^ " list"
  | TOption element -> string_of_ty element ^ " option"
  | TTuple elements ->
      elements
      |> List.map ~fn:string_of_ty
      |> String.concat " * "
  | TArrow (parameter, result) -> string_of_ty parameter ^ " -> " ^ string_of_ty result
  | TVar { var = Unbound (id, _) } -> "'_" ^ Int.to_string id
  | TVar { var = Generic id } -> "'a" ^ Int.to_string id
  | TVar { var = Link linked_ty } -> string_of_ty linked_ty

exception Occurs

let rec occurs_adjust_levels = fun id level ty ->
  match prune ty with
  | TVar ({ var = Unbound (other_id, other_level) } as cell) ->
      if Int.equal id other_id then
        raise Occurs;
      if other_level > level then
        cell.var <- Unbound (other_id, level)
  | TVar { var = Generic _ } -> ()
  | TList element -> occurs_adjust_levels id level element
  | TOption element -> occurs_adjust_levels id level element
  | TTuple elements -> List.for_each elements ~fn:(occurs_adjust_levels id level)
  | TArrow (parameter, result) ->
      occurs_adjust_levels id level parameter;
      occurs_adjust_levels id level result
  | TInt
  | TBool
  | TChar
  | TString
  | TFloat
  | TUnit -> ()
  | TVar { var = Link linked_ty } -> occurs_adjust_levels id level linked_ty

let rec unify = fun state ~at left right ->
  match (prune left, prune right) with
  | (TVar left_cell, TVar right_cell) when Ptr.equal left_cell right_cell -> ()
  | (TInt, TInt)
  | (TBool, TBool)
  | (TChar, TChar)
  | (TString, TString)
  | (TFloat, TFloat)
  | (TUnit, TUnit) -> ()
  | (TList left, TList right) -> unify state ~at left right
  | (TOption left, TOption right) -> unify state ~at left right
  | (TTuple left, TTuple right) ->
      if Int.equal (List.length left) (List.length right) then
        List.zip left right
        |> List.for_each ~fn:(fun (left, right) -> unify state ~at left right)
      else
        add_diagnostic
          state
          (unsupported_type
            at
            ("tuple arity mismatch: expected "
            ^ Int.to_string (List.length left)
            ^ " but got "
            ^ Int.to_string (List.length right)))
  | (TArrow (left_parameter, left_result), TArrow (right_parameter, right_result)) ->
      unify state ~at left_parameter right_parameter;
      unify state ~at left_result right_result
  | (TVar ({ var = Unbound (id, level) } as cell), ty)
  | (ty, TVar ({ var = Unbound (id, level) } as cell)) -> (
      try
        occurs_adjust_levels id level ty;
        cell.var <- Link ty
      with
      | Occurs -> add_diagnostic state (unsupported_type at "occurs check failed")
    )
  | (TVar { var = Generic _ }, _)
  | (_, TVar { var = Generic _ }) ->
      add_diagnostic state (unsupported_type at "unexpected generic type variable")
  | (left, right) ->
      add_diagnostic
        state
        (unsupported_type at ("type mismatch: " ^ string_of_ty left ^ " vs " ^ string_of_ty right))

let rec generalize = fun level ty ->
  match prune ty with
  | TVar ({ var = Unbound (id, other_level) } as cell) when other_level > level ->
      cell.var <- Generic id;
      TVar cell
  | TList element -> TList (generalize level element)
  | TOption element -> TOption (generalize level element)
  | TTuple elements -> TTuple (List.map elements ~fn:(generalize level))
  | TArrow (parameter, result) -> TArrow (generalize level parameter, generalize level result)
  | ty -> ty

let instantiate = fun state ~level ty ->
  let subst = ref [] in
  let rec loop ty =
    match prune ty with
    | TVar { var = Generic id } -> (
        match List.find !subst ~fn:(fun (other_id, _) -> Int.equal id other_id) with
        | Some (_, replacement) -> replacement
        | None ->
            let replacement = fresh_tyvar state ~level in
            subst := (id, replacement) :: !subst;
            replacement
      )
    | TList element -> TList (loop element)
    | TOption element -> TOption (loop element)
    | TTuple elements -> TTuple (List.map elements ~fn:loop)
    | TArrow (parameter, result) -> TArrow (loop parameter, loop result)
    | ty -> ty
  in
  loop ty

let path_segments = fun path ->
  let segments = Vector.with_capacity ~size:(Ast.Node.child_count path) in
  Ast.Path.for_each_ident path ~fn:(fun token -> Vector.push segments ~value:(Ast.Token.text token));
  Vector.to_array segments
  |> Array.to_list

let surface_path_of_path = fun path ->
  path
  |> path_segments
  |> SurfacePath.of_segments

let surface_path_of_name_tokens = fun for_each_token ->
  let text = IO.Buffer.create ~size:16 in
  for_each_token ~fn:(fun token -> IO.Buffer.add_string text (Ast.Token.text token));
  IO.Buffer.contents text
  |> SurfacePath.of_name

let token_text_surface_path = fun token -> SurfacePath.of_name (Ast.Token.text token)

let path_int = SurfacePath.of_name "int"

let path_bool = SurfacePath.of_name "bool"

let path_char = SurfacePath.of_name "char"

let path_string = SurfacePath.of_name "string"

let path_float = SurfacePath.of_name "float"

let path_unit = SurfacePath.of_name "unit"

let path_list = SurfacePath.of_name "list"

let path_option = SurfacePath.of_name "option"

let path_none = SurfacePath.of_name "None"

let path_some = SurfacePath.of_name "Some"

let path_not = SurfacePath.of_name "not"

let path_plus = SurfacePath.of_name "+"

let path_minus = SurfacePath.of_name "-"

let path_star = SurfacePath.of_name "*"

let path_slash = SurfacePath.of_name "/"

let path_plus_dot = SurfacePath.of_name "+."

let path_minus_dot = SurfacePath.of_name "-."

let path_star_dot = SurfacePath.of_name "*."

let path_slash_dot = SurfacePath.of_name "/."

type builtin = {
  path: SurfacePath.t;
  ty: ty;
}

let builtin_bindings = [
  {
    path = path_not;
    ty = TArrow (TBool, TBool);
  };
  {
    path = path_plus;
    ty = TArrow (TInt, TArrow (TInt, TInt));
  };
  {
    path = path_minus;
    ty = TArrow (TInt, TArrow (TInt, TInt));
  };
  {
    path = path_star;
    ty = TArrow (TInt, TArrow (TInt, TInt));
  };
  {
    path = path_slash;
    ty = TArrow (TInt, TArrow (TInt, TInt));
  };
  {
    path = path_plus_dot;
    ty = TArrow (TFloat, TArrow (TFloat, TFloat));
  };
  {
    path = path_minus_dot;
    ty = TArrow (TFloat, TArrow (TFloat, TFloat));
  };
  {
    path = path_star_dot;
    ty = TArrow (TFloat, TArrow (TFloat, TFloat));
  };
  {
    path = path_slash_dot;
    ty = TArrow (TFloat, TArrow (TFloat, TFloat));
  };
]

let rec lookup_builtin = fun path builtins ->
  match builtins with
  | [] -> None
  | builtin :: rest ->
      if SurfacePath.equal builtin.path path then
        Some builtin.ty
      else
        lookup_builtin path rest

let rec public_type_of_ty = fun vars ty ->
  match prune ty with
  | TInt -> Typing_context.Int
  | TBool -> Typing_context.Bool
  | TChar -> Typing_context.Char
  | TString -> Typing_context.String
  | TFloat -> Typing_context.Float
  | TUnit -> Typing_context.Unit
  | TList element -> Typing_context.List (public_type_of_ty vars element)
  | TOption element -> Typing_context.Option (public_type_of_ty vars element)
  | TTuple elements -> Typing_context.Tuple (List.map elements ~fn:(public_type_of_ty vars))
  | TArrow (parameter, result) ->
      Typing_context.Arrow {
        parameter = public_type_of_ty vars parameter;
        result = public_type_of_ty vars result;
      }
  | TVar { var = Generic id } -> Typing_context.Var (public_tyvar_id vars id)
  | TVar { var = Unbound (id, _) } -> Typing_context.Var (public_tyvar_id vars id)
  | TVar { var = Link linked_ty } -> public_type_of_ty vars linked_ty

and public_tyvar_id = fun vars id ->
  match List.find !vars ~fn:(fun (other_id, _) -> Int.equal id other_id) with
  | Some (_, public_id) -> public_id
  | None ->
      let public_id = List.length !vars in
      vars := (id, public_id) :: !vars;
      public_id

let public_scheme_of_ty = fun ty ->
  let vars = ref [] in
  let body = public_type_of_ty vars ty in
  let forall =
    !vars
    |> List.map ~fn:(fun (_, public_id) -> public_id)
    |> List.reverse
  in
  { Typing_context.forall; body }

let public_binding_of_binding = fun binding -> {
  Typing_context.binding_id = binding.binding_id;
  entity_id = binding.entity_id;
  scheme = public_scheme_of_ty binding.ty;
}

let import_scheme = fun scheme ->
  let rec loop type_expr =
    match type_expr with
    | Typing_context.Int -> TInt
    | Typing_context.Bool -> TBool
    | Typing_context.Char -> TChar
    | Typing_context.String -> TString
    | Typing_context.Float -> TFloat
    | Typing_context.Unit -> TUnit
    | Typing_context.List element -> TList (loop element)
    | Typing_context.Option element -> TOption (loop element)
    | Typing_context.Tuple elements -> TTuple (List.map elements ~fn:loop)
    | Typing_context.Arrow { parameter; result } -> TArrow (loop parameter, loop result)
    | Typing_context.Var id -> TVar { var = Generic id }
  in
  let _ = scheme.Typing_context.forall in
  loop scheme.body

let env_of_typing_context = fun typing_context ->
  List.fold_left
    typing_context.Typing_context.values
    ~init:[]
    ~fn:(fun env (value_binding: Typing_context.value_binding) ->
      {
        binding_id = value_binding.binding_id;
        entity_id = value_binding.entity_id;
        ty = import_scheme value_binding.scheme;
      }
      :: env)

let rec lookup_env_binding = fun env surface_path ->
  match env with
  | [] -> None
  | binding :: rest ->
      if SurfacePath.equal (EntityId.surface_path binding.entity_id) surface_path then
        Some binding
      else
        lookup_env_binding rest surface_path

let lookup_surface_path = fun state env ~level ~at surface_path ->
  match lookup_env_binding env surface_path with
  | Some binding -> instantiate state ~level binding.ty
  | None -> (
      match lookup_builtin surface_path builtin_bindings with
      | Some ty -> instantiate state ~level ty
      | None ->
          add_diagnostic
            state
            (unsupported_type at ("unbound value " ^ SurfacePath.to_string surface_path));
          fresh_tyvar state ~level
    )

let literal_type_of_token = fun token ->
  match Ast.Token.kind token with
  | SyntaxKind.INT -> TInt
  | SyntaxKind.FLOAT -> TFloat
  | SyntaxKind.CHAR -> TChar
  | SyntaxKind.STRING -> TString
  | SyntaxKind.TRUE_KW
  | SyntaxKind.FALSE_KW -> TBool
  | _ -> TUnit

let rec collect_parameter_pattern = fun parameters pattern ->
  match Ast.Pattern.view pattern with
  | Ast.Pattern.Constraint { pattern = Some inner; _ } -> collect_parameter_pattern parameters inner
  | Ast.Pattern.Construct { constructor; payload = Some payload } ->
      Vector.push parameters ~value:constructor;
      collect_parameter_pattern parameters payload
  | _ -> Vector.push parameters ~value:pattern

let child_exprs = fun expr ->
  let children = Vector.with_capacity ~size:(Ast.Node.child_count expr) in
  Ast.Expr.for_each_child_expr expr ~fn:(fun child -> Vector.push children ~value:child);
  Vector.to_array children
  |> Array.to_list

let child_patterns = fun pattern ->
  let children = Vector.with_capacity ~size:(Ast.Node.child_count pattern) in
  Ast.Pattern.for_each_child_pattern pattern ~fn:(fun child -> Vector.push children ~value:child);
  Vector.to_array children
  |> Array.to_list

let let_binding_parameters = fun binding ->
  let parameters = Vector.with_capacity ~size:(Ast.Node.child_count binding) in
  Ast.LetBinding.for_each_parameter
    binding
    ~fn:(fun parameter -> Vector.push parameters ~value:parameter);
  Vector.to_array parameters
  |> Array.to_list

let let_binding_return_annotations = fun binding ->
  let annotations = Vector.with_capacity ~size:1 in
  let seen_first_pattern = ref false in
  Ast.Node.for_each_child_node
    binding
    ~fn:(fun node ->
      match Ast.Pattern.cast node with
      | Some pattern ->
          if !seen_first_pattern then
            (
              match Ast.Node.kind node with
              | Syn.SyntaxKind.CONSTRAINT_PATTERN -> (
                  match Ast.Pattern.view pattern with
                  | Ast.Pattern.Constraint { annotation = Some annotation; _ } ->
                      Vector.push annotations ~value:annotation
                  | _ -> ()
                )
              | _ -> ()
            )
          else
            seen_first_pattern := true
      | None -> ());
  Vector.to_array annotations
  |> Array.to_list

let fun_parameters = fun expr ->
  let parameters = Vector.with_capacity ~size:(Ast.Node.child_count expr) in
  Ast.Node.for_each_child_node
    expr
    ~fn:(fun node ->
      match Ast.Pattern.cast node with
      | Some pattern -> collect_parameter_pattern parameters pattern
      | None -> ());
  Vector.to_array parameters
  |> Array.to_list

let path_last_name = fun path ->
  match Ast.Path.last_ident path with
  | Some token -> Some (Ast.Token.text token)
  | None -> None

let is_uppercase_ascii = fun char ->
  let code = Char.code char in
  code >= Char.code 'A' && code <= Char.code 'Z'

let is_constructor_path = fun path ->
  match path_last_name path with
  | Some name when String.length name > 0 -> is_uppercase_ascii (String.get_unchecked name ~at:0)
  | _ -> false

let surface_path_is_qualified = fun path ->
  match SurfacePath.to_segments path with
  | _ :: _ :: _ -> true
  | _ -> false

let unsupported_qualified_path = fun node summary ->
  Diagnostics.Diagnostic.UnsupportedSyntax {
    span = span_of_node node;
    kind = SyntaxKind.FIELD_ACCESS_EXPR;
    summary;
  }

let rec lookup_type_var = fun vars name ->
  match vars with
  | [] -> None
  | (other_name, ty) :: rest ->
      if SurfacePath.equal name other_name then
        Some ty
      else
        lookup_type_var rest name

let type_expr_is_coercion_marker = fun type_expr ->
  match Ast.TypeExpr.view type_expr with
  | Ast.TypeExpr.Unknown node -> String.equal (Ast.Node.text node) ">"
  | _ -> false

let first_non_coercion_type_arg = fun args ->
  let rec loop index =
    if Int.(index >= Vector.length args) then
      None
    else
      let argument = Vector.get_unchecked args ~at:index in
      if type_expr_is_coercion_marker argument then
        loop (Int.add index 1)
      else
        Some argument
  in
  loop 0

let rec lower_core_type = fun state ~level vars type_expr ->
  match Ast.TypeExpr.view type_expr with
  | Ast.TypeExpr.Wildcard -> fresh_tyvar state ~level
  | Ast.TypeExpr.Var { name = Some name } -> (
      let name = token_text_surface_path name in
      match lookup_type_var vars name with
      | Some ty -> ty
      | None -> fresh_tyvar state ~level
    )
  | Ast.TypeExpr.Var { name = None } -> fresh_tyvar state ~level
  | Ast.TypeExpr.Ident { path } -> (
      let path = surface_path_of_path path in
      match path with
      | path when SurfacePath.equal path path_int -> TInt
      | path when SurfacePath.equal path path_bool -> TBool
      | path when SurfacePath.equal path path_char -> TChar
      | path when SurfacePath.equal path path_string -> TString
      | path when SurfacePath.equal path path_float -> TFloat
      | path when SurfacePath.equal path path_unit -> TUnit
      | _ ->
          add_diagnostic
            state
            (unsupported_type
              type_expr
              ("unsupported type constructor " ^ SurfacePath.to_string path));
          fresh_tyvar state ~level
    )
  | Ast.TypeExpr.Apply { ident; args } -> (
      let path = surface_path_of_path ident in
      if Int.equal (Vector.length args) 0 && SurfacePath.equal path path_unit then
        TUnit
      else
        match first_non_coercion_type_arg args with
        | Some argument ->
          let lowered_argument = lower_core_type state ~level vars argument in
          if SurfacePath.equal path path_list then
            TList lowered_argument
          else if SurfacePath.equal path path_option then
            TOption lowered_argument
          else (
            add_diagnostic
              state
              (unsupported_type
                type_expr
                ("unsupported type constructor " ^ SurfacePath.to_string path));
            fresh_tyvar state ~level
          )
        | None ->
          add_diagnostic
            state
            (unsupported_type
              ident
              ("unsupported type constructor " ^ SurfacePath.to_string path));
          fresh_tyvar state ~level
    )
  | Ast.TypeExpr.Arrow { arg = Some parameter_type; ret = Some result_type; _ } ->
      TArrow (
        lower_core_type state ~level vars parameter_type,
        lower_core_type state ~level vars result_type
      )
  | Ast.TypeExpr.Arrow _ ->
      add_diagnostic state (unsupported_type type_expr "incomplete arrow type");
      fresh_tyvar state ~level
  | Ast.TypeExpr.Tuple _ -> TTuple (lower_tuple_type_elements state ~level vars type_expr)
  | Ast.TypeExpr.Poly _ ->
      add_diagnostic state (unsupported_type type_expr "polymorphic annotation");
      fresh_tyvar state ~level
  | Ast.TypeExpr.Error node ->
      add_diagnostic state (unsupported_type node "invalid type");
      fresh_tyvar state ~level
  | Ast.TypeExpr.Unknown node ->
      add_diagnostic state (unsupported_type node "unsupported type");
      fresh_tyvar state ~level

and child_type_exprs = fun type_expr ->
  let children = Vector.with_capacity ~size:(Ast.Node.child_count type_expr) in
  Ast.TypeExpr.for_each_child_type type_expr ~fn:(fun child -> Vector.push children ~value:child);
  Vector.to_array children
  |> Array.to_list

and lower_tuple_type_elements = fun state ~level vars type_expr ->
  match Ast.TypeExpr.view type_expr with
  | Ast.TypeExpr.Tuple { parts } ->
      let items = Vector.with_capacity ~size:(Vector.length parts) in
      Vector.for_each
        parts
        ~fn:(fun part ->
          lower_tuple_type_elements state ~level vars part
          |> List.for_each ~fn:(fun item -> Vector.push items ~value:item));
      Vector.to_array items
      |> Array.to_list
  | _ -> [ lower_core_type state ~level vars type_expr ]

let extend_mono = fun (env: env) (bindings: binding list) ->
  List.fold_left
    bindings
    ~init:env
    ~fn:(fun extended_env binding -> binding :: extended_env)

let extend_generalized = fun (env: env) ~level (bindings: binding list) ->
  List.fold_left
    bindings
    ~init:env
    ~fn:(fun extended_env binding ->
      { binding with ty = generalize level binding.ty } :: extended_env)

let rec infer_pattern = fun state env ~level pattern ->
  let _ = env in
  match Ast.Pattern.view pattern with
  | Ast.Pattern.Unit -> (TUnit, [])
  | Ast.Pattern.Construct { constructor = path; payload } ->
      infer_constructor_pattern state env ~level pattern path payload
  | Ast.Pattern.Ident { path } when is_constructor_path path ->
      infer_constructor_pattern state env ~level pattern path None
  | Ast.Pattern.Ident { path } ->
      let ty = fresh_tyvar state ~level in
      let name = surface_path_of_path path in
      let binding = make_binding state ~name ~ty in
      (ty, [ binding ])
  | Ast.Pattern.Wildcard -> (fresh_tyvar state ~level, [])
  | Ast.Pattern.Literal { token = Some token } -> (literal_type_of_token token, [])
  | Ast.Pattern.Literal { token = None } -> (fresh_tyvar state ~level, [])
  | Ast.Pattern.Tuple { parts } ->
      let (element_types, binding_groups) =
        (
          Vector.to_array parts
          |> Array.to_list
        )
        |> List.map ~fn:(fun element -> infer_pattern state env ~level element)
        |> List.unzip
      in
      (TTuple element_types, List.concat binding_groups)
  | Ast.Pattern.Constraint { pattern = Some inner; annotation = Some annotation } ->
      let (pattern_ty, bindings) = infer_pattern state env ~level inner in
      let annotated = lower_core_type state ~level [] annotation in
      unify state ~at:pattern pattern_ty annotated;
      (pattern_ty, bindings)
  | Ast.Pattern.Constraint { pattern = Some inner; annotation = None } ->
      infer_pattern state env ~level inner
  | Ast.Pattern.Constraint _ -> (fresh_tyvar state ~level, [])
  | Ast.Pattern.Alias { pattern = inner; alias } ->
      let (pattern_ty, bindings) = infer_pattern state env ~level inner in
      let alias_name =
        match Ast.Pattern.view alias with
        | Ast.Pattern.Ident { path } -> surface_path_of_path path
        | _ -> SurfacePath.of_name (Ast.Node.text alias)
      in
      let alias_binding = make_binding state ~name:alias_name ~ty:pattern_ty in
      (pattern_ty, alias_binding :: bindings)
  | Ast.Pattern.List { items } ->
      let element_ty = fresh_tyvar state ~level in
      let bindings =
        (
          Vector.to_array items
          |> Array.to_list
        )
        |> List.flat_map
          ~fn:(fun element ->
            let (inferred_ty, bindings) = infer_pattern state env ~level element in
            unify state ~at:element element_ty inferred_ty;
            bindings)
      in
      (TList element_ty, bindings)
  | Ast.Pattern.Cons { head = Some head; tail = Some tail } ->
      let (head_ty, head_bindings) = infer_pattern state env ~level head in
      let (tail_ty, tail_bindings) = infer_pattern state env ~level tail in
      unify state ~at:pattern tail_ty (TList head_ty);
      (TList head_ty, List.append head_bindings tail_bindings)
  | Ast.Pattern.Cons _ ->
      add_diagnostic state (unsupported_syntax pattern "cons pattern");
      (fresh_tyvar state ~level, [])
  | Ast.Pattern.Lazy { pattern = Some inner } ->
      add_diagnostic state (unsupported_syntax pattern "lazy pattern");
      let _ = infer_pattern state env ~level inner in
      (fresh_tyvar state ~level, [])
  | Ast.Pattern.Lazy { pattern = None } ->
      add_diagnostic state (unsupported_syntax pattern "lazy pattern");
      (fresh_tyvar state ~level, [])
  | Ast.Pattern.Exception { pattern = Some inner } ->
      add_diagnostic state (unsupported_syntax pattern "exception pattern");
      let _ = infer_pattern state env ~level inner in
      (fresh_tyvar state ~level, [])
  | Ast.Pattern.Exception { pattern = None } ->
      add_diagnostic state (unsupported_syntax pattern "exception pattern");
      (fresh_tyvar state ~level, [])
  | Ast.Pattern.Interval _
  | Ast.Pattern.Or _ ->
      add_diagnostic state (unsupported_syntax pattern "operator pattern");
      (fresh_tyvar state ~level, [])
  | Ast.Pattern.Array _ ->
      add_diagnostic state (unsupported_syntax pattern "array pattern");
      (fresh_tyvar state ~level, [])
  | Ast.Pattern.Record _ ->
      add_diagnostic state (unsupported_syntax pattern "record pattern");
      (fresh_tyvar state ~level, [])
  | Ast.Pattern.PolyVariant _ ->
      add_diagnostic state (unsupported_syntax pattern "polymorphic variant pattern");
      (fresh_tyvar state ~level, [])
  | Ast.Pattern.FirstClassModule _ ->
      add_diagnostic state (unsupported_syntax pattern "first-class module pattern");
      (fresh_tyvar state ~level, [])
  | Ast.Pattern.Error node ->
      add_diagnostic state (unsupported_syntax node "invalid pattern");
      (fresh_tyvar state ~level, [])
  | Ast.Pattern.Unknown node ->
      if Syn.SyntaxKind.(Ast.Node.kind node = CONSTRUCT_PATTERN) then
        add_diagnostic state (unsupported_syntax node "operator pattern")
      else
        add_diagnostic state (unsupported_syntax node "unsupported pattern");
      (fresh_tyvar state ~level, [])

and infer_constructor_pattern = fun state env ~level pattern path argument ->
  let path = surface_path_of_path path in
  match (path, argument) with
  | (path, None) when SurfacePath.equal path path_none -> (TOption (fresh_tyvar state ~level), [])
  | (path, Some argument) when SurfacePath.equal path path_some ->
      let (argument_ty, bindings) = infer_pattern state env ~level argument in
      (TOption argument_ty, bindings)
  | (path, _) ->
      add_diagnostic
        state
        (unsupported_syntax
          pattern
          ("unsupported constructor pattern " ^ SurfacePath.to_string path));
      (fresh_tyvar state ~level, [])

let rec infer_expression = fun state env ~level expression ->
  match Ast.Expr.view expression with
  | Ast.Expr.Unit -> TUnit
  | Ast.Expr.Ident { path } when is_constructor_path path ->
      infer_constructor_expression state env ~level expression path None
  | Ast.Expr.Ident { path } ->
      let surface_path = surface_path_of_path path in
      if surface_path_is_qualified surface_path then (
        add_diagnostic state (unsupported_qualified_path expression "field access");
        fresh_tyvar state ~level
      ) else
        lookup_surface_path state env ~level ~at:expression surface_path
  | Ast.Expr.Literal { token = Some token } -> literal_type_of_token token
  | Ast.Expr.Literal { token = None } -> fresh_tyvar state ~level
  | Ast.Expr.Tuple { items } ->
      TTuple (
        List.map
          (
            Vector.to_array items
            |> Array.to_list
          )
          ~fn:(infer_expression state env ~level)
      )
  | Ast.Expr.List { items } ->
      let element_ty = fresh_tyvar state ~level in
      List.for_each
        (
          Vector.to_array items
          |> Array.to_list
        )
        ~fn:(fun element ->
          let inferred = infer_expression state env ~level element in
          unify state ~at:element element_ty inferred);
      TList element_ty
  | Ast.Expr.If { condition = Some condition; then_branch = Some then_branch; else_branch } ->
      let condition_ty = infer_expression state env ~level condition in
      unify state ~at:expression condition_ty TBool;
      let then_ty = infer_expression state env ~level then_branch in
      let else_ty =
        match else_branch with
        | Some else_branch -> infer_expression state env ~level else_branch
        | None -> TUnit
      in
      unify state ~at:expression then_ty else_ty;
      then_ty
  | Ast.Expr.If _ ->
      add_diagnostic state (unsupported_syntax expression "if expression");
      fresh_tyvar state ~level
  | Ast.Expr.Fun { body = Some body } ->
      infer_lambda state env ~level (fun_parameters expression) body
  | Ast.Expr.Fun { body = None } ->
      add_diagnostic state (unsupported_syntax expression "function expression");
      fresh_tyvar state ~level
  | Ast.Expr.Apply { callee; argument } -> (
      match Ast.Expr.view callee with
      | Ast.Expr.Ident { path } when is_constructor_path path ->
          infer_constructor_expression state env ~level expression path (Some argument)
      | _ ->
          let callee_ty = infer_expression state env ~level callee in
          let argument_ty = infer_apply_argument state env ~level argument in
          let result_ty = fresh_tyvar state ~level in
          unify state ~at:expression callee_ty (TArrow (argument_ty, result_ty));
          result_ty
    )
  | Ast.Expr.Let { first_binding = Some first_binding; body = Some body } ->
      infer_let_expression state env ~level expression first_binding body
  | Ast.Expr.Let _ ->
      add_diagnostic state (unsupported_syntax expression "let expression");
      fresh_tyvar state ~level
  | Ast.Expr.Annotated { expr = Some inner; annotation = Some annotation } ->
      let inferred = infer_expression state env ~level inner in
      let annotated = lower_core_type state ~level [] annotation in
      unify state ~at:expression inferred annotated;
      inferred
  | Ast.Expr.Annotated { expr = Some inner; annotation = None } ->
      infer_expression state env ~level inner
  | Ast.Expr.Annotated _ -> fresh_tyvar state ~level
  | Ast.Expr.Infix { left = Some left; operator = Some operator; right = Some right } ->
      infer_infix state env ~level expression left operator right
  | Ast.Expr.Infix _ ->
      if
        Syn.SyntaxKind.(Ast.Node.kind expression = ARRAY_INDEX_EXPR)
        || Syn.SyntaxKind.(Ast.Node.kind expression = STRING_INDEX_EXPR)
      then
        add_diagnostic state (unsupported_syntax expression "index expression")
      else
        add_diagnostic state (unsupported_syntax expression "operator expression");
      fresh_tyvar state ~level
  | Ast.Expr.Sequence { left = Some left; right = Some right } ->
      let _ = infer_expression state env ~level left in
      infer_expression state env ~level right
  | Ast.Expr.Sequence { left = Some left; right = None } -> infer_expression state env ~level left
  | Ast.Expr.Sequence _ -> TUnit
  | Ast.Expr.Prefix { operator = Some operator; operand = Some operand } ->
      infer_prefix state env ~level expression operator operand
  | Ast.Expr.Prefix _ ->
      add_diagnostic state (unsupported_syntax expression "operator expression");
      fresh_tyvar state ~level
  | Ast.Expr.Match _ ->
      add_diagnostic state (unsupported_syntax expression "match expression");
      fresh_tyvar state ~level
  | Ast.Expr.LocalOpen _
  | Ast.Expr.LetModule _
  | Ast.Expr.LetException _ ->
      add_diagnostic state (unsupported_syntax expression "local module binding");
      fresh_tyvar state ~level
  | Ast.Expr.Array _ ->
      add_diagnostic state (unsupported_syntax expression "array expression");
      fresh_tyvar state ~level
  | Ast.Expr.Record _ ->
      add_diagnostic state (unsupported_syntax expression "record expression");
      fresh_tyvar state ~level
  | Ast.Expr.PolyVariant _ ->
      add_diagnostic state (unsupported_syntax expression "polymorphic variant expression");
      fresh_tyvar state ~level
  | Ast.Expr.While _ ->
      add_diagnostic state (unsupported_syntax expression "while expression");
      fresh_tyvar state ~level
  | Ast.Expr.For _ ->
      add_diagnostic state (unsupported_syntax expression "for expression");
      fresh_tyvar state ~level
  | Ast.Expr.FieldAccess _ ->
      add_diagnostic state (unsupported_syntax expression "field access");
      fresh_tyvar state ~level
  | Ast.Expr.Assign _ ->
      add_diagnostic state (unsupported_syntax expression "assignment");
      fresh_tyvar state ~level
  | Ast.Expr.Try _ ->
      add_diagnostic state (unsupported_syntax expression "try expression");
      fresh_tyvar state ~level
  | Ast.Expr.Error node ->
      add_diagnostic state (unsupported_syntax node "invalid expression");
      fresh_tyvar state ~level
  | Ast.Expr.Unknown node ->
      if Option.is_some (Ast.FirstClassModuleExpr.cast node) then
        add_diagnostic state (unsupported_syntax node "first-class module expression")
      else
        add_diagnostic state (unsupported_syntax node "unsupported expression");
      fresh_tyvar state ~level

and infer_constructor_expression = fun state env ~level expression path payload ->
  let path = surface_path_of_path path in
  match (path, payload) with
  | (path, None) when SurfacePath.equal path path_none -> TOption (fresh_tyvar state ~level)
  | (path, Some payload) when SurfacePath.equal path path_some ->
      TOption (infer_expression state env ~level payload)
  | (path, _) ->
      let diagnostic =
        if surface_path_is_qualified path && Option.is_none payload then
          unsupported_qualified_path
            expression
            ("unsupported constructor " ^ SurfacePath.to_string path)
        else
          unsupported_syntax expression ("unsupported constructor " ^ SurfacePath.to_string path)
      in
      add_diagnostic state diagnostic;
      fresh_tyvar state ~level

and infer_infix = fun state env ~level expression left operator right ->
  let surface_path = token_text_surface_path operator in
  let callee_ty = lookup_surface_path state env ~level ~at:expression surface_path in
  let left_ty = infer_expression state env ~level left in
  let right_ty = infer_expression state env ~level right in
  let result_ty = fresh_tyvar state ~level in
  unify state ~at:expression callee_ty (TArrow (left_ty, TArrow (right_ty, result_ty)));
  result_ty

and infer_prefix = fun state env ~level expression operator operand ->
  let surface_path = token_text_surface_path operator in
  let callee_ty = lookup_surface_path state env ~level ~at:expression surface_path in
  let operand_ty = infer_expression state env ~level operand in
  let result_ty = fresh_tyvar state ~level in
  unify state ~at:expression callee_ty (TArrow (operand_ty, result_ty));
  result_ty

and infer_apply_argument = fun state env ~level argument ->
  if Syn.SyntaxKind.(Ast.Node.kind argument = LABELED_ARG) then (
    add_diagnostic state (unsupported_syntax argument "labeled argument");
    match child_exprs argument with
    | value :: _ -> infer_expression state env ~level value
    | [] -> fresh_tyvar state ~level
  ) else if Syn.SyntaxKind.(Ast.Node.kind argument = OPTIONAL_ARG) then (
    add_diagnostic state (unsupported_syntax argument "optional argument");
    match child_exprs argument with
    | value :: _ -> infer_expression state env ~level value
    | [] -> fresh_tyvar state ~level
  ) else
    infer_expression state env ~level argument

and infer_lambda = fun state env ~level parameters body ->
  match parameters with
  | [] -> infer_expression state env ~level body
  | parameter :: rest ->
      let (parameter_ty, parameter_bindings) = infer_parameter state env ~level parameter in
      let extended_env = extend_mono env parameter_bindings in
      let result_ty = infer_lambda state extended_env ~level rest body in
      TArrow (parameter_ty, result_ty)

and infer_parameter = fun state env ~level parameter ->
  match Ast.Parameter.cast parameter with
  | Some parameter_node -> (
      match Ast.Parameter.view parameter_node with
      | Ast.Parameter.Positional { pattern } ->
          if Option.is_some (Ast.LocallyAbstractTypePattern.cast pattern) then (
            add_diagnostic state (unsupported_syntax pattern "locally abstract parameter");
            (fresh_tyvar state ~level, [])
          ) else
            infer_pattern state env ~level pattern
      | Ast.Parameter.Labeled _ ->
          add_diagnostic state (unsupported_syntax parameter "labeled parameter");
          infer_labeled_parameter state env ~level parameter_node
      | Ast.Parameter.Optional _ ->
          add_diagnostic state (unsupported_syntax parameter "optional parameter");
          infer_labeled_parameter state env ~level parameter_node
      | Ast.Parameter.OptionalDefault _ ->
          add_diagnostic state (unsupported_syntax parameter "optional parameter");
          infer_optional_default_parameter state env ~level parameter_node
      | Ast.Parameter.Unknown _ -> (fresh_tyvar state ~level, [])
    )
  | None ->
      if Option.is_some (Ast.LocallyAbstractTypePattern.cast parameter) then (
        add_diagnostic state (unsupported_syntax parameter "locally abstract parameter");
        (fresh_tyvar state ~level, [])
      ) else
        infer_pattern state env ~level parameter

and infer_labeled_parameter = fun state env ~level parameter ->
  match Ast.Parameter.view parameter with
  | Ast.Parameter.Labeled { label = Some label; pattern = None }
  | Ast.Parameter.Optional { label = Some label; pattern = None } ->
      let ty = fresh_tyvar state ~level in
      let binding = make_binding state ~name:(token_text_surface_path label) ~ty in
      (ty, [ binding ])
  | Ast.Parameter.Labeled { pattern = Some pattern; _ }
  | Ast.Parameter.Optional { pattern = Some pattern; _ } -> infer_pattern state env ~level pattern
  | _ -> (fresh_tyvar state ~level, [])

and infer_optional_default_parameter = fun state env ~level parameter ->
  match Ast.Parameter.view parameter with
  | Ast.Parameter.OptionalDefault { label = Some label; pattern = None; default } ->
      let ty = fresh_tyvar state ~level in
      let binding = make_binding state ~name:(token_text_surface_path label) ~ty in
      (
        match default with
        | Some default ->
            let default_ty = infer_expression state env ~level default in
            unify state ~at:parameter ty default_ty
        | None -> ()
      );
      (ty, [ binding ])
  | Ast.Parameter.OptionalDefault { pattern = Some pattern; default; _ } ->
      let (ty, bindings) = infer_pattern state env ~level pattern in
      (
        match default with
        | Some default ->
            let default_ty = infer_expression state env ~level default in
            unify state ~at:parameter ty default_ty
        | None -> ()
      );
      (ty, bindings)
  | _ -> (fresh_tyvar state ~level, [])

and infer_let_expression = fun state env ~level syntax_node first_binding body ->
  let (extended_env, _) = infer_let_binding_like state env ~level ~syntax_node first_binding in
  infer_expression state extended_env ~level body

and infer_let_binding_like = fun state env ~level ~syntax_node binding ->
  let parameters = let_binding_parameters binding in
  let return_annotations = let_binding_return_annotations binding in
  let binding_view = Ast.LetBinding.view binding in
  let value_ty =
    match binding_view.body with
    | Some bound_value when List.is_empty parameters ->
        infer_expression
          state
          env
          ~level:(Int.add level 1)
          bound_value
    | Some bound_value -> infer_lambda
      state
      env
      ~level:(Int.add level 1)
      parameters
      bound_value
    | None -> fresh_tyvar state ~level:(Int.add level 1)
  in
  List.for_each
    return_annotations
    ~fn:(fun annotation ->
      let _ = lower_core_type state ~level [] annotation in
      ());
  let (pattern_ty, bindings) =
    match binding_view.pattern with
    | Some pattern -> infer_pattern state env ~level pattern
    | None -> (fresh_tyvar state ~level, [])
  in
  unify state ~at:syntax_node pattern_ty value_ty;
  let public_bindings = List.map bindings ~fn:public_binding_of_binding in
  let extended_env = extend_generalized env ~level bindings in
  (extended_env, public_bindings)

let check_let_declaration = fun state env ~level declaration ->
  let env_ref = ref env in
  let public_bindings = Vector.with_capacity ~size:(Ast.Node.child_count declaration) in
  if Option.is_some (Ast.LetDeclaration.rec_token declaration) then
    add_diagnostic state (unsupported_syntax declaration "recursive let binding");
  Ast.LetDeclaration.for_each_binding
    declaration
    ~fn:(fun binding ->
      let (next_env, bindings) =
        infer_let_binding_like state !env_ref ~level ~syntax_node:binding binding
      in
      env_ref := next_env;
      List.for_each bindings ~fn:(fun binding -> Vector.push public_bindings ~value:binding));
  (!env_ref, Vector.to_array public_bindings
  |> Array.to_list)

let infer_structure_item = fun state env ~level item ->
  match Ast.StructureItem.view item with
  | Ast.StructureItem.Let declaration -> check_let_declaration state env ~level declaration
  | Ast.StructureItem.Expr expr_item -> (
      match Ast.ExprItem.expr expr_item with
      | Some expression ->
          let _ = infer_expression state env ~level expression in
          (env, [])
      | None -> (env, [])
    )
  | Ast.StructureItem.Type (Ast.TypeDeclarationItem declaration) ->
      add_diagnostic
        state
        (unsupported_syntax_with_span
          ~span:(span_of_type_declaration declaration)
          ~kind:(Ast.Node.kind declaration)
          "type declaration");
      (env, [])
  | Ast.StructureItem.Type (Ast.TypeExtensionItem declaration) ->
      add_diagnostic state (unsupported_syntax declaration "type extension");
      (env, [])
  | Ast.StructureItem.External declaration -> (
      match Ast.ExternalDeclaration.type_annotation declaration with
      | Some type_ ->
          let ty = lower_core_type state ~level [] type_ in
          let name =
            surface_path_of_name_tokens (Ast.ExternalDeclaration.for_each_name_token declaration)
          in
          let binding = make_binding state ~name ~ty in
          let public_binding = public_binding_of_binding binding in
          let extended_env = { binding with ty = generalize level ty } :: env in
          (extended_env, [ public_binding ])
      | None -> (env, [])
    )
  | Ast.StructureItem.Exception declaration ->
      add_diagnostic state (unsupported_syntax declaration "exception declaration");
      (env, [])
  | Ast.StructureItem.Attribute declaration ->
      add_diagnostic state (unsupported_syntax declaration "attribute");
      (env, [])
  | Ast.StructureItem.Extension declaration ->
      add_diagnostic state (unsupported_syntax declaration "extension");
      (env, [])
  | Ast.StructureItem.Module _
  | Ast.StructureItem.ModuleType _
  | Ast.StructureItem.Open _
  | Ast.StructureItem.Include _ -> (env, [])
  | Ast.StructureItem.Error node ->
      add_diagnostic state (unsupported_syntax node "invalid structure item");
      (env, [])
  | Ast.StructureItem.Unknown node ->
      add_diagnostic state (unsupported_syntax node "unsupported structure item");
      (env, [])

let check_implementation = fun ~typing_context implementation ->
  let state = make_state ~next_binding_stamp:typing_context.Typing_context.next_binding_stamp in
  let env = env_of_typing_context typing_context in
  let env_ref = ref env in
  let bindings = Vector.with_capacity ~size:(Ast.Node.child_count implementation) in
  Ast.Implementation.for_each_item
    implementation
    ~fn:(fun item ->
      let (next_env, item_bindings) = infer_structure_item state !env_ref ~level:0 item in
      env_ref := next_env;
      List.for_each item_bindings ~fn:(fun binding -> Vector.push bindings ~value:binding));
  let bindings =
    Vector.to_array bindings
    |> Array.to_list
  in
  {
    File.diagnostics = List.reverse state.diagnostics;
    bindings;
    typing_context = {
      Typing_context.next_binding_stamp = state.next_binding_stamp;
      values = List.append typing_context.values bindings;
    };
  }

let check_signature_item = fun state env ~level item ->
  match Ast.SignatureItem.view item with
  | Ast.SignatureItem.Value declaration -> (
      match Ast.ValueDeclaration.type_annotation declaration with
      | Some type_ ->
          let ty = lower_core_type state ~level [] type_ in
          let name =
            surface_path_of_name_tokens (Ast.ValueDeclaration.for_each_name_token declaration)
          in
          let binding = make_binding state ~name ~ty in
          let public_binding = public_binding_of_binding binding in
          let extended_env = { binding with ty = generalize level ty } :: env in
          (extended_env, [ public_binding ])
      | None -> (env, [])
    )
  | Ast.SignatureItem.External declaration -> (
      match Ast.ExternalDeclaration.type_annotation declaration with
      | Some type_ ->
          let ty = lower_core_type state ~level [] type_ in
          let name =
            surface_path_of_name_tokens (Ast.ExternalDeclaration.for_each_name_token declaration)
          in
          let binding = make_binding state ~name ~ty in
          let public_binding = public_binding_of_binding binding in
          let extended_env = { binding with ty = generalize level ty } :: env in
          (extended_env, [ public_binding ])
      | None -> (env, [])
    )
  | Ast.SignatureItem.Type (Ast.TypeDeclarationItem declaration) ->
      add_diagnostic
        state
        (unsupported_syntax_with_span
          ~span:(span_of_type_declaration declaration)
          ~kind:(Ast.Node.kind declaration)
          "type declaration");
      (env, [])
  | Ast.SignatureItem.Type (Ast.TypeExtensionItem declaration) ->
      add_diagnostic state (unsupported_syntax declaration "type extension");
      (env, [])
  | Ast.SignatureItem.Exception declaration ->
      add_diagnostic state (unsupported_syntax declaration "exception declaration");
      (env, [])
  | Ast.SignatureItem.Attribute declaration ->
      add_diagnostic state (unsupported_syntax declaration "attribute");
      (env, [])
  | Ast.SignatureItem.Extension declaration ->
      add_diagnostic state (unsupported_syntax declaration "extension");
      (env, [])
  | Ast.SignatureItem.Module _
  | Ast.SignatureItem.ModuleType _
  | Ast.SignatureItem.Open _
  | Ast.SignatureItem.Include _ -> (env, [])
  | Ast.SignatureItem.Error node ->
      add_diagnostic state (unsupported_syntax node "invalid signature item");
      (env, [])
  | Ast.SignatureItem.Unknown node ->
      add_diagnostic state (unsupported_syntax node "unsupported signature item");
      (env, [])

let check_interface = fun ~typing_context interface ->
  let state = make_state ~next_binding_stamp:typing_context.Typing_context.next_binding_stamp in
  let env = env_of_typing_context typing_context in
  let env_ref = ref env in
  let bindings = Vector.with_capacity ~size:(Ast.Node.child_count interface) in
  Ast.Interface.for_each_item
    interface
    ~fn:(fun item ->
      let (next_env, item_bindings) = check_signature_item state !env_ref ~level:0 item in
      env_ref := next_env;
      List.for_each item_bindings ~fn:(fun binding -> Vector.push bindings ~value:binding));
  let bindings =
    Vector.to_array bindings
    |> Array.to_list
  in
  {
    File.diagnostics = List.reverse state.diagnostics;
    bindings;
    typing_context = {
      Typing_context.next_binding_stamp = state.next_binding_stamp;
      values = List.append typing_context.values bindings;
    };
  }

let check_source_file = fun ~typing_context (parse_result: Syn.Parser.parse_result) ->
  let source_file = Ast.SourceFile.make parse_result.tree in
  match Ast.SourceFile.view source_file with
  | Ast.SourceFile.Implementation implementation ->
      check_implementation ~typing_context implementation
  | Ast.SourceFile.Interface interface -> check_interface ~typing_context interface
  | Ast.SourceFile.Empty -> { File.empty with typing_context }

let check_expression = fun expression ->
  let state = make_state ~next_binding_stamp:0 in
  let _ = infer_expression state [] ~level:0 expression in
  List.reverse state.diagnostics

let check_pattern = fun pattern ->
  let state = make_state ~next_binding_stamp:0 in
  let _ = infer_pattern state [] ~level:0 pattern in
  List.reverse state.diagnostics

let check_let_binding = fun binding ->
  let state = make_state ~next_binding_stamp:0 in
  let _ = infer_let_binding_like state [] ~level:0 ~syntax_node:binding binding in
  List.reverse state.diagnostics

let check_core_type = fun type_expr ->
  let state = make_state ~next_binding_stamp:0 in
  let _ = lower_core_type state ~level:0 [] type_expr in
  List.reverse state.diagnostics
