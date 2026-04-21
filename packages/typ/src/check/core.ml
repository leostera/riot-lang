open Std
open Syn
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

and tyvar_cell = {
  mutable var: tvar;
}

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

let unsupported_syntax = fun syntax_node summary ->
  Diagnostics.Diagnostic.UnsupportedSyntax {
    span = Cst.token_body_span syntax_node;
    kind = Cst.syntax_kind syntax_node;
    summary
  }

let unsupported_type = fun syntax_node summary ->
  Diagnostics.Diagnostic.UnsupportedType { span = Cst.token_body_span syntax_node; summary }

let add_diagnostic = fun state diagnostic -> state.diagnostics <- diagnostic :: state.diagnostics

let make_state = fun ~next_binding_stamp -> { next_tyvar = 0; next_binding_stamp; diagnostics = [] }

let fresh_tyvar = fun state ~level ->
  let id = state.next_tyvar in
  state.next_tyvar <- state.next_tyvar + 1;
  TVar { var = Unbound (id, level) }

let fresh_binding_id = fun state ~name ->
  let stamp = state.next_binding_stamp in
  state.next_binding_stamp <- stamp + 1;
  BindingId.local ~stamp ~name

let make_binding = fun state ~name ~ty ->
  let binding_id = fresh_binding_id state ~name in
  let entity_id = EntityId.resolved ~binding_id ~surface_path:name in
  { binding_id; entity_id; ty }

let rec prune = fun ty ->
  match ty with
  | TVar ({ var=Link linked_ty } as cell) ->
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
  | TTuple elements -> elements |> List.map ~fn:string_of_ty |> String.concat " * "
  | TArrow (parameter, result) -> string_of_ty parameter ^ " -> " ^ string_of_ty result
  | TVar { var=Unbound (id, _) } -> "'_" ^ Int.to_string id
  | TVar { var=Generic id } -> "'a" ^ Int.to_string id
  | TVar { var=Link linked_ty } -> string_of_ty linked_ty

exception Occurs

let rec occurs_adjust_levels = fun id level ty ->
  match prune ty with
  | TVar ({ var=Unbound (other_id, other_level) } as cell) ->
      if Int.equal id other_id then
        raise Occurs;
      if other_level > level then
        cell.var <- Unbound (other_id, level)
  | TVar { var=Generic _ } ->
      ()
  | TList element ->
      occurs_adjust_levels id level element
  | TOption element ->
      occurs_adjust_levels id level element
  | TTuple elements ->
      List.for_each elements ~fn:(occurs_adjust_levels id level)
  | TArrow (parameter, result) ->
      occurs_adjust_levels id level parameter;
      occurs_adjust_levels id level result
  | TInt
  | TBool
  | TChar
  | TString
  | TFloat
  | TUnit ->
      ()
  | TVar { var=Link linked_ty } ->
      occurs_adjust_levels id level linked_ty

let rec unify = fun state ~at left right ->
  match prune left, prune right with
  | TVar left_cell, TVar right_cell when Ptr.equal left_cell right_cell ->
      ()
  | (TInt, TInt)
  | (TBool, TBool)
  | (TChar, TChar)
  | (TString, TString)
  | (TFloat, TFloat)
  | (TUnit, TUnit) ->
      ()
  | TList left, TList right ->
      unify state ~at left right
  | TOption left, TOption right ->
      unify state ~at left right
  | TTuple left, TTuple right ->
      if Int.equal (List.length left) (List.length right) then
        List.zip left right |> List.for_each ~fn:(fun (left, right) -> unify state ~at left right)
      else
        add_diagnostic
          state
          (unsupported_type
            at
            ("tuple arity mismatch: expected "
            ^ Int.to_string (List.length left)
            ^ " but got "
            ^ Int.to_string (List.length right)))
  | TArrow (left_parameter, left_result), TArrow (right_parameter, right_result) ->
      unify state ~at left_parameter right_parameter;
      unify state ~at left_result right_result
  | (TVar ({ var=Unbound (id, level) } as cell), ty)
  | (ty, TVar ({ var=Unbound (id, level) } as cell)) -> (
      try
        occurs_adjust_levels id level ty;
        cell.var <- Link ty
      with
      | Occurs -> add_diagnostic state (unsupported_type at "occurs check failed")
    )
  | (TVar { var=Generic _ }, _)
  | (_, TVar { var=Generic _ }) ->
      add_diagnostic state (unsupported_type at "unexpected generic type variable")
  | left, right ->
      add_diagnostic
        state
        (unsupported_type at ("type mismatch: " ^ string_of_ty left ^ " vs " ^ string_of_ty right))

let rec generalize = fun level ty ->
  match prune ty with
  | TVar ({ var=Unbound (id, other_level) } as cell) when other_level > level ->
      cell.var <- Generic id;
      TVar cell
  | TList element ->
      TList (generalize level element)
  | TOption element ->
      TOption (generalize level element)
  | TTuple elements ->
      TTuple (List.map elements ~fn:(generalize level))
  | TArrow (parameter, result) ->
      TArrow (generalize level parameter, generalize level result)
  | ty ->
      ty

let instantiate = fun state ~level ty ->
  let subst = ref [] in
  let rec loop ty =
    match prune ty with
    | TVar { var=Generic id } -> (
        match
          List.find !subst
            ~fn:(fun (other_id, _) ->
              Int.equal id other_id)
        with
        | Some (_, replacement) -> replacement
        | None ->
            let replacement = fresh_tyvar state ~level in
            subst := (id, replacement) :: !subst;
            replacement
      )
    | TList element ->
        TList (loop element)
    | TOption element ->
        TOption (loop element)
    | TTuple elements ->
        TTuple (List.map elements ~fn:loop)
    | TArrow (parameter, result) ->
        TArrow (loop parameter, loop result)
    | ty ->
        ty
  in
  loop ty

let surface_path_of_ident = fun ident ->
  ident |> Cst.Ident.segments |> List.map ~fn:Cst.Token.text |> SurfacePath.of_segments

let surface_path_of_name_tokens = fun tokens ->
  tokens |> List.map ~fn:Cst.Token.text |> String.concat "" |> SurfacePath.of_name

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
  { path = path_not; ty = TArrow (TBool, TBool) };
  { path = path_plus; ty = TArrow (TInt, TArrow (TInt, TInt)) };
  { path = path_minus; ty = TArrow (TInt, TArrow (TInt, TInt)) };
  { path = path_star; ty = TArrow (TInt, TArrow (TInt, TInt)) };
  { path = path_slash; ty = TArrow (TInt, TArrow (TInt, TInt)) };
  { path = path_plus_dot; ty = TArrow (TFloat, TArrow (TFloat, TFloat)) };
  { path = path_minus_dot; ty = TArrow (TFloat, TArrow (TFloat, TFloat)) };
  { path = path_star_dot; ty = TArrow (TFloat, TArrow (TFloat, TFloat)) };
  { path = path_slash_dot; ty = TArrow (TFloat, TArrow (TFloat, TFloat)) };
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
  | TArrow (parameter, result) -> Typing_context.Arrow {
    parameter = public_type_of_ty vars parameter;
    result = public_type_of_ty vars result
  }
  | TVar { var=Generic id } -> Typing_context.Var (public_tyvar_id vars id)
  | TVar { var=Unbound (id, _) } -> Typing_context.Var (public_tyvar_id vars id)
  | TVar { var=Link linked_ty } -> public_type_of_ty vars linked_ty

and public_tyvar_id = fun vars id ->
  match
    List.find !vars
      ~fn:(fun (other_id, _) ->
        Int.equal id other_id)
  with
  | Some (_, public_id) -> public_id
  | None ->
      let public_id = List.length !vars in
      vars := (id, public_id) :: !vars;
      public_id

let public_scheme_of_ty = fun ty ->
  let vars = ref [] in
  let body = public_type_of_ty vars ty in
  let forall = !vars |> List.map ~fn:(fun (_, public_id) -> public_id) |> List.reverse in
  { Typing_context.forall; body }

let public_binding_of_binding = fun binding ->
  {
    Typing_context.binding_id = binding.binding_id;
    entity_id = binding.entity_id;
    scheme = public_scheme_of_ty binding.ty
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
        ty = import_scheme value_binding.scheme
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

let literal_type = fun literal ->
  match literal with
  | Cst.Literal.Int _ -> TInt
  | Cst.Literal.Float _ -> TFloat
  | Cst.Literal.Char _ -> TChar
  | Cst.Literal.String _ -> TString
  | Cst.Literal.Bool _ -> TBool
  | Cst.Literal.Unit _ -> TUnit

let operator_surface_path = fun tokens ->
  tokens |> List.map ~fn:Cst.Token.text |> String.concat "" |> SurfacePath.of_name

let rec lookup_type_var = fun vars name ->
  match vars with
  | [] -> None
  | (other_name, ty) :: rest ->
      if SurfacePath.equal name other_name then
        Some ty
      else
        lookup_type_var rest name

let rec lower_core_type = fun state ~level vars core_type ->
  match core_type with
  | Cst.CoreType.Wildcard _ ->
      fresh_tyvar state ~level
  | Cst.CoreType.Var var -> (
      let name = SurfacePath.of_name (Cst.Token.text var.name_token) in
      match lookup_type_var vars name with
      | Some ty -> ty
      | None ->
          let ty = fresh_tyvar state ~level in
          ty
    )
  | Cst.CoreType.Constr { syntax_node; constructor_path; arguments } -> (
      let lowered_arguments = List.map arguments ~fn:(lower_core_type state ~level vars) in
      let path = surface_path_of_ident constructor_path in
      match lowered_arguments with
      | [] when SurfacePath.equal path path_int ->
          TInt
      | [] when SurfacePath.equal path path_bool ->
          TBool
      | [] when SurfacePath.equal path path_char ->
          TChar
      | [] when SurfacePath.equal path path_string ->
          TString
      | [] when SurfacePath.equal path path_float ->
          TFloat
      | [] when SurfacePath.equal path path_unit ->
          TUnit
      | [ element ] when SurfacePath.equal path path_list ->
          TList element
      | [ element ] when SurfacePath.equal path path_option ->
          TOption element
      | _ ->
          add_diagnostic
            state
            (unsupported_type
              syntax_node
              ("unsupported type constructor " ^ SurfacePath.to_string path));
          fresh_tyvar state ~level
    )
  | Cst.CoreType.Alias { type_; _ } ->
      lower_core_type state ~level vars type_
  | Cst.CoreType.Parenthesized { inner=type_; _ } ->
      lower_core_type state ~level vars type_
  | Cst.CoreType.Arrow { parameter_type; result_type; _ } ->
      TArrow (
        lower_core_type state ~level vars parameter_type,
        lower_core_type state ~level vars result_type
      )
  | Cst.CoreType.Tuple { elements; _ } ->
      TTuple (List.map elements ~fn:(lower_core_type state ~level vars))
  | Cst.CoreType.Attribute { syntax_node; type_; _ } ->
      add_diagnostic state (unsupported_type syntax_node "attributed type");
      lower_core_type state ~level vars type_
  | Cst.CoreType.Extension extension ->
      add_diagnostic state (unsupported_type extension.syntax_node "type extension");
      fresh_tyvar state ~level
  | Cst.CoreType.Poly { syntax_node; _ } ->
      add_diagnostic state (unsupported_type syntax_node "polymorphic annotation");
      fresh_tyvar state ~level
  | Cst.CoreType.Class { syntax_node; _ } ->
      add_diagnostic state (unsupported_type syntax_node "class type");
      fresh_tyvar state ~level
  | Cst.CoreType.PolyVariant poly_variant ->
      add_diagnostic state (unsupported_type poly_variant.syntax_node "polymorphic variant type");
      fresh_tyvar state ~level
  | Cst.CoreType.Record { syntax_node; _ } ->
      add_diagnostic state (unsupported_type syntax_node "record type");
      fresh_tyvar state ~level
  | Cst.CoreType.FirstClassModule { syntax_node; _ } ->
      add_diagnostic state (unsupported_type syntax_node "first-class module type");
      fresh_tyvar state ~level
  | Cst.CoreType.Object { syntax_node; _ } ->
      add_diagnostic state (unsupported_type syntax_node "object type");
      fresh_tyvar state ~level

let extend_mono = fun (env: env) (bindings: binding list) ->
  List.fold_left
    bindings
    ~init:env
    ~fn:(fun (extended_env: env) (binding: binding) -> binding :: extended_env)

let extend_generalized = fun (env: env) ~level (bindings: binding list) ->
  List.fold_left
    bindings
    ~init:env
    ~fn:(fun (extended_env: env) (binding: binding) ->
      { binding with ty = generalize level binding.ty } :: extended_env)

let rec infer_pattern = fun state env ~level pattern ->
  let _ = env in
  match pattern with
  | Cst.Pattern.Identifier identifier ->
      let ty = fresh_tyvar state ~level in
      let name = SurfacePath.of_name (Cst.Token.text identifier.name_token) in
      let binding = make_binding state ~name ~ty in
      (ty, [ binding ])
  | Cst.Pattern.Wildcard _ ->
      (fresh_tyvar state ~level, [])
  | Cst.Pattern.Literal literal ->
      (literal_type literal.literal, [])
  | Cst.Pattern.Tuple tuple ->
      let element_types, binding_groups = tuple.elements
      |> List.map
        ~fn:(fun (element: Cst.tuple_pattern_element) -> infer_pattern state env ~level element.pattern)
      |> List.unzip in
      (TTuple element_types, List.concat binding_groups)
  | Cst.Pattern.Parenthesized parenthesized ->
      infer_pattern state env ~level parenthesized.inner
  | Cst.Pattern.Typed typed ->
      let pattern_ty, bindings = infer_pattern state env ~level typed.pattern in
      let annotated = lower_core_type state ~level [] typed.type_ in
      unify state ~at:typed.syntax_node pattern_ty annotated;
      (pattern_ty, bindings)
  | Cst.Pattern.Alias alias ->
      let pattern_ty, bindings = infer_pattern state env ~level alias.pattern in
      let alias_name = SurfacePath.of_name (Cst.Token.text alias.name_token) in
      let alias_binding = make_binding state ~name:alias_name ~ty:pattern_ty in
      (pattern_ty, alias_binding :: bindings)
  | Cst.Pattern.List list_pattern ->
      let element_ty = fresh_tyvar state ~level in
      let bindings =
        list_pattern.elements
        |> List.flat_map
          ~fn:(fun element ->
            let inferred_ty, bindings = infer_pattern state env ~level element in
            unify state ~at:(Cst.Pattern.syntax_node element) element_ty inferred_ty;
            bindings)
      in
      (TList element_ty, bindings)
  | Cst.Pattern.Cons cons ->
      let head_ty, head_bindings = infer_pattern state env ~level cons.head in
      let tail_ty, tail_bindings = infer_pattern state env ~level cons.tail in
      unify state ~at:cons.syntax_node tail_ty (TList head_ty);
      (TList head_ty, List.append head_bindings tail_bindings)
  | Cst.Pattern.Constructor constructor -> (
      let path = surface_path_of_ident constructor.constructor_path in
      match path, constructor.arguments with
      | path, [] when SurfacePath.equal path path_none ->
          (TOption (fresh_tyvar state ~level), [])
      | path, [ argument ] when SurfacePath.equal path path_some ->
          let argument_ty, bindings = infer_pattern state env ~level argument in
          (TOption argument_ty, bindings)
      | path, _ ->
          add_diagnostic
            state
            (unsupported_syntax
              constructor.syntax_node
              ("unsupported constructor pattern " ^ SurfacePath.to_string path));
          (fresh_tyvar state ~level, [])
    )
  | Cst.Pattern.Extension extension ->
      add_diagnostic state (unsupported_syntax extension.syntax_node "pattern extension");
      (fresh_tyvar state ~level, [])
  | Cst.Pattern.Lazy lazy_pattern ->
      add_diagnostic state (unsupported_syntax lazy_pattern.syntax_node "lazy pattern");
      (fresh_tyvar state ~level, [])
  | Cst.Pattern.Exception exception_pattern ->
      add_diagnostic state (unsupported_syntax exception_pattern.syntax_node "exception pattern");
      (fresh_tyvar state ~level, [])
  | Cst.Pattern.Range range_pattern ->
      add_diagnostic state (unsupported_syntax range_pattern.syntax_node "range pattern");
      (fresh_tyvar state ~level, [])
  | Cst.Pattern.Operator operator_pattern ->
      add_diagnostic state (unsupported_syntax operator_pattern.syntax_node "operator pattern");
      (fresh_tyvar state ~level, [])
  | Cst.Pattern.FirstClassModule first_class_module_pattern ->
      add_diagnostic
        state
        (unsupported_syntax first_class_module_pattern.syntax_node "first-class module pattern");
      (fresh_tyvar state ~level, [])
  | Cst.Pattern.PolyVariant poly_variant_pattern ->
      add_diagnostic
        state
        (unsupported_syntax poly_variant_pattern.syntax_node "polymorphic variant pattern");
      (fresh_tyvar state ~level, [])
  | Cst.Pattern.PolyVariantInherit poly_variant_inherit_pattern ->
      add_diagnostic
        state
        (unsupported_syntax poly_variant_inherit_pattern.syntax_node "polymorphic variant inherit pattern");
      (fresh_tyvar state ~level, [])
  | Cst.Pattern.Array array_pattern ->
      add_diagnostic state (unsupported_syntax array_pattern.syntax_node "array pattern");
      (fresh_tyvar state ~level, [])
  | Cst.Pattern.Record record_pattern ->
      add_diagnostic state (unsupported_syntax record_pattern.syntax_node "record pattern");
      (fresh_tyvar state ~level, [])
  | Cst.Pattern.Or or_pattern ->
      add_diagnostic state (unsupported_syntax or_pattern.syntax_node "or pattern");
      (fresh_tyvar state ~level, [])
  | Cst.Pattern.Effect effect_pattern ->
      add_diagnostic state (unsupported_syntax effect_pattern.syntax_node "effect pattern");
      (fresh_tyvar state ~level, [])
  | Cst.Pattern.LocalOpen local_open_pattern ->
      add_diagnostic state (unsupported_syntax local_open_pattern.syntax_node "local open pattern");
      (fresh_tyvar state ~level, [])

let rec infer_expression = fun state env ~level expression ->
  match expression with
  | Cst.Expression.Path path_expression ->
      let surface_path = surface_path_of_ident path_expression.path in
      lookup_surface_path state env ~level ~at:(Cst.Ident.syntax_node path_expression.path) surface_path
  | Cst.Expression.Literal literal ->
      literal_type literal
  | Cst.Expression.Parenthesized parenthesized ->
      infer_expression state env ~level parenthesized.inner
  | Cst.Expression.Tuple tuple ->
      TTuple (List.map tuple.elements ~fn:(infer_expression state env ~level))
  | Cst.Expression.List list_expression ->
      let element_ty = fresh_tyvar state ~level in
      List.for_each list_expression.elements
        ~fn:(fun element ->
          let inferred = infer_expression state env ~level element in
          unify state ~at:(Cst.Expression.syntax_node element) element_ty inferred);
      TList element_ty
  | Cst.Expression.Constructor constructor -> (
      let path = surface_path_of_ident constructor.constructor_path in
      match path, constructor.payload with
      | path, None when SurfacePath.equal path path_none ->
          TOption (fresh_tyvar state ~level)
      | path, Some payload when SurfacePath.equal path path_some ->
          TOption (infer_expression state env ~level payload)
      | path, _ ->
          add_diagnostic
            state
            (unsupported_syntax
              constructor.syntax_node
              ("unsupported constructor " ^ SurfacePath.to_string path));
          fresh_tyvar state ~level
    )
  | Cst.Expression.If if_expression ->
      let condition_ty = infer_expression state env ~level if_expression.condition in
      unify state ~at:if_expression.syntax_node condition_ty TBool;
      let then_ty = infer_expression state env ~level if_expression.then_branch in
      let else_ty =
        match if_expression.else_branch with
        | Some else_branch -> infer_expression state env ~level else_branch
        | None -> TUnit
      in
      unify state ~at:if_expression.syntax_node then_ty else_ty;
      then_ty
  | Cst.Expression.Fun fun_expression ->
      infer_lambda state env ~level fun_expression.parameters fun_expression.body
  | Cst.Expression.Apply apply_expression ->
      let callee_ty = infer_expression state env ~level apply_expression.callee in
      let argument_ty = infer_apply_argument state env ~level apply_expression.argument in
      let result_ty = fresh_tyvar state ~level in
      unify state ~at:apply_expression.syntax_node callee_ty (TArrow (argument_ty, result_ty));
      result_ty
  | Cst.Expression.Let let_expression ->
      infer_let_expression state env ~level let_expression
  | Cst.Expression.TypeAscription type_ascription ->
      let inferred = infer_expression state env ~level type_ascription.expression in
      let annotated = lower_type_ascription state ~level type_ascription.kind in
      unify state ~at:type_ascription.syntax_node inferred annotated;
      inferred
  | Cst.Expression.Polymorphic polymorphic ->
      let inferred = infer_expression state env ~level polymorphic.expression in
      let annotated = lower_core_type state ~level [] polymorphic.type_ in
      unify state ~at:polymorphic.syntax_node inferred annotated;
      inferred
  | Cst.Expression.Infix infix ->
      infer_infix state env ~level infix
  | Cst.Expression.Sequence sequence ->
      infer_sequence state env ~level sequence.expressions
  | Cst.Expression.Assert assert_expression ->
      let inferred = infer_expression state env ~level assert_expression.asserted in
      unify state ~at:assert_expression.syntax_node inferred TBool;
      TUnit
  | Cst.Expression.Prefix prefix ->
      infer_prefix state env ~level prefix
  | Cst.Expression.Match match_expression ->
      add_diagnostic state (unsupported_syntax match_expression.syntax_node "match expression");
      fresh_tyvar state ~level
  | Cst.Expression.Function function_expression ->
      add_diagnostic state (unsupported_syntax function_expression.syntax_node "function expression");
      fresh_tyvar state ~level
  | Cst.Expression.Operator operator_expression ->
      let surface_path = operator_surface_path operator_expression.operator_tokens in
      lookup_surface_path state env ~level ~at:operator_expression.syntax_node surface_path
  | Cst.Expression.LocalOpen local_open ->
      add_diagnostic
        state
        (unsupported_syntax (syntax_node_of_local_open_expression local_open) "local open expression");
      fresh_tyvar state ~level
  | Cst.Expression.Array array_expression ->
      add_diagnostic state (unsupported_syntax array_expression.syntax_node "array expression");
      fresh_tyvar state ~level
  | Cst.Expression.Record record_expression ->
      add_diagnostic
        state
        (unsupported_syntax (syntax_node_of_record_expression record_expression) "record expression");
      fresh_tyvar state ~level
  | Cst.Expression.Object object_expression ->
      add_diagnostic state (unsupported_syntax object_expression.syntax_node "object expression");
      fresh_tyvar state ~level
  | Cst.Expression.PolyVariant poly_variant_expression ->
      add_diagnostic
        state
        (unsupported_syntax poly_variant_expression.syntax_node "polymorphic variant expression");
      fresh_tyvar state ~level
  | Cst.Expression.ModulePack module_pack_expression ->
      add_diagnostic
        state
        (unsupported_syntax module_pack_expression.syntax_node "first-class module expression");
      fresh_tyvar state ~level
  | Cst.Expression.LetModule let_module_expression ->
      add_diagnostic
        state
        (unsupported_syntax let_module_expression.syntax_node "local module binding");
      fresh_tyvar state ~level
  | Cst.Expression.LetException let_exception_expression ->
      add_diagnostic
        state
        (unsupported_syntax let_exception_expression.syntax_node "local exception binding");
      fresh_tyvar state ~level
  | Cst.Expression.Lazy lazy_expression ->
      add_diagnostic state (unsupported_syntax lazy_expression.syntax_node "lazy expression");
      fresh_tyvar state ~level
  | Cst.Expression.While while_expression ->
      add_diagnostic state (unsupported_syntax while_expression.syntax_node "while expression");
      fresh_tyvar state ~level
  | Cst.Expression.For for_expression ->
      add_diagnostic state (unsupported_syntax for_expression.syntax_node "for expression");
      fresh_tyvar state ~level
  | Cst.Expression.MethodCall method_call_expression ->
      add_diagnostic state (unsupported_syntax method_call_expression.syntax_node "method call");
      fresh_tyvar state ~level
  | Cst.Expression.New new_expression ->
      add_diagnostic state (unsupported_syntax new_expression.syntax_node "object instantiation");
      fresh_tyvar state ~level
  | Cst.Expression.FieldAccess field_access_expression ->
      add_diagnostic state (unsupported_syntax field_access_expression.syntax_node "field access");
      fresh_tyvar state ~level
  | Cst.Expression.Index index_expression ->
      add_diagnostic state (unsupported_syntax index_expression.syntax_node "index expression");
      fresh_tyvar state ~level
  | Cst.Expression.ObjectOverride object_override_expression ->
      add_diagnostic
        state
        (unsupported_syntax object_override_expression.syntax_node "object override");
      fresh_tyvar state ~level
  | Cst.Expression.InstanceVariableAssign instance_variable_assign_expression ->
      add_diagnostic
        state
        (unsupported_syntax instance_variable_assign_expression.syntax_node "instance variable assignment");
      fresh_tyvar state ~level
  | Cst.Expression.FieldAssign field_assign_expression ->
      add_diagnostic
        state
        (unsupported_syntax field_assign_expression.syntax_node "field assignment");
      fresh_tyvar state ~level
  | Cst.Expression.Assign assign_expression ->
      add_diagnostic state (unsupported_syntax assign_expression.syntax_node "assignment");
      fresh_tyvar state ~level
  | Cst.Expression.Unreachable unreachable_expression ->
      add_diagnostic
        state
        (unsupported_syntax unreachable_expression.syntax_node "unreachable expression");
      fresh_tyvar state ~level
  | Cst.Expression.Extension extension ->
      add_diagnostic state (unsupported_syntax extension.syntax_node "expression extension");
      fresh_tyvar state ~level
  | Cst.Expression.LetOperator let_operator_expression ->
      add_diagnostic
        state
        (unsupported_syntax let_operator_expression.syntax_node "binding operator expression");
      fresh_tyvar state ~level
  | Cst.Expression.Try try_expression ->
      add_diagnostic state (unsupported_syntax try_expression.syntax_node "try expression");
      fresh_tyvar state ~level

and infer_sequence = fun state env ~level expressions ->
  match expressions with
  | [] ->
      TUnit
  | [ expression ] ->
      infer_expression state env ~level expression
  | expression :: rest ->
      let _ = infer_expression state env ~level expression in
      infer_sequence state env ~level rest

and infer_infix = fun state env ~level infix ->
  let surface_path = operator_surface_path infix.operator_tokens in
  let callee_ty = lookup_surface_path state env ~level ~at:infix.syntax_node surface_path in
  let left_ty = infer_expression state env ~level infix.left in
  let right_ty = infer_expression state env ~level infix.right in
  let result_ty = fresh_tyvar state ~level in
  unify state ~at:infix.syntax_node callee_ty (TArrow (left_ty, TArrow (right_ty, result_ty)));
  result_ty

and infer_prefix = fun state env ~level prefix ->
  let surface_path = SurfacePath.of_name (Cst.Token.text prefix.operator_token) in
  let callee_ty = lookup_surface_path state env ~level ~at:prefix.syntax_node surface_path in
  let operand_ty = infer_expression state env ~level prefix.operand in
  let result_ty = fresh_tyvar state ~level in
  unify state ~at:prefix.syntax_node callee_ty (TArrow (operand_ty, result_ty));
  result_ty

and infer_apply_argument = fun state env ~level argument ->
  match argument with
  | Cst.Positional expression ->
      infer_expression state env ~level expression
  | Cst.Labeled labeled ->
      add_diagnostic state (unsupported_syntax labeled.syntax_node "labeled argument");
      (
        match labeled.value with
        | Some value -> infer_expression state env ~level value
        | None -> fresh_tyvar state ~level
      )
  | Cst.Optional optional ->
      add_diagnostic state (unsupported_syntax optional.syntax_node "optional argument");
      (
        match optional.value with
        | Some value -> infer_expression state env ~level value
        | None -> fresh_tyvar state ~level
      )

and infer_lambda = fun state env ~level parameters body ->
  match parameters with
  | [] -> infer_fun_body state env ~level body
  | parameter :: rest ->
      let parameter_ty, parameter_bindings = infer_parameter state env ~level parameter in
      let extended_env = extend_mono env parameter_bindings in
      let result_ty = infer_lambda state extended_env ~level rest body in
      TArrow (parameter_ty, result_ty)

and infer_fun_body = fun state env ~level body ->
  match body with
  | Expression expression -> infer_expression state env ~level expression
  | Cases case_body ->
      add_diagnostic state (unsupported_syntax case_body.syntax_node "function case body");
      fresh_tyvar state ~level

and infer_parameter = fun state env ~level parameter ->
  match parameter with
  | Cst.Parameter.Positional positional ->
      infer_pattern state env ~level positional.pattern
  | Cst.Parameter.Labeled labeled ->
      add_diagnostic state (unsupported_syntax labeled.syntax_node "labeled parameter");
      (
        match labeled.binding_pattern with
        | Some pattern -> infer_pattern state env ~level pattern
        | None ->
            let ty = fresh_tyvar state ~level in
            let name = SurfacePath.of_name (Cst.Token.text labeled.label_token) in
            let binding = make_binding state ~name ~ty in
            (ty, [ binding ])
      )
  | Cst.Parameter.Optional optional ->
      add_diagnostic state (unsupported_syntax optional.syntax_node "optional parameter");
      (
        match optional.binding_pattern with
        | Some binding_pattern ->
            let ty, bindings = infer_pattern state env ~level binding_pattern in
            (
              match optional.default_value with
              | Some default_value ->
                  let default_ty = infer_expression state env ~level default_value in
                  unify state ~at:optional.syntax_node ty default_ty
              | None -> ()
            );
            (ty, bindings)
        | None ->
            let ty = fresh_tyvar state ~level in
            let name = SurfacePath.of_name (Cst.Token.text optional.label_token) in
            let binding = make_binding state ~name ~ty in
            (ty, [ binding ])
      )
  | Cst.Parameter.LocallyAbstract locally_abstract ->
      add_diagnostic
        state
        (unsupported_syntax locally_abstract.syntax_node "locally abstract parameter");
      (fresh_tyvar state ~level, [])

and infer_let_expression = fun state env ~level let_expression ->
  let extended_env, _ = infer_let_binding_like
    state
    env
    ~level
    ~syntax_node:let_expression.syntax_node
    ~binding_pattern:let_expression.binding_pattern
    ~parameters:let_expression.parameters
    ~bound_value:let_expression.bound_value
    ~and_binding:let_expression.and_binding in
  infer_expression state extended_env ~level let_expression.body

and infer_let_binding_like = fun state env ~level ~syntax_node ~binding_pattern ~parameters ~bound_value ~and_binding ->
  let value_ty =
    if List.is_empty parameters then
      infer_expression state env ~level:(level + 1) bound_value
    else
      infer_lambda state env ~level:(level + 1) parameters (Expression bound_value)
  in
  let pattern_ty, bindings = infer_pattern state env ~level binding_pattern in
  unify state ~at:syntax_node pattern_ty value_ty;
  let public_bindings = List.map bindings ~fn:public_binding_of_binding in
  let extended_env = extend_generalized env ~level bindings in
  match and_binding with
  | Some binding ->
      let and_env, and_public_bindings = infer_let_binding_chain state extended_env ~level binding in
      (and_env, List.append public_bindings and_public_bindings)
  | None -> (extended_env, public_bindings)

and infer_let_binding_chain = fun state env ~level (binding: Cst.let_binding) ->
  if Option.is_some binding.rec_token then
    add_diagnostic state (unsupported_syntax binding.syntax_node "recursive let binding");
  infer_let_binding_like
    state
    env
    ~level
    ~syntax_node:binding.syntax_node
    ~binding_pattern:binding.binding_pattern
    ~parameters:binding.parameters
    ~bound_value:binding.value
    ~and_binding:binding.and_binding

and lower_type_ascription = fun state ~level kind ->
  match kind with
  | Cst.Type { type_; _ } ->
      lower_core_type state ~level [] type_
  | Cst.Coerce { type_; _ } ->
      lower_core_type state ~level [] type_
  | Cst.ConstraintCoerce { from_type; to_type; _ } ->
      let _ = lower_core_type state ~level [] from_type in
      lower_core_type state ~level [] to_type

and syntax_node_of_record_expression = fun record_expression ->
  match record_expression with
  | Cst.RecordExpression.Literal literal -> literal.syntax_node
  | Cst.RecordExpression.Update update -> update.syntax_node

and syntax_node_of_local_open_expression = fun local_open_expression ->
  match local_open_expression with
  | Cst.LetOpen { syntax_node; _ } -> syntax_node
  | Cst.Delimited { syntax_node; _ } -> syntax_node

let infer_structure_item = fun state env ~level item ->
  match item with
  | Cst.StructureItem.LetBinding binding ->
      infer_let_binding_chain state env ~level binding
  | Cst.StructureItem.Expression expression ->
      let _ = infer_expression state env ~level expression in
      (env, [])
  | Cst.StructureItem.TypeDeclaration declaration ->
      add_diagnostic
        state
        (unsupported_syntax (Cst.TypeDeclaration.syntax_node declaration) "type declaration");
      (env, [])
  | Cst.StructureItem.TypeExtension declaration ->
      add_diagnostic
        state
        (unsupported_syntax (Cst.TypeExtension.syntax_node declaration) "type extension");
      (env, [])
  | Cst.StructureItem.ExternalDeclaration declaration ->
      let ty = lower_core_type state ~level [] declaration.type_ in
      let name = surface_path_of_name_tokens declaration.name_tokens in
      let binding = make_binding state ~name ~ty in
      let public_binding = public_binding_of_binding binding in
      let extended_env = { binding with ty = generalize level ty } :: env in
      (extended_env, [ public_binding ])
  | Cst.StructureItem.ExceptionDeclaration declaration ->
      add_diagnostic state (unsupported_syntax declaration.syntax_node "exception declaration");
      (env, [])
  | Cst.StructureItem.Attribute attribute ->
      add_diagnostic state (unsupported_syntax attribute.syntax_node "attribute");
      (env, [])
  | Cst.StructureItem.Extension extension ->
      add_diagnostic state (unsupported_syntax extension.syntax_node "extension");
      (env, [])
  | Cst.StructureItem.ClassDeclaration declaration ->
      add_diagnostic
        state
        (unsupported_syntax (Cst.ClassDefinition.syntax_node declaration) "class declaration");
      (env, [])
  | Cst.StructureItem.ClassTypeDeclaration declaration ->
      add_diagnostic state (unsupported_syntax declaration.syntax_node "class type declaration");
      (env, [])
  | Cst.StructureItem.ModuleDeclaration _
  | Cst.StructureItem.ModuleTypeDeclaration _
  | Cst.StructureItem.OpenStatement _
  | Cst.StructureItem.IncludeStatement _ ->
      (env, [])
  | Cst.StructureItem.Docstring _
  | Cst.StructureItem.Comment _ ->
      (env, [])

let check_implementation = fun ~typing_context (implementation: Cst.implementation) ->
  let state = make_state ~next_binding_stamp:typing_context.Typing_context.next_binding_stamp in
  let env = env_of_typing_context typing_context in
  let _, bindings =
    List.fold_left implementation.items ~init:(env, [])
      ~fn:(fun (env, bindings) item ->
        let next_env, item_bindings = infer_structure_item state env ~level:0 item in
        (next_env, List.append bindings item_bindings))
  in
  {
    File.diagnostics = List.reverse state.diagnostics;
    bindings;
    typing_context = {
      Typing_context.next_binding_stamp = state.next_binding_stamp;
      values = List.append typing_context.values bindings
    }
  }

let check_signature_item = fun state env ~level item ->
  match item with
  | Cst.SignatureItem.ValueDeclaration declaration ->
      let ty = lower_core_type state ~level [] declaration.type_ in
      let name = surface_path_of_name_tokens declaration.name_tokens in
      let binding = make_binding state ~name ~ty in
      let public_binding = public_binding_of_binding binding in
      let extended_env = { binding with ty = generalize level ty } :: env in
      (extended_env, [ public_binding ])
  | Cst.SignatureItem.ExternalDeclaration declaration ->
      let ty = lower_core_type state ~level [] declaration.type_ in
      let name = surface_path_of_name_tokens declaration.name_tokens in
      let binding = make_binding state ~name ~ty in
      let public_binding = public_binding_of_binding binding in
      let extended_env = { binding with ty = generalize level ty } :: env in
      (extended_env, [ public_binding ])
  | Cst.SignatureItem.TypeDeclaration declaration ->
      add_diagnostic
        state
        (unsupported_syntax (Cst.TypeDeclaration.syntax_node declaration) "type declaration");
      (env, [])
  | Cst.SignatureItem.TypeExtension declaration ->
      add_diagnostic
        state
        (unsupported_syntax (Cst.TypeExtension.syntax_node declaration) "type extension");
      (env, [])
  | Cst.SignatureItem.ExceptionDeclaration declaration ->
      add_diagnostic state (unsupported_syntax declaration.syntax_node "exception declaration");
      (env, [])
  | Cst.SignatureItem.Attribute attribute ->
      add_diagnostic state (unsupported_syntax attribute.syntax_node "attribute");
      (env, [])
  | Cst.SignatureItem.Extension extension ->
      add_diagnostic state (unsupported_syntax extension.syntax_node "extension");
      (env, [])
  | Cst.SignatureItem.ClassDeclaration declaration ->
      add_diagnostic
        state
        (unsupported_syntax (Cst.ClassDeclaration.syntax_node declaration) "class declaration");
      (env, [])
  | Cst.SignatureItem.ClassTypeDeclaration declaration ->
      add_diagnostic state (unsupported_syntax declaration.syntax_node "class type declaration");
      (env, [])
  | Cst.SignatureItem.ModuleDeclaration _
  | Cst.SignatureItem.ModuleTypeDeclaration _
  | Cst.SignatureItem.OpenStatement _
  | Cst.SignatureItem.IncludeStatement _ ->
      (env, [])
  | Cst.SignatureItem.Docstring _
  | Cst.SignatureItem.Comment _ ->
      (env, [])

let check_interface = fun ~typing_context (interface: Cst.interface) ->
  let state = make_state ~next_binding_stamp:typing_context.Typing_context.next_binding_stamp in
  let env = env_of_typing_context typing_context in
  let _, bindings =
    List.fold_left interface.items ~init:(env, [])
      ~fn:(fun (env, bindings) item ->
        let next_env, item_bindings = check_signature_item state env ~level:0 item in
        (next_env, List.append bindings item_bindings))
  in
  {
    File.diagnostics = List.reverse state.diagnostics;
    bindings;
    typing_context = {
      Typing_context.next_binding_stamp = state.next_binding_stamp;
      values = List.append typing_context.values bindings
    }
  }

let check_source_file = fun ~typing_context source_file ->
  match source_file with
  | Cst.Implementation implementation -> check_implementation ~typing_context implementation
  | Cst.Interface interface -> check_interface ~typing_context interface

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
  let _ = infer_let_binding_chain state [] ~level:0 binding in
  List.reverse state.diagnostics

let check_core_type = fun core_type ->
  let state = make_state ~next_binding_stamp:0 in
  let _ = lower_core_type state ~level:0 [] core_type in
  List.reverse state.diagnostics
