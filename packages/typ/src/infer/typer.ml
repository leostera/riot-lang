open Std
open Ast
open TypeScheme

let unify (state: State.t) ~expected ~actual ~on_error =
  match Unifier.unify ~expected ~actual with
  | Ok () -> ()
  | Error err -> Diagnostics.add (State.diagnostics state) (on_error err)

let annotation_diagnostic (annotation: core_type) err =
  match err with
  | Unifier.TypeMismatch { expected; actual } ->
      Diagnostics.Diagnostic.annotation_mismatch
        ~span:annotation.origin.span
        ~annotation_span:annotation.origin.span
        ~expected:(Type.to_string expected)
        ~actual:(Type.to_string actual)
  | Unifier.InfiniteSubstitution { var; type_ } ->
      Diagnostics.Diagnostic.infinite_substitution
        ~span:annotation.origin.span
        ~var:(TypeVar.to_string var.id)
        ~type_:(Type.to_string type_)

let expression_hint_diagnostic (expr: expression) (hint: expression_type_hint) err =
  match err with
  | Unifier.TypeMismatch { expected; actual } ->
      Diagnostics.Diagnostic.annotation_mismatch
        ~span:expr.origin.span
        ~annotation_span:hint.type_.origin.span
        ~expected:(Type.to_string expected)
        ~actual:(Type.to_string actual)
  | Unifier.InfiniteSubstitution { var; type_ } ->
      Diagnostics.Diagnostic.infinite_substitution
        ~span:expr.origin.span
        ~var:(TypeVar.to_string var.id)
        ~type_:(Type.to_string type_)

let expression_diagnostic (expr: expression) err =
  match err with
  | Unifier.TypeMismatch { expected; actual } ->
      Diagnostics.Diagnostic.annotation_mismatch
        ~span:expr.origin.span
        ~annotation_span:expr.origin.span
        ~expected:(Type.to_string expected)
        ~actual:(Type.to_string actual)
  | Unifier.InfiniteSubstitution { var; type_ } ->
      Diagnostics.Diagnostic.infinite_substitution
        ~span:expr.origin.span
        ~var:(TypeVar.to_string var.id)
        ~type_:(Type.to_string type_)

module Builtin = struct
  open Model

  let make ?(arguments = []) name =
    let ident = Model.Surface_path.from_name name in
    Type.Constructor { ident; arguments }

  let int = make "int"

  let bool = make "bool"

  let float = make "float"

  let char = make "char"

  let string = make "string"

  let unit = make "unit"

  let list el = make "list" ~arguments:[ el ]
end

let arrow_label_to_type_label = function
  | NoLabel -> Type.Label.NoLabel
  | Labelled label -> Type.Label.Labelled label
  | Optional label -> Type.Label.Optional label

let rec core_type_to_type (state: State.t) (annotation: core_type) =
  let type_ =
    match annotation.kind with
    | TypeIdent ident -> Type.Constructor { ident; arguments = [] }
    | Apply { constructor; arguments } -> (
        let constructor = core_type_to_type state constructor in
        let arguments = List.map arguments ~fn:(core_type_to_type state) in
        match Unifier.resolve constructor with
        | Type.Constructor { ident; arguments = existing_arguments } ->
            Type.Constructor { ident; arguments = List.append existing_arguments arguments }
        | _ -> State.fresh_var state
      )
    | Arrow { label; parameter; result } ->
        Type.Arrow {
          label = arrow_label_to_type_label label;
          parameter = core_type_to_type state parameter;
          result = core_type_to_type state result;
        }
    | Tuple parts -> Type.Tuple (List.map parts ~fn:(core_type_to_type state))
    | Parenthesized inner
    | ForAll { body = inner; _ } -> core_type_to_type state inner
    | Wildcard
    | Var _
    | PolyVariant _
    | Package _ -> State.fresh_var state
  in
  annotation.type_ <- Some type_;
  type_

let rec bind_pattern ~mode (state: State.t) (pattern: pattern) type_ =
  pattern.type_ <- Some type_;
  match pattern.kind with
  | Bind name ->
      let scheme =
        match mode with
        | Local -> TypeScheme.monomorphic type_
        | Generalized -> Quantifier.generalize type_
      in
      State.add_value state ~name ~scheme
  | Constraint { pattern; annotation } ->
      let expected = core_type_to_type state annotation in
      unify state ~expected ~actual:type_ ~on_error:(annotation_diagnostic annotation);
      bind_pattern ~mode state pattern expected
  | Attribute pattern -> bind_pattern ~mode state pattern type_
  | Alias { pattern; alias } ->
      bind_pattern ~mode state pattern type_;
      bind_pattern ~mode state alias type_
  | _ -> ()

let infer_literal _state (lit: literal) =
  let open Builtin in
  match lit with
  | Int -> int
  | Float -> float
  | Char -> char
  | String -> string
  | Bool -> bool

let infer_ident (state: State.t) ident =
  match State.get_value state ~name:ident with
  | Some scheme -> Quantifier.instantiate state scheme
  | None -> State.fresh_var state

let infer_constructor (state: State.t) constructor =
  match State.get_constructor state ~name:constructor.ident with
  | Some scheme -> Quantifier.instantiate state scheme
  | None -> State.fresh_var state

let infer_function_param state (param: parameter) =
  let param_type = State.fresh_var state in
  bind_pattern ~mode:Local state param.pattern param_type;
  (
    match param.annotation with
    | Some hint ->
        let expected = core_type_to_type state hint in
        unify state ~expected ~actual:param_type ~on_error:(annotation_diagnostic hint)
    | None -> ()
  );
  param_type

let rec infer_expression (state: State.t) (expr: expression) =
  let inferred =
    match expr.kind with
    | If ifelse -> infer_if_else state ifelse
    | Function fn -> infer_function state fn
    | Apply apply -> infer_apply state apply
    | Literal lit -> infer_literal state lit
    | Ident ident -> infer_ident state ident
    | Constructor constructor -> infer_constructor state constructor
    | Tuple parts -> infer_tuple state parts
    | List items -> infer_list state items
    | _ -> State.fresh_var state
  in
  let unified =
    match expr.type_hint with
    | None -> inferred
    | Some hint ->
        let expected = core_type_to_type state hint.type_ in
        unify
          state
          ~expected
          ~actual:inferred
          ~on_error:(expression_hint_diagnostic expr hint);
        expected
  in
  expr.type_ <- Some unified;
  unified

(**
   When inferring lists, we will start with a fresh variable and unify it
   against every list element type.
*)
and infer_list state items =
  let element = State.fresh_var state in
  List.for_each
    items
    ~fn:(fun item ->
      let actual = infer_expression state item in
      unify state ~expected:element ~actual ~on_error:(expression_diagnostic item));
  Builtin.list element

and infer_apply state apply =
  let callee = infer_expression state apply.callee in
  let result = State.fresh_var state in
  let expected =
    List.fold_right
      apply.arguments
      ~init:result
      ~fn:(fun arg ret ->
        let arg_type =
          match arg.kind with
          | Positional expr -> infer_expression state expr
          | _ -> State.fresh_var state
        in
        Ast.Type.arrow arg_type ret)
  in
  unify state ~expected ~actual:callee ~on_error:(expression_diagnostic apply.callee);
  result

and infer_if_else state ifelse =
  let condition = infer_expression state ifelse.condition in
  unify
    state
    ~expected:Builtin.bool
    ~actual:condition
    ~on_error:(expression_diagnostic ifelse.condition);
  let then_ = infer_expression state ifelse.then_branch in
  let else_ =
    match ifelse.else_branch with
    | Some else_ -> infer_expression state else_
    | None -> Builtin.unit
  in
  unify state ~expected:then_ ~actual:else_ ~on_error:(expression_diagnostic ifelse.then_branch);
  then_

and infer_tuple (state: State.t) parts =
  let types = List.map ~fn:(infer_expression state) parts in
  Type.Tuple types

and infer_function state fn_decl =
  State.push_scope state;
  let params = List.map ~fn:(infer_function_param state) fn_decl.parameters in
  let body =
    match fn_decl.body with
    | Body expr -> infer_expression state expr
    | Cases _ -> State.fresh_var state
  in
  State.pop_scope state;
  List.fold_right params ~init:body ~fn:Ast.Type.arrow

let type_let_binding (state: State.t) (lb: let_binding) =
  let type_ = infer_expression state lb.body in
  bind_pattern ~mode:Generalized state lb.pattern type_

let type_let_decl (state: State.t) (ld: let_declaration) =
  List.for_each ld.bindings ~fn:(type_let_binding state)

let type_impl_item (state: State.t) (item: structure_item) =
  match item.kind with
  | Let ld -> type_let_decl state ld
  | _ -> ()

let type_impl (state: State.t) (items: structure_item list) =
  List.for_each items ~fn:(type_impl_item state)

let type_intf (_state: State.t) (_items: signature_item list) = ()

let type_ast (state: State.t) (ast: Ast.t) =
  match ast.kind with
  | Implementation items -> type_impl state items
  | Interface items -> type_intf state items
