open Std
open Ast
open TypeScheme

module HashMap = Std.Collections.HashMap
module InferenceEnv = State.InferenceEnv
module SurfacePath = Model.Surface_path

let unify (state: State.t) ~expected ~actual ~on_error =
  match Unifier.unify ~expected ~actual with
  | Ok () -> ()
  | Error err -> State.add_diagnostic state (on_error err)

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

let expression_constraint_diagnostic (expr: expression) err =
  match err with
  | Unifier.TypeMismatch { expected; actual } ->
      Diagnostics.Diagnostic.type_mismatch
        ~span:expr.origin.span
        ~expected:(Type.to_string expected)
        ~actual:(Type.to_string actual)
  | Unifier.InfiniteSubstitution { var; type_ } ->
      Diagnostics.Diagnostic.infinite_substitution
        ~span:expr.origin.span
        ~var:(TypeVar.to_string var.id)
        ~type_:(Type.to_string type_)

let pattern_constraint_diagnostic (pat: pattern) err =
  match err with
  | Unifier.TypeMismatch { expected; actual } ->
      Diagnostics.Diagnostic.type_mismatch
        ~span:pat.origin.span
        ~expected:(Type.to_string expected)
        ~actual:(Type.to_string actual)
  | Unifier.InfiniteSubstitution { var; type_ } ->
      Diagnostics.Diagnostic.infinite_substitution
        ~span:pat.origin.span
        ~var:(TypeVar.to_string var.id)
        ~type_:(Type.to_string type_)

module Builtin = struct
  let ident name =
    Model.Surface_path.from_parts [ name ]
    |> Result.expect ~msg:("expected builtin identifier " ^ name)

  let int_ident = ident "int"

  let bool_ident = ident "bool"

  let float_ident = ident "float"

  let char_ident = ident "char"

  let string_ident = ident "string"

  let unit_ident = ident "unit"

  let list_ident = ident "list"

  let make ?(arguments = []) ident = Type.Apply { ident; arguments }

  let int = make int_ident

  let bool = make bool_ident

  let float = make float_ident

  let char = make char_ident

  let string = make string_ident

  let unit = make unit_ident

  let list el = make list_ident ~arguments:[ el ]

  let is_unit ident = Model.Surface_path.equal ident unit_ident
end

let arrow_label_to_type_label = function
  | NoLabel -> Type.Label.NoLabel
  | Labelled label -> Type.Label.Labelled label
  | Optional label -> Type.Label.Optional label

let rec core_type_to_type (state: State.t) (annotation: core_type) =
  let type_ =
    match annotation.kind with
    | TypeIdent ident -> Type.Apply { ident; arguments = [] }
    | Apply { constructor; arguments } -> (
        let constructor = core_type_to_type state constructor in
        let arguments = List.map arguments ~fn:(core_type_to_type state) in
        match Unifier.resolve constructor with
        | Type.Apply { ident; arguments = existing_arguments } ->
            Type.Apply { ident; arguments = List.append existing_arguments arguments }
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
    | Var (Some name) -> (
        match State.get_type_param state ~name with
        | Some type_ -> type_
        | None -> State.fresh_var state
      )
    | Var None -> State.fresh_var state
    | Wildcard
    | PolyVariant _
    | Package _ -> State.fresh_var state
  in
  annotation.type_ <- Some type_;
  type_

let type_declaration_to_type ?(arguments = []) (decl: type_declaration) =
  Type.Apply { ident = decl.name; arguments }

let inline_record_payload_ident (decl: type_declaration) (ctr: type_constructor) =
  let owner = SurfacePath.to_segments decl.name in
  let constructor = SurfacePath.to_string ctr.name in
  SurfacePath.from_parts (owner @ [ "$" ^ constructor ])
  |> Result.expect ~msg:"inline record constructor payload paths are non-empty"

let fresh_type_parameters state parameters =
  let scope = HashMap.with_capacity ~size:(List.length parameters) in
  let arguments =
    List.map
      parameters
      ~fn:(fun param ->
        match param with
        | Some name ->
            let type_ = State.fresh_var state in
            let _ = HashMap.insert scope ~key:name ~value:type_ in
            type_
        | None -> State.fresh_var state)
  in
  (scope, arguments)

let generalize_type type_ = (Quantifier.generalize type_).body

let generalize_constructor_arguments arguments =
  match arguments with
  | InferenceEnv.Tuple arguments -> InferenceEnv.Tuple (List.map arguments ~fn:generalize_type)
  | InferenceEnv.InlineRecord inline_record ->
      InferenceEnv.InlineRecord {
        inline_record with
        payload_type = generalize_type inline_record.payload_type;
        fields = List.map
          inline_record.fields
          ~fn:(fun (field: InferenceEnv.inline_record_field) -> {
            field with
            type_ = generalize_type field.type_;
          });
      }

let instantiate_constructor_description state (description: InferenceEnv.constructor_description) =
  let substitutions = HashMap.with_capacity ~size:8 in
  let fresh_for_generic id =
    match HashMap.get substitutions ~key:id with
    | Some type_ -> type_
    | None ->
        let type_ = State.fresh_var state in
        let _ = HashMap.insert substitutions ~key:id ~value:type_ in
        type_
  in
  let rec instantiate_type type_ =
    match Unifier.resolve type_ with
    | Type.Generic id -> fresh_for_generic id
    | Type.Var _ as type_ -> type_
    | Type.Tuple parts -> Type.Tuple (List.map parts ~fn:instantiate_type)
    | Type.Arrow arrow ->
        Type.Arrow {
          arrow with
          parameter = instantiate_type arrow.parameter;
          result = instantiate_type arrow.result;
        }
    | Type.Apply application ->
        Type.Apply {
          application with
          arguments = List.map application.arguments ~fn:instantiate_type;
        }
  in
  let instantiate_arguments arguments =
    match arguments with
    | InferenceEnv.Tuple arguments -> InferenceEnv.Tuple (List.map arguments ~fn:instantiate_type)
    | InferenceEnv.InlineRecord inline_record ->
        InferenceEnv.InlineRecord {
          inline_record with
          payload_type = instantiate_type inline_record.payload_type;
          fields = List.map
            inline_record.fields
            ~fn:(fun (field: InferenceEnv.inline_record_field) -> {
              field with
              type_ = instantiate_type field.type_;
            });
        }
  in
  {
    description with
    scheme = TypeScheme.monomorphic (instantiate_type description.scheme.body);
    result = instantiate_type description.result;
    arguments = instantiate_arguments description.arguments;
  }

let instantiate_constructor state ident =
  State.get_constructor state ~name:ident
  |> Option.map ~fn:(instantiate_constructor_description state)

let instantiate_record_field state (info: State.InferenceEnv.record_field_info) =
  let (scope, arguments) = fresh_type_parameters state info.owner.parameters in
  State.with_type_params
    state
    scope
    (fun state ->
      let owner_type = type_declaration_to_type ~arguments info.owner in
      let field_type = core_type_to_type state info.field.type_annotation in
      (owner_type, field_type))

let bind_value ~mode state ~name type_ =
  let scheme =
    match mode with
    | Local -> TypeScheme.monomorphic type_
    | Generalized -> Quantifier.generalize type_
  in
  State.add_value state ~name ~scheme

let rec bind_pattern ~mode (state: State.t) (pattern: pattern) type_ =
  pattern.type_ <- Some type_;
  match pattern.kind with
  | Constructor ctr -> bind_constructor state pattern ctr type_
  | Bind name -> bind_value ~mode state ~name type_
  | Constraint { pattern; annotation } ->
      let expected = core_type_to_type state annotation in
      unify state ~expected ~actual:type_ ~on_error:(annotation_diagnostic annotation);
      bind_pattern ~mode state pattern expected
  | Attribute pattern -> bind_pattern ~mode state pattern type_
  | Alias { pattern; alias } ->
      bind_pattern ~mode state pattern type_;
      bind_pattern ~mode state alias type_
  | Tuple parts -> bind_tuple ~mode state pattern parts type_
  | Record record -> bind_record ~mode state pattern record type_
  | _ -> ()

and bind_record_field ~mode state pattern record_type (field: record_pattern_field) =
  match State.get_record_field state ~name:field.name with
  | None -> ()
  | Some info ->
      let (owner_type, field_type) = instantiate_record_field state info in
      unify
        state
        ~expected:record_type
        ~actual:owner_type
        ~on_error:(pattern_constraint_diagnostic pattern);
      match field.pattern with
      | Some field_pattern -> bind_pattern ~mode state field_pattern field_type
      | None -> bind_value ~mode state ~name:field.name field_type

and bind_record ~mode state pattern fields type_ =
  match fields with
  | [] -> ()
  | first_field :: _ ->
      match State.get_record_field state ~name:first_field.name with
      | None -> ()
      | Some info ->
          let (record_type, _) = instantiate_record_field state info in
          unify
            state
            ~expected:record_type
            ~actual:type_
            ~on_error:(pattern_constraint_diagnostic pattern);
          List.for_each fields ~fn:(bind_record_field ~mode state pattern record_type)

and bind_tuple ~mode state pattern parts type_ =
  let expected_parts = List.map parts ~fn:(fun _ -> State.fresh_var state) in
  let expected = Type.Tuple expected_parts in
  unify state ~expected ~actual:type_ ~on_error:(pattern_constraint_diagnostic pattern);
  List.zip parts expected_parts
  |> List.for_each ~fn:(fun (part, type_) -> bind_pattern ~mode state part type_)

and inline_record_find_field inline_record name =
  List.find
    inline_record.InferenceEnv.fields
    ~fn:(fun field -> SurfacePath.equal field.InferenceEnv.declaration.name name)

and bind_inline_record_pattern state (payload_pattern: pattern) (fields: record_pattern_field list) inline_record =
  payload_pattern.type_ <- Some inline_record.State.InferenceEnv.payload_type;
  List.for_each
    fields
    ~fn:(fun field ->
      match inline_record_find_field inline_record field.name with
      | None -> ()
      | Some expected_field -> (
          match field.pattern with
          | Some field_pattern -> bind_pattern ~mode:Local state field_pattern expected_field.type_
          | None -> bind_value ~mode:Local state ~name:field.name expected_field.type_
        ))

and bind_constructor state pattern ctr type_ =
  match ctr with
  | { ident; payload = None } when Builtin.is_unit ident ->
      unify
        state
        ~expected:Builtin.unit
        ~actual:type_
        ~on_error:(pattern_constraint_diagnostic pattern)
  | { ident; payload = None } ->
      let constructor_type =
        match instantiate_constructor state ident with
        | None -> State.fresh_var state
        | Some constructor -> constructor.scheme.body
      in
      unify
        state
        ~expected:constructor_type
        ~actual:type_
        ~on_error:(pattern_constraint_diagnostic pattern)
  | { ident; payload = Some ({ kind = Record fields; _ } as payload_pattern) } -> (
      match instantiate_constructor state ident with
      | None ->
          let payload_type = State.fresh_var state in
          unify
            state
            ~expected:(Ast.Type.arrow payload_type type_)
            ~actual:(State.fresh_var state)
            ~on_error:(pattern_constraint_diagnostic pattern);
          bind_pattern ~mode:Local state payload_pattern payload_type
      | Some constructor -> (
          let payload_type = State.fresh_var state in
          unify
            state
            ~expected:constructor.scheme.body
            ~actual:(Ast.Type.arrow payload_type type_)
            ~on_error:(pattern_constraint_diagnostic pattern);
          match constructor.arguments with
          | InferenceEnv.InlineRecord inline_record ->
              bind_inline_record_pattern state payload_pattern fields inline_record
          | InferenceEnv.Tuple _ -> bind_pattern ~mode:Local state payload_pattern payload_type
        )
    )
  | { ident; payload = Some payload_pattern } ->
      let constructor_type =
        match instantiate_constructor state ident with
        | None -> State.fresh_var state
        | Some constructor -> constructor.scheme.body
      in
      let payload_type = State.fresh_var state in
      unify
        state
        ~expected:constructor_type
        ~actual:(Ast.Type.arrow payload_type type_)
        ~on_error:(pattern_constraint_diagnostic pattern);
      bind_pattern ~mode:Local state payload_pattern payload_type

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
    | Constructor constructor -> infer_constructor state expr constructor
    | Tuple parts -> infer_tuple state parts
    | List items -> infer_list state items
    | Let letexpr -> infer_let_expr state letexpr
    | Match match_ -> infer_match state match_
    | Record record -> infer_record state record
    | FieldAccess access -> infer_field_access state access
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

and infer_unknown_record state (fields: record_expression_field list) =
  List.for_each
    fields
    ~fn:(fun field ->
      let _ = infer_expression state field.value in
      ());
  State.fresh_var state

and infer_visible_record_field_type state record_type (field: record_expression_field) =
  match State.get_record_field state ~name:field.name with
  | Some info ->
      let (field_owner_type, field_type) = instantiate_record_field state info in
      unify
        state
        ~expected:record_type
        ~actual:field_owner_type
        ~on_error:(expression_constraint_diagnostic field.value);
      Some field_type
  | None -> None

and infer_record_fields state (fields: record_expression_field list) ~field_type =
  List.for_each
    fields
    ~fn:(fun field ->
      match field_type field with
      | None ->
          let _ = infer_expression state field.value in
          ()
      | Some expected ->
          let value_type = infer_expression state field.value in
          unify
            state
            ~expected
            ~actual:value_type
            ~on_error:(expression_constraint_diagnostic field.value);
          field.value.type_ <- Some expected)

and infer_record_body state (fields: record_expression_field list) =
  match fields with
  | [] -> State.fresh_var state
  | first_field :: _ -> (
      match State.get_record_field state ~name:first_field.name with
      | None -> infer_unknown_record state fields
      | Some info ->
          let (record_type, _) = instantiate_record_field state info in
          infer_record_fields
            state
            fields
            ~field_type:(infer_visible_record_field_type state record_type);
          record_type
    )

and infer_record state record =
  match record with
  (* if you have `{}` we'll just fallback to a hole *)
  | { update = None; fields = [] } -> State.fresh_var state

  (* if you have `{hello}` we'll fallback to hello's type assuming its a record update *)
  | { update = Some update; fields = [] } -> infer_expression state update

  (* if you have `{hello=1}` we'll type the record body *)
  | { update = None; fields } -> infer_record_body state fields

  (* if you have `{hello with x=1}` we'll type the record body and unify against the updated value *)
  | { update = Some update; fields } ->
      let record_type = infer_record_body state fields in
      let update_type = infer_expression state update in
      unify
        state
        ~expected:update_type
        ~actual:record_type
        ~on_error:(expression_constraint_diagnostic update);
      record_type

and infer_field_access state access =
  let receiver_type = infer_expression state access.receiver in
  match State.get_record_field state ~name:access.field with
  | None -> State.fresh_var state
  | Some info ->
      let (owner_type, field_type) = instantiate_record_field state info in
      unify
        state
        ~expected:owner_type
        ~actual:receiver_type
        ~on_error:(expression_constraint_diagnostic access.receiver);
      field_type

and infer_match state match_ =
  let scrutinee_type = infer_expression state match_.scrutinee in
  let result_type = State.fresh_var state in
  let infer_case state scrutinee_type (case: match_case) =
    bind_pattern ~mode:Local state case.pattern scrutinee_type;
    (
      match case.guard with
      | Some guard ->
          let guard_type = infer_expression state guard in
          unify
            state
            ~expected:Builtin.bool
            ~actual:guard_type
            ~on_error:(expression_constraint_diagnostic guard)
      | None -> ()
    );
    let body_type = infer_expression state case.body in
    unify
      state
      ~expected:result_type
      ~actual:body_type
      ~on_error:(expression_constraint_diagnostic case.body)
  in
  List.for_each match_.cases ~fn:(infer_case state scrutinee_type);
  result_type

and infer_let_binding state ~mode (bind: let_binding) =
  let expr_type = infer_expression state bind.expr in
  let binding_type =
    match bind.type_hint with
    | None -> expr_type
    | Some hint ->
        let expected = core_type_to_type state hint in
        unify state ~expected ~actual:expr_type ~on_error:(annotation_diagnostic hint);
        expected
  in
  bind_pattern ~mode state bind.pattern binding_type;
  binding_type

and infer_let_expr state (let_: let_expression) =
  State.push_scope state;
  List.for_each
    let_.bindings
    ~fn:(fun binding ->
      let _ = infer_let_binding state ~mode:Generalized binding in
      ());
  let body_type = infer_expression state let_.body in
  State.pop_scope state;
  body_type

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
      unify state ~expected:element ~actual ~on_error:(expression_constraint_diagnostic item));
  Builtin.list element

and infer_apply state (apply: application) =
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
  unify state ~expected ~actual:callee ~on_error:(expression_constraint_diagnostic apply.callee);
  result

and infer_inline_record_field_type inline_record (field: record_expression_field) =
  match inline_record_find_field inline_record field.name with
  | None -> None
  | Some expected_field -> Some expected_field.type_

and infer_inline_record_payload state payload_expr (fields: record_expression_field list) (inline_record:
  InferenceEnv.inline_record) =
  payload_expr.type_ <- Some inline_record.payload_type;
  infer_record_fields
    state
    fields
    ~field_type:(infer_inline_record_field_type inline_record)

and infer_inline_record_constructor state (expr: expression) (payload_expr: expression) (fields:
  record_expression_field list) (constructor: InferenceEnv.constructor_description) inline_record =
  let result = State.fresh_var state in
  unify
    state
    ~expected:constructor.scheme.body
    ~actual:(Ast.Type.arrow inline_record.InferenceEnv.payload_type result)
    ~on_error:(expression_constraint_diagnostic expr);
  infer_inline_record_payload state payload_expr fields inline_record;
  result

and infer_constructor state (expr: expression) (constructor: constructor_expression) =
  match constructor with
  (* when there's no payload and the ident is a built in, just return the builtin type *)
  | { ident; payload = None } when Builtin.is_unit ident -> Builtin.unit
  | { ident; payload = None } -> (
      match instantiate_constructor state ident with
      | Some constructor -> constructor.scheme.body
      | None -> State.fresh_var state
    )
  | { ident; payload = Some ({ kind = Record { update = None; fields }; _ } as payload_expr) } -> (
      match instantiate_constructor state ident with
      | Some ({ arguments = InferenceEnv.InlineRecord inline_record; _ } as constructor) ->
          infer_inline_record_constructor state expr payload_expr fields constructor inline_record
      | _ -> infer_constructor_payload state expr ident payload_expr
    )
  | { ident; payload = Some payload_expr } ->
      infer_constructor_payload state expr ident payload_expr

and infer_constructor_payload state expr ident payload_expr =
  let payload_type = infer_expression state payload_expr in
  let result = State.fresh_var state in
  let constructor_type =
    match instantiate_constructor state ident with
    | None -> State.fresh_var state
    | Some constructor -> constructor.scheme.body
  in
  unify
    state
    ~expected:constructor_type
    ~actual:(Ast.Type.arrow payload_type result)
    ~on_error:(expression_constraint_diagnostic expr);
  result

and infer_if_else state ifelse =
  let condition = infer_expression state ifelse.condition in
  unify
    state
    ~expected:Builtin.bool
    ~actual:condition
    ~on_error:(expression_constraint_diagnostic ifelse.condition);
  let then_ = infer_expression state ifelse.then_branch in
  (
    match ifelse.else_branch with
    | Some else_branch ->
        let else_ = infer_expression state else_branch in
        unify
          state
          ~expected:then_
          ~actual:else_
          ~on_error:(expression_constraint_diagnostic else_branch);
        then_
    | None ->
        unify
          state
          ~expected:Builtin.unit
          ~actual:then_
          ~on_error:(expression_constraint_diagnostic ifelse.then_branch);
        Builtin.unit
  )

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

let register_constructor state (decl: type_declaration) (ctr: type_constructor) =
  let (scope, arguments) = fresh_type_parameters state decl.parameters in
  let description =
    State.with_type_params
      state
      scope
      (fun state ->
        let result =
          match ctr.result with
          | Some result -> core_type_to_type state result
          | None -> type_declaration_to_type ~arguments decl
        in
        let constructor_arguments =
          match ctr.arguments with
          | Tuple arguments -> InferenceEnv.Tuple (List.map arguments ~fn:(core_type_to_type state))
          | Record fields ->
              let payload_type =
                Type.Apply { ident = inline_record_payload_ident decl ctr; arguments }
              in
              let inline_record: InferenceEnv.inline_record = {
                owner = decl;
                constructor = ctr;
                payload_type;
                fields = List.map
                  fields
                  ~fn:(fun field -> ({
                    declaration = field;
                    type_ = core_type_to_type state field.type_annotation;
                  }: InferenceEnv.inline_record_field));
              }
              in
              InferenceEnv.InlineRecord inline_record
        in
        let body =
          match constructor_arguments with
          | InferenceEnv.Tuple [] -> result
          | InferenceEnv.Tuple [ argument ] -> Ast.Type.arrow argument result
          | InferenceEnv.Tuple arguments -> Ast.Type.arrow (Type.Tuple arguments) result
          | InferenceEnv.InlineRecord inline_record ->
              Ast.Type.arrow inline_record.payload_type result
        in
        ({
          name = ctr.name;
          scheme = Quantifier.generalize body;
          result = generalize_type result;
          arguments = generalize_constructor_arguments constructor_arguments;
        }: InferenceEnv.constructor_description))
  in
  State.add_constructor state ~name:ctr.name ~description

let register_record_field state (decl: type_declaration) (field: record_field_declaration) =
  State.add_record_field state ~name:field.name ~owner:decl ~field

let register_type_decl state (decl: type_declaration) =
  let name = decl.name in
  State.add_type state ~name ~declaration:decl;
  match decl.definition.kind with
  | Variant ctrs -> List.for_each ctrs ~fn:(register_constructor state decl)
  | Record fields -> List.for_each fields ~fn:(register_record_field state decl)
  | _ -> ()

let type_let_binding (state: State.t) (lb: let_binding) =
  let _ = infer_let_binding state ~mode:Generalized lb in
  ()

let type_let_decl (state: State.t) (ld: let_declaration) =
  List.for_each ld.bindings ~fn:(type_let_binding state)

let type_impl_item (state: State.t) (item: structure_item) =
  match item.kind with
  | Type decl -> List.for_each decl ~fn:(register_type_decl state)
  | Let ld -> type_let_decl state ld
  | _ -> ()

let type_impl (state: State.t) (items: structure_item list) =
  List.for_each items ~fn:(type_impl_item state)

let type_intf (_state: State.t) (_items: signature_item list) = ()

let type_ast (state: State.t) (ast: Ast.t) =
  match ast.kind with
  | Implementation items -> type_impl state items
  | Interface items -> type_intf state items
