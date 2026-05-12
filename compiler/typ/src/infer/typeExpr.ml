open Std
open Ast

module HashMap = Std.Collections.HashMap

(**
   Source arrow labels and solver arrow labels are intentionally separate
   types. The source shape comes from `Typ.Ast.core_type`; the solver shape is
   what unification compares. This conversion is pure because labels carry no
   source-local state after lowering.
*)
let arrow_label_to_type_label = fun __tmp1 ->
  match __tmp1 with
  | NoLabel -> Type.Label.NoLabel
  | Labelled label -> Type.Label.Labelled label
  | Optional label -> Type.Label.Optional label

(**
   Convert a source type expression into the solver algebra.

   The function also writes the converted type back onto the `core_type` node.
   That mutation is the same annotation strategy used for expressions and
   patterns: later diagnostics, LSP hovers, and tests can inspect the typed tree
   without rebuilding the conversion.
*)
let rec core_type_to_type (state: State.t) (annotation: core_type) =
  let type_ =
    match annotation.kind with
    (* A bare type name such as `int` or `Result.t`. *)
    | TypeIdent ident -> Type.Apply { ident; arguments = [] }
    | Apply { constructor; arguments } -> (
        (*
           Type application is represented source-first as "constructor plus
           arguments". If the constructor itself lowers to a nominal type, append
           the new arguments. This handles both `int list` and curried-looking
           type application shapes without introducing a separate solver node.
        *)
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

    (* Tuple syntax is already semantic enough for the current solver. *)
    | Tuple parts -> Type.Tuple (List.map parts ~fn:(core_type_to_type state))
    | Parenthesized inner ->
        (*
           Parentheses are syntax-only at this stage. They may still exist in
           the typed tree because the tree is source-shaped, but the solver
           should not see a separate parenthesized type node.
        *)
        core_type_to_type state inner
    | ForAll { body = inner; _ } ->
        (*
           TODO(@leostera): model explicit quantifier scope. For now we type
           the body so annotated examples can continue through the checker;
           real scoped quantification belongs here when that feature slice
           lands.
        *)
        core_type_to_type state inner
    | Var (Some name) -> (
        (*
           Type variables inside a declaration first consult the active
           declaration parameter scope. If the name was not declared, use a fresh
           hole so the checker can continue and report later constraints.
        *)
        match State.get_type_param state ~name with
        | Some type_ -> type_
        | None -> State.fresh_var state
      )
    | Var None -> State.fresh_var state
    | Wildcard
    | PolyVariant _
    | Package _ ->
        (*
           TODO(@leostera): lower wildcard, polymorphic variant, and package
           types into first-class solver forms. Until then, use a fresh hole so
           the surrounding constraint still participates in inference instead
           of poisoning the whole file.
        *)
        State.fresh_var state
  in
  annotation.type_ <- Some type_;
  type_

let type_declaration_to_type ?(arguments = []) (decl: type_declaration) =
  Type.Apply { ident = decl.name; arguments }

(**
   Allocate one fresh solver variable per declared type parameter.

   The map is installed only while converting the declaration that introduced
   those parameters. This makes `type 'a t = 'a list` preserve the same variable
   for both uses of `'a`, while avoiding a global table of type parameters.
*)
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

(**
   Record field lookup must instantiate the owning record declaration at the
   lookup site.

   For `type 'a box = { value : 'a }`, every use of `value` needs a fresh owner
   type `'x box` and field type `'x`; otherwise two unrelated field accesses
   would accidentally share the same solver variable.
*)
let instantiate_record_field state (info: State.InferenceEnv.record_field_info) =
  let (scope, arguments) = fresh_type_parameters state info.owner.parameters in
  State.with_type_params
    state
    scope
    (fun state ->
      let owner_type = type_declaration_to_type ~arguments info.owner in
      let field_type = core_type_to_type state info.field.type_annotation in
      (owner_type, field_type))
