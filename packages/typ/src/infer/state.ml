module InferenceEnv = Env

open Std
open Std.Collections

type type_param_scope = (string, Ast.Type.t) HashMap.t

type t = {
  mutable next_var: Ast.TypeVar.t;
  mutable env: InferenceEnv.t;
  mutable type_params: type_param_scope option;
  diagnostics: Diagnostics.t;
}

let create () = {
  next_var = Ast.TypeVar.first;
  diagnostics = Diagnostics.create ();
  env = InferenceEnv.create ();
  type_params = None;
}

let env state = state.env

let set_env state env =
  state.env <- env

let add_value state ~name ~scheme =
  state.env <- InferenceEnv.add_value state.env ~name ~scheme

let get_value state ~name = InferenceEnv.get_value state.env ~name

let has_value state ~name = InferenceEnv.has_value state.env ~name

let add_constructor state ~name ~description =
  state.env <- InferenceEnv.add_constructor state.env ~name ~description

let get_constructor state ~name = InferenceEnv.get_constructor state.env ~name

let has_constructor state ~name = InferenceEnv.has_constructor state.env ~name

let add_record_field state ~name ~owner ~field =
  let info: InferenceEnv.record_field_info = { owner; field } in
  state.env <- InferenceEnv.add_record_field state.env ~name ~info

let get_record_field state ~name = InferenceEnv.get_record_field state.env ~name

let has_record_field state ~name = InferenceEnv.has_record_field state.env ~name

let add_type state ~name ~declaration =
  state.env <- InferenceEnv.add_type state.env ~name ~declaration

let get_type state ~name = InferenceEnv.get_type state.env ~name

let has_type state ~name = InferenceEnv.has_type state.env ~name

let diagnostics state = state.diagnostics

let add_diagnostic state diagnostic = Diagnostics.add state.diagnostics diagnostic

(** Instantiates a new fresh type variable in the current typing environment. *)
let fresh_var state =
  let id = state.next_var in
  state.next_var <- Ast.TypeVar.next id;
  Ast.Type.Var { id; link = None }

let with_type_params state scope fn =
  let previous = state.type_params in
  state.type_params <- Some scope;
  let result = fn state in
  state.type_params <- previous;
  result

let get_type_param state ~name =
  match state.type_params with
  | None -> None
  | Some scope -> HashMap.get scope ~key:name

let push_scope state =
  state.env <- InferenceEnv.push_scope state.env

let pop_scope state =
  state.env <- InferenceEnv.pop_scope state.env

let push_module state ~name =
  state.env <- InferenceEnv.push_module state.env ~name

let pop_module state =
  state.env <- InferenceEnv.pop_module state.env
