type t = {
  mutable next_var: Ast.TypeVar.t;
  mutable env: Env.t;
  diagnostics: Diagnostics.t;
}

let create () = {
  next_var = Ast.TypeVar.first;
  diagnostics = Diagnostics.create ();
  env = Env.create ();
}

let env state = state.env

let set_env state env =
  state.env <- env

let add_value state ~name ~scheme =
  state.env <- Env.add_value state.env ~name ~scheme

let get_value state ~name = Env.get_value state.env ~name

let has_value state ~name = Env.has_value state.env ~name

let add_constructor state ~name ~scheme =
  state.env <- Env.add_constructor state.env ~name ~scheme

let get_constructor state ~name = Env.get_constructor state.env ~name

let has_constructor state ~name = Env.has_constructor state.env ~name

let add_type state ~name ~declaration =
  state.env <- Env.add_type state.env ~name ~declaration

let get_type state ~name = Env.get_type state.env ~name

let has_type state ~name = Env.has_type state.env ~name

let diagnostics state = state.diagnostics

(** Instantiates a new fresh type variable in the current typing environment. *)
let fresh_var state =
  let id = state.next_var in
  state.next_var <- Ast.TypeVar.next id;
  Ast.Type.Var { id; link = None }

let push_scope state =
  state.env <- Env.push_scope state.env

let pop_scope state =
  state.env <- Env.pop_scope state.env

let push_module state ~name =
  state.env <- Env.push_module state.env ~name

let pop_module state =
  state.env <- Env.pop_module state.env
