module Legacy_env = Env

open Std
open Model

type bindings = Legacy_env.Binding.t list

type delta = {
  bindings: bindings;
  type_decls: FileSummary.type_decl list;
}

type t =
  | Empty
  | Snapshot of delta
  | Bind of t * t
  | Bind_in_scope of t * IdentPath.t * t
  | Open of t * IdentPath.t
  | Qualify of t * IdentPath.t

let empty = Empty

let snapshot = fun ~bindings ~type_decls -> Snapshot { bindings; type_decls }

let bind = fun summary introduced -> Bind (summary, introduced)

let bind_in_scope = fun summary ~scope_path introduced ->
  Bind_in_scope (summary, scope_path, introduced)

let open_ = fun summary module_path -> Open (summary, module_path)

let qualify = fun summary ~scope_path -> Qualify (summary, scope_path)

let delta_of_legacy_delta = fun (delta: Legacy_env.summary_delta) ->
  {
    bindings = delta.bindings;
    type_decls = delta.type_decls;
  }

let legacy_delta_of_delta = fun delta ->
  {
    Legacy_env.bindings = delta.bindings;
    type_decls = delta.type_decls;
  }

let rec of_legacy_summary = function
  | Legacy_env.Summary_empty -> Empty
  | Legacy_env.Summary_snapshot delta -> Snapshot (delta_of_legacy_delta delta)
  | Legacy_env.Summary_bind (summary, introduced) ->
      Bind (of_legacy_summary summary, of_legacy_summary introduced)
  | Legacy_env.Summary_bind_in_scope (summary, scope_path, introduced) ->
      Bind_in_scope (of_legacy_summary summary, scope_path, of_legacy_summary introduced)
  | Legacy_env.Summary_open (summary, module_path) ->
      Open (of_legacy_summary summary, module_path)
  | Legacy_env.Summary_qualify (summary, scope_path) ->
      Qualify (of_legacy_summary summary, scope_path)

let rec to_legacy_summary = function
  | Empty -> Legacy_env.Summary_empty
  | Snapshot delta -> Legacy_env.Summary_snapshot (legacy_delta_of_delta delta)
  | Bind (summary, introduced) ->
      Legacy_env.Summary_bind (to_legacy_summary summary, to_legacy_summary introduced)
  | Bind_in_scope (summary, scope_path, introduced) ->
      Legacy_env.Summary_bind_in_scope
        (to_legacy_summary summary, scope_path, to_legacy_summary introduced)
  | Open (summary, module_path) ->
      Legacy_env.Summary_open (to_legacy_summary summary, module_path)
  | Qualify (summary, scope_path) ->
      Legacy_env.Summary_qualify (to_legacy_summary summary, scope_path)
