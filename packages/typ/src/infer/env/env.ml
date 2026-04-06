open Std
open Analysis
open Model

module Binding = Binding
module Value_env = Value_env

type bindings = Binding.t list

type t = {
  values: Value_env.t;
}

type scope = {
  entries: Value_env.scope_entries;
  opens: Value_env.scope_opens;
}

let empty = { values = [] }

let empty_scope = { entries = []; opens = [] }

let of_entries = fun ~provenance entries ->
  { values = Value_env.of_entries ~provenance entries }

let of_bindings = fun values -> { values }

let singleton = fun ~name ~scheme ~provenance ->
  { values = Value_env.singleton ~name ~scheme ~provenance }

let bindings = fun env -> env.values

let unique = fun env -> { values = Value_env.unique env.values }

let render = fun env -> Value_env.render env.values

let lookup = fun env path -> Value_env.lookup env.values path

let lookup_all = fun env path -> Value_env.lookup_all env.values path

let names = fun env -> Value_env.names env.values

let introduced_names = fun before after ->
  Value_env.introduced_names before.values after.values

let bind = fun env introduced ->
  { values = Value_env.bind env.values introduced.values }

let extend = fun env introduced ->
  { values = Value_env.bind env.values introduced }

let with_local_open = fun env module_path ->
  { values = Value_env.with_local_open env.values module_path }

let entries_for_include = fun env module_path ->
  { values = Value_env.entries_for_include env.values module_path }

let export_names_for_module_alias = fun env ~alias_name ~module_path ->
  Value_env.export_names_for_module_alias env.values ~alias_name ~module_path

let entries_for_module_alias = fun env ~alias_name ~module_path ->
  { values = Value_env.entries_for_module_alias env.values ~alias_name ~module_path }

let export = fun config env ->
  { values = Value_env.export config env.values }

let export_with_forced_names = fun state env ->
  { values = Value_env.export_with_forced_names state env.values }

let introduced_entries = fun before after ->
  { values = Value_env.introduced_entries before.values after.values }

let qualify = fun ~scope_path env ->
  { values = Value_env.qualify_entries scope_path env.values }

let register_entries = fun scope ~scope_path env ->
  {
    scope with
    entries = Value_env.update_scope_entries scope.entries scope_path env.values;
  }

let register_open = fun scope ~scope_path ~module_path ->
  {
    scope with
    opens = Value_env.update_scope_opens scope.opens scope_path module_path;
  }

let for_item_scope = fun env scope ~scope_path ->
  {
    values = Value_env.for_item_scope env.values scope.entries scope.opens scope_path;
  }
