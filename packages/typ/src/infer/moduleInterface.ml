open Std.Collections
open Std.Iter
open Ast

type t = {
  types: (ident * type_declaration) Vector.t;
  values: (ident * TypeScheme.t) Vector.t;
  modules: (ident * t) Vector.t;
}

let types t = Vector.iter t.types

let values t = Vector.iter t.values

let modules t = Vector.iter t.modules

let rec from_module_summary (summary: Env.module_summary) =
  let types = Vector.with_capacity ~size:16 in
  let values = Vector.with_capacity ~size:16 in
  let modules = Vector.with_capacity ~size:8 in
  Iterator.for_each (Env.module_types summary) ~fn:(fun type_ -> Vector.push types ~value:type_);
  Iterator.for_each (Env.module_values summary) ~fn:(fun value -> Vector.push values ~value);
  Iterator.for_each
    (Env.module_modules summary)
    ~fn:(fun (name, summary) -> Vector.push modules ~value:(name, from_module_summary summary));
  { types; values; modules }

let from_env (env: Env.t) =
  let types = Vector.with_capacity ~size:16 in
  let values = Vector.with_capacity ~size:16 in
  let modules = Vector.with_capacity ~size:8 in
  Iterator.for_each (Env.exported_types env) ~fn:(fun type_ -> Vector.push types ~value:type_);
  Iterator.for_each (Env.exports env) ~fn:(fun value -> Vector.push values ~value);
  Iterator.for_each
    (Env.exported_modules env)
    ~fn:(fun (name, summary) -> Vector.push modules ~value:(name, from_module_summary summary));
  { types; values; modules }
