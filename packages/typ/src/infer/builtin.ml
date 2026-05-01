open Std

(**
   Convert a hard-coded prelude name into the same structured identifier shape
   used for user-written names.

   This is intentionally the only string-to-path step in this module. Everywhere
   else works with `Ast.ident`, so the rest of the checker does not parse or
   split names by hand.
*)
let ident name =
  Model.Surface_path.from_parts [ name ]
  |> Result.expect ~msg:("expected builtin identifier " ^ name)

(* These identifiers are canonical names, not special type constructors. *)
let int_ident = ident "int"

let bool_ident = ident "bool"

let float_ident = ident "float"

let char_ident = ident "char"

let string_ident = ident "string"

let unit_ident = ident "unit"

let list_ident = ident "list"

let option_ident = ident "option"

let some_ident = ident "Some"

let none_ident = ident "None"

(**
   Built-ins are represented as nominal applications with zero or more type
   arguments. That keeps them on the same path as user-defined abstract types:
   `int`, `bool`, and `string` are not separate variants in the type algebra.
*)
let make ?(arguments = []) ident = Ast.Type.Apply { ident; arguments }

let int = make int_ident

let bool = make bool_ident

let float = make float_ident

let char = make char_ident

let string = make string_ident

let unit = make unit_ident

let list el = make list_ident ~arguments:[ el ]

let option el = make option_ident ~arguments:[ el ]

(**
   Unit is special only because the source language exposes `()` as syntax, and
   earlier lowering represents that constructor through this canonical name.
*)
let is_unit ident = Model.Surface_path.equal ident unit_ident

(**
   Built-in option constructors are registered as ordinary constructor
   descriptions.

   The type variable is generic from the start because these descriptions live
   in the initial environment and are instantiated at every use. This gives
   `Some 'x` type `char option` and `None` type `'a option` without adding a
   special option branch to expression inference.
*)
let option_parameter = Ast.Type.Generic Ast.TypeVar.first

let option_result = option option_parameter

let none_description: State.InferenceEnv.constructor_description = {
  name = none_ident;
  scheme = TypeScheme.monomorphic option_result;
  result = option_result;
  arguments = State.InferenceEnv.Tuple [];
}

let some_description: State.InferenceEnv.constructor_description = {
  name = some_ident;
  scheme = TypeScheme.monomorphic (Ast.Type.arrow option_parameter option_result);
  result = option_result;
  arguments = State.InferenceEnv.Tuple [ option_parameter ];
}

let install state =
  State.add_constructor state ~name:none_ident ~description:none_description;
  State.add_constructor state ~name:some_ident ~description:some_description
