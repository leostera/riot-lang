(* NOTE: test support only. This stays internal to the package so fixture typing
   support does not leak through the public Raml API. *)

module Compiler_config = Raml_core.Config
open Std
open Typ.Model

let ambient_print_endline = (
  SurfacePath.from_name "print_endline",
  TypeScheme.from_type
    (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.string ~rhs:TypeRepr.unit_)
)

let ambient_print_newline = (
  SurfacePath.from_name "print_newline",
  TypeScheme.from_type (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.unit_ ~rhs:TypeRepr.unit_)
)

let ambient_print_int = (
  SurfacePath.from_name "print_int",
  TypeScheme.from_type (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.unit_)
)

let ambient_print_string = (
  SurfacePath.from_name "print_string",
  TypeScheme.from_type
    (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.string ~rhs:TypeRepr.unit_)
)

let ambient_print_char = (
  SurfacePath.from_name "print_char",
  TypeScheme.from_type (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.char ~rhs:TypeRepr.unit_)
)

let ambient_mod = (
  SurfacePath.from_name "mod",
  TypeScheme.from_type
    (TypeRepr.arrow
      ~label:TypeRepr.Nolabel
      ~lhs:TypeRepr.int
      ~rhs:(TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.int))
)

let ambient_printf =
  let result_var_id = 0 in
  let result_var = TypeRepr.make_var result_var_id in
  (
    SurfacePath.from_string "Printf.printf",
    TypeScheme.from_explicit
      ~quantified:[ result_var_id ]
      (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.string ~rhs:result_var)
  )

let ambient_sqrt = (
  SurfacePath.from_name "sqrt",
  TypeScheme.from_type (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.float ~rhs:TypeRepr.float)
)

let ambient_string_of_int = (
  SurfacePath.from_name "string_of_int",
  TypeScheme.from_type (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.string)
)

let ambient_string_of_float = (
  SurfacePath.from_name "string_of_float",
  TypeScheme.from_type
    (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.float ~rhs:TypeRepr.string)
)

let ambient_int_of_string = (
  SurfacePath.from_name "int_of_string",
  TypeScheme.from_type (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.string ~rhs:TypeRepr.int)
)

let ambient_float_of_string = (
  SurfacePath.from_name "float_of_string",
  TypeScheme.from_type
    (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.string ~rhs:TypeRepr.float)
)

let ambient_list_append =
  let element_var_id = 1 in
  let element_var = TypeRepr.make_var element_var_id in
  let list_type = TypeRepr.list element_var in
  (
    SurfacePath.from_name "@",
    TypeScheme.from_explicit
      ~quantified:[ element_var_id ]
      (TypeRepr.arrow
        ~label:TypeRepr.Nolabel
        ~lhs:list_type
        ~rhs:(TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:list_type ~rhs:list_type))
  )

let ambient_list_iter =
  let element_var_id = 2 in
  let element_var = TypeRepr.make_var element_var_id in
  (
    SurfacePath.from_string "List.iter",
    TypeScheme.from_explicit
      ~quantified:[ element_var_id ]
      (TypeRepr.arrow
        ~label:TypeRepr.Nolabel
        ~lhs:(TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:element_var ~rhs:TypeRepr.unit_)
        ~rhs:(TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:(TypeRepr.list element_var) ~rhs:TypeRepr.unit_))
  )

let typing_config =
  Typ.Config.default
  |> Typ.Config.with_ambient
    ~ambient:[
      ambient_print_endline;
      ambient_print_newline;
      ambient_print_int;
      ambient_print_string;
      ambient_print_char;
      ambient_mod;
      ambient_printf;
      ambient_sqrt;
      ambient_string_of_int;
      ambient_string_of_float;
      ambient_int_of_string;
      ambient_float_of_string;
      ambient_list_append;
      ambient_list_iter;
    ]

let raml_config = fun ~host ~target -> Compiler_config.make ~host ~target ~typing_config ()
