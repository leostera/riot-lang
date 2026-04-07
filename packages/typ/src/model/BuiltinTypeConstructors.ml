open Std

let list_type_constructor_id = TypeConstructorId.of_int (-1)

let option_type_constructor_id = TypeConstructorId.of_int (-2)

let result_type_constructor_id = TypeConstructorId.of_int (-3)

let exn_type_constructor_id = TypeConstructorId.of_int (-4)

let of_path = fun path ->
  match IdentPath.to_segments path with
  | [ "list" ] -> TypeRepr.Resolved list_type_constructor_id
  | [ "option" ] -> TypeRepr.Resolved option_type_constructor_id
  | [ "result" ] -> TypeRepr.Resolved result_type_constructor_id
  | [ "exn" ] -> TypeRepr.Resolved exn_type_constructor_id
  | _ -> TypeRepr.Unresolved
