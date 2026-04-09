open Std

let builtin_owner = "$builtin"

let list_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-1)

let exn_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-2)

let normalized_segments = fun path ->
  match IdentPath.to_segments path with
  | ["Stdlib";builtin_name] -> [ builtin_name ]
  | other -> other

let head_of_path = fun path ->
  let segments = normalized_segments path in
  match segments with
  | [ "list" ] -> Some (TypeRepr.named_head ~type_constructor_id:list_type_constructor_id ~name:path)
  | [ "exn" ] -> Some (TypeRepr.named_head ~type_constructor_id:exn_type_constructor_id ~name:path)
  | _ -> None

let type_of_path = fun path arguments ->
  match (normalized_segments path, arguments) with
  | ([ "int" ], []) -> Some TypeRepr.int
  | ([ "float" ], []) -> Some TypeRepr.float
  | ([ "bool" ], []) -> Some TypeRepr.bool
  | ([ "string" ], []) -> Some TypeRepr.string
  | ([ "char" ], []) -> Some TypeRepr.char
  | ([ "unit" ], []) -> Some TypeRepr.unit_
  | ([ "array" ], [ argument ]) -> Some (TypeRepr.array argument)
  | ([ "list" ], [ argument ]) -> Some (TypeRepr.list argument)
  | ([ "option" ], [ argument ]) -> Some (TypeRepr.option argument)
  | ([ "result" ], [ ok_ty; error_ty ]) -> Some (TypeRepr.result ok_ty error_ty)
  | ([ "seq" ], [ argument ]) -> Some (TypeRepr.seq argument)
  | _ -> None
