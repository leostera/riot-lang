open Std

let builtin_owner = "$builtin"

let list_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-1)

let option_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-2)

let result_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-3)

let exn_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-4)

let bytes_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-5)

let int32_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-6)

let int64_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-7)

let nativeint_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-8)

let lazy_t_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-9)

let ref_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-10)

let in_channel_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-11)

let out_channel_type_constructor_id = TypeConstructorId.make ~owner:builtin_owner ~local_id:(-12)

let normalized_segments = fun path ->
  match IdentPath.to_segments path with
  | [ "Stdlib"; builtin_name ] -> [ builtin_name ]
  | other -> other

let head_of_path = fun path ->
  let segments = normalized_segments path in
  match segments with
  | [ "list" ] -> Some (TypeRepr.named_head ~type_constructor_id:list_type_constructor_id ~name:path)
  | [ "option" ] -> Some (TypeRepr.named_head ~type_constructor_id:option_type_constructor_id ~name:path)
  | [ "result" ] -> Some (TypeRepr.named_head ~type_constructor_id:result_type_constructor_id ~name:path)
  | [ "exn" ] -> Some (TypeRepr.named_head ~type_constructor_id:exn_type_constructor_id ~name:path)
  | [ "bytes" ] -> Some (TypeRepr.named_head ~type_constructor_id:bytes_type_constructor_id ~name:path)
  | [ "int32" ] -> Some (TypeRepr.named_head ~type_constructor_id:int32_type_constructor_id ~name:path)
  | [ "int64" ] -> Some (TypeRepr.named_head ~type_constructor_id:int64_type_constructor_id ~name:path)
  | [ "nativeint" ] -> Some (TypeRepr.named_head
    ~type_constructor_id:nativeint_type_constructor_id
    ~name:path)
  | [ "lazy_t" ] -> Some (TypeRepr.named_head ~type_constructor_id:lazy_t_type_constructor_id ~name:path)
  | [ "ref" ] -> Some (TypeRepr.named_head ~type_constructor_id:ref_type_constructor_id ~name:path)
  | [ "in_channel" ] -> Some (TypeRepr.named_head
    ~type_constructor_id:in_channel_type_constructor_id
    ~name:path)
  | [ "out_channel" ] -> Some (TypeRepr.named_head
    ~type_constructor_id:out_channel_type_constructor_id
    ~name:path)
  | _ -> None

let type_of_path = fun path arguments ->
  match (normalized_segments path, arguments) with
  | ([ "int" ], []) -> Some TypeRepr.int
  | ([ "float" ], []) -> Some TypeRepr.float
  | ([ "bool" ], []) -> Some TypeRepr.bool
  | ([ "string" ], []) -> Some TypeRepr.string
  | ([ "char" ], []) -> Some TypeRepr.char
  | ([ "unit" ], []) -> Some TypeRepr.unit_
  | ([ "option" ], [ argument ]) -> Some (TypeRepr.option argument)
  | ([ "result" ], [ ok_ty; error_ty ]) -> Some (TypeRepr.result ok_ty error_ty)
  | ([ "array" ], [ argument ]) -> Some (TypeRepr.array argument)
  | ([ "list" ], [ argument ]) -> Some (TypeRepr.list argument)
  | ([ "Seq"; "t" ], [ argument ])
  | ([ "Std"; "Seq"; "t" ], [ argument ]) -> Some (TypeRepr.seq argument)
  | _ -> None
