open Std

val list_type_constructor_id: TypeConstructorId.t

val option_type_constructor_id: TypeConstructorId.t

val result_type_constructor_id: TypeConstructorId.t

val exn_type_constructor_id: TypeConstructorId.t

val bytes_type_constructor_id: TypeConstructorId.t

val int32_type_constructor_id: TypeConstructorId.t

val int64_type_constructor_id: TypeConstructorId.t

val nativeint_type_constructor_id: TypeConstructorId.t

val lazy_t_type_constructor_id: TypeConstructorId.t

val ref_type_constructor_id: TypeConstructorId.t

val in_channel_type_constructor_id: TypeConstructorId.t

val out_channel_type_constructor_id: TypeConstructorId.t

val head_of_path: IdentPath.t -> TypeRepr.named_type_head option

val type_of_path: IdentPath.t -> TypeRepr.t list -> TypeRepr.t option
