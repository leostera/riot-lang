open Std

val list_type_constructor_id: TypeConstructorId.t

val exn_type_constructor_id: TypeConstructorId.t

val head_of_path: SurfacePath.t -> TypeRepr.named_type_head option

val type_of_path: SurfacePath.t -> TypeRepr.t list -> TypeRepr.t option
