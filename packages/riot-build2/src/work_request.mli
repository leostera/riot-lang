open Std

type t =
  | Existing of Work_node.key
  | Materialize of Work_node.kind

val existing: Work_node.key -> t

val materialize: Work_node.kind -> t

val from_key: Work_node.key -> t

val from_keys: Work_node.key list -> t list

val key: t -> Work_node.key

val kind: t -> Work_node.kind option
