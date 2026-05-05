open Std
open Riot_model

type artifact_kind =
  | Library
  | RuntimeBinary of { name: string }
  | TestBinary of { name: string }
  | ExampleBinary of { name: string }
  | BenchBinary of { name: string }
  | SyntheticTool of { name: string }
type key = {
  package: Package_name.t;
  artifact: artifact_kind;
  target: Target.t;
  profile: Profile.t;
}
type id = Crypto.hash
type t

val artifact_kind_to_string: artifact_kind -> string

val key_to_string: key -> string

val compare_key: key -> key -> Order.t

val equal_key: key -> key -> bool

val package_key: key -> Package.key

val id_of_key: key -> id

val id: t -> id

val key: t -> key

val package: t -> Package.t

val artifact: t -> artifact_kind

val target: t -> Target.t

val profile: t -> Profile.t

val package_name: t -> Package_name.t

val from_artifact:
  package:Package.t ->
  artifact:artifact_kind ->
  target:Target.t ->
  profile:Profile.t ->
  t

val library: package:Package.t -> target:Target.t -> profile:Profile.t -> t

val executable: package:Package.t -> name:string -> target:Target.t -> profile:Profile.t -> t

val test: package:Package.t -> name:string -> target:Target.t -> profile:Profile.t -> t

val example: package:Package.t -> name:string -> target:Target.t -> profile:Profile.t -> t

val bench: package:Package.t -> name:string -> target:Target.t -> profile:Profile.t -> t

val synthetic: package:Package.t -> name:string -> target:Target.t -> profile:Profile.t -> t
