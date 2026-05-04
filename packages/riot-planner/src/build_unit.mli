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
type t = {
  key: key;
  package: Package.t;
}

val artifact_kind_to_string: artifact_kind -> string

val key_to_string: key -> string

val compare_key: key -> key -> Order.t

val equal_key: key -> key -> bool

val package_key: key -> Package.key
