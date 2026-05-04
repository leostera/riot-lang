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

let artifact_kind_to_string = fun __tmp1 ->
  match __tmp1 with
  | Library -> "library"
  | RuntimeBinary { name } -> "runtime-bin:" ^ name
  | TestBinary { name } -> "test-bin:" ^ name
  | ExampleBinary { name } -> "example-bin:" ^ name
  | BenchBinary { name } -> "bench-bin:" ^ name
  | SyntheticTool { name } -> "synthetic-tool:" ^ name

let artifact_kind_rank = fun __tmp1 ->
  match __tmp1 with
  | Library -> 0
  | RuntimeBinary _ -> 1
  | TestBinary _ -> 2
  | ExampleBinary _ -> 3
  | BenchBinary _ -> 4
  | SyntheticTool _ -> 5

let artifact_kind_name = fun __tmp1 ->
  match __tmp1 with
  | Library -> ""
  | RuntimeBinary { name }
  | TestBinary { name }
  | ExampleBinary { name }
  | BenchBinary { name }
  | SyntheticTool { name } -> name

let compare_artifact_kind = fun left right ->
  match Int.compare (artifact_kind_rank left) (artifact_kind_rank right) with
  | Order.EQ -> String.compare (artifact_kind_name left) (artifact_kind_name right)
  | order -> order

let key_to_string = fun (key: key) ->
  String.concat
    ":"
    [
      Package_name.to_string key.package;
      artifact_kind_to_string key.artifact;
      Target.to_string key.target;
      key.profile.name;
    ]

let compare_key = fun (left: key) (right: key) ->
  match Package_name.compare left.package right.package with
  | Order.EQ -> (
      match compare_artifact_kind left.artifact right.artifact with
      | Order.EQ -> (
          match Target.compare left.target right.target with
          | Order.EQ -> String.compare left.profile.name right.profile.name
          | order -> order
        )
      | order -> order
    )
  | order -> order

let equal_key = fun (left: key) (right: key) -> compare_key left right = Order.EQ

let package_key = fun (key: key) -> Package.key_of_string (key_to_string key)
