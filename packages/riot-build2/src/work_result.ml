open Std

type t =
  | Complete of Work_node.key list
  | RequeueWithDependencies of Work_node.key list
