open Std

type t =
  | Complete of Work_request.t list
  | RequeueWithDependencies of Work_request.t list
