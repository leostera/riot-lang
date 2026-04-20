open Global
module Slice = IO.Iovec.IoSlice

type t =
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch
  | Extension of string

let of_string = function
  | "GET" -> Get
  | "HEAD" -> Head
  | "POST" -> Post
  | "PUT" -> Put
  | "DELETE" -> Delete
  | "CONNECT" -> Connect
  | "OPTIONS" -> Options
  | "TRACE" -> Trace
  | "PATCH" -> Patch
  | s -> Extension s

let from_slice = fun value ->
  match Slice.length value with
  | 3 when Slice.equal_string value "GET" -> Get
  | 3 when Slice.equal_string value "PUT" -> Put
  | 4 when Slice.equal_string value "HEAD" -> Head
  | 4 when Slice.equal_string value "POST" -> Post
  | 5 when Slice.equal_string value "PATCH" -> Patch
  | 5 when Slice.equal_string value "TRACE" -> Trace
  | 6 when Slice.equal_string value "DELETE" -> Delete
  | 7 when Slice.equal_string value "CONNECT" -> Connect
  | 7 when Slice.equal_string value "OPTIONS" -> Options
  | _ -> Extension (Slice.to_string value)

let to_string = function
  | Get -> "GET"
  | Head -> "HEAD"
  | Post -> "POST"
  | Put -> "PUT"
  | Delete -> "DELETE"
  | Connect -> "CONNECT"
  | Options -> "OPTIONS"
  | Trace -> "TRACE"
  | Patch -> "PATCH"
  | Extension s -> s

let is_safe = function
  | Get
  | Head
  | Options
  | Trace -> true
  | Post
  | Put
  | Delete
  | Connect
  | Patch
  | Extension _ -> false

let is_idempotent = function
  | Get
  | Head
  | Put
  | Delete
  | Options
  | Trace -> true
  | Post
  | Connect
  | Patch
  | Extension _ -> false

let is_cacheable = function
  | Get
  | Head
  | Post -> true
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch
  | Extension _ -> false

let compare = fun m1 m2 ->
  let method_priority = function
    | Get -> 0
    | Head -> 1
    | Post -> 2
    | Put -> 3
    | Delete -> 4
    | Connect -> 5
    | Options -> 6
    | Trace -> 7
    | Patch -> 8
    | Extension _ -> 9
  in
  match (m1, m2) with
  | Extension s1, Extension s2 -> String.compare s1 s2
  | _ -> Int.compare (method_priority m1) (method_priority m2)

let equal = fun m1 m2 -> compare m1 m2 = 0
