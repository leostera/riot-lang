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
  | Get | Head | Options | Trace -> true
  | Post | Put | Delete | Connect | Patch | Extension _ -> false

let is_idempotent = function
  | Get | Head | Put | Delete | Options | Trace -> true
  | Post | Connect | Patch | Extension _ -> false

let is_cacheable = function
  | Get | Head | Post -> true
  | Put | Delete | Connect | Options | Trace | Patch | Extension _ -> false

let compare m1 m2 =
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

let equal m1 m2 = compare m1 m2 = 0
