open Global
module Slice = IO.IoVec.IoSlice

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

let equal_tail = fun value ~at suffix ->
  let suffix_len = String.length suffix in
  if Slice.length value - at != suffix_len then
    false
  else
    let rec loop index =
      if index >= suffix_len then
        true
      else if Slice.get_unchecked value ~at:(at + index) = String.get_unchecked suffix ~at:index then
        loop (index + 1)
      else
        false
    in
    loop 0

let from_slice = fun value ->
  match Slice.length value with
  | 3 -> (
      match Slice.get_unchecked value ~at:0 with
      | 'G' when equal_tail value ~at:1 "ET" -> Get
      | 'P' when equal_tail value ~at:1 "UT" -> Put
      | _ -> Extension (Slice.to_string value)
    )
  | 4 -> (
      match Slice.get_unchecked value ~at:0 with
      | 'H' when equal_tail value ~at:1 "EAD" -> Head
      | 'P' when equal_tail value ~at:1 "OST" -> Post
      | _ -> Extension (Slice.to_string value)
    )
  | 5 -> (
      match Slice.get_unchecked value ~at:0 with
      | 'P' when equal_tail value ~at:1 "ATCH" -> Patch
      | 'T' when equal_tail value ~at:1 "RACE" -> Trace
      | _ -> Extension (Slice.to_string value)
    )
  | 6 ->
      if Slice.get_unchecked value ~at:0 = 'D' && equal_tail value ~at:1 "ELETE" then
        Delete
      else
        Extension (Slice.to_string value)
  | 7 -> (
      match Slice.get_unchecked value ~at:0 with
      | 'C' when equal_tail value ~at:1 "ONNECT" -> Connect
      | 'O' when equal_tail value ~at:1 "PTIONS" -> Options
      | _ -> Extension (Slice.to_string value)
    )
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
