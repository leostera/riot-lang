(** Common types and utilities for HTTP/1.1 parsing *)
open Std

type 'a parse_result =
  | Done of { value: 'a; remaining: string }
  | Need_more
  | Error of string

(** Helper: Find substring in string *)
let find_substring = fun ~needle haystack ->
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec search pos =
    if pos + needle_len > haystack_len then
      None
    else if String.sub haystack ~offset:pos ~len:needle_len = needle then
      Some pos
    else
      search (pos + 1)
  in
  search 0

(** Helper: Split string at position *)
let split_at = fun str pos ->
  let left = String.sub str ~offset:0 ~len:pos in
  let right = String.sub str ~offset:pos ~len:(String.length str - pos) in
  (left, right)
