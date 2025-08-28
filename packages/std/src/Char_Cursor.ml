(*

let eof_char = Char.unsafe_of_int 0

(** A peekable iterator over a Char sequence.

    We can peek into the next characters via `first`,
    and eat a char with `next`. *)
type t =
  { source : string
  ; mutable position : int
  ; mutable length_remaining : int
  ; mutable chars : Uchar.t MutIterator.t
  ; mutable last_char : Uchar.t
  }

let from_string source =
  { source
  ; last_char = eof_char
  ; chars = String.into_mut_iter source
  ; length_remaining = String.length source
  ; position = 0
  }
;;

let source t = t.source
let last_char t = t.last_char
let length_remaining t = t.length_remaining
let position t = t.position

let clone ({ source; position; length_remaining; chars; last_char } : t) =
  { source; position; length_remaining; chars; last_char }
;;

let next t =
  (* let char, chars = Iterator.next t.chars in *)
  (* t.chars <- chars; *)
  let char = MutIterator.next t.chars in
  match char with
  | Some char ->
    let length = Uchar.utf_8_byte_length char in
    t.last_char <- char;
    t.length_remaining <- t.length_remaining - length;
    t.position <- t.position + length;
    char
  | None -> eof_char
;;

let skip t = ignore (next t)

let first t =
  (* let chars, _ = Iterator.next t.chars in *)
  let iter = MutIterator.clone t.chars in
  let chars = MutIterator.next iter in
  Option.value ~default:eof_char chars
;;

let second t =
  (* let _, chars = Iterator.next t.chars in *)
  (* let chars, _ = Iterator.next chars in *)
  let iter = MutIterator.clone t.chars in
  let _ = MutIterator.next iter in
  let chars = MutIterator.next iter in
  Option.value ~default:eof_char chars
;;

let third t =
  let iter = MutIterator.clone t.chars in
  let _ = MutIterator.next iter in
  let _ = MutIterator.next iter in
  let chars = MutIterator.next iter in
  Option.value ~default:eof_char chars
;;

let is_eof t = Uchar.equal (first t) eof_char

let eat_while fn t =
  while fn (first t) && not (is_eof t) do
    skip t
  done
;;

(*************************************************************************************************)

module Iter = struct
  type state = t
  type item = Uchar.t

  let next cursor =
    let item = next cursor in
    (if item = eof_char then None else Some item), cursor
  ;;

  let size cursor = length_remaining cursor
end

let into_iter t : Uchar.t Iterator.t = Iterator.make (module Iter) t

(*************************************************************************************************)

(*
module Tests = struct
  let%test "empty string yields EOF" =
    let cursor = from_string "" in
    Uchar.equal (next cursor) eof_char
  ;;

  let%test "end of cursor yields EOF" =
    let cursor = from_string "abc" in
    let _ = next cursor in
    let _ = next cursor in
    let _ = next cursor in
    Uchar.equal (next cursor) eof_char
  ;;

  let%test "last_char has the last character" =
    let cursor = from_string "abc" in
    let _ = next cursor in
    Uchar.equal (last_char cursor) (Uchar.of_char 'a')
  ;;

  let%test "first doesn't eat a character" =
    let cursor = from_string "abc" in
    let peek1 = first cursor in
    let eat1 = next cursor in
    Uchar.equal peek1 eat1
  ;;

  let%test "second doesn't eat a character" =
    let cursor = from_string "abc" in
    let peek2 = second cursor in
    let _eat1 = next cursor in
    let eat2 = next cursor in
    Uchar.equal peek2 eat2
  ;;

  let%test "third doesn't eat a character" =
    let cursor = from_string "abc" in
    let peek3 = third cursor in
    let _eat1 = next cursor in
    let _eat2 = next cursor in
    let eat3 = next cursor in
    Uchar.equal peek3 eat3
  ;;

  let%test "peeking beyond the end returns EOF" =
    let cursor = from_string "abc" in
    let _ = next cursor in
    let peek3 = third cursor in
    Uchar.equal peek3 eof_char
  ;;

  let%test "check for end of file on empty cursors" =
    let cursor = from_string "" in
    is_eof cursor = true
  ;;

  let%test "check for end of file on non-empty cursors" =
    let cursor = from_string "abc" in
    is_eof cursor = false
  ;;

  let%test "check for end of file on consumed cursors" =
    let cursor = from_string "abc" in
    let _ = next cursor in
    let _ = next cursor in
    let _ = next cursor in
    is_eof cursor = true
  ;;

  let%test "check for end of file on consumed cursors" =
    let cursor = from_string "abc" in
    let _ = next cursor in
    let _ = next cursor in
    let _ = next cursor in
    let _ = next cursor in
    is_eof cursor = true
  ;;
end
*)

*)
