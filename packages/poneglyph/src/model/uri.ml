open Std
open Std.Collections

module Interner = struct
  type t = {
    string_to_id : (string, int) HashMap.t;
    id_to_string : (int, string) HashMap.t;
    mutable next_id : int;
  }

  let create () =
    {
      string_to_id = HashMap.create ();
      id_to_string = HashMap.create ();
      next_id = 1;
    }

  let intern interner str =
    match HashMap.get interner.string_to_id str with
    | Some id -> id
    | None ->
        let id = interner.next_id in
        interner.next_id <- id + 1;
        let _ = HashMap.insert interner.string_to_id str id in
        let _ = HashMap.insert interner.id_to_string id str in
        id

  let to_string interner id =
    match HashMap.get interner.id_to_string id with
    | Some str -> str
    | None -> format "<unknown-uri-%d>" id
end

let global_interner = Interner.create ()

type t = int
type part = Ns of string | Kind of string | Id of string | Field of string

let expand_shorthand str =
  if String.starts_with ~prefix:"@" str then
    "poneglyph:" ^ String.sub str 1 (String.length str - 1)
  else str

let of_string str =
  let expanded = expand_shorthand str in
  Interner.intern global_interner expanded

let part_to_string = function Ns s | Kind s | Id s | Field s -> s

let make parts =
  let str = String.concat ":" (List.map part_to_string parts) in
  of_string str

let to_string id = Interner.to_string global_interner id
let equal = Int.equal
let compare = Int.compare
let ns s = Ns s
let kind s = Kind s
let id fmt = Printf.ksprintf (fun s -> Id s) fmt
let field s = Field s
