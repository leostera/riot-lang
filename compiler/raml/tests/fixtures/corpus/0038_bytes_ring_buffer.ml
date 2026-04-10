(* Low-level ring buffer over bytes. *)
type t = {
  buf : bytes;
  mutable head : int;
  mutable len : int;
}

let create n = { buf = Bytes.make n '_'; head = 0; len = 0 }

let capacity t = Bytes.length t.buf

let push t c =
  let pos = (t.head + t.len) mod capacity t in
  Bytes.set t.buf pos c;
  if t.len < capacity t then
    t.len <- t.len + 1
  else
    t.head <- (t.head + 1) mod capacity t

let contents t =
  String.init t.len (fun i ->
      Bytes.get t.buf ((t.head + i) mod capacity t))

let () =
  let t = create 5 in
  List.iter (push t) [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ];
  print_endline (contents t)
