open Std

type 'msg t = {
  handlers: (string, string -> 'msg) Std.Collections.HashMap.t;
  mutable next_id: int;
}

let create () = {
  handlers = Std.Collections.HashMap.create ();
  next_id = 0;
}

let register t handler =
  let id = "lv-" ^ Int.to_string t.next_id in
  t.next_id <- t.next_id + 1;
  let _ = Std.Collections.HashMap.insert t.handlers id handler in
  id

let find t id =
  Std.Collections.HashMap.get t.handlers id

let clear t =
  Std.Collections.HashMap.clear t.handlers;
  t.next_id <- 0

let size t =
  Std.Collections.HashMap.len t.handlers
