open Std

type 'msg t = {
  handlers: (string, string -> 'msg) Std.Collections.HashMap.t;
  mutable next_id: int;
}

let create = fun () -> { handlers = Std.Collections.HashMap.create (); next_id = 0 }

let register = fun t handler ->
  let id = "lv-" ^ Int.to_string t.next_id in
  t.next_id <- t.next_id + 1;
  let _ = Std.Collections.HashMap.insert t.handlers ~key:id ~value:handler in
  id

let find = fun t id -> Std.Collections.HashMap.get t.handlers ~key:id

let clear = fun t ->
  Std.Collections.HashMap.clear t.handlers;
  t.next_id <- 0

let size = fun t -> Std.Collections.HashMap.length t.handlers
