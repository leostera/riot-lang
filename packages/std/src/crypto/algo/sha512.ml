(** SHA512 hash algorithm *)
open Kernel

type state = Common.state

let create = Common.create_state

let write = Common.write_string

let write_iovec = Common.write_iovec

let write_hash = Common.write_hash

let write_unit = fun state () -> ()

let write_int = fun state value -> Common.push_bytes state (Common.bytes_of_int value)

let write_int32 = fun state value -> Common.push_bytes state (Common.bytes_of_int32 value)

let write_int64 = fun state value -> Common.push_bytes state (Common.bytes_of_int64 value)

let write_float = fun state value -> Common.push_bytes state (Common.bytes_of_float value)

let write_bool = fun state value -> Common.push_bytes state (Common.bytes_of_bool value)

let write_list = fun writer state lst ->
  write_int state (Common.list_length lst);
  Common.iter_list (writer state) lst

let write_array = fun writer state arr ->
  write_int state (Array.length arr);
  Array.for_each arr ~fn:(writer state)

let finish = fun state -> Common.finish_iovec Ffi.sha512_iovec state

let hash_string = Common.hash_string_with Ffi.sha512_iovec

let hash_bytes = Common.hash_bytes_with Ffi.sha512_iovec

let hash_unit = fun () ->
  let state = create () in
  write_unit state ();
  finish state

let hash_int = fun i ->
  let state = create () in
  write_int state i;
  finish state

let hash_int32 = fun i ->
  let state = create () in
  write_int32 state i;
  finish state

let hash_int64 = fun i ->
  let state = create () in
  write_int64 state i;
  finish state

let hash_float = fun f ->
  let state = create () in
  write_float state f;
  finish state

let hash_bool = fun b ->
  let state = create () in
  write_bool state b;
  finish state

let hash_list = fun hasher lst ->
  let state = create () in
  write_list
    (fun s x ->
      let h = hasher x in
      write_hash s h)
    state
    lst;
  finish state

let hash_array = fun hasher arr ->
  let state = create () in
  write_array
    (fun s x ->
      let h = hasher x in
      write_hash s h)
    state
    arr;
  finish state
