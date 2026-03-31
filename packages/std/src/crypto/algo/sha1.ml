(** SHA-1 cryptographic hash implementation *)
open Global
module Bytes = IO.Bytes
module List = Collections.List
module Array = Collections.Array

type state = {
  mutable buffer: IO.Buffer.t;
}

let create = fun () -> {buffer = IO.Buffer.create 256}

let write = fun state bytes ->
  IO.Buffer.add_bytes state.buffer bytes

let write_string = fun state s -> write state (Bytes.unsafe_of_string s)

let write_unit = fun state () -> ()

let write_int = fun state i ->
  let b = Bytes.create 8 in
  Bytes.set_int64_ne b 0 (Int64.of_int i);
  write state b

let write_int32 = fun state i ->
  let b = Bytes.create 4 in
  Bytes.set_int32_ne b 0 i;
  write state b

let write_int64 = fun state i ->
  let b = Bytes.create 8 in
  Bytes.set_int64_ne b 0 i;
  write state b

let write_float = fun state f -> write_int64 state (Int64.bits_of_float f)

let write_bool = fun state b ->
  let byte = Bytes.create 1 in
  Bytes.set byte 0
    (
      if b then
        '\001'
      else
        '\000'
    );
  write state byte

let write_list = fun writer state lst ->
  write_int state (List.length lst);
  List.iter (writer state) lst

let write_array = fun writer state arr ->
  write_int state (Array.length arr);
  Array.iter (writer state) arr

let finish = fun state ->
  let data = IO.Buffer.to_bytes state.buffer in
  Kernel.Crypto.FFI.sha1 (Bytes.unsafe_to_string data)

let hash_string = fun s ->
  let state = create () in
  write_string state s;
  finish state

let hash_bytes = fun b ->
  let state = create () in
  write state b;
  finish state

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
      write s (Kernel.Crypto.Hash.to_bytes h))
    state
    lst;
  finish state

let hash_array = fun hasher arr ->
  let state = create () in
  write_array
    (fun s x ->
      let h = hasher x in
      write s (Kernel.Crypto.Hash.to_bytes h))
    state
    arr;
  finish state
