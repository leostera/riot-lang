(** SHA-256 cryptographic hash implementation *)
open Global
module Bytes = IO.Bytes
module Iovec = IO.Iovec
module List = Collections.List
module Array = Collections.Array

type state = {
  mutable segments_rev: Iovec.iov list;
}

let create = fun () -> { segments_rev = [] }

let push_segment = fun state ba ~off ~len -> state.segments_rev <- { Iovec.ba; off; len } :: state.segments_rev

let write = fun state s ->
  if String.length s > 0 then
    push_segment state (Bytes.unsafe_of_string s) ~off:0 ~len:(String.length s)

let write_hash = fun state hash ->
  let data = Kernel.Crypto.Hash.to_bytes hash |> Bytes.to_string in
  write state data

let write_unit = fun state () ->
  (* SHA256 specific: unit is hashed as empty byte sequence *)
  ()

let write_int = fun state i ->
  let bytes = Bytes.create 8 in
  Bytes.set_int64_ne bytes 0 (Int64.of_int i);
  write state (Bytes.unsafe_to_string bytes)

let write_int32 = fun state i ->
  let bytes = Bytes.create 4 in
  Bytes.set_int32_ne bytes 0 i;
  write state (Bytes.unsafe_to_string bytes)

let write_int64 = fun state i ->
  let bytes = Bytes.create 8 in
  Bytes.set_int64_ne bytes 0 i;
  write state (Bytes.unsafe_to_string bytes)

let write_float = fun state f -> write_int64 state (Int64.bits_of_float f)

let write_bool = fun state b ->
  let bytes = Bytes.create 1 in
  Bytes.set bytes 0
    (
      if b then
        '\001'
      else
        '\000'
    );
  write state (Bytes.unsafe_to_string bytes)

let write_list = fun writer state lst ->
  (* SHA256: include length for collision resistance *)
  write_int state (List.length lst);
  List.iter (writer state) lst

let write_array = fun writer state arr ->
  (* SHA256: include length for collision resistance *)
  write_int state (Array.length arr);
  Array.iter (writer state) arr

let finish = fun state ->
  let segments = state.segments_rev |> List.rev |> Array.of_list in
  Kernel.Crypto.FFI.sha256_iovec segments

(* Convenience functions *)

let hash_string = fun s ->
  let state = create () in
  write state s;
  finish state

let hash_bytes = fun b ->
  Kernel.Crypto.FFI.sha256_iovec [|{ Iovec.ba = Bytes.copy b; off = 0; len = Bytes.length b }|]

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
