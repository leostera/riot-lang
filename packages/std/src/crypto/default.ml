(** Default hasher and random state for HashMap/HashSet *)
open Kernel

(** Default hasher using kernel's default hash algorithm *)
module DefaultHasher = struct
  type state = Common.state

  let create = Common.create_state

  let write = Common.write_string

  let write_iovec = Common.write_iovec

  let write_hash = Common.write_hash

  let write_unit = fun state () -> Common.push_bytes state (Common.bytes_of_unit ())

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

  let finish = fun state -> Common.finish_iovec Ffi.default_hash_iovec state

  let hash_string = Common.hash_string_with Ffi.default_hash_iovec

  let hash_bytes = Common.hash_bytes_with Ffi.default_hash_iovec

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
end

(** Random state for HashMap/HashSet - provides seeded hashing *)
module RandomState = struct
  type t = {
    seed1: int64;
    seed2: int64;
  }

  let seed_material = fun label ->
    let pid = Int.to_string (Process.current_pid ()) in
    match Time.Monotonic.now () with
    | Ok now ->
        let (secs, nanos) = Time.Monotonic.to_parts now in
        String.concat ":" [ label; pid; Int.to_string secs; Int.to_string nanos; ]
    | Error _ -> String.concat ":" [ label; pid; "0"; "0"; ]

  let create = fun () -> {
    seed1 = Digest.to_int64 (DefaultHasher.hash_string (seed_material "seed1"));
    seed2 = Digest.to_int64 (DefaultHasher.hash_string (seed_material "seed2"));
  }

  (** Hash with this random state for DoS resistance *)
  let hash_with_seed = fun state data seed1 seed2 ->
    let hasher = DefaultHasher.create () in
    DefaultHasher.write_int64 hasher seed1;
    DefaultHasher.write hasher data;
    DefaultHasher.write_int64 hasher seed2;
    DefaultHasher.finish hasher

  let hash_with = fun state data -> hash_with_seed state data state.seed1 state.seed2

  let to_int64 = fun state hash -> Int64.add (Digest.to_int64 hash) state.seed1
end
