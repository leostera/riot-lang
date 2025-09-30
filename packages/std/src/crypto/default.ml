(** Default hasher and random state for HashMap/HashSet *)

(** Default hasher using kernel's default hash algorithm *)
module DefaultHasher = struct
  type state = { mutable buffer : Buffer.t }

  let create () = { buffer = Buffer.create 64 }
  let write state bytes = Buffer.add_bytes state.buffer bytes
  let write_string state s = write state (Bytes.unsafe_of_string s)

  let write_unit state () =
    (* Default: unit is a single zero byte *)
    write state (Bytes.create 1)

  let write_int state i =
    let b = Bytes.create 8 in
    Bytes.set_int64_ne b 0 (Int64.of_int i);
    write state b

  let write_int32 state i =
    let b = Bytes.create 4 in
    Bytes.set_int32_ne b 0 i;
    write state b

  let write_int64 state i =
    let b = Bytes.create 8 in
    Bytes.set_int64_ne b 0 i;
    write state b

  let write_float state f = write_int64 state (Int64.bits_of_float f)

  let write_bool state b =
    let byte = Bytes.create 1 in
    Bytes.set byte 0 (if b then '\001' else '\000');
    write state byte

  let write_list writer state lst =
    (* Default: include length for better distribution *)
    write_int state (List.length lst);
    List.iter (writer state) lst

  let write_array writer state arr =
    (* Default: include length for better distribution *)
    write_int state (Array.length arr);
    Array.iter (writer state) arr

  let finish state =
    let data = Buffer.to_bytes state.buffer in
    Kernel.Crypto.FFI.default_hash (Bytes.unsafe_to_string data)

  (* Convenience functions *)
  let hash_string s = Kernel.Crypto.FFI.default_hash s
  let hash_bytes b = Kernel.Crypto.FFI.default_hash (Bytes.unsafe_to_string b)

  let hash_unit () =
    let state = create () in
    write_unit state ();
    finish state

  let hash_int i =
    let state = create () in
    write_int state i;
    finish state

  let hash_int32 i =
    let state = create () in
    write_int32 state i;
    finish state

  let hash_int64 i =
    let state = create () in
    write_int64 state i;
    finish state

  let hash_float f =
    let state = create () in
    write_float state f;
    finish state

  let hash_bool b =
    let state = create () in
    write_bool state b;
    finish state

  let hash_list hasher lst =
    let state = create () in
    write_list
      (fun s x ->
        let h = hasher x in
        write s (Kernel.Crypto.Hash.to_bytes h))
      state lst;
    finish state

  let hash_array hasher arr =
    let state = create () in
    write_array
      (fun s x ->
        let h = hasher x in
        write s (Kernel.Crypto.Hash.to_bytes h))
      state arr;
    finish state
end

(** Random state for HashMap/HashSet - provides seeded hashing *)
module RandomState = struct
  type t = { seed1 : int64; seed2 : int64 }

  let create () =
    (* In production, these should be truly random *)
    { seed1 = Random.int64 Int64.max_int; seed2 = Random.int64 Int64.max_int }

  (** Hash with this random state for DoS resistance *)
  let hash_with_seed state data seed1 seed2 =
    (* Mix the default hash with random seeds *)
    let base_hash = DefaultHasher.hash_string data in
    let bytes = Kernel.Crypto.Hash.to_bytes base_hash in
    let h = ref (Bytes.get_int64_ne bytes 0) in
    h := Int64.logxor !h seed1;
    h := Int64.mul !h 0x85ebca6bL;
    h := Int64.logxor !h (Int64.shift_right_logical !h 13);
    h := Int64.logxor !h seed2;
    h := Int64.mul !h 0xc2b2ae35L;
    h := Int64.logxor !h (Int64.shift_right_logical !h 16);
    let result = Bytes.create 8 in
    Bytes.set_int64_ne result 0 !h;
    Kernel.Crypto.Hash.of_bytes result

  let hash_with state data = hash_with_seed state data state.seed1 state.seed2

  let to_int64 state hash =
    let base = Digest.to_int64 hash in
    (* Mix with seed for consistency within a HashMap *)
    Int64.logxor base state.seed1
end
