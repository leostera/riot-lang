open Global

(*
   TODO: This module currently uses pure OCaml implementations for UUID 
   generation and hashing. We should replace this with bindings to a 
   battle-tested C or Rust library for:
   
   1. Cryptographically secure random number generation (v4)
   2. Native SHA-1 and MD5 implementations (v3, v5)
   3. Better performance
   4. Security audited implementations
   
   Consider binding to:
   - libuuid (C, widely available on Unix systems)
   - uuid crate (Rust, via raml-ffi)
   
   The current implementation is based on uuidm and works correctly but
   should not be considered production-ready for security-critical uses.
*)

type t = string

let md5 = Digest.string

let sha_1 s =
  let sha_1_pad s =
    let len = String.length s in
    let blen = 8 * len in
    let rem = len mod 64 in
    let mlen = if rem > 55 then len + 128 - rem else len + 64 - rem in
    let m = Bytes.create mlen in
    Bytes.blit_string s 0 m 0 len;
    Bytes.fill m len (mlen - len) '\x00';
    Bytes.set m len '\x80';
    if Sys.word_size > 32 then (
      Bytes.set_uint8 m (mlen - 8) (blen lsr 56 land 0xFF);
      Bytes.set_uint8 m (mlen - 7) (blen lsr 48 land 0xFF);
      Bytes.set_uint8 m (mlen - 6) (blen lsr 40 land 0xFF);
      Bytes.set_uint8 m (mlen - 5) (blen lsr 32 land 0xFF));
    Bytes.set_uint8 m (mlen - 4) (blen lsr 24 land 0xFF);
    Bytes.set_uint8 m (mlen - 3) (blen lsr 16 land 0xFF);
    Bytes.set_uint8 m (mlen - 2) (blen lsr 8 land 0xFF);
    Bytes.set_uint8 m (mlen - 1) (blen land 0xFF);
    m
  in
  let ( &&& ) = ( land ) in
  let ( lor ) = Int32.logor in
  let ( lxor ) = Int32.logxor in
  let ( land ) = Int32.logand in
  let ( ++ ) = Int32.add in
  let lnot = Int32.lognot in
  let sl = Int32.shift_left in
  let cls n x = sl x n lor Int32.shift_right_logical x (32 - n) in
  let m = sha_1_pad s in
  let w = Array.make 16 0l in
  let h0 = cell 0x67452301l in
  let h1 = cell 0xEFCDAB89l in
  let h2 = cell 0x98BADCFEl in
  let h3 = cell 0x10325476l in
  let h4 = cell 0xC3D2E1F0l in
  let a = cell 0l in
  let b = cell 0l in
  let c = cell 0l in
  let d = cell 0l in
  let e = cell 0l in
  for i = 0 to (Bytes.length m / 64) - 1 do
    let base = i * 64 in
    for j = 0 to 15 do
      w.(j) <- Bytes.get_int32_be m (base + (j * 4))
    done;
    Cell.set a (Cell.get h0);
    Cell.set b (Cell.get h1);
    Cell.set c (Cell.get h2);
    Cell.set d (Cell.get h3);
    Cell.set e (Cell.get h4);
    for t = 0 to 79 do
      let f, k =
        if t <= 19 then
          (Cell.get b land Cell.get c) lor (lnot (Cell.get b) land Cell.get d),
          0x5A827999l
        else if t <= 39 then Cell.get b lxor Cell.get c lxor Cell.get d, 0x6ED9EBA1l
        else if t <= 59 then
          (Cell.get b land Cell.get c)
          lor (Cell.get b land Cell.get d)
          lor (Cell.get c land Cell.get d),
          0x8F1BBCDCl
        else Cell.get b lxor Cell.get c lxor Cell.get d, 0xCA62C1D6l
      in
      let s = t &&& 0xF in
      if t >= 16 then
        w.(s) <-
          cls 1
            (w.((s + 13) &&& 0xF)
            lxor w.((s + 8) &&& 0xF)
            lxor w.((s + 2) &&& 0xF)
            lxor w.(s));
      let temp = cls 5 (Cell.get a) ++ f ++ Cell.get e ++ w.(s) ++ k in
      Cell.set e (Cell.get d);
      Cell.set d (Cell.get c);
      Cell.set c (cls 30 (Cell.get b));
      Cell.set b (Cell.get a);
      Cell.set a temp
    done;
    Cell.set h0 (Cell.get h0 ++ Cell.get a);
    Cell.set h1 (Cell.get h1 ++ Cell.get b);
    Cell.set h2 (Cell.get h2 ++ Cell.get c);
    Cell.set h3 (Cell.get h3 ++ Cell.get d);
    Cell.set h4 (Cell.get h4 ++ Cell.get e)
  done;
  let h = Bytes.create 20 in
  let i2s h k i = Bytes.set_int32_be h k i in
  i2s h 0 (Cell.get h0);
  i2s h 4 (Cell.get h1);
  i2s h 8 (Cell.get h2);
  i2s h 12 (Cell.get h3);
  i2s h 16 (Cell.get h4);
  Bytes.unsafe_to_string h

let make u ~version =
  let b6 = (version lsl 4) lor (Bytes.get_uint8 u 6 land 0b0000_1111) in
  let b8 = 0b1000_0000 lor (Bytes.get_uint8 u 8 land 0b0011_1111) in
  Bytes.set_uint8 u 6 b6;
  Bytes.set_uint8 u 8 b8;
  Bytes.unsafe_to_string u

let make_named ~version digest ns n =
  let hash = Bytes.unsafe_of_string (digest (ns ^ n)) in
  make (Bytes.sub hash 0 16) ~version

let v3 ~namespace ~name = make_named ~version:3 md5 namespace name
let v5 ~namespace ~name = make_named ~version:5 sha_1 namespace name
let v4_from_bytes b = make (Bytes.sub b 0 16) ~version:4

let v7_from_parts ~time_ms ~rand_a ~rand_b =
  let u = Bytes.create 16 in
  Bytes.set_int64_be u 0 (Int64.shift_left time_ms 16);
  Bytes.set_int16_be u 6 rand_a;
  Bytes.set_int64_be u 8 rand_b;
  make u ~version:7

let default_random_state =
  lazy (Random.State.make_self_init ())

let v4 () =
  let rstate = Lazy.force default_random_state in
  let r0 = Random.State.bits64 rstate in
  let r1 = Random.State.bits64 rstate in
  let u = Bytes.create 16 in
  Bytes.set_int64_be u 0 r0;
  Bytes.set_int64_be u 8 r1;
  make u ~version:4

let v7 () =
  let rstate = Lazy.force default_random_state in
  let time_ms = Int64.of_int (Time.SystemTime.(now () |> elapsed) |> Time.Duration.to_millis) in
  let rand_a = Random.State.bits rstate in
  let rand_b = Random.State.bits64 rstate in
  v7_from_parts ~time_ms ~rand_a ~rand_b

let nil = "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
let max = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
let ns_dns = "\x6b\xa7\xb8\x10\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"
let ns_url = "\x6b\xa7\xb8\x11\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"
let ns_oid = "\x6b\xa7\xb8\x12\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"
let ns_x500 = "\x6b\xa7\xb8\x14\x9d\xad\x11\xd1\x80\xb4\x00\xc0\x4f\xd4\x30\xc8"

let variant u = String.get_uint8 u 8 lsr 4

let version u =
  let v = String.get_uint8 u 6 lsr 4 in
  if v = 0 then None else Some v

let time u =
  let variant = variant u in
  if not (0x8 <= variant && variant <= 0xB && version u = Some 7) then None
  else
    let ms = Int64.shift_right_logical (String.get_int64_be u 0) 16 in
    let ms = Int64.to_int ms in
    Some (Time.Duration.from_millis ms)

let equal = String.equal
let compare = String.compare
let is_nil u = equal u nil

let of_bytes bytes =
  if Bytes.length bytes <> 16 then
    Error (`Invalid_uuid (format "Expected 16 bytes, got %d" (Bytes.length bytes)))
  else Ok (Bytes.unsafe_to_string bytes)

let to_bytes u = Bytes.unsafe_of_string u

let of_string s =
  let pos = 0 in
  let len = String.length s in
  if
    pos + 36 > len
    || s.[pos + 8] <> '-'
    || s.[pos + 13] <> '-'
    || s.[pos + 18] <> '-'
    || s.[pos + 23] <> '-'
  then Error (`Invalid_uuid "Invalid UUID format")
  else
    try
      let u = Bytes.create 16 in
      let i = cell 0 in
      let j = cell pos in
      let ihex c =
        let code = Char.code c in
        if code < 0x30 then raise Exit
        else if code <= 0x39 then code - 0x30
        else if code < 0x41 then raise Exit
        else if code <= 0x46 then code - 0x37
        else if code < 0x61 then raise Exit
        else if code <= 0x66 then code - 0x57
        else raise Exit
      in
      let byte s j =
        Char.unsafe_chr (ihex s.[j] lsl 4 lor ihex s.[j + 1])
      in
      while Cell.get i < 4 do
        Bytes.set u (Cell.get i) (byte s (Cell.get j));
        Cell.set j (Cell.get j + 2);
        Cell.set i (Cell.get i + 1)
      done;
      Cell.set j (Cell.get j + 1);
      while Cell.get i < 6 do
        Bytes.set u (Cell.get i) (byte s (Cell.get j));
        Cell.set j (Cell.get j + 2);
        Cell.set i (Cell.get i + 1)
      done;
      Cell.set j (Cell.get j + 1);
      while Cell.get i < 8 do
        Bytes.set u (Cell.get i) (byte s (Cell.get j));
        Cell.set j (Cell.get j + 2);
        Cell.set i (Cell.get i + 1)
      done;
      Cell.set j (Cell.get j + 1);
      while Cell.get i < 10 do
        Bytes.set u (Cell.get i) (byte s (Cell.get j));
        Cell.set j (Cell.get j + 2);
        Cell.set i (Cell.get i + 1)
      done;
      Cell.set j (Cell.get j + 1);
      while Cell.get i < 16 do
        Bytes.set u (Cell.get i) (byte s (Cell.get j));
        Cell.set j (Cell.get j + 2);
        Cell.set i (Cell.get i + 1)
      done;
      Ok (Bytes.unsafe_to_string u)
    with Exit -> Error (`Invalid_uuid "Invalid hexadecimal characters")

let to_string ?(upper = false) u =
  let hbase = if upper then 0x37 else 0x57 in
  let hex hbase i = Char.unsafe_chr (if i < 10 then 0x30 + i else hbase + i) in
  let s = Bytes.of_string "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX" in
  let i = cell 0 in
  let j = cell 0 in
  let byte s i c =
    Bytes.set s i (hex hbase (c lsr 4));
    Bytes.set s (i + 1) (hex hbase (c land 0x0F))
  in
  while Cell.get j < 4 do
    byte s (Cell.get i) (Char.code u.[Cell.get j]);
    Cell.set i (Cell.get i + 2);
    Cell.set j (Cell.get j + 1)
  done;
  Cell.set i (Cell.get i + 1);
  while Cell.get j < 6 do
    byte s (Cell.get i) (Char.code u.[Cell.get j]);
    Cell.set i (Cell.get i + 2);
    Cell.set j (Cell.get j + 1)
  done;
  Cell.set i (Cell.get i + 1);
  while Cell.get j < 8 do
    byte s (Cell.get i) (Char.code u.[Cell.get j]);
    Cell.set i (Cell.get i + 2);
    Cell.set j (Cell.get j + 1)
  done;
  Cell.set i (Cell.get i + 1);
  while Cell.get j < 10 do
    byte s (Cell.get i) (Char.code u.[Cell.get j]);
    Cell.set i (Cell.get i + 2);
    Cell.set j (Cell.get j + 1)
  done;
  Cell.set i (Cell.get i + 1);
  while Cell.get j < 16 do
    byte s (Cell.get i) (Char.code u.[Cell.get j]);
    Cell.set i (Cell.get i + 2);
    Cell.set j (Cell.get j + 1)
  done;
  Bytes.unsafe_to_string s

let to_string_nodash ?(upper = false) u =
  let s = to_string ~upper u in
  let without_dashes = Buffer.create 32 in
  String.iter
    (fun c -> if c <> '-' then Buffer.add_char without_dashes c)
    s;
  Buffer.contents without_dashes
