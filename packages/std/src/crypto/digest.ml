(** Digest functions for converting hashes to various formats *)
open Global
open IO
open Kernel.Crypto

(** Convert hash to hexadecimal string *)
let hex = fun hash ->
    let bytes = Hash.to_bytes hash in
    let len = Bytes.length bytes in
    let buf = Buffer.create (len * 2) in
    for i = 0 to len - 1 do
      let byte = Bytes.get bytes i |> Char.code in
      let hi = byte lsr 4 in
      let lo = byte land 0x0f in
      let hex_char n =
        if n < 10 then
          Char.chr (48 + n)
        else
          Char.chr (87 + n)
      in
      Buffer.add_char buf (hex_char hi);
      Buffer.add_char buf (hex_char lo)
    done;
    Buffer.contents buf

(** Convert hash to base64 string *)
let base64 = fun hash ->
    let bytes = Hash.to_bytes hash in
    (* Simple base64 encoding - could be optimized *)
    let b64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
    let len = Bytes.length bytes in
    let buf = Buffer.create ((len + 2) / 3 * 4) in
    let rec encode i =
      if i < len then
        (
          let b1 = Bytes.get bytes i |> Char.code in
          let b2 =
            if i + 1 < len then
              Bytes.get bytes (i + 1) |> Char.code
            else
              0
          in
          let b3 =
            if i + 2 < len then
              Bytes.get bytes (i + 2) |> Char.code
            else
              0
          in
          Buffer.add_char buf b64_chars.[b1 lsr 2];
          Buffer.add_char buf b64_chars.[((b1 land 0x03) lsl 4) lor (b2 lsr 4)];
          if i + 1 < len then
            Buffer.add_char buf b64_chars.[((b2 land 0x0f) lsl 2) lor (b3 lsr 6)]
          else
            Buffer.add_char buf '=';
            if i + 2 < len then
              Buffer.add_char buf b64_chars.[b3 land 0x3f]
            else
              Buffer.add_char buf '=';
              encode (i + 3)
        )
    in
    encode 0;
    Buffer.contents buf

(** Convert hash to URL-safe base64 string *)
let base64_url = fun hash ->
    let b64 = base64 hash in
    String.map
      (
        function
        | '+' -> '-'
        | '/' -> '_'
        | c -> c
      )
      b64

(** Get raw bytes of hash *)
let bytes = Hash.to_bytes

(** Convert hash to int64 (truncates if necessary) *)
let to_int64 = fun hash ->
    let bytes = Hash.to_bytes hash in
    let len = min 8 (Bytes.length bytes) in
    let result = ref 0L in
    for i = 0 to len - 1 do
      let byte = Bytes.get bytes i |> Char.code |> Int64.of_int in
      result := Int64.logor !result (Int64.shift_left byte (i * 8))
    done;
    !result

(** Convert hash to int (truncates) *)
let to_int = fun hash -> Int64.to_int (to_int64 hash)
