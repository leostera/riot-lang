(** Digest functions for converting hashes to various formats *)
open Kernel

(** Convert hash to hexadecimal string *)
let hex = fun hash ->
  let bytes = Hash.to_bytes hash in
  let len = Bytes.length bytes in
  let out = Bytes.create ~size:(len * 2) in
  let hex_char value =
    if value < 10 then
      Char.from_int_unchecked (48 + value)
    else
      Char.from_int_unchecked (87 + value)
  in
  for i = 0 to len - 1 do
    let byte = Bytes.get_unchecked bytes ~at:i |> Char.code in
    let hi = byte lsr 4 in
    let lo = byte land 0x0f in
    Bytes.set_unchecked out ~at:(i * 2) ~char:(hex_char hi);
    Bytes.set_unchecked out ~at:((i * 2) + 1) ~char:(hex_char lo)
  done;
  Bytes.to_string out

(** Convert hash to base64 string *)
let base64 = fun hash ->
  let bytes = Hash.to_bytes hash in
  let b64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let len = Bytes.length bytes in
  let out = Bytes.create ~size:(((len + 2) / 3) * 4) in
  let rec encode input_index output_index =
    if input_index < len then
      (
        let b1 = Bytes.get_unchecked bytes ~at:input_index |> Char.code in
        let b2 =
          if input_index + 1 < len then
            Bytes.get_unchecked bytes ~at:(input_index + 1) |> Char.code
          else
            0
        in
        let b3 =
          if input_index + 2 < len then
            Bytes.get_unchecked bytes ~at:(input_index + 2) |> Char.code
          else
            0
        in
        Bytes.set_unchecked
          out
          ~at:output_index
          ~char:(String.get_unchecked b64_chars ~at:(b1 lsr 2));
        Bytes.set_unchecked
          out
          ~at:(output_index + 1)
          ~char:(String.get_unchecked b64_chars ~at:(((b1 land 0x03) lsl 4) lor (b2 lsr 4)));
        Bytes.set_unchecked out ~at:(output_index + 2)
          ~char:(
            if input_index + 1 < len then
              String.get_unchecked b64_chars ~at:(((b2 land 0x0f) lsl 2) lor (b3 lsr 6))
            else
              '='
          );
        Bytes.set_unchecked out ~at:(output_index + 3)
          ~char:(
            if input_index + 2 < len then
              String.get_unchecked b64_chars ~at:(b3 land 0x3f)
            else
              '='
          );
        encode (input_index + 3) (output_index + 4)
      )
  in
  encode 0 0;
  Bytes.to_string out

(** Convert hash to URL-safe base64 string *)
let base64_url = fun hash ->
  let out = Bytes.from_string (base64 hash) in
  let len = Bytes.length out in
  let rec loop index =
    if index < len then
      (
        match Bytes.get_unchecked out ~at:index with
        | '+' ->
            Bytes.set_unchecked out ~at:index ~char:'-';
            loop (index + 1)
        | '/' ->
            Bytes.set_unchecked out ~at:index ~char:'_';
            loop (index + 1)
        | _ ->
            ();
            loop (index + 1)
      )
  in
  loop 0;
  Bytes.to_string out

(** Get raw bytes of hash *)
let bytes = Hash.to_bytes

(** Convert hash to int64 (truncates if necessary) *)
let to_int64 = fun hash ->
  let bytes = Hash.to_bytes hash in
  let len = Int.min 8 (Bytes.length bytes) in
  let rec loop index factor acc =
    if index >= len then
      acc
    else
      let byte = Bytes.get_unchecked bytes ~at:index |> Char.code |> Int64.from_int in
      loop (index + 1) (Int64.mul factor 256L) (Int64.add acc (Int64.mul byte factor))
  in
  loop 0 1L 0L

(** Convert hash to int (truncates) *)
let to_int = fun hash -> Int64.to_int (to_int64 hash)
