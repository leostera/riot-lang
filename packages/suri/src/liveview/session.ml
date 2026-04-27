open Std

(* Constant-time string comparison to prevent timing attacks *)

let secure_compare = fun s1 s2 ->
  if not (String.length s1 = String.length s2) then
    false
  else
    let mismatch = ref 0 in
    for i = 0 to String.length s1 - 1 do
      let c1 = Char.code (String.get_unchecked s1 ~at:i) in
      let c2 = Char.code (String.get_unchecked s2 ~at:i) in
      mismatch := !mismatch lor (c1 lxor c2)
    done;
  !mismatch = 0

let digest_to_string = fun digest ->
  digest
  |> Crypto.Digest.bytes
  |> IO.Bytes.to_string

let normalize_hmac_key = fun secret ->
  let key =
    if String.length secret > 64 then
      Crypto.Sha256.hash_string secret
      |> digest_to_string
    else
      secret
  in
  if String.length key < 64 then
    key ^ String.make ~len:(64 - String.length key) ~char:'\000'
  else
    key

let xor_with_byte = fun data byte ->
  String.init
    ~len:(String.length data)
    ~fn:(fun index ->
      let value = Char.code (String.get_unchecked data ~at:index) lxor byte in
      Char.chr value)

let hmac_sha256 = fun ~secret data ->
  let key = normalize_hmac_key secret in
  let inner_key = xor_with_byte key 0x36 in
  let outer_key = xor_with_byte key 0x5c in
  let inner_hash =
    Crypto.Sha256.hash_string (inner_key ^ data)
    |> digest_to_string
  in
  Crypto.Sha256.hash_string (outer_key ^ inner_hash)
  |> digest_to_string

(* Sign data with HMAC-SHA256 and return base64-encoded signature *)

let sign = fun ~secret ~data ->
  hmac_sha256 ~secret data
  |> Encoding.Base64.encode

(* Verify HMAC signature using constant-time comparison *)

let verify = fun ~secret ~data ~signature ->
  let expected_sig = sign ~secret ~data in
  secure_compare expected_sig signature

(* Encode JSON as signed session token *)

let encode = fun ~secret ~json ->
  (* 1. Serialize JSON to string *)
  let json_str = Data.Json.to_string json in
  (* 2. Base64-encode the JSON *)
  let payload = Encoding.Base64.encode json_str in
  (* 3. Sign the payload *)
  let signature = sign ~secret ~data:payload in
  (* 4. Return "<payload>.<signature>" *)
  payload ^ "." ^ signature

(* Decode and verify signed session token *)

let decode = fun ~secret ~token ->
  (* 1. Split on "." to get payload and signature *)
  match String.split_on_char '.' token with
  | [ payload; signature ] ->
      (* 2. Verify signature *)
      if not (verify ~secret ~data:payload ~signature) then
        Error "Invalid signature (token may have been tampered with)"
      else
        (* 3. Base64-decode the payload *)
        (
          match Encoding.Base64.decode payload with
          | Error _ -> Error "Invalid base64 encoding in payload"
          | Ok json_str ->
              (* 4. Parse JSON *)
              match Data.Json.of_string json_str with
              | Error err -> Error ("Failed to parse JSON: " ^ Data.Json.error_to_string err)
              | Ok json -> Ok json
        )
  | _ -> Error "Invalid token format (expected '<payload>.<signature>')"
