open Std

(* Sign data with HMAC-SHA256 and return base64-encoded signature *)
let sign = fun ~secret ~data ->
  let payload = secret ^ "\x00" ^ data in
  let mac_bytes = Crypto.Digest.bytes (Crypto.hash_string payload) in
  let mac_string = IO.Bytes.to_string mac_bytes in Encoding.Base64.encode mac_string

(* Constant-time string comparison to prevent timing attacks *)
let secure_compare = fun s1 s2 ->
  if not (String.length s1 = String.length s2) then
    false
  else
    let mismatch = ref 0 in
    for i = 0 to String.length s1 - 1 do
      let c1 = Char.code (String.get_unchecked s1 ~at:i) in
      let c2 = Char.code (String.get_unchecked s2 ~at:i) in mismatch := !mismatch lor (c1 lxor c2)
    done;
  !mismatch = 0

(* Verify HMAC signature using constant-time comparison *)
let verify = fun ~secret ~data ~signature ->
  let expected_sig = sign ~secret ~data in secure_compare expected_sig signature

(* Encode JSON as signed session token *)
let encode = fun ~secret ~json ->
  (* 1. Serialize JSON to string *)
  let json_str = Data.Json.to_string json in
  (* 2. Base64-encode the JSON *)
  let payload = Encoding.Base64.encode json_str in
  (* 3. Sign the payload *)
  let signature = sign ~secret ~data:payload in (* 4. Return "<payload>.<signature>" *)
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
