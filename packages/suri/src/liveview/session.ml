open Std

type decode_error =
  | InvalidTokenFormat
  | InvalidSignature
  | InvalidPayloadBase64
  | InvalidJson of Data.Json.error

let decode_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InvalidTokenFormat -> "invalid token format; expected '<payload>.<signature>'"
  | InvalidSignature -> "invalid signature"
  | InvalidPayloadBase64 -> "invalid base64 encoding in payload"
  | InvalidJson error -> "invalid JSON payload: " ^ Data.Json.error_to_string error

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

(* Sign data with HMAC-SHA256 and return base64-encoded signature *)

let sign = fun ~secret ~data ->
  Crypto.hmac_sha256 ~key:secret ~data
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

let decode_payload = fun payload ->
  match Encoding.Base64.decode payload with
  | Ok decoded -> Ok decoded
  | Error Encoding.Base64.InvalidBase64 -> Error InvalidPayloadBase64

(* Decode and verify signed session token *)

let decode = fun ~secret ~token ->
  (* 1. Split on "." to get payload and signature *)
  match String.split_on_char '.' token with
  | [ payload; signature ] ->
      (* 2. Verify signature *)
      if not (verify ~secret ~data:payload ~signature) then
        Error InvalidSignature
      else
        (* 3. Base64-decode the payload *)
        (
          match decode_payload payload with
          | Error error -> Error error
          | Ok json_str ->
              (* 4. Parse JSON *)
              match Data.Json.from_string json_str with
              | Error err -> Error (InvalidJson err)
              | Ok json -> Ok json
        )
  | _ -> Error InvalidTokenFormat
