open Std

(**
   Session token signing and verification for LiveView components.

   Provides cryptographically signed session tokens to securely pass
   initialization arguments from HTTP embed to WebSocket mount.
*)
val encode: secret:string -> json:Data.Json.t -> string

(**
   Encode a JSON value as a signed session token.

   Process:
   1. Serialize JSON to string
   2. Base64-encode the JSON
   3. Sign with HMAC-SHA256
   4. Return "<base64_json>.<base64_signature>"

   Example:
   ```ocaml
   let json = Data.Json.Object [("id", Data.Json.String "abc-123")] in
   let token = Session.encode ~secret:"my-secret" ~json
   (* Returns: "eyJpZCI6ImFiYy0xMjMifQ.a1b2c3..." *)
   ```
*)
val decode: secret:string -> token:string -> (Data.Json.t, string) result

(**
   Decode and verify a signed session token.

   Process:
   1. Split token on "." to get payload and signature
   2. Verify HMAC signature (constant-time comparison)
   3. Base64-decode the payload
   4. Parse JSON

   Returns Error if:
   - Token format is invalid
   - Signature verification fails (tampering detected)
   - Base64 decoding fails
   - JSON parsing fails

   Example:
   ```ocaml
   match Session.decode ~secret:"my-secret" ~token with
   | Ok json -> (* Use deserialized JSON *)
   | Error err -> Log.error ("Invalid session: " ^ err)
   ```
*)
val sign: secret:string -> data:string -> string

(**
   Sign data with HMAC-SHA256 and return base64-encoded signature.

   Low-level function for signing arbitrary data.
   Most users should use {!encode} instead.
*)
val verify: secret:string -> data:string -> signature:string -> bool

(**
   Verify an HMAC-SHA256 signature using constant-time comparison.

   Low-level function for signature verification.
   Most users should use {!decode} instead.

   @param secret The signing secret
   @param data The data that was signed
   @param signature Base64-encoded signature to verify
   @return true if signature is valid, false otherwise
*)
