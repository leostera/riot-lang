(**
   Hash digest formatting helpers.

   Convert cryptographic hash values to various output formats for different
   use cases.

   ## Examples

   ```ocaml open Std

   let hash = Crypto.hash_string "Hello, World!" in

   (* Hexadecimal - for display and logging *) let hex = Crypto.Digest.hex hash
   in (* "315f5bdb76d078c43b8ac0064e4a0164612b1fce77c869345bfc94c75894edd3" *)

   (* Base64 - compact representation *) let b64 = Crypto.Digest.base64 hash in
   (* "MV9b23bQeMQ7isAGTkoJZGErH8534ocpRb/JTHWJTt0=" *)

   (* URL-safe Base64 - for URLs and filenames *) let b64url =
   Crypto.Digest.base64_url hash in (*
   "MV9b23bQeMQ7isAGTkoJZGErH8534ocpRb_JTHWJTt0=" *)

   (* Integer - for hash table indexing *) let idx = Crypto.Digest.to_int hash
   ```

   ## When to Use Each Format

   - **hex**: Human-readable, logging, debugging
   - **base64**: Compact storage, API responses
   - **base64_url**: URLs, filenames, query parameters
   - **bytes**: Raw binary, further processing
   - **to_int/to_int64**: Hash table indexing, fast comparison
*)

(** Convert hash to hexadecimal string. *)
val hex: Hash.t -> string

(** Convert hash to base64 string. *)
val base64: Hash.t -> string

(** Convert hash to URL-safe base64 string. *)
val base64_url: Hash.t -> string

(** Get raw bytes of hash. *)
val bytes: Hash.t -> bytes

(** Convert hash to int64 (truncates if necessary). *)
val to_int64: Hash.t -> int64

(** Convert hash to int (truncates). *)
val to_int: Hash.t -> int
