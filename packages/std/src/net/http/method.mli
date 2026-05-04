(**
   HTTP request methods.

   HTTP request method types and utilities following RFC 7231.

   ## Examples

   Basic usage:

   ```ocaml open Std.Net.Http

   let method_ = Method.Get in Method.to_string method_ (* "GET" *)

   let method_ = Method.from_string "POST" in (* Method.Post *) ```

   Checking method properties:

   ```ocaml (* Safe methods don't modify server state *) Method.is_safe
   Method.Get (* true *) Method.is_safe Method.Post (* false *)

   (* Idempotent methods produce same result when repeated *)
   Method.is_idempotent Method.Put (* true *) Method.is_idempotent Method.Post
   (* false *)

   (* Cacheable methods can have responses cached *) Method.is_cacheable
   Method.Get (* true *) Method.is_cacheable Method.Post (* false *) ```

   Custom methods:

   ```ocaml (* Non-standard methods *) let custom = Method.Extension "PURGE" in
   Method.to_string custom (* "PURGE" *) ```

   ## Method Properties

   | Method | Safe | Idempotent | Cacheable |
   |--------|------|------------|-----------| | GET | ✓ | ✓ | ✓ | | HEAD | ✓ |
   ✓ | ✓ | | POST | ✗ | ✗ | ✗ | | PUT | ✗ | ✓ | ✗ | | DELETE | ✗ | ✓ | ✗ | |
   PATCH | ✗ | ✗ | ✗ | | OPTIONS | ✓ | ✓ | ✗ | | TRACE | ✓ | ✓ | ✗ |
*)
type t =
  | Get
  (** GET - Retrieve resource *)
  | Head
  (** HEAD - GET without body *)
  | Post
  (** POST - Submit data *)
  | Put
  (** PUT - Replace resource *)
  | Delete
  (** DELETE - Remove resource *)
  | Connect
  (** CONNECT - Tunnel proxy *)
  | Options
  (** OPTIONS - Communication options *)
  | Trace
  (** TRACE - Echo request *)
  | Patch
  (** PATCH - Partial modification *)

  (** Non-standard method. *)
  | Extension of string

(**
   Parses an HTTP method from string. Case-insensitive for standard methods.

   ## Examples

   ```ocaml Method.from_string "GET" (* Get *) Method.from_string "get" (* Get *)
   Method.from_string "PURGE" (* Extension "PURGE" *) ```
*)
val from_string: string -> t

(** Parses an HTTP method from a borrowed slice, copying only non-standard extensions. *)
val from_slice: IO.IoVec.IoSlice.t -> t

(**
   Converts HTTP method to uppercase string.

   ## Examples

   ```ocaml Method.to_string Method.Get (* "GET" *) Method.to_string
   (Method.Extension "PURGE") (* "PURGE" *) ```
*)
val to_string: t -> string

(**
   Returns [true] if the method is safe (read-only, doesn't modify state).

   Safe methods: GET, HEAD, OPTIONS, TRACE

   ## Examples

   ```ocaml Method.is_safe Method.Get (* true *) Method.is_safe Method.Post (*
   false *) ```
*)
val is_safe: t -> bool

(**
   Returns [true] if the method is idempotent (same result when repeated).

   Idempotent methods: GET, HEAD, PUT, DELETE, OPTIONS, TRACE

   ## Examples

   ```ocaml Method.is_idempotent Method.Put (* true - PUT same resource twice =
   same result *) Method.is_idempotent Method.Post (* false - POST twice
   creates two resources *) ```
*)
val is_idempotent: t -> bool

(**
   Returns [true] if responses to this method can be cached.

   Cacheable methods: GET, HEAD, POST (conditionally)

   ## Examples

   ```ocaml Method.is_cacheable Method.Get (* true *) Method.is_cacheable
   Method.Delete (* false *) ```
*)
val is_cacheable: t -> bool

(**
   Compares two HTTP methods.

   ## Examples

   ```ocaml Method.compare Method.Get Method.Post (* < 0 *) ```
*)
val compare: t -> t -> Order.t

(**
   Checks if two HTTP methods are equal.

   ## Examples

   ```ocaml Method.equal Method.Get Method.Get (* true *) ```
*)
val equal: t -> t -> bool
