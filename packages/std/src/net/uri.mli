open Global

(**
   URL/URI parsing and manipulation.

   URL and URI parsing with support for all standard components: scheme,
   authority, path, query, and fragment. Provides builder pattern and query
   parameter utilities.

   ## Examples

   Parsing URLs:

   ```ocaml open Std.Net

   match Uri.of_string "https://api.example.com:443/v1/users?page=1#top" with |
   Ok uri -> Uri.scheme uri (* Some "https" *) Uri.host uri (* Some
   "api.example.com" *) Uri.port uri (* Some 443 *) Uri.path uri (* "/v1/users"
   *) Uri.query uri (* Some "page=1" *) Uri.fragment uri (* Some "top" *) |
   Error err -> Log.error "Invalid URI" ```

   Building URLs:

   ```ocaml let uri = Uri.Builder.create () |> Uri.Builder.scheme "https" |>
   Uri.Builder.host "api.example.com" |> Uri.Builder.path "/v1/users" |>
   Uri.Builder.query "status=active" |> Uri.Builder.build |> Result.unwrap in

   Uri.to_string uri (* "https://api.example.com/v1/users?status=active" *) ```

   Working with query parameters:

   ```ocaml let query_str = "page=1&limit=10&sort=name" in let params =
   Uri.Query.parse query_str in

   (* Get specific parameter *) Uri.Query.get params "page" (* Some "1" *)

   (* Add parameter *) let params = Uri.Query.add params "filter" "active" in
   Uri.Query.to_string params (* "page=1&limit=10&sort=name&filter=active" *)
   ```

   Joining paths:

   ```ocaml let base = Uri.of_string "https://example.com/api" |> Result.unwrap
   in let full = Uri.join base "v1/users" |> Result.unwrap in Uri.to_string
   full (* "https://example.com/api/v1/users" *) ```
*)

(** A parsed URL/URI with all components. *)

(** Alias for [t]. *)
type t
(** URL parsing errors. *)
type url = t
type error =
  | InvalidScheme
  (** Invalid or unsupported scheme *)
  | InvalidAuthority
  (** Malformed authority section *)
  | InvalidPath
  (** Invalid path component *)
  | InvalidQuery
  (** Malformed query string *)
  | InvalidFragment
  (** Invalid fragment identifier *)
  | InvalidFormat
  (** General parsing error *)
  | TooLong

(** URL exceeds maximum length *)

(** Parse a string into a URL *)
val of_string: string -> (t, error) Kernel.result

(** Parse a borrowed slice into a URL without first materializing the full input string. *)
val from_slice: IO.IoVec.IoSlice.t -> (t, error) Kernel.result

(** Convert a URL back to string representation *)
val to_string: t -> string

(** Get the scheme (e.g., "http", "https") *)
val scheme: t -> string option

(** Get the full authority part (e.g., "user:pass@host:port") *)
val authority: t -> string option

(** Get just the host part *)
val host: t -> string option

(** Get the port number if specified *)
val port: t -> int option

(** Get the path component (always present, defaults to "/") *)
val path: t -> string

(** Get the query string without the '?' *)
val query: t -> string option

(** Get the fragment without the '#' *)
val fragment: t -> string option

(** Get combined path and query (e.g., "/path?query") *)
val path_and_query: t -> string

(**
   Encode string per RFC 3986, encoding all except unreserved characters.

   Unreserved: a-z A-Z 0-9 - . _ ~

   Examples:
   {[
     percent_encode "Hello World"  (* "Hello%20World" *)
     percent_encode "test@example.com"  (* "test%40example.com" *)
     percent_encode "100%"  (* "100%25" *)
   ]}
*)
val percent_encode: string -> string

(**
   Decode percent-encoded string per RFC 3986.

   Converts %XX sequences to their corresponding characters.

   Examples:
   {[
     percent_decode "Hello%20World"  (* "Hello World" *)
     percent_decode "test%40example.com"  (* "test@example.com" *)
     percent_decode "100%25"  (* "100%" *)
   ]}

   Invalid sequences (e.g., "%ZZ") are left as-is.
*)
val percent_decode: string -> string

(**
   Encode for application/x-www-form-urlencoded.

   Like percent_encode but space becomes '+' instead of '%20'.
   Used for encoding form data and query strings.

   Examples:
   {[
     form_encode "Hello World"  (* "Hello+World" *)
     form_encode "test@example.com"  (* "test%40example.com" *)
   ]}
*)
val form_encode: string -> string

(**
   Decode application/x-www-form-urlencoded string.

   Like percent_decode but also converts '+' to space.
   Used for parsing form data and query strings.

   Examples:
   {[
     form_decode "Hello+World"  (* "Hello World" *)
     form_decode "test%40example.com"  (* "test@example.com" *)
   ]}

   Note: Query.parse automatically uses form_decode.
*)
val form_decode: string -> string

module Scheme: sig
  type t = string

  val http: t

  val https: t

  val ftp: t

  val file: t

  val of_string: string -> (t, error) Kernel.result

  val to_string: t -> string
end

module Authority: sig
  type t

  val host: t -> string

  val port: t -> int option

  val userinfo: t -> string option

  val of_string: string -> (t, error) Kernel.result

  val to_string: t -> string
end

module PathAndQuery: sig
  type t

  val path: t -> string

  val query: t -> string option

  val of_string: string -> (t, error) Kernel.result

  val to_string: t -> string
end

module Builder: sig
  type t

  val create: unit -> t

  val scheme: t -> string -> t

  val authority: t -> string -> t

  val host: t -> string -> t

  val port: t -> int -> t

  val path: t -> string -> t

  val query: t -> string -> t

  val fragment: t -> string -> t

  val build: t -> (url, error) Kernel.result
end

(** Check if URL is absolute (has scheme) *)
val is_absolute: t -> bool

(** Check if URL is relative (no scheme) *)
val is_relative: t -> bool

(** Join a base URL with a relative path *)
val join: t -> string -> (t, error) Kernel.result

(** Compare two URLs for equality *)
val equal: t -> t -> bool

(** Compare two URLs *)
val compare: t -> t -> Order.t

module Query: sig
  type param = string * string
  (**
     Parse query string into parameter list.

     Automatically decodes percent-encoded values using form_decode.
     Converts '+' to space per application/x-www-form-urlencoded.

     Examples:
     {[
       parse "page=1&sort=name"
       (* [("page", "1"); ("sort", "name")] *)

       parse "name=John+Doe&email=test%40example.com"
       (* [("name", "John Doe"); ("email", "test@example.com")] *)
     ]}

     {b Breaking Change}: Previously returned encoded values.
     Now returns decoded values per RFC 3986.
  *)
  type t = param list

  val parse: string -> t

  (**
     Convert parameter list to query string.

     Automatically encodes keys and values using form_encode.
     Spaces become '+', special characters become '%XX'.

     Examples:
     {[
       to_string [("page", "1"); ("sort", "name")]
       (* "page=1&sort=name" *)

       to_string [("name", "John Doe"); ("email", "test@example.com")]
       (* "name=John+Doe&email=test%40example.com" *)
     ]}

     {b Breaking Change}: Previously did not encode values.
     Now encodes per application/x-www-form-urlencoded.
  *)
  val to_string: t -> string

  (** Get first value for a parameter name *)
  val get: t -> string -> string option

  (** Get all values for a parameter name *)
  val get_all: t -> string -> string list

  (** Add a parameter *)
  val add: t -> string -> string -> t

  val remove: t -> string -> t

  (** Remove all parameters with given name *)
end
