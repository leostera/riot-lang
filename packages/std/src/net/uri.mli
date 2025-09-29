(** URL parsing and manipulation similar to Rust's HTTP URI module *)

type t
(** The main URL/URI type *)

type url = t

(** URL parsing errors *)
type error =
  | InvalidScheme
  | InvalidAuthority
  | InvalidPath
  | InvalidQuery
  | InvalidFragment
  | InvalidFormat
  | TooLong

(** {1 Creation and Parsing} *)

val of_string : string -> (t, error) result
(** Parse a string into a URL *)

val to_string : t -> string
(** Convert a URL back to string representation *)

(** {1 Components Access} *)

val scheme : t -> string option
(** Get the scheme (e.g., "http", "https") *)

val authority : t -> string option
(** Get the full authority part (e.g., "user:pass@host:port") *)

val host : t -> string option
(** Get just the host part *)

val port : t -> int option
(** Get the port number if specified *)

val path : t -> string
(** Get the path component (always present, defaults to "/") *)

val query : t -> string option
(** Get the query string without the '?' *)

val fragment : t -> string option
(** Get the fragment without the '#' *)

val path_and_query : t -> string
(** Get combined path and query (e.g., "/path?query") *)

(** {1 Component Types} *)

module Scheme : sig
  type t = string

  val http : t
  val https : t
  val ftp : t
  val file : t
  val of_string : string -> (t, error) result
  val to_string : t -> string
end

module Authority : sig
  type t

  val host : t -> string
  val port : t -> int option
  val userinfo : t -> string option
  val of_string : string -> (t, error) result
  val to_string : t -> string
end

module PathAndQuery : sig
  type t

  val path : t -> string
  val query : t -> string option
  val of_string : string -> (t, error) result
  val to_string : t -> string
end

(** {1 URL Builder} *)

module Builder : sig
  type t

  val create : unit -> t
  val scheme : t -> string -> t
  val authority : t -> string -> t
  val host : t -> string -> t
  val port : t -> int -> t
  val path : t -> string -> t
  val query : t -> string -> t
  val fragment : t -> string -> t
  val build : t -> (url, error) result
end

(** {1 Utilities} *)

val is_absolute : t -> bool
(** Check if URL is absolute (has scheme) *)

val is_relative : t -> bool
(** Check if URL is relative (no scheme) *)

val join : t -> string -> (t, error) result
(** Join a base URL with a relative path *)

val equal : t -> t -> bool
(** Compare two URLs for equality *)

val compare : t -> t -> int
(** Compare two URLs *)

(** {1 Query Parameter Utilities} *)

module Query : sig
  type param = string * string
  type t = param list

  val parse : string -> t
  (** Parse query string into parameter list *)

  val to_string : t -> string
  (** Convert parameter list back to query string *)

  val get : t -> string -> string option
  (** Get first value for a parameter name *)

  val get_all : t -> string -> string list
  (** Get all values for a parameter name *)

  val add : t -> string -> string -> t
  (** Add a parameter *)

  val remove : t -> string -> t
  (** Remove all parameters with given name *)
end
