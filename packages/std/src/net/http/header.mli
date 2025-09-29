(** HTTP headers **)

type name = string
type value = string
type t

val empty : t
(** Create empty headers **)

val of_list : (name * value) list -> t
(** Create headers from list of name-value pairs **)

val to_list : t -> (name * value) list
(** Convert headers to list of name-value pairs **)

val add : t -> name -> value -> t
(** Add a header (allows duplicates) **)

val set : t -> name -> value -> t
(** Set a header (replaces existing) **)

val get : t -> name -> value option
(** Get first value for header name **)

val get_all : t -> name -> value list
(** Get all values for header name **)

val remove : t -> name -> t
(** Remove all headers with given name **)

val has : t -> name -> bool
(** Check if header exists **)

val iter : (name -> value -> unit) -> t -> unit
(** Iterate over all headers **)

val fold : (name -> value -> 'a -> 'a) -> t -> 'a -> 'a
(** Fold over all headers **)

val length : t -> int
(** Get number of header entries **)

val is_empty : t -> bool
(** Check if headers are empty **)

(** Common header names **)
module Name : sig
  val content_type : name
  val content_length : name
  val authorization : name
  val user_agent : name
  val accept : name
  val accept_encoding : name
  val accept_language : name
  val cache_control : name
  val connection : name
  val cookie : name
  val host : name
  val referer : name
  val server : name
  val set_cookie : name
  val transfer_encoding : name
  val location : name
  val www_authenticate : name
  val date : name
  val etag : name
  val expires : name
  val last_modified : name
  val if_modified_since : name
  val if_none_match : name
  val vary : name
  val x_forwarded_for : name
  val x_real_ip : name
end

(** Header value parsing utilities **)
module Value : sig
  val parse_content_type :
    value -> (string * (string * string) list, [ `InvalidContentType ]) result
  (** Parse Content-Type header into media type and parameters **)

  val parse_authorization :
    value -> (string * string, [ `InvalidAuthorization ]) result
  (** Parse Authorization header into scheme and credentials **)

  val parse_cache_control : value -> (string * string option) list
  (** Parse Cache-Control directives **)

  val parse_accept :
    value -> (string * float option * (string * string) list) list
  (** Parse Accept header with quality values **)
end
