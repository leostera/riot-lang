(** HTTP version representation **)

type t = Http09 | Http10 | Http11 | Http2 | Http3

val of_string : string -> (t, [ `InvalidVersion ]) result
(** Parse an HTTP version string (e.g., "HTTP/1.1") **)

val to_string : t -> string
(** Convert HTTP version to string representation **)

val compare : t -> t -> int
(** Compare two HTTP versions **)

val equal : t -> t -> bool
(** Check if two HTTP versions are equal **)

val is_supported : t -> bool
(** Check if the HTTP version is supported **)
