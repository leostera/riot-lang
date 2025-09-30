open Common

type t
(** Directory reading iterator *)

type state = t
type item = Path.t

val create : Path.t -> (t, error) result
(** Open a directory for reading *)

val next : t -> Path.t option
(** Get next entry from directory, skipping . and .. *)

val close : t -> (unit, error) Result.t
(** Close the directory handle *)

val size : t -> int
(** MutIterator interface *)

val clone : t -> t
