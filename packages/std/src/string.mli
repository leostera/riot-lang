(** String module with UTF-8 iteration support *)

include module type of Stdlib.String
(** Include all standard library String functions *)

val into_mut_iter : string -> Uchar.t MutIterator.t
(** Create a mutable iterator over UTF-8 characters in a string *)

val into_iter : string -> Uchar.t Iterator.t
(** Create an iterator over UTF-8 characters in a string *)
