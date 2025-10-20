open Std
open Model
include Intf.S

val create_with_file : string -> t
(** Create storage backed by a file *)
