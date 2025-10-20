open Std
open Model
include Intf.S

val with_facts : t -> Fact.t list -> t
(** Create storage pre-populated with facts (for testing/bootstrap) *)
