open Std

val clear: unit -> unit

val providers: unit -> Provider.t list

val register_provider: Provider.t -> unit

val register_providers: Provider.t list -> unit

val rules: unit -> Rule.t list

val rule_ids: unit -> string list
