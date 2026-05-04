open Std

type error = System.TargetTriple.error =
  | InvalidTripletFormat of { value: string }
type t = System.TargetTriple.t = {
  architecture: string;
  vendor: string;
  os: string;
  abi: string option;
}

module Set: sig
  type elt = t
  type t

  val empty: unit -> t

  val singleton: elt -> t

  val from_list: elt list -> t

  val insert: t -> elt -> unit

  val contains: t -> elt -> bool

  val length: t -> int

  val is_empty: t -> bool

  val to_list: t -> elt list
end

type request =
  | Host
  | All
  | Pattern of string
  | Exact of Set.t
type resolve_error = {
  pattern: string;
  available_targets: t list;
}

val current: t

val error_message: error -> string

val from_string: string -> (t, error) result

val to_string: t -> string

val equal: t -> t -> bool

val compare: t -> t -> Order.t

val host: unit -> t

val make_set: t list -> Set.t

val parse: string -> request

val configured_targets: host:t -> Toolchain_config.t -> Set.t

val resolve: host:t -> configured_targets:Set.t -> request -> (Set.t, resolve_error) result

val request_to_string: request -> string

val is_cross: t -> bool

val platform_name: t -> string

val hash: Crypto.Sha256.state -> t -> unit
