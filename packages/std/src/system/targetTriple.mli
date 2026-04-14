(** Platform identity and executable metadata for a concrete target triple. *)
type t = Kernel.System.Host.t = {
  architecture: string;
  vendor: string;
  os: string;
  abi: string option;
}

val current: t

(** Render as `arch-vendor-os[-abi]`. *)
val to_string: t -> string

(** Parse `arch-vendor-os[-abi]`. *)
val from_string: string -> (t, string) Result.t

val equal: t -> t -> bool
