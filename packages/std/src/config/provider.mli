open Global

type env = Loader.env

type t =
  | Empty
  | Env of { env : env }
  | Path of { path : Path.t }
  | Static of { toml_string : string }

val env : ?env:env -> unit -> t
(** Load config from environment-based path (./config/{dev,test,prod}.toml) *)

val empty : t
(** The empty configuration provider *)

val file : Path.t -> t
(** Load config from explicit file path *)

val static : string -> t
(** Load config from inline TOML string *)

val load : t -> (Data.Toml.value, string) result
(** Load and parse the TOML configuration from the provider *)
