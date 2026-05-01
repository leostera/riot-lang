open Global

type env = Loader.env
(** Load config from environment-based path (./config/{dev,test,prod}.toml) *)
type t =
  | Empty
  | Env of {
      env: env;
    }
  | Path of {
      path: Path.t;
    }
  | Static of { toml_string: string }

val env: ?env:env -> unit -> t

(** The empty configuration provider *)
val empty: t

(** Load config from explicit file path *)
val file: Path.t -> t

(** Load config from inline TOML string *)
val static: string -> t

(** Load and parse the TOML configuration from the provider *)
val load: t -> (Data.Toml.value, string) result
