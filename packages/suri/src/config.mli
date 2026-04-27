(**
   Server Configuration

   Compound configuration for the entire Suri server including
   network settings, HTTP limits, protocol-specific options, and
   LiveView session security.

   ## Usage with Std.Config

   1. Create `./config/dev.toml`:
   ```toml
   [suri]
   host = "0.0.0.0"
   port = 4000
   liveview_secret = "your-secret-key-at-least-32-chars-long"
   ```

   2. Load configuration in your application:
   ```ocaml
   let config = Suri.Config.get_config () in
   Suri.start_link ~config ()
   ```

   If no config is provided, Suri will try to load from Std.Config,
   and if that fails, it will fall back to default values.
*)
open Std

type env =
  | Development
  | Test
  | Production
type t = {
  env: env;
  host: string;
  port: int;
  acceptors: int;
  max_request_line_length: int;
  max_header_count: int;
  max_header_length: int;
  buffer_size: int;
  liveview_secret: string;
  (**
     Secret key for signing LiveView session tokens.
     Must be at least 32 characters for security.
  *)
}
val default: t

(**
   Default configuration with sensible defaults.
   Note: Uses a placeholder for liveview_secret - you should override this!
*)
val spec: Std.Config.Spec.t

(** Configuration spec for Std.Config - automatically registered on module load *)
type liveview_secret_error =
  | Missing
  | TooShort of int
  | Placeholder
type invalid_env = {
  value: string;
  normalized: string;
  allowed: env list;
}
type error =
  | InvalidEnv of invalid_env
  | InvalidPort of int
  | InvalidAcceptors of int
  | InvalidMaxRequestLineLength of int
  | InvalidMaxHeaderCount of int
  | InvalidMaxHeaderLength of int
  | InvalidBufferSize of int
  | InvalidLiveViewSecret of liveview_secret_error
val env_to_string: env -> string

val env_from_string: string -> (env, error) result

val error_to_string: error -> string

val errors_to_string: error list -> string

val validate: t -> (t, error list) result

val get: Std.Config.Spec.value -> (t, Std.Config.error) result

(** Extract typed config from validated spec values *)
