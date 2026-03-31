(** Central configuration for all log handlers.

    This module defines the configuration structure for the logging system,
    supporting multiple handler types (stdout, file, etc.) configured via
    the [~app:"log"] section of the config.

    Example TOML configuration:
    {v
    [[log.handler]]
    type = "stdout"
    format = "full"

    [[log.handler]]
    type = "file"
    path = "./app.log"
    format = "compact"
    v}
*)
open Global

(** Format style for log output *)
type format_style =
  | Full
  (** Timestamp, level, message, and metadata *)
  | Compact
(** Level and message only *)
(** Handler configuration discriminated union *)
type handler_config =
  | Stdout of {
      format: format_style;
    }
  | File of {
      path: string;
      format: format_style;
    }
(** Log configuration containing list of handlers *)
type t = {
  handlers: handler_config list;
}
(** Config spec for log configuration *)
val spec: Config.Spec.t

(** Parse configuration from validated config *)
val get: Config.Spec.value -> (t, Config.error) result
