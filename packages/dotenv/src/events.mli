(**
   # Dotenv telemetry events

   Events emitted while parsing and loading dotenv files.
*)

type Std.Telemetry.event +=
  (** Emitted after source text parses successfully. *)
  | Parsed of {
      (** Number of bindings parsed from the source text. *)
      binding_count: int;
    }
  (** Emitted after source text fails to parse. *)
  | ParseFailed of {
      (** 1-based source line where parsing failed. *)
      line: int;
      (** Human-readable parse failure. *)
      message: string;
    }
  (** Emitted before a loader attempts to read a path. *)
  | LoadStarted of {
      (** Path the loader is about to read. *)
      path: Std.Path.t;
    }
  (** Emitted after a path parses successfully. *)
  | Loaded of {
      (** Path that was parsed. *)
      path: Std.Path.t;
      (** Number of bindings parsed from the path. *)
      binding_count: int;
    }
  (** Emitted when a missing path is allowed to be skipped. *)
  | LoadSkipped of {
      (** Missing path that was skipped. *)
      path: Std.Path.t;
    }
  (** Emitted when reading or parsing a path fails. *)
  | LoadFailed of {
      (** Path that failed to load. *)
      path: Std.Path.t;
      (** Human-readable failure. *)
      reason: string;
    }
