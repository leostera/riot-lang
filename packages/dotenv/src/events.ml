open Std

type Telemetry.event +=
  | Parsed of { binding_count: int }
  | ParseFailed of { line: int; message: string }
  | LoadStarted of {
      path: Std.Path.t;
    }
  | Loaded of {
      path: Std.Path.t;
      binding_count: int;
    }
  | LoadSkipped of {
      path: Std.Path.t;
    }
  | LoadFailed of {
      path: Std.Path.t;
      reason: string;
    }
