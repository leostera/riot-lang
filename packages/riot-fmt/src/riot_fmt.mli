open Std

type event = Krasny.Report.event

val command: Std.ArgParser.command

val event_to_json: root:Path.t -> event -> Data.Json.t

val run_check_paths: ?workspace:Riot_model.Workspace.t -> ?on_event:(event -> unit) -> Path.t list -> (unit, exn) result

val run: ?workspace:Riot_model.Workspace.t -> ?stdout:(string -> unit) -> ?stderr:(string -> unit) -> Std.ArgParser.matches -> (unit, exn) result
