open Std
open Std.Collections

let format = fun ~displayed_packages:_ (event: Riot_build.Event.t) ->
  match event with
  | Riot_build.Event.Pm event ->
      Riot_model.Event.display event.kind
  | Riot_build.Event.BuildingTarget { target; host } ->
      let kind =
        if host then
          "host"
        else
          "target"
      in
      "building " ^ kind ^ " " ^ Riot_model.Target.to_string target
  | Riot_build.Event.CacheGc _
  | Riot_build.Event.Telemetry _
  | Riot_build.Event.Phase _ ->
      ""
