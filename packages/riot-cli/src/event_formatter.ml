open Std
open Std.Collections

let format = fun ~displayed_packages:_ (event: Riot_model.Event.t) ->
  match event.kind with
  | Riot_model.Event.Build (
    Riot_model.Event.BuildTargetBuilding { target; host }
  ) ->
      let kind =
        if host then
          "host"
        else
          "target"
      in
      "building " ^ kind ^ " " ^ Riot_model.Target.to_string target
  | kind -> Riot_model.Event.display kind
