open Std

let of_build_runtime_event = fun (event: Build_runtime.build_event) ->
  match event with
  | Build_runtime.Pm event -> Some (Event.Pm event)
  | Build_runtime.BuildingTarget { target; host } -> Some (Event.BuildingTarget { target; host })
  | Build_runtime.CacheGc event -> Some (Event.CacheGc event)
  | Build_runtime.Phase phase -> Some (Event.Phase phase)
