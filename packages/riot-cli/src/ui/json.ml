open Std

let clock_origin = ref None

let reset_clock = fun ~started_at -> clock_origin := Some started_at

let clock_origin_or_set = fun started_at ->
  match !clock_origin with
  | Some origin -> origin
  | None ->
      clock_origin := Some started_at;
      started_at

let elapsed_us_since_origin = fun instant ->
  let origin = clock_origin_or_set instant in
  Time.Instant.saturating_duration_since ~earlier:origin instant
  |> Time.Duration.to_micros

let event_elapsed_us = fun () -> elapsed_us_since_origin (Time.Instant.now ())

let stamp_event = fun ?timestamp (json: Data.Json.t) ->
  match json with
  | Data.Json.Object fields ->
      let emitted_at_us = event_elapsed_us () in
      let fields =
        if
          Option.is_some (List.find fields ~fn:(fun (name, _) -> String.equal name "emitted_at_us"))
        then
          fields
        else
          fields @ [ ("emitted_at_us", Data.Json.Int emitted_at_us); ]
      in
      let fields =
        match timestamp with
        | Some (name, instant) ->
            if
              Option.is_some
                (List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name))
            then
              fields
            else
              fields @ [ (name, Data.Json.Int (elapsed_us_since_origin instant)); ]
        | None ->
            if
              Option.is_some
                (List.find fields ~fn:(fun (name, _) -> String.equal name "created_at_us"))
            then
              fields
            else
              fields @ [ ("created_at_us", Data.Json.Int emitted_at_us); ]
      in
      Data.Json.Object fields
  | other -> other

let write_json_event = fun ?timestamp (json: Data.Json.t) ->
  println
    (Data.Json.to_string (stamp_event ?timestamp json))

let write_serde_event = fun serializer value ->
  match Serde_json.to_string serializer value with
  | Ok content -> println content
  | Error err ->
      write_json_event
        (Data.Json.Object [
          ("type", Data.Json.String "JsonEncodingFailed");
          ("error", Data.Json.String (Serde.Error.to_string err));
        ])

let write_event = fun event -> write_json_event (Riot_model.Event.to_json event)

let write_command_error = fun kind details ->
  write_json_event
    (Common.command_error_event_to_json kind details)
