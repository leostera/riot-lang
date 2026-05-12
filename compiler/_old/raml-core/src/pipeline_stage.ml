open Std
open Std.Data

type 'value t = {
  json: Json.t;
  value: 'value option;
  errors: Json.t list;
}

let ok = fun ~key ~render value ->
  {
    json = Json.obj [ ("status", Json.string "ok"); (key, render value); ];
    value = Some value;
    errors = []
  }

let ok_with_json = fun ~json value -> { json; value = Some value; errors = [] }

let error = fun ~stage errors ->
  {
    json = Json.obj
      [
        ("status", Json.string "error");
        ("stage", Json.string stage);
        ("errors", Json.array errors);
      ];
    value = None;
    errors
  }

let blocked = fun ~blocked_on errors ->
  {
    json = Json.obj
      [
        ("status", Json.string "blocked");
        ("blocked_on", Json.string blocked_on);
        ("errors", Json.array errors);
      ];
    value = None;
    errors
  }

let unavailable = fun ~reason ->
  {
    json = Json.obj [ ("status", Json.string "unavailable"); ("reason", Json.string reason); ];
    value = None;
    errors = []
  }

let status = fun stage ->
  let status =
    match Json.get_field "status" stage.json with
    | Some value -> Json.get_string value
    | None -> None
  in
  match status with
  | Some "ok" -> Event.Ok
  | Some "blocked" -> Event.Blocked
  | Some "unavailable" -> Event.Unavailable
  | Some "error" -> Event.Error
  | _ -> Event.Error

let error_message = fun ~default stage ->
  match stage.errors with
  | [] -> default
  | errors -> Json.array errors |> Json.to_string_pretty
