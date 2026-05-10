open Std

let ( let* ) value fn = Result.and_then value ~fn

module Config = Super.Config

type t = {
  config: Config.t;
}

let make = fun ?config () ->
  match config with
  | Some config -> Ok { config }
  | None ->
      let* config = Config.from_env () in
      Ok { config }

let config = fun client -> client.config

let request = fun client method_ path ?body ?headers () ->
  Api.request
    client.config
    method_
    path
    ?body
    ?headers
    ()

let ping = fun client ->
  let* body = request client Net.Http.Method.Get "/_ping" () in
  if String.equal (String.trim body) "OK" then
    Ok ()
  else
    Error (Error.HttpError ("unexpected Docker ping response: " ^ body))
