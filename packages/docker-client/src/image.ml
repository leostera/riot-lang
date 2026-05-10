open Std

let ( let* ) value fn = Result.and_then value ~fn

module Config = Super.Config

let create_path = fun ?platform ?config_platform ~name ~tag () ->
  let params =
    [ ("fromImage", name); ("tag", tag) ] @ (
      match platform with
      | Some platform -> [ ("platform", platform) ]
      | None -> (
          match config_platform with
          | Some platform -> [ ("platform", platform) ]
          | None -> []
        )
    )
  in
  "/images/create" ^ Api.query params

let pull = fun ?platform client ~name ~tag ->
  let config_platform = (Client.config client).Config.platform in
  let* _body =
    Client.request
      client
      Net.Http.Method.Post
      (create_path ?platform ?config_platform ~name ~tag ())
      ()
  in
  Ok ()
