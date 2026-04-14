open Std
open Std.Result.Syntax

let new_package = fun ~workspace ~path ~name ~is_library ->
  let* client =
    Client.connect_local ~workspace ()
    |> Result.map_err ~fn:Client.error_message
  in
  Client.new_package client ~path ~name ~is_library
