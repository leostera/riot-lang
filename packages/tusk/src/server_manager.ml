open Std
(** Server manager - Handles starting and managing the tusk server in the
    background *)

module Daemon = struct
  type t = {
    workspace : Workspace.t;
    os_pid : int; (* Unix process ID *)
    port : int;
    host : string;
  }

  let daemon_exists ~workspace =
    (* FXIXME: implement logic to extract project id and check if fiels exist in `/.tusk/daemons/<proj id>` if so then read them and put together t value and return Some t *)
    None

  (** Start the daemon process *)
  let of_workspace ~workspace =
    (* 1. first get the workspace id and check if the right files exist in ~/.tusk/daemons/<project-id> -- if they do, read and return those files *)
    match daemon_exists ~workspace with
    | Some daemon -> Ok daemon
    | None ->
        (* 2. if they dont' exist, because we can't use Unix.fork we have to use Std.Command.spawn : cmd:string -> ?args:string list -> (t, Std.Command.error) result 

        to create a new OS-level process -- that STd.Command module doesn't exist but please create it

        keep in mind we are not trying to connect a client to this daemon!!!
      *)
        Ok { workspace; os_pid = 0; port = 9771; host = "localhost" }
end

let ensure_running ~workspace =
  (* 1. Get a daemon for the workspace *)
  let daemon = Daemon.of_workspace ~workspace |> Result.unwrap in
  (* 2. Wait for server to be ready *)
  let rec wait_server ~retries =
    if retries <= 0 then Error Error.ScanWorkspaceError
    else
      match Tusk_jsonrpc.Client.create ~host:daemon.host ~port:daemon.port with
      | Ok client -> (
          (* Try to ping to make sure it's really ready *)
          match Tusk_jsonrpc.Client.ping client with
          | Ok _ -> Ok client
          | Error _ ->
              Tusk_jsonrpc.Client.close client;
              Miniriot.sleep 50;
              (* 50ms *)
              wait_server ~retries:(retries - 1))
      | Error _ ->
          Miniriot.sleep 50;
          (* 50ms *)
          wait_server ~retries:(retries - 1)
  in
  wait_server ~retries:60 (* Wait up to 3 seconds *)
