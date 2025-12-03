(** Server manager - Handles starting and managing the tusk server in the
    background *)

open Std

(* Ensures a server is running at the current workspace, as a reusable daemon,
   and returns a valid connected client to it. *)
val ensure_running :
  workspace:Tusk_model.Workspace.t -> config:Server_config.t -> (Tusk_client.t, Tusk_model.Error.t) result
