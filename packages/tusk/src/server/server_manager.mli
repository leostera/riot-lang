(** Server manager - Handles starting and managing the tusk server in the
    background *)

(* Ensures a server is running at the current workspace, as a reusable daemon,
   and returns a valid connected client to it. *)
val ensure_running :
  workspace:Model.Workspace.t -> (Tusk_jsonrpc.Client.t, Model.Error.t) result
