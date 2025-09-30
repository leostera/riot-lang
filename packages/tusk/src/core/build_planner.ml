(** Build planner - plans a build node. *)

type skip_reason = DependenciesFailed of string list

type plan_result =
  | Planned of Build_node.t
  | MissingDependencies of { node : Build_node.t; deps : Build_node.t list }
  | Skipped of { node : Build_node.t; reason : skip_reason }

type error = string

let plan_node ~graph ~node ~build_results ~session_id () =
  (* Temporary implementation - just mark everything as planned *)
  Ok (Planned node)
