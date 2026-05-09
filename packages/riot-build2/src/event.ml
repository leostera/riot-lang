type t =
  | WorkQueued of { node: Work_node.t }
  | WorkStarted of { node: Work_node.t }
  | WorkCompleted of { node: Work_node.t }
  | WorkFailed of { node: Work_node.t; error: Error.t }
  | WorkSpawned of { node: Work_node.t; spawned: Work_node.t list }
  | WorkDependenciesRegistered of {
      node: Work_node.t;
      dependencies: Work_node.t list;
    }
  | WorkRequeued of { node: Work_node.t }
