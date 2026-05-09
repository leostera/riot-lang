open Std

let execute_node = fun context node ->
  match Work_node.kind node with
  | Work_node.UserIntent intent ->
      let goals =
        intent
        |> Intent_planner.expand
        |> List.map ~fn:(fun goal -> Work_node.GoalKey goal)
      in
      Ok (Executor.Complete goals)
  | Work_node.Goal _ -> Ok (Executor.Complete [])
  | PackageWork _
  | ToolchainReady _
  | SourceAnalysis _
  | ModulePlan _
  | PackageFinalize _
  | ActionExecution _ ->
      Error (Error.ExecutorInvariantViolated {
        message = "default work graph only supports user intent and goal nodes";
      })

let run_intent = fun ~config intent ->
  Executor.run
    ~config
    ~seeds:[ Work_node.user_intent ~id:(Work_node.Node_id.from_int 1) intent ]
    ~execute:execute_node
    ()

let completed_goals = fun summary ->
  List.filter_map
    summary.Executor.Summary.results
    ~fn:(fun result ->
      match (result.Executor.Summary.status, Work_node.kind result.node) with
      | (Completed, Work_node.Goal goal) -> Some goal
      | _ -> None)
