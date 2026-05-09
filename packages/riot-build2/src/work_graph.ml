open Std

let dependencies_of_node = fun node ->
  match Work_node.kind node with
  | Work_node.UserIntent intent ->
      intent
      |> Intent_planner.expand
      |> List.map ~fn:(fun goal -> Work_node.GoalKey goal)
      |> Result.ok
  | Work_node.Goal _ -> Ok []
  | PackageWork _
  | ToolchainReady _
  | SourceAnalysis _
  | ModulePlan _
  | PackageFinalize _
  | ActionExecution _ ->
      Error (Error.ExecutorInvariantViolated {
        message = "default work graph only supports user intent and goal nodes";
      })

let execute_node = fun _registry node ->
  match Work_node.kind node with
  | Work_node.UserIntent _
  | Work_node.Goal _ -> Error (Error.ExecutorInvariantViolated {
      message = "default work graph virtual node reached concrete execution";
    })
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
  Executor.Runner.run_with_handlers
    ~config
    ~seeds:[ Work_node.user_intent ~id:(Work_node.Node_id.from_int 1) intent ]
    ~dependencies:dependencies_of_node
    ~execute:execute_node
    ()

let completed_goals = fun summary ->
  List.filter_map
    summary.ExecutionSummary.results
    ~fn:(fun result ->
      match (result.ExecutionSummary.status, Work_node.kind result.node) with
      | (Completed, Work_node.Goal goal) -> Some goal
      | _ -> None)
