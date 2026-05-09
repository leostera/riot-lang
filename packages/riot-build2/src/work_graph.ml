open Std

let execute_node = fun _registry node ->
  match Work_node.kind node with
  | Work_node.UserIntent _
  | Work_node.Goal _ ->
      Error (Error.ExecutorInvariantViolated {
        message = "default work graph virtual node reached concrete execution";
      })
  | ToolchainReady _
  | SourceAnalysis _
  | ModulePlan _
  | ActionExecution _ ->
      Error (Error.ExecutorInvariantViolated {
        message = "default work graph only supports user intent and goal nodes";
      })

let run_intent = fun ~config intent ->
  let catalog = Package_catalog.create config.Build_config.workspace in
  let plan_dependencies = fun _registry node ->
    match Work_node.kind node with
    | Work_node.UserIntent intent ->
        Intent_planner.expand catalog intent
        |> Result.map ~fn:(fun goals -> List.map goals ~fn:(fun goal -> Work_node.GoalKey goal))
    | Work_node.Goal _ -> Ok []
    | ToolchainReady _
    | SourceAnalysis _
    | ModulePlan _
    | ActionExecution _ ->
        Error (Error.ExecutorInvariantViolated {
          message = "default work graph only supports user intent and goal nodes";
        })
  in
  Executor.Runner.run_with_handlers
    ~config
    ~seeds:[ Work_node.user_intent ~id:(Work_node.Node_id.from_int 1) intent ]
    ~plan_dependencies
    ~execution_mode:(fun node ->
      match Work_node.kind node with
      | Work_node.UserIntent _
      | Goal _ -> Work_node.Virtual
      | ToolchainReady _
      | SourceAnalysis _
      | ModulePlan _
      | ActionExecution _ -> Work_node.Concrete)
    ~execute:execute_node
    ()

let completed_goals = fun summary ->
  List.filter_map
    summary.ExecutionSummary.results
    ~fn:(fun result ->
      match (result.ExecutionSummary.status, Work_node.kind result.node) with
      | (Completed, Work_node.Goal goal) -> Some goal
      | _ -> None)
