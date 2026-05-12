open Std
module HashSet = Collections.HashSet
module Types = Types
module Core = Raml_core.Core_ir

type planner = {
  top_level_functions: string HashSet.t;
  seen_table_elements: string HashSet.t;
  mutable table_rev: Core.Entity_id.t list;
  mutable has_indirect_calls: bool;
  mutable needs_closure_runtime: bool;
}

let entity_key = fun entity_id ->
  match Core.Entity_id.binding_id entity_id with
  | Some binding_id -> (
      match Core.Binding_id.stamp binding_id with
      | Some stamp -> Core.Binding_id.name binding_id ^ "#" ^ Int.to_string stamp
      | None -> Core.Surface_path.to_string (Core.Entity_id.surface_path entity_id)
    )
  | None -> Core.Surface_path.to_string (Core.Entity_id.surface_path entity_id)

let create_planner = fun (program: Types.Compilation_unit.t) ->
  let top_level_functions = HashSet.create () in
  List.for_each
    program.functions
    ~fn:(fun (function_: Types.Function.t) ->
      let _ = HashSet.insert top_level_functions ~value:(entity_key function_.entity_id) in
      ());
  {
    top_level_functions;
    seen_table_elements = HashSet.create ();
    table_rev = [];
    has_indirect_calls = false;
    needs_closure_runtime = false;
  }

let add_table_element = fun planner entity_id ->
  let key = entity_key entity_id in
  if
    HashSet.contains planner.top_level_functions ~value:key
    && not (HashSet.contains planner.seen_table_elements ~value:key)
  then
    let _ = HashSet.insert planner.seen_table_elements ~value:key in
    planner.table_rev <- entity_id :: planner.table_rev

let rec walk_expr = fun planner expr ->
  match expr with
  | Types.Expr.Constant _ ->
      ()
  | Types.Expr.Var entity_id ->
      add_table_element planner entity_id
  | Types.Expr.Direct_call call ->
      List.for_each call.arguments ~fn:(walk_expr planner)
  | Types.Expr.Indirect_call call ->
      planner.has_indirect_calls <- true;
      walk_expr planner call.callee;
      List.for_each call.arguments ~fn:(walk_expr planner)
  | Types.Expr.Lambda lambda ->
      planner.needs_closure_runtime <- true;
      walk_expr planner lambda.body
  | Types.Expr.Let let_ ->
      List.for_each let_.bindings ~fn:(fun (binding: Types.Expr.binding) -> walk_expr planner binding.expr);
      walk_expr planner let_.body
  | Types.Expr.Sequence sequence ->
      walk_expr planner sequence.first;
      walk_expr planner sequence.second
  | Types.Expr.Tuple elements ->
      List.for_each elements ~fn:(walk_expr planner)
  | Types.Expr.Tuple_get tuple_get ->
      walk_expr planner tuple_get.tuple
  | Types.Expr.If_then_else if_then_else ->
      walk_expr planner if_then_else.condition;
      walk_expr planner if_then_else.then_;
      walk_expr planner if_then_else.else_
  | Types.Expr.Primitive primitive ->
      List.for_each primitive.arguments ~fn:(walk_expr planner)

let walk_global = fun planner (global: Types.Global.t) -> walk_expr planner global.expr

let walk_function = fun planner (function_: Types.Function.t) -> walk_expr planner function_.body

let walk_init_item = fun planner (item: Types.Init_item.t) ->
  match item with
  | Types.Init_item.Global global -> walk_global planner global
  | Types.Init_item.Eval expr -> walk_expr planner expr

let program = fun (program: Types.Compilation_unit.t) ->
  let planner = create_planner program in
  List.for_each program.globals ~fn:(walk_global planner);
  List.for_each program.functions ~fn:(walk_function planner);
  List.for_each program.init ~fn:(walk_init_item planner);
  {
    program
    with runtime_plan =
      Types.Runtime_plan.{
        function_table_elements = List.rev planner.table_rev;
        has_indirect_calls = planner.has_indirect_calls;
        needs_closure_runtime = planner.needs_closure_runtime
      };
  }
