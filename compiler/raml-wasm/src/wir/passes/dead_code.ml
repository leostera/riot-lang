open Std
module HashMap = Collections.HashMap
module HashSet = Collections.HashSet
module Types = Types
module Core = Raml_core.Core_ir

type env = {
  functions_by_key: (string, Types.Function.t) HashMap.t;
  globals_by_key: (string, Types.Global.t) HashMap.t;
  init_global_keys: string HashSet.t;
  reachable_functions: string HashSet.t;
  reachable_globals: string HashSet.t;
}

let entity_key = fun entity_id ->
  match Core.Entity_id.binding_id entity_id with
  | Some binding_id -> (
      match Core.Binding_id.stamp binding_id with
      | Some stamp -> Core.Binding_id.name binding_id ^ "#" ^ Int.to_string stamp
      | None -> Core.Surface_path.to_string (Core.Entity_id.surface_path entity_id)
    )
  | None -> Core.Surface_path.to_string (Core.Entity_id.surface_path entity_id)

let create_env = fun (program: Types.Compilation_unit.t) ->
  let functions_by_key = HashMap.with_capacity 32 in
  let globals_by_key = HashMap.with_capacity 32 in
  let init_global_keys = HashSet.create () in
  List.iter
    (fun (function_: Types.Function.t) ->
      let _ = HashMap.insert functions_by_key (entity_key function_.entity_id) function_ in
      ())
    program.functions;
  List.iter
    (fun (global: Types.Global.t) ->
      let _ = HashMap.insert globals_by_key (entity_key global.entity_id) global in
      ())
    program.globals;
  List.iter
    (fun item ->
      match item with
      | Types.Init_item.Global global ->
          let _ = HashSet.insert init_global_keys (entity_key global.entity_id) in
          ()
      | Types.Init_item.Eval _ -> ())
    program.init;
  {
    functions_by_key;
    globals_by_key;
    init_global_keys;
    reachable_functions = HashSet.create ();
    reachable_globals = HashSet.create ();
  }

let rec mark_expr = fun env expr ->
  match expr with
  | Types.Expr.Constant _ ->
      ()
  | Types.Expr.Var entity_id ->
      mark_entity env entity_id
  | Types.Expr.Direct_call call ->
      mark_entity env call.callee;
      List.iter (mark_expr env) call.arguments
  | Types.Expr.Indirect_call call ->
      mark_expr env call.callee;
      List.iter (mark_expr env) call.arguments
  | Types.Expr.Lambda lambda ->
      mark_expr env lambda.body
  | Types.Expr.Let let_ ->
      List.iter (fun (binding: Types.Expr.binding) -> mark_expr env binding.expr) let_.bindings;
      mark_expr env let_.body
  | Types.Expr.Sequence sequence ->
      mark_expr env sequence.first;
      mark_expr env sequence.second
  | Types.Expr.Tuple elements ->
      List.iter (mark_expr env) elements
  | Types.Expr.Tuple_get tuple_get ->
      mark_expr env tuple_get.tuple
  | Types.Expr.If_then_else if_then_else ->
      mark_expr env if_then_else.condition;
      mark_expr env if_then_else.then_;
      mark_expr env if_then_else.else_
  | Types.Expr.Primitive primitive ->
      List.iter (mark_expr env) primitive.arguments

and mark_entity = fun env entity_id ->
  let key = entity_key entity_id in
  match (HashMap.get env.functions_by_key key, HashMap.get env.globals_by_key key) with
  | (Some function_, _) when not (HashSet.contains env.reachable_functions key) ->
      let _ = HashSet.insert env.reachable_functions key in
      mark_expr env function_.body
  | (_, Some global) when not (HashSet.contains env.reachable_globals key) ->
      let _ = HashSet.insert env.reachable_globals key in
      mark_expr env global.expr
  | _ ->
      ()

let mark_export = fun env (export: Core.Export.t) -> mark_entity env export.symbol

let mark_init_item = fun env item ->
  match item with
  | Types.Init_item.Global global ->
      let key = entity_key global.entity_id in
      if HashSet.contains env.reachable_globals key then
        ()
      else
        let _ = HashSet.insert env.reachable_globals key in
        mark_expr env global.expr
  | Types.Init_item.Eval expr -> mark_expr env expr

let keep_global = fun env (global: Types.Global.t) ->
  let key = entity_key global.entity_id in
  HashSet.contains env.init_global_keys key || HashSet.contains env.reachable_globals key

let keep_function = fun env (function_: Types.Function.t) ->
  HashSet.contains env.reachable_functions (entity_key function_.entity_id)

let keep_init_item = fun env item ->
  match item with
  | Types.Init_item.Global global -> keep_global env global
  | Types.Init_item.Eval _ -> true

let program = fun (program: Types.Compilation_unit.t) ->
  let env = create_env program in
  List.iter (mark_export env) program.exports;
  List.iter (mark_init_item env) program.init;
  {
    program
    with globals = List.filter (keep_global env) program.globals;
    functions = List.filter (keep_function env) program.functions;
    init = List.filter (keep_init_item env) program.init
  }
