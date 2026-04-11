open Std
module HashSet = Collections.HashSet
module Core = Raml_core.Core_ir
module Wasm_types = Types
module Wasm_runtime_imports = Runtime_imports

type state = {
  seen_imports: string HashSet.t;
  mutable imports_rev: Wasm_types.Import.t list;
}

let empty_state = fun () -> { seen_imports = HashSet.create (); imports_rev = [] }

let register_import = fun state import ->
  let key = Wasm_types.Import.key import in
  if HashSet.contains state.seen_imports key then
    ()
  else
    let _ = HashSet.insert state.seen_imports key in
    state.imports_rev <- import :: state.imports_rev

let rec lower_expr = fun state (expr: Core.Expr.t) ->
  match expr with
  | Core.Expr.Constant constant ->
      Wasm_types.Expr.Constant constant
  | Core.Expr.Var entity_id ->
      Wasm_types.Expr.Var entity_id
  | Core.Expr.Apply apply ->
      let arguments = List.map (lower_expr state) apply.arguments in
      begin
        match apply.callee with
        | Core.Expr.Direct callee ->
            Option.iter (register_import state) (Wasm_runtime_imports.import_of_direct_callee callee);
            Wasm_types.Expr.Direct_call Wasm_types.Expr.{ callee; arguments }
        | Core.Expr.Indirect callee -> Wasm_types.Expr.Indirect_call Wasm_types.Expr.{
          callee = lower_expr state callee;
          arguments
        }
      end
  | Core.Expr.Lambda lambda ->
      Wasm_types.Expr.Lambda Wasm_types.Expr.{
        params = List.map lower_param lambda.params;
        body = lower_expr state lambda.body
      }
  | Core.Expr.Let let_ ->
      Wasm_types.Expr.Let Wasm_types.Expr.{
        rec_flag = let_.rec_flag;
        bindings = List.map (lower_binding state) let_.bindings;
        body = lower_expr state let_.body
      }
  | Core.Expr.Sequence sequence ->
      Wasm_types.Expr.Sequence Wasm_types.Expr.{
        first = lower_expr state sequence.first;
        second = lower_expr state sequence.second
      }
  | Core.Expr.Tuple tuple ->
      Wasm_types.Expr.Tuple (List.map (lower_expr state) tuple)
  | Core.Expr.Tuple_get tuple_get ->
      Wasm_types.Expr.Tuple_get Wasm_types.Expr.{
        tuple = lower_expr state tuple_get.tuple;
        index = tuple_get.index
      }
  | Core.Expr.If_then_else if_then_else ->
      Wasm_types.Expr.If_then_else Wasm_types.Expr.{
        condition = lower_expr state if_then_else.condition;
        then_ = lower_expr state if_then_else.then_;
        else_ = lower_expr state if_then_else.else_
      }
  | Core.Expr.Primitive primitive ->
      Option.iter
        (register_import state)
        (Wasm_runtime_imports.import_of_primitive_name primitive.name);
      Wasm_types.Expr.Primitive Wasm_types.Expr.{
        name = primitive.name;
        kind = Wasm_runtime_imports.classify_primitive primitive.name;
        arguments = List.map (lower_expr state) primitive.arguments
      }

and lower_binding = fun state (binding: Core.Expr.binding) ->
  Wasm_types.Expr.{
    entity_id = binding.entity_id;
    name = binding.name;
    expr = lower_expr state binding.expr
  }

and lower_param = fun (param: Core.Expr.param) ->
  Wasm_types.Expr.{ entity_id = param.entity_id; name = param.name }

let lower_top_level_binding = fun state (binding: Core.Binding.t) ->
  match binding.expr with
  | Core.Expr.Lambda lambda -> `Function Wasm_types.Function.{
    entity_id = binding.entity_id;
    name = binding.name;
    params = List.map lower_param lambda.params;
    body = lower_expr state lambda.body
  }
  | _ ->
      let global =
        Wasm_types.Global.{
          entity_id = binding.entity_id;
          name = binding.name;
          expr = lower_expr state binding.expr
        } in
      `Global global

let lower_init_item = fun state (item: Core.Init_item.t) ->
  match item with
  | Core.Init_item.Binding binding -> begin
      match lower_top_level_binding state binding with
      | `Function function_ -> (`Function function_, None)
      | `Global global -> (`Global global, Some (Wasm_types.Init_item.Global global))
    end
  | Core.Init_item.Eval expr ->
      let expr = lower_expr state expr in
      (`Eval expr, Some (Wasm_types.Init_item.Eval expr))

let lower_binding_group = fun state (group: Core.Binding_group.t) ->
  List.fold_left
    (fun (functions, globals, init_items) item ->
      match lower_init_item state item with
      | (`Function function_, None) -> (functions @ [ function_ ], globals, init_items)
      | (`Function function_, Some init_item) -> (
        functions @ [ function_ ],
        globals,
        init_items @ [ init_item ]
      )
      | (`Global global, None) -> (functions, globals @ [ global ], init_items)
      | (`Global global, Some init_item) -> (
        functions,
        globals @ [ global ],
        init_items @ [ init_item ]
      )
      | (`Eval _, None) -> (functions, globals, init_items)
      | (`Eval _, Some init_item) -> (functions, globals, init_items @ [ init_item ]))
    ([], [], [])
    group.items

let lower_compilation_unit = fun (compilation_unit: Core.Compilation_unit.t) ->
  let state = empty_state () in
  let functions, globals, init =
    List.fold_left
      (fun (functions, globals, init_items) group ->
        let group_functions, group_globals, group_init = lower_binding_group state group in
        (functions @ group_functions, globals @ group_globals, init_items @ group_init))
      ([], [], [])
      compilation_unit.init
  in
  Wasm_types.Compilation_unit.{
    unit_id = compilation_unit.unit_id;
    imports = List.rev state.imports_rev;
    globals;
    functions;
    init;
    exports = compilation_unit.exports;
  }
