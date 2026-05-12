open Std
open Std.Data
module Core = Raml_core.Core_ir
module Wasm_types = Types
module Passes = Passes

type pass_snapshot = {
  name: string;
  program: Wasm_types.Compilation_unit.t;
}

type trace = {
  initial: Wasm_types.Compilation_unit.t;
  passes: pass_snapshot list;
  final: Wasm_types.Compilation_unit.t;
}

let rec lower_expr = fun (expr: Core.Expr.t) ->
  match expr with
  | Core.Expr.Constant constant ->
      Wasm_types.Expr.Constant constant
  | Core.Expr.Var entity_id ->
      Wasm_types.Expr.Var entity_id
  | Core.Expr.Apply apply ->
      let arguments = List.map apply.arguments ~fn:lower_expr in
      begin
        match apply.callee with
        | Core.Expr.Direct callee -> Wasm_types.Expr.Direct_call Wasm_types.Expr.{
          callee;
          arguments
        }
        | Core.Expr.Indirect callee -> Wasm_types.Expr.Indirect_call Wasm_types.Expr.{
          callee = lower_expr callee;
          arguments
        }
      end
  | Core.Expr.Lambda lambda ->
      Wasm_types.Expr.Lambda Wasm_types.Expr.{
        params = List.map lambda.params ~fn:lower_param;
        body = lower_expr lambda.body
      }
  | Core.Expr.Let let_ ->
      Wasm_types.Expr.Let Wasm_types.Expr.{
        rec_flag = let_.rec_flag;
        bindings = List.map let_.bindings ~fn:lower_binding;
        body = lower_expr let_.body
      }
  | Core.Expr.Sequence sequence ->
      Wasm_types.Expr.Sequence Wasm_types.Expr.{
        first = lower_expr sequence.first;
        second = lower_expr sequence.second
      }
  | Core.Expr.Tuple tuple ->
      Wasm_types.Expr.Tuple (List.map tuple ~fn:lower_expr)
  | Core.Expr.Tuple_get tuple_get ->
      Wasm_types.Expr.Tuple_get Wasm_types.Expr.{
        tuple = lower_expr tuple_get.tuple;
        index = tuple_get.index
      }
  | Core.Expr.Record record ->
      Wasm_types.Expr.Tuple (List.map
        record
        ~fn:(fun (field: Core.Expr.record_field) -> lower_expr field.value))
  | Core.Expr.Record_get record_get ->
      Wasm_types.Expr.Tuple_get Wasm_types.Expr.{
        tuple = lower_expr record_get.record;
        index = record_get.index
      }
  | Core.Expr.If_then_else if_then_else ->
      Wasm_types.Expr.If_then_else Wasm_types.Expr.{
        condition = lower_expr if_then_else.condition;
        then_ = lower_expr if_then_else.then_;
        else_ = lower_expr if_then_else.else_
      }
  | Core.Expr.Primitive primitive ->
      Wasm_types.Expr.Primitive Wasm_types.Expr.{
        primitive = primitive.primitive;
        kind = Runtime_imports.classify_primitive primitive.primitive;
        arguments = List.map primitive.arguments ~fn:lower_expr
      }

and lower_binding = fun (binding: Core.Expr.binding) ->
  Wasm_types.Expr.{
    entity_id = binding.entity_id;
    name = binding.name;
    expr = lower_expr binding.expr
  }

and lower_param = fun (param: Core.Expr.param) ->
  Wasm_types.Expr.{ entity_id = param.entity_id; name = param.name }

let lower_top_level_binding = fun (binding: Core.Binding.t) ->
  match binding.expr with
  | Core.Expr.Lambda lambda -> `Function Wasm_types.Function.{
    entity_id = binding.entity_id;
    name = binding.name;
    params = List.map lambda.params ~fn:lower_param;
    body = lower_expr lambda.body
  }
  | _ ->
      let global =
        Wasm_types.Global.{
          entity_id = binding.entity_id;
          name = binding.name;
          expr = lower_expr binding.expr
        } in
      `Global global

let lower_init_item = fun (item: Core.Init_item.t) ->
  match item with
  | Core.Init_item.Binding binding -> begin
      match lower_top_level_binding binding with
      | `Function function_ -> (`Function function_, None)
      | `Global global -> (`Global global, Some (Wasm_types.Init_item.Global global))
    end
  | Core.Init_item.Eval expr ->
      let expr = lower_expr expr in
      (`Eval expr, Some (Wasm_types.Init_item.Eval expr))

let lower_binding_group = fun (group: Core.Binding_group.t) ->
  List.fold_left group.items ~init:([], [], [])
    ~fn:(fun (functions, globals, init_items) item ->
      match lower_init_item item with
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

let lower_compilation_unit = fun (compilation_unit: Core.Compilation_unit.t) ->
  let functions, globals, init =
    List.fold_left compilation_unit.init ~init:([], [], [])
      ~fn:(fun (functions, globals, init_items) group ->
        let group_functions, group_globals, group_init = lower_binding_group group in
        (functions @ group_functions, globals @ group_globals, init_items @ group_init))
  in
  Wasm_types.Compilation_unit.{
    unit_id = compilation_unit.unit_id;
    imports = [];
    runtime_plan = Wasm_types.Runtime_plan.empty;
    globals;
    functions;
    init;
    exports = compilation_unit.exports;
  }

let lower_compilation_unit_with_trace = fun compilation_unit ->
  let initial = lower_compilation_unit compilation_unit in
  let normalize = Passes.Normalize.program initial in
  let plan_runtime = Passes.Plan_runtime.program normalize in
  let dead_code = Passes.Dead_code.program plan_runtime in
  let collect_imports = Passes.Collect_imports.program dead_code in
  {
    initial;
    passes = [
      { name = "normalize"; program = normalize };
      { name = "plan_runtime"; program = plan_runtime };
      { name = "dead_code"; program = dead_code };
      { name = "collect_imports"; program = collect_imports };
    ];
    final = collect_imports
  }

let trace_to_json = fun trace ->
  Json.obj
    [
      ("initial", Wasm_types.Compilation_unit.to_json trace.initial);
      (
        "passes",
        Json.array
          (List.map
            trace.passes
            ~fn:(fun pass ->
              Json.obj
                [
                  ("name", Json.string pass.name);
                  ("program", Wasm_types.Compilation_unit.to_json pass.program);
                ]))
      );
      ("final", Wasm_types.Compilation_unit.to_json trace.final);
    ]
