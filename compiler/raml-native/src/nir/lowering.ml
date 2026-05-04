open Std
open Std.Data
module Compiler_source_unit = Raml_core.Source_unit
module Core = Raml_core.Core_ir
module Nir = Types

type error =
  | UnsupportedModuleKind of { kind: Compiler_source_unit.kind }
  | UnsupportedGroup of { group_index: int; reason: string }
  | UnsupportedBinding of { name: string; reason: string }
  | UnsupportedExpr of { reason: string }

type pass_snapshot = {
  name: string;
  program: Nir.Program.t;
}

type trace = {
  initial: Nir.Program.t;
  passes: pass_snapshot list;
  final: Nir.Program.t;
}

type 'value validation = ('value, error list) result

type lowered_item =
  | LoweredFunction of Nir.Function.t
  | LoweredEntry of Nir.Entry_item.t

type lowered_expr = {
  expr: Nir.Expr.t;
  lifted_functions: Nir.Function.t list;
}

type lowered_let_binding = {
  binding: Nir.Expr.binding option;
  lifted_functions: Nir.Function.t list;
}

type local_function = {
  source_name: string;
  lifted_name: string;
  closure_name: string;
  captures: string list;
  params: string list;
  escapes: bool;
}

type lowering_state = {
  mutable next_local_function: int;
  mutable next_partial_application: int;
  mutable next_sequence_binding: int;
}

type env = {
  bound_values: string list;
  top_level_functions: (string * int) list;
  local_functions: local_function list;
  current_function: string option;
  lowering_state: lowering_state;
}

let ok = fun value -> Ok value

let error = fun value -> Error [ value ]

let validation_map2 = fun left right f ->
  match (left, right) with
  | (Ok left, Ok right) -> Ok (f left right)
  | (Error left, Ok _) -> Error left
  | (Ok _, Error right) -> Error right
  | (Error left, Error right) -> Error (left @ right)

let validation_map3 = fun first second third f ->
  match (first, second, third) with
  | (Ok first, Ok second, Ok third) -> Ok (f first second third)
  | (Error first, Ok _, Ok _) -> Error first
  | (Ok _, Error second, Ok _) -> Error second
  | (Ok _, Ok _, Error third) -> Error third
  | (Error first, Error second, Ok _) -> Error (first @ second)
  | (Error first, Ok _, Error third) -> Error (first @ third)
  | (Ok _, Error second, Error third) -> Error (second @ third)
  | (Error first, Error second, Error third) -> Error (first @ second @ third)

let map_results = fun items f ->
  List.fold_right
    items
    ~init:(Ok [])
    ~fn:(fun item acc -> validation_map2 (f item) acc (fun item acc -> item :: acc))

let source_kind_to_string = fun kind ->
  match kind with
  | Compiler_source_unit.Implementation -> "implementation"
  | Compiler_source_unit.Interface -> "interface"

let trace_to_json = fun trace ->
  Json.obj
    [
      ("status", Json.string "ok");
      ("initial", Nir.Program.to_json trace.initial);
      (
        "passes",
        Json.obj
          (List.map trace.passes ~fn:(fun pass -> (pass.name, Nir.Program.to_json pass.program)))
      );
      ("program", Nir.Program.to_json trace.final);
    ]

let error_to_json = fun error ->
  match error with
  | UnsupportedModuleKind { kind } -> Json.obj
    [
      ("kind", Json.string "unsupported_module_kind");
      ("source_kind", Json.string (source_kind_to_string kind));
    ]
  | UnsupportedGroup { group_index; reason } -> Json.obj
    [
      ("group_index", Json.int group_index);
      ("kind", Json.string "unsupported_group");
      ("reason", Json.string reason);
    ]
  | UnsupportedBinding { name; reason } -> Json.obj
    [
      ("kind", Json.string "unsupported_binding");
      ("name", Json.string name);
      ("reason", Json.string reason);
    ]
  | UnsupportedExpr { reason } -> Json.obj
    [ ("kind", Json.string "unsupported_expr"); ("reason", Json.string reason); ]

let lower_constant = fun constant ->
  match constant with
  | Core.Constant.Unit -> Nir.Literal.Unit
  | Core.Constant.Bool value -> Nir.Literal.Bool value
  | Core.Constant.Int value -> Nir.Literal.Int value
  | Core.Constant.Float value -> Nir.Literal.Float value
  | Core.Constant.Char value -> Nir.Literal.String value
  | Core.Constant.String value -> Nir.Literal.String value

let tuple_make_helper = fun ~arity ->
  let symbol = format Format.[ str "raml_tuple_make_"; int arity ] in
  Nir.Runtime_helper.{ name = symbol; symbol }

let eq_helper = Nir.Runtime_helper.{ name = "raml_eq"; symbol = "raml_eq" }

let tuple_get_helper = Nir.Runtime_helper.{ name = "raml_tuple_get"; symbol = "raml_tuple_get" }

let runtime_call = fun (helper: Nir.Runtime_helper.t) arguments ->
  Nir.Expr.Call Nir.Expr.{ callee = Direct helper.symbol; arguments }

let primitive_helper = fun primitive ->
  match primitive with
  | Core.Primitive.Equal -> Some eq_helper
  | _ -> None

let entity_name = fun entity_id -> Core.Entity_id.to_string entity_id

let local_entity_name = fun entity_id ->
  match Core.Entity_id.binding_id entity_id with
  | Some binding_id -> Some (Core.Binding_id.name binding_id)
  | None -> None

let param_names = fun params -> List.map params ~fn:(fun (param: Core.Expr.param) -> param.name)

let has_name = fun names name ->
  List.exists (String.equal name) names

let add_unique_name = fun names name ->
  if has_name names name then
    names
  else
    names @ [ name ]

let find_local_function = fun env name ->
  env.local_functions |> List.find
    ~fn:(fun (local_function: local_function) ->
      String.equal local_function.source_name name)

let find_top_level_function_arity = fun env name ->
  env.top_level_functions |> List.find
    ~fn:(fun (function_name, _) ->
      String.equal function_name name) |> Option.map ~fn:(fun (_function_name, arity) -> arity)

let lowered_expr = fun expr -> { expr; lifted_functions = [] }

let collect_lowered_exprs = fun lowered_exprs ->
  (
    List.map lowered_exprs ~fn:(fun (lowered_expr: lowered_expr) -> lowered_expr.expr),
    List.flat_map
      lowered_exprs
      ~fn:(fun (lowered_expr: lowered_expr) -> lowered_expr.lifted_functions)
  )

let extend_bound_values = fun env names ->
  { env with bound_values = List.fold_left names ~init:env.bound_values ~fn:add_unique_name }

let extend_local_functions = fun env local_functions ->
  { env with local_functions = local_functions @ env.local_functions }

let fresh_lifted_name = fun env local_name ->
  match env.current_function with
  | None -> error
    (UnsupportedBinding {
      name = local_name;
      reason = "local function lifting requires an owning function or entry context"
    })
  | Some current_function ->
      let next_index = env.lowering_state.next_local_function in
      env.lowering_state.next_local_function <- next_index + 1;
      ok
        (format
          Format.[ str current_function; str "__local_"; str local_name; str "_"; int next_index; ])

let fresh_sequence_binding_name = fun env ->
  let next_index = env.lowering_state.next_sequence_binding in
  env.lowering_state.next_sequence_binding <- next_index + 1;
  format Format.[ str "__seq_"; int next_index; ]

let fresh_partial_application_prefix = fun env function_name ->
  match env.current_function with
  | None -> error
    (UnsupportedExpr {
      reason = "partial application lowering requires an owning function or entry context"
    })
  | Some current_function ->
      let next_index = env.lowering_state.next_partial_application in
      env.lowering_state.next_partial_application <- next_index + 1;
      ok
        (format
          Format.[
            str current_function;
            str "__partial_";
            str function_name;
            str "_";
            int next_index;
          ])

let partial_application_wrapper_name = fun prefix remaining_arity ->
  format Format.[ str prefix; str "__step_"; int remaining_arity ]

let closure_slot = fun closure index ->
  runtime_call tuple_get_helper [ closure; Nir.Expr.Literal (Nir.Literal.Int index) ]

let partial_application_closure = fun ~entrypoint ~original ~bound_arguments ->
  runtime_call
    (tuple_make_helper ~arity:(2 + List.length bound_arguments))
    (Nir.Expr.Symbol_address entrypoint :: original :: bound_arguments)

let rec build_partial_application_functions = fun ~prefix ~bound_argument_count ~remaining_arity ->
  let closure_name = "__closure__" in
  let argument_name = "__arg__" in
  let current_name = partial_application_wrapper_name prefix remaining_arity in
  let closure = Nir.Expr.Symbol closure_name in
  let original = closure_slot closure 1 in
  let bound_arguments =
    List.init ~count:bound_argument_count ~fn:(fun index -> closure_slot closure (index + 2))
  in
  let body =
    if Int.equal remaining_arity 1 then
      Nir.Expr.Call Nir.Expr.{
        callee = Indirect original;
        arguments = bound_arguments @ [ Nir.Expr.Symbol argument_name ]
      }
    else
      partial_application_closure
        ~entrypoint:(partial_application_wrapper_name prefix (remaining_arity - 1))
        ~original
        ~bound_arguments:(bound_arguments @ [ Nir.Expr.Symbol argument_name ])
  in
  let current = Nir.Function.{ name = current_name; params = [ closure_name; argument_name ]; body } in
  if Int.equal remaining_arity 1 then
    [ current ]
  else
    current
    :: build_partial_application_functions
      ~prefix
      ~bound_argument_count:(bound_argument_count + 1)
      ~remaining_arity:(remaining_arity - 1)

let lower_partial_application = fun env ~function_name ~original_symbol ~bound_arguments ~remaining_arity ->
  Result.map (fresh_partial_application_prefix env function_name)
    ~fn:(fun prefix ->
      let entrypoint = partial_application_wrapper_name prefix remaining_arity in
      {
        expr = partial_application_closure
          ~entrypoint
          ~original:(Nir.Expr.Symbol_address original_symbol)
          ~bound_arguments;
        lifted_functions = build_partial_application_functions
          ~prefix
          ~bound_argument_count:(List.length bound_arguments)
          ~remaining_arity
      })

let overapplied_direct_call_error = fun ~name ~expected_arity ~actual_arity ->
  UnsupportedExpr {
    reason =
      format
        Format.[
          str "direct call to `";
          str name;
          str "` supplies ";
          int actual_arity;
          str " arguments but the current native slice only supports ";
          int expected_arity;
          str " parameter";
          str
            (
              if Int.equal expected_arity 1 then
                ""
              else
                "s"
            );
          str " for that function";
        ];
  }

let lower_capture_argument = fun env (local_function: local_function) capture_name ->
  if has_name env.bound_values capture_name then
    ok (lowered_expr (Nir.Expr.Symbol capture_name))
  else
    error
      (UnsupportedExpr {
        reason = format
          Format.[
            str "local function `";
            str local_function.source_name;
            str "` requires captured value `";
            str capture_name;
            str "` outside the first non-escaping local-function NIR slice";
          ]
      })

let lower_direct_call = fun env name lowered_arguments ->
  let arguments, lifted_functions = collect_lowered_exprs lowered_arguments in
  match find_local_function env name with
  | Some local_function ->
      Result.and_then (map_results
        local_function.captures
        (lower_capture_argument env local_function))
        ~fn:(fun lowered_captures ->
          let captures, capture_functions = collect_lowered_exprs lowered_captures in
          let expected_arity = List.length local_function.params in
          let actual_arity = List.length arguments in
          if actual_arity > expected_arity then
            error
              (overapplied_direct_call_error ~name:local_function.source_name ~expected_arity ~actual_arity)
          else if Int.equal actual_arity expected_arity then
            ok
              {
                expr = Nir.Expr.Call Nir.Expr.{
                  callee = Direct local_function.lifted_name;
                  arguments = captures @ arguments
                };
                lifted_functions = capture_functions @ lifted_functions
              }
          else
            Result.map
              (lower_partial_application
                env
                ~function_name:local_function.source_name
                ~original_symbol:local_function.lifted_name
                ~bound_arguments:(captures @ arguments)
                ~remaining_arity:(expected_arity - actual_arity))
              ~fn:(fun (partial: lowered_expr) ->
                {
                  partial
                  with lifted_functions = capture_functions @ lifted_functions @ partial.lifted_functions
                }))
  | None -> (
      match find_top_level_function_arity env name with
      | Some expected_arity ->
          let actual_arity = List.length arguments in
          if actual_arity > expected_arity then
            error (overapplied_direct_call_error ~name ~expected_arity ~actual_arity)
          else if Int.equal actual_arity expected_arity then
            ok
              { expr = Nir.Expr.Call Nir.Expr.{ callee = Direct name; arguments }; lifted_functions }
          else
            Result.map
              (lower_partial_application
                env
                ~function_name:name
                ~original_symbol:name
                ~bound_arguments:arguments
                ~remaining_arity:(expected_arity - actual_arity))
              ~fn:(fun (partial: lowered_expr) ->
                { partial with lifted_functions = lifted_functions @ partial.lifted_functions })
      | None -> ok
        { expr = Nir.Expr.Call Nir.Expr.{ callee = Direct name; arguments }; lifted_functions }
    )

let local_function_of_binding = fun env ~escaping_names (binding: Core.Expr.binding) ->
  match binding.expr with
  | Core.Expr.Lambda lambda ->
      Result.map (fresh_lifted_name env binding.name)
        ~fn:(fun lifted_name ->
          Some {
            source_name = binding.name;
            lifted_name;
            closure_name = format Format.[ str lifted_name; str "__closure" ];
            captures = Analysis.captures_of_lambda
              ~name_of_entity:local_entity_name
              ~bound_values:env.bound_values
              lambda;
            params = param_names lambda.params;
            escapes = has_name escaping_names binding.name;
          })
  | _ -> ok None

let lower_var = fun env entity_id ->
  match local_entity_name entity_id with
  | Some name -> (
      match find_local_function env name with
      | Some local_function ->
          if local_function.escapes then
            Result.and_then (map_results
              local_function.captures
              (lower_capture_argument env local_function))
              ~fn:(fun lowered_captures ->
                let captures, lifted_functions = collect_lowered_exprs lowered_captures in
                ok
                  {
                    expr = runtime_call
                      (tuple_make_helper ~arity:(1 + List.length local_function.captures))
                      (Nir.Expr.Symbol_address local_function.closure_name :: captures);
                    lifted_functions
                  })
          else
            error
              (UnsupportedExpr {
                reason = format
                  Format.[
                    str "local function value `";
                    str name;
                    str "` escapes local-function lowering without closure materialization";
                  ]
              })
      | None -> ok (lowered_expr (Nir.Expr.Symbol name))
    )
  | None -> ok (lowered_expr (Nir.Expr.Symbol (entity_name entity_id)))

let recursive_local_let_is_function_only = fun (let_: Core.Expr.let_) ->
  List.for_all
    (fun (binding: Core.Expr.binding) ->
      match binding.expr with
      | Core.Expr.Lambda _ -> true
      | _ -> false)
    let_.bindings

let rec lower_expr = fun env expr ->
  match expr with
  | Core.Expr.Constant constant ->
      ok (lowered_expr (Nir.Expr.Literal (lower_constant constant)))
  | Core.Expr.Var name ->
      lower_var env name
  | Core.Expr.Apply { callee=Core.Expr.Direct name; arguments } ->
      Result.and_then (map_results arguments (lower_expr env))
        ~fn:(
          match local_entity_name name with
          | Some local_name -> lower_direct_call env local_name
          | None -> lower_direct_call env (entity_name name)
        )
  | Core.Expr.Apply { callee=Core.Expr.Indirect callee; arguments } ->
      validation_map2 (lower_expr env callee) (map_results arguments (lower_expr env))
        (fun callee lowered_arguments ->
          let arguments, argument_functions = collect_lowered_exprs lowered_arguments in
          let closure_name = fresh_sequence_binding_name env in
          let closure = Nir.Expr.Symbol closure_name in
          {
            expr = Nir.Expr.Let Nir.Expr.{
              bindings = [ Nir.Expr.{ name = closure_name; expr = callee.expr } ];
              body = Nir.Expr.Call Nir.Expr.{
                callee = Indirect (runtime_call
                  tuple_get_helper
                  [ closure; Nir.Expr.Literal (Nir.Literal.Int 0) ]);
                arguments = closure :: arguments
              }
            };
            lifted_functions = callee.lifted_functions @ argument_functions
          })
  | Core.Expr.Tuple tuple ->
      Result.map (map_results tuple (lower_expr env))
        ~fn:(fun lowered_arguments ->
          let arguments, lifted_functions = collect_lowered_exprs lowered_arguments in
          {
            expr = runtime_call (tuple_make_helper ~arity:(List.length tuple)) arguments;
            lifted_functions
          })
  | Core.Expr.Tuple_get tuple_get ->
      Result.map
        (lower_expr env tuple_get.tuple)
        ~fn:(fun tuple ->
          {
            expr = runtime_call
              tuple_get_helper
              [ tuple.expr; Nir.Expr.Literal (Nir.Literal.Int tuple_get.index) ];
            lifted_functions = tuple.lifted_functions
          })
  | Core.Expr.Record record ->
      Result.map (map_results
        record
        (fun (field: Core.Expr.record_field) ->
          Result.map (lower_expr env field.value) ~fn:(fun value -> (field.label, value))))
        ~fn:(fun lowered_fields ->
          let values =
            List.map lowered_fields ~fn:(fun (_label, value) -> value)
          in
          let arguments, lifted_functions = collect_lowered_exprs values in
          {
            expr = runtime_call (tuple_make_helper ~arity:(List.length record)) arguments;
            lifted_functions
          })
  | Core.Expr.Record_get record_get ->
      Result.map
        (lower_expr env record_get.record)
        ~fn:(fun record ->
          {
            expr = runtime_call
              tuple_get_helper
              [ record.expr; Nir.Expr.Literal (Nir.Literal.Int record_get.index) ];
            lifted_functions = record.lifted_functions
          })
  | Core.Expr.If_then_else if_then_else ->
      validation_map3
        (lower_expr env if_then_else.condition)
        (lower_expr env if_then_else.then_)
        (lower_expr env if_then_else.else_)
        (fun condition then_ else_ ->
          {
            expr = Nir.Expr.If_then_else Nir.Expr.{
              condition = condition.expr;
              then_ = then_.expr;
              else_ = else_.expr
            };
            lifted_functions = condition.lifted_functions @ then_.lifted_functions @ else_.lifted_functions
          })
  | Core.Expr.Lambda lambda ->
      lower_nested_lambda env lambda
  | Core.Expr.Let let_ ->
      if let_.rec_flag = Core.Rec_flag.Recursive && not (recursive_local_let_is_function_only let_) then
        error
          (UnsupportedExpr {
            reason = "recursive let expressions are only supported when every binding is a local lambda"
          })
      else
        let escaping_names =
          let_.bindings
          |> List.filter_map
            ~fn:(fun (binding: Core.Expr.binding) ->
              match binding.expr with
              | Core.Expr.Lambda _ ->
                  if
                    Analysis.expr_uses_name_as_value
                      ~name_of_entity:local_entity_name
                      ~shadowed:[]
                      binding.name
                      let_.body
                  then
                    Some binding.name
                  else
                    None
              | _ -> None)
        in
        Result.and_then (map_results let_.bindings (local_function_of_binding env ~escaping_names))
          ~fn:(fun local_functions ->
            let local_functions =
              List.filter_map local_functions ~fn:(fun local_function -> local_function)
            in
            let value_binding_names =
              let_.bindings
              |> List.filter_map
                ~fn:(fun (binding: Core.Expr.binding) ->
                  match binding.expr with
                  | Core.Expr.Lambda _ -> None
                  | _ -> Some binding.name)
            in
            let env_for_body = extend_local_functions (extend_bound_values env value_binding_names) local_functions in
            Result.and_then (map_results let_.bindings (lower_let_binding env local_functions))
              ~fn:(fun lowered_bindings ->
                Result.map (lower_expr env_for_body let_.body)
                  ~fn:(fun (body: lowered_expr) ->
                    let bindings = lowered_bindings
                    |> List.filter_map
                      ~fn:(fun (lowered_binding: lowered_let_binding) -> lowered_binding.binding) in
                    let lifted_functions = List.flat_map
                      lowered_bindings
                      ~fn:(fun (lowered_binding: lowered_let_binding) -> lowered_binding.lifted_functions)
                    @ body.lifted_functions in
                    {
                      expr =
                        if bindings = [] then
                          body.expr
                        else
                          Nir.Expr.Let Nir.Expr.{ bindings; body = body.expr };
                      lifted_functions;
                    })))
  | Core.Expr.Sequence sequence ->
      validation_map2
        (lower_expr env sequence.first)
        (lower_expr env sequence.second)
        (fun first second ->
          {
            expr = Nir.Expr.Let Nir.Expr.{
              bindings = [ Nir.Expr.{ name = fresh_sequence_binding_name env; expr = first.expr } ];
              body = second.expr
            };
            lifted_functions = first.lifted_functions @ second.lifted_functions
          })
  | Core.Expr.Primitive primitive -> (
      match primitive_helper primitive.primitive with
      | Some helper ->
          Result.map (map_results primitive.arguments (lower_expr env))
            ~fn:(fun lowered_arguments ->
              let arguments, lifted_functions = collect_lowered_exprs lowered_arguments in
              { expr = runtime_call helper arguments; lifted_functions })
      | None -> error
        (UnsupportedExpr {
          reason = format
            Format.[
              str "primitive `";
              str (Core.Primitive.to_string primitive.primitive);
              str "` is outside the first Core IR -> NIR lowering slice";
            ]
        })
    )

and lower_nested_lambda = fun env (lambda: Core.Expr.lambda) ->
  Result.and_then (fresh_lifted_name env "lambda")
    ~fn:(fun lifted_name ->
      let closure_name = format Format.[ str lifted_name; str "__closure" ] in
      let capture_names = Analysis.captures_of_lambda
        ~name_of_entity:local_entity_name
        ~bound_values:env.bound_values
        lambda in
      let params = param_names lambda.params in
      let lambda_env = {
        bound_values = capture_names @ params;
        top_level_functions = env.top_level_functions;
        local_functions = env.local_functions;
        current_function = Some lifted_name;
        lowering_state = env.lowering_state;
      }
      in
      Result.and_then (lower_expr lambda_env lambda.body)
        ~fn:(fun (lowered_body: lowered_expr) ->
          Result.map (map_results
            capture_names
            (fun capture_name -> lower_var env (Core.Entity_id.from_name capture_name)))
            ~fn:(fun lowered_captures ->
              let capture_values, capture_functions = collect_lowered_exprs lowered_captures in
              let closure_param = "__closure__" in
              let closure_function =
                Nir.Function.{
                  name = closure_name;
                  params = closure_param :: params;
                  body = Nir.Expr.Call Nir.Expr.{
                    callee = Direct lifted_name;
                    arguments = (capture_names
                    |> List.enumerate
                    |> List.map
                      ~fn:(fun (index, _) ->
                        runtime_call
                          tuple_get_helper
                          [
                            Nir.Expr.Symbol closure_param;
                            Nir.Expr.Literal (Nir.Literal.Int (index + 1));
                          ]))
                    @ List.map params ~fn:(fun param -> Nir.Expr.Symbol param)
                  }
                } in
              {
                expr = runtime_call
                  (tuple_make_helper ~arity:(1 + List.length capture_names))
                  (Nir.Expr.Symbol_address closure_name :: capture_values);
                lifted_functions = capture_functions
                @ lowered_body.lifted_functions
                @ [
                  closure_function;
                  Nir.Function.{
                    name = lifted_name;
                    params = capture_names @ params;
                    body = lowered_body.expr
                  };
                ]
              })))

and lower_let_binding = fun env local_functions (binding: Core.Expr.binding) ->
  match binding.expr with
  | Core.Expr.Lambda lambda -> (
      match find_local_function (extend_local_functions env local_functions) binding.name with
      | None -> error
        (UnsupportedBinding {
          name = binding.name;
          reason = "missing lifted local-function metadata"
        })
      | Some local_function ->
          let params = param_names lambda.params in
          let lambda_env = {
            bound_values = local_function.captures @ params;
            top_level_functions = env.top_level_functions;
            local_functions = local_functions @ env.local_functions;
            current_function = Some local_function.lifted_name;
            lowering_state = env.lowering_state;
          }
          in
          Result.map (lower_expr lambda_env lambda.body)
            ~fn:(fun (lowered_body: lowered_expr) ->
              let closure_param = "__closure__" in
              let closure_functions =
                if local_function.escapes then
                  [
                    Nir.Function.{
                      name = local_function.closure_name;
                      params = closure_param :: local_function.params;
                      body = Nir.Expr.Call Nir.Expr.{
                        callee = Direct local_function.lifted_name;
                        arguments = (local_function.captures
                        |> List.enumerate
                        |> List.map
                          ~fn:(fun (index, _) ->
                            runtime_call
                              tuple_get_helper
                              [
                                Nir.Expr.Symbol closure_param;
                                Nir.Expr.Literal (Nir.Literal.Int (index + 1));
                              ]))
                        @ List.map local_function.params ~fn:(fun param -> Nir.Expr.Symbol param)
                      }
                    }
                  ]
                else
                  []
              in
              {
                binding = None;
                lifted_functions = lowered_body.lifted_functions
                @ closure_functions
                @ [
                  Nir.Function.{
                    name = local_function.lifted_name;
                    params = local_function.captures @ params;
                    body = lowered_body.expr
                  }
                ]
              })
    )
  | expr -> Result.map
    (lower_expr env expr)
    ~fn:(fun (lowered_expr: lowered_expr) ->
      {
        binding = Some Nir.Expr.{ name = binding.name; expr = lowered_expr.expr };
        lifted_functions = lowered_expr.lifted_functions
      })

let env_for_function = fun state ~top_level_functions ~current_function ~bound_values ->
  {
    bound_values;
    top_level_functions;
    local_functions = [];
    current_function = Some current_function;
    lowering_state = state;
  }

let lower_binding = fun state ~top_level_functions (binding: Core.Binding.t) ->
  match binding.expr with
  | Core.Expr.Lambda lambda ->
      Result.map (lower_expr
        (env_for_function
          state
          ~top_level_functions
          ~current_function:binding.name
          ~bound_values:(param_names lambda.params))
        lambda.body)
        ~fn:(fun (body: lowered_expr) ->
          let params = param_names lambda.params in
          List.map body.lifted_functions ~fn:(fun function_ -> LoweredFunction function_)
          @ [ LoweredFunction Nir.Function.{ name = binding.name; params; body = body.expr } ])
  | expr -> Result.map
    (lower_expr
      (env_for_function state ~top_level_functions ~current_function:binding.name ~bound_values:[])
      expr)
    ~fn:(fun (lowered_expr: lowered_expr) ->
      List.map lowered_expr.lifted_functions ~fn:(fun function_ -> LoweredFunction function_)
      @ [
        LoweredEntry (Nir.Entry_item.Binding Nir.Binding.{
          name = binding.name;
          expr = lowered_expr.expr
        })
      ])

let lower_item = fun state ~top_level_functions item ->
  match item with
  | Core.Init_item.Binding binding -> lower_binding state ~top_level_functions binding
  | Core.Init_item.Eval expr -> Result.map
    (lower_expr
      (env_for_function state ~top_level_functions ~current_function:"__entry__" ~bound_values:[])
      expr)
    ~fn:(fun (lowered_expr: lowered_expr) ->
      List.map lowered_expr.lifted_functions ~fn:(fun function_ -> LoweredFunction function_)
      @ [ LoweredEntry (Nir.Entry_item.Eval lowered_expr.expr) ])

let recursive_group_is_function_only = fun (group: Core.Binding_group.t) ->
  List.for_all
    (fun item ->
      match item with
      | Core.Init_item.Binding { expr=Core.Expr.Lambda _; _ } -> true
      | _ -> false)
    group.items

let lower_group = fun state ~top_level_functions group_index (group: Core.Binding_group.t) ->
  match group.rec_flag with
  | Core.Rec_flag.Recursive ->
      if recursive_group_is_function_only group then
        Result.map (map_results group.items (lower_item state ~top_level_functions)) ~fn:List.concat
      else
        error
          (UnsupportedGroup {
            group_index;
            reason = "recursive groups are only supported when every item is a top-level lambda binding"
          })
  | Core.Rec_flag.Nonrecursive -> Result.map
    (map_results group.items (lower_item state ~top_level_functions))
    ~fn:List.concat

let lower_export = fun (export: Core.Export.t) ->
  Nir.Export.{ name = export.name; symbol = entity_name export.symbol }

let top_level_functions_of_compilation_unit = fun (compilation_unit: Core.Compilation_unit.t) ->
  compilation_unit.init |> List.flat_map
    ~fn:(fun (group: Core.Binding_group.t) ->
      group.items |> List.filter_map
        ~fn:(fun item ->
          match item with
          | Core.Init_item.Binding { name; expr=Core.Expr.Lambda lambda } -> Some (
            name,
            List.length lambda.params
          )
          | _ -> None))

let has_prefix = fun ~prefix value ->
  let prefix_length = String.length prefix in
  let value_length = String.length value in
  if value_length < prefix_length then
    false
  else
    let rec loop index =
      if index = prefix_length then
        true
      else if value.[index] = prefix.[index] then
        loop (index + 1)
      else
        false
    in
    loop 0

let runtime_helper_of_symbol = fun symbol ->
  if String.equal symbol eq_helper.symbol then
    Some eq_helper
  else if String.equal symbol tuple_get_helper.symbol then
    Some tuple_get_helper
  else if has_prefix ~prefix:"raml_tuple_make_" symbol then
    Some Nir.Runtime_helper.{ name = symbol; symbol }
  else
    None

let add_unique_runtime_helper = fun helpers (helper: Nir.Runtime_helper.t) ->
  if List.exists
      (fun (existing: Nir.Runtime_helper.t) ->
        String.equal existing.symbol helper.symbol)
      helpers then
    helpers
  else
    helpers @ [ helper ]

let rec collect_expr_runtime_helpers = fun helpers expr ->
  match expr with
  | Nir.Expr.Literal _ ->
      helpers
  | Nir.Expr.Symbol _ ->
      helpers
  | Nir.Expr.Symbol_address _ ->
      helpers
  | Nir.Expr.Call { callee; arguments } ->
      let helpers =
        match callee with
        | Nir.Expr.Direct symbol -> (
            match runtime_helper_of_symbol symbol with
            | Some helper -> add_unique_runtime_helper helpers helper
            | None -> helpers
          )
        | Nir.Expr.Indirect callee -> collect_expr_runtime_helpers helpers callee
      in
      List.fold_left arguments ~init:helpers ~fn:collect_expr_runtime_helpers
  | Nir.Expr.If_then_else if_then_else ->
      let helpers = collect_expr_runtime_helpers helpers if_then_else.condition in
      let helpers = collect_expr_runtime_helpers helpers if_then_else.then_ in
      collect_expr_runtime_helpers helpers if_then_else.else_
  | Nir.Expr.Let let_ ->
      let helpers =
        List.fold_left
          let_.bindings
          ~init:helpers
          ~fn:(fun helpers (binding: Nir.Expr.binding) ->
            collect_expr_runtime_helpers helpers binding.expr)
      in
      collect_expr_runtime_helpers helpers let_.body

let runtime_helpers_of_program = fun (program: Nir.Program.t) ->
  let helpers =
    List.fold_left
      program.functions
      ~init:[]
      ~fn:(fun helpers (function_: Nir.Function.t) ->
        collect_expr_runtime_helpers helpers function_.body)
  in
  List.fold_left program.entry ~init:helpers
    ~fn:(fun helpers entry_item ->
      match entry_item with
      | Nir.Entry_item.Binding binding -> collect_expr_runtime_helpers helpers binding.expr
      | Nir.Entry_item.Eval expr -> collect_expr_runtime_helpers helpers expr)

let imports_of_runtime_helpers = fun helpers ->
  List.map
    helpers
    ~fn:(fun (helper: Nir.Runtime_helper.t) ->
      Imports.make ~linkage:Imports.Runtime ~symbol:helper.symbol ())

let trace_program = fun initial ->
  let normalize = Passes.Normalize.program initial in
  let simplify = Passes.Simplify.program normalize in
  {
    initial;
    passes = [
      { name = "normalize"; program = normalize };
      { name = "simplify"; program = simplify }
    ];
    final = simplify
  }

let lower_compilation_unit_with_trace = fun (compilation_unit: Core.Compilation_unit.t) ->
  match compilation_unit.unit_id.kind with
  | Compiler_source_unit.Interface -> error
    (UnsupportedModuleKind { kind = compilation_unit.unit_id.kind })
  | Compiler_source_unit.Implementation ->
      let lowering_state = {
        next_local_function = 0;
        next_partial_application = 0;
        next_sequence_binding = 0
      } in
      let top_level_functions = top_level_functions_of_compilation_unit compilation_unit in
      let groups = List.enumerate compilation_unit.init
      |> List.map ~fn:(fun (index, group) -> (index + 1, group)) in
      Result.map (map_results
        groups
        (fun (group_index, group) -> lower_group lowering_state ~top_level_functions group_index group))
        ~fn:(fun groups ->
          let items = List.concat groups in
          let functions, entry =
            List.fold_left items ~init:([], [])
              ~fn:(fun (functions, entry) item ->
                match item with
                | LoweredFunction function_ -> (functions @ [ function_ ], entry)
                | LoweredEntry entry_item -> (functions, entry @ [ entry_item ]))
          in
          let runtime_helpers = runtime_helpers_of_program
            Nir.Program.{
              module_name = compilation_unit.unit_id.unit_name;
              imports = [];
              runtime_helpers = [];
              functions;
              entry;
              exports = List.map compilation_unit.exports ~fn:lower_export;
            }
          in
          let imports = imports_of_runtime_helpers runtime_helpers in
          Nir.Program.{
            module_name = compilation_unit.unit_id.unit_name;
            imports;
            runtime_helpers;
            functions;
            entry;
            exports = List.map compilation_unit.exports ~fn:lower_export;
          } |> trace_program)

let lower_compilation_unit = fun compilation_unit ->
  Result.map (lower_compilation_unit_with_trace compilation_unit) ~fn:(fun trace -> trace.final)
