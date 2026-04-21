open Std
open Std.Data
module Source = Nir
module Target = Types

type pass_snapshot = {
  name: string;
  program: Target.Program.t;
}

type trace = {
  initial: Target.Program.t;
  passes: pass_snapshot list;
  final: Target.Program.t;
}

type state = {
  next_temp: int;
}

type env = (string * Target.Operand.t) list

let fresh_temp = fun state ->
  let name = format Format.[ str "t"; int state.next_temp ] in
  (name, { next_temp = state.next_temp + 1 })

let lower_literal = fun literal ->
  match literal with
  | Source.Literal.Unit -> Target.Literal.Unit
  | Source.Literal.Bool value -> Target.Literal.Bool value
  | Source.Literal.Int value -> Target.Literal.Int value
  | Source.Literal.Float value -> Target.Literal.Float value
  | Source.Literal.String value -> Target.Literal.String value

let operand_of_symbol = fun ~env name ->
  env |> List.find
    ~fn:(fun (bound_name, _operand) ->
      String.equal bound_name name) |> Option.map ~fn:(fun (_bound_name, operand) -> operand) |> Option.unwrap_or
    ~default:(Target.Operand.Global name)

let bind_env = fun env bindings ->
  List.fold_right bindings ~init:env ~fn:(fun binding env -> binding :: env)

let env_of_params = fun params ->
  List.map params ~fn:(fun name -> (name, Target.Operand.Register name))

let rec lower_operand_list = fun ~env state values ->
  List.fold_left values ~init:([], [], state)
    ~fn:(fun (instructions, operands, state) value ->
      let next_instructions, operand, state = lower_expr ~env state value in
      (instructions @ next_instructions, operands @ [ operand ], state))

and lower_callee = fun ~env state callee ->
  match callee with
  | Source.Expr.Direct name -> ([], Target.Callee.Direct name, state)
  | Source.Expr.Indirect expr ->
      let instructions, operand, state = lower_expr ~env state expr in
      (instructions, Target.Callee.Indirect operand, state)

and lower_if_then_else = fun ~env state (if_then_else: Source.Expr.if_then_else) ->
  let condition_instructions, condition, state = lower_expr ~env state if_then_else.condition in
  let then_instructions, then_operand, state = lower_expr ~env state if_then_else.then_ in
  let else_instructions, else_operand, state = lower_expr ~env state if_then_else.else_ in
  let dst, state = fresh_temp state in
  (
    condition_instructions
    @ [
      Target.Instruction.If_then_else Target.Instruction.{
        condition;
        then_ = then_instructions @ [ Target.Instruction.Move { dst; src = then_operand } ];
        else_ = else_instructions @ [ Target.Instruction.Move { dst; src = else_operand } ]
      }
    ],
    Target.Operand.Register dst,
    state
  )

and lower_let = fun ~env state (let_: Source.Expr.let_) ->
  let binding_instructions, binding_values, state =
    List.fold_left let_.bindings ~init:([], [], state)
      ~fn:(fun (instructions, binding_values, state) (binding: Source.Expr.binding) ->
        let next_instructions, operand, state = lower_expr ~env state binding.expr in
        (instructions @ next_instructions, binding_values @ [ (binding.name, operand) ], state))
  in
  let storage_instructions, bound_locals, state =
    List.fold_left binding_values ~init:([], [], state)
      ~fn:(fun (instructions, bound_locals, state) (name, operand) ->
        let dst, state = fresh_temp state in
        (
          instructions @ [ Target.Instruction.Move { dst; src = operand } ],
          bound_locals @ [ (name, Target.Operand.Register dst) ],
          state
        ))
  in
  let body_instructions, body_operand, state = lower_expr
    ~env:(bind_env env bound_locals)
    state
    let_.body in
  (binding_instructions @ storage_instructions @ body_instructions, body_operand, state)

and lower_expr = fun ~env state expr ->
  match expr with
  | Source.Expr.Literal literal ->
      let dst, state = fresh_temp state in
      (
        [ Target.Instruction.Move { dst; src = Target.Operand.Literal (lower_literal literal) }; ],
        Target.Operand.Register dst,
        state
      )
  | Source.Expr.Symbol name ->
      ([], operand_of_symbol ~env name, state)
  | Source.Expr.Symbol_address name ->
      ([], Target.Operand.Symbol_address name, state)
  | Source.Expr.Call { callee; arguments } ->
      let callee_instructions, callee, state = lower_callee ~env state callee in
      let argument_instructions, arguments, state = lower_operand_list ~env state arguments in
      let dst, state = fresh_temp state in
      (
        callee_instructions
        @ argument_instructions
        @ [ Target.Instruction.Call { dst = Some dst; callee; arguments } ],
        Target.Operand.Register dst,
        state
      )
  | Source.Expr.If_then_else if_then_else ->
      lower_if_then_else ~env state if_then_else
  | Source.Expr.Let let_ ->
      lower_let ~env state let_

let lower_entry_item = fun state item ->
  match item with
  | Source.Entry_item.Binding binding ->
      let instructions, operand, state = lower_expr ~env:[] state binding.expr in
      (
        instructions @ [ Target.Instruction.Store_global { symbol = binding.name; src = operand } ],
        state
      )
  | Source.Entry_item.Eval expr ->
      let instructions, _, state = lower_expr ~env:[] state expr in
      (instructions, state)

let lower_entry = fun items ->
  let body, _ =
    List.fold_left items ~init:([], { next_temp = 0 })
      ~fn:(fun (body, state) item ->
        let instructions, state = lower_entry_item state item in
        (body @ instructions, state))
  in
  if body = [] then
    None
  else
    Some Target.Procedure.{ name = "__entry__"; kind = Entry; params = []; body }

let lower_function = fun (function_: Source.Function.t) ->
  let body, operand, _ = lower_expr
    ~env:(env_of_params function_.params)
    { next_temp = 0 }
    function_.body in
  Target.Procedure.{
    name = function_.name;
    kind = Function;
    params = function_.params;
    body = body @ [ Target.Instruction.Return (Some operand) ]
  }

let lower_export = fun (export: Source.Export.t) ->
  Target.Export.{ name = export.name; symbol = export.symbol }

let trace_to_json = fun trace ->
  Json.obj
    [
      ("status", Json.string "ok");
      ("initial", Target.Program.to_json trace.initial);
      (
        "passes",
        Json.obj
          (List.map trace.passes ~fn:(fun pass -> (pass.name, Target.Program.to_json pass.program)))
      );
      ("program", Target.Program.to_json trace.final);
    ]

let trace_program = fun initial ->
  let canonicalize = Passes.Canonicalize.program initial in
  let insert_polls = Passes.Insert_polls.program canonicalize in
  let cse = Passes.Cse.program insert_polls in
  let copy_propagate = Passes.Copy_propagate.program cse in
  let dead_code = Passes.Dead_code.program copy_propagate in
  {
    initial;
    passes = [
      { name = "canonicalize"; program = canonicalize };
      { name = "insert_polls"; program = insert_polls };
      { name = "cse"; program = cse };
      { name = "copy_propagate"; program = copy_propagate };
      { name = "dead_code"; program = dead_code };
    ];
    final = dead_code
  }

let lower_program_with_trace = fun (program: Source.Program.t) ->
  let procedures = List.map program.functions ~fn:lower_function
  @ (lower_entry program.entry
  |> Option.map ~fn:(fun entry -> [ entry ])
  |> Option.unwrap_or ~default:[]) in
  Target.Program.{
    module_name = program.module_name;
    procedures;
    exports = List.map program.exports ~fn:lower_export
  }
  |> trace_program

let lower_program = fun program -> (lower_program_with_trace program).final
