open Std
module Source = Nir
module Target = Types

type state = {
  next_temp: int;
}

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

let operand_of_symbol = fun ~locals name ->
  if List.exists (String.equal name) locals then
    Target.Operand.Register name
  else
    Target.Operand.Global name

let rec lower_operand_list = fun ~locals state values ->
  List.fold_left
    (fun (instructions, operands, state) value ->
      let next_instructions, operand, state = lower_expr ~locals state value in
      (instructions @ next_instructions, operands @ [ operand ], state))
    ([], [], state)
    values

and lower_callee = fun ~locals state callee ->
  match callee with
  | Source.Expr.Direct name -> ([], Target.Callee.Direct name, state)
  | Source.Expr.Indirect expr ->
      let instructions, operand, state = lower_expr ~locals state expr in
      (instructions, Target.Callee.Indirect operand, state)

and lower_expr = fun ~locals state expr ->
  match expr with
  | Source.Expr.Literal literal ->
      let dst, state = fresh_temp state in
      (
        [ Target.Instruction.Move { dst; src = Target.Operand.Literal (lower_literal literal) }; ],
        Target.Operand.Register dst,
        state
      )
  | Source.Expr.Symbol name ->
      ([], operand_of_symbol ~locals name, state)
  | Source.Expr.Call { callee; arguments } ->
      let callee_instructions, callee, state = lower_callee ~locals state callee in
      let argument_instructions, arguments, state = lower_operand_list ~locals state arguments in
      let dst, state = fresh_temp state in
      (
        callee_instructions
        @ argument_instructions
        @ [ Target.Instruction.Call { dst = Some dst; callee; arguments } ],
        Target.Operand.Register dst,
        state
      )

let lower_entry_item = fun state item ->
  match item with
  | Source.Entry_item.Binding binding ->
      let instructions, operand, state = lower_expr ~locals:[] state binding.expr in
      (
        instructions @ [ Target.Instruction.Store_global { symbol = binding.name; src = operand } ],
        state
      )
  | Source.Entry_item.Eval expr ->
      let instructions, _, state = lower_expr ~locals:[] state expr in
      (instructions, state)

let lower_entry = fun items ->
  let body, _ =
    List.fold_left
      (fun (body, state) item ->
        let instructions, state = lower_entry_item state item in
        (body @ instructions, state))
      ([], { next_temp = 0 })
      items
  in
  if body = [] then
    None
  else
    Some Target.Procedure.{ name = "__entry__"; kind = Entry; params = []; body }

let lower_function = fun (function_: Source.Function.t) ->
  let body, operand, _ = lower_expr ~locals:function_.params { next_temp = 0 } function_.body in
  Target.Procedure.{
    name = function_.name;
    kind = Function;
    params = function_.params;
    body = body @ [ Target.Instruction.Return (Some operand) ]
  }

let lower_export = fun (export: Source.Export.t) ->
  Target.Export.{ name = export.name; symbol = export.symbol }

let lower_program = fun (program: Source.Program.t) ->
  let procedures = List.map lower_function program.functions
  @ (lower_entry program.entry |> Option.map List.singleton |> Option.unwrap_or ~default:[]) in
  Target.Program.{
    module_name = program.module_name;
    procedures;
    exports = List.map lower_export program.exports
  }
  |> Passes.Canonicalize.program
  |> Passes.Insert_polls.program
