open Std
module Source = Mir
module Target = Types

let lower_literal = fun literal ->
  match literal with
  | Source.Literal.Unit -> Target.Literal.Unit
  | Source.Literal.Bool value -> Target.Literal.Bool value
  | Source.Literal.Int value -> Target.Literal.Int value
  | Source.Literal.Float value -> Target.Literal.Float value
  | Source.Literal.String value -> Target.Literal.String value

let lower_operand = fun operand ->
  match operand with
  | Source.Operand.Register name -> Target.Operand.Register name
  | Source.Operand.Global name -> Target.Operand.Global name
  | Source.Operand.Literal literal -> Target.Operand.Literal (lower_literal literal)

let lower_callee = fun callee ->
  match callee with
  | Source.Callee.Direct name -> Target.Callee.Direct name
  | Source.Callee.Indirect operand -> Target.Callee.Indirect (lower_operand operand)

let lower_instruction = fun instruction ->
  match instruction with
  | Source.Instruction.Move { dst; src } -> Target.Instruction.Move { dst; src = lower_operand src }
  | Source.Instruction.Store_global { symbol; src } -> Target.Instruction.Store_global {
    symbol;
    src = lower_operand src
  }
  | Source.Instruction.Call { dst; callee; arguments } -> Target.Instruction.Call {
    dst;
    callee = lower_callee callee;
    arguments = List.map lower_operand arguments
  }
  | Source.Instruction.Return operand -> Target.Instruction.Return (Option.map lower_operand operand)
  | Source.Instruction.Comment text -> Target.Instruction.Comment text

let lower_kind = fun kind ->
  match kind with
  | Source.Procedure.Function -> Target.Procedure.Function
  | Source.Procedure.Entry -> Target.Procedure.Entry

let lower_procedure = fun (procedure: Source.Procedure.t) ->
  Target.Procedure.{
    name = procedure.name;
    kind = lower_kind procedure.kind;
    params = procedure.params;
    body = Target.Instruction.Label procedure.name :: List.map lower_instruction procedure.body
  }

let lower_export = fun (export: Source.Export.t) ->
  Target.Export.{ name = export.name; symbol = export.symbol }

let lower_program = fun (program: Source.Program.t) ->
  Target.Program.{
    module_name = program.module_name;
    procedures = List.map lower_procedure program.procedures;
    exports = List.map lower_export program.exports
  }
  |> Passes.Layout_frames.program
  |> Passes.Schedule.program
