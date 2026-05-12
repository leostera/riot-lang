open Std
open Std.Data
module Source = Mir
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
  next_label: int;
}

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
  | Source.Operand.Symbol_address name -> Target.Operand.Symbol_address name
  | Source.Operand.Literal literal -> Target.Operand.Literal (lower_literal literal)

let lower_callee = fun callee ->
  match callee with
  | Source.Callee.Direct name -> Target.Callee.Direct name
  | Source.Callee.Indirect operand -> Target.Callee.Indirect (lower_operand operand)

let fresh_label = fun state prefix ->
  let label = format Format.[ str "L__raml_"; str prefix; str "_"; int state.next_label ] in
  (label, { next_label = state.next_label + 1 })

let rec lower_instruction_list = fun state instructions ->
  List.fold_left instructions ~init:([], state)
    ~fn:(fun (body, state) instruction ->
      let lowered, state = lower_instruction state instruction in
      (body @ lowered, state))

and lower_instruction = fun state instruction ->
  match instruction with
  | Source.Instruction.Move { dst; src } ->
      (
        [
          Target.Instruction.Move { dst = Target.Destination.Register dst; src = lower_operand src }
        ],
        state
      )
  | Source.Instruction.Store_global { symbol; src } ->
      ([ Target.Instruction.Store_global { symbol; src = lower_operand src } ], state)
  | Source.Instruction.Call { dst; callee; arguments } ->
      (
        [
          Target.Instruction.Call {
            dst = Option.map dst ~fn:(fun dst -> Target.Destination.Register dst);
            callee = lower_callee callee;
            arguments = List.map arguments ~fn:lower_operand
          }
        ],
        state
      )
  | Source.Instruction.If_then_else if_then_else ->
      let else_label, state = fresh_label state "else" in
      let end_label, state = fresh_label state "endif" in
      let then_body, state = lower_instruction_list state if_then_else.then_ in
      let else_body, state = lower_instruction_list state if_then_else.else_ in
      (
        [
          Target.Instruction.Branch_if_zero {
            operand = lower_operand if_then_else.condition;
            target = else_label
          }
        ]
        @ then_body
        @ [ Target.Instruction.Jump end_label; Target.Instruction.Label else_label ]
        @ else_body
        @ [ Target.Instruction.Label end_label ],
        state
      )
  | Source.Instruction.Return operand ->
      ([ Target.Instruction.Return (Option.map operand ~fn:lower_operand) ], state)
  | Source.Instruction.Comment text ->
      ([ Target.Instruction.Comment text ], state)

let lower_kind = fun kind ->
  match kind with
  | Source.Procedure.Function -> Target.Procedure.Function
  | Source.Procedure.Entry -> Target.Procedure.Entry

let lower_procedure = fun (procedure: Source.Procedure.t) ->
  let body, _ = lower_instruction_list { next_label = 0 } procedure.body in
  Target.Procedure.{
    name = procedure.name;
    kind = lower_kind procedure.kind;
    params = procedure.params;
    frame = Target.Frame.empty;
    body = Target.Instruction.Label procedure.name :: body;
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

let trace_program = fun ~ctx initial ->
  let simplify = Passes.Simplify.program initial in
  let dead_code = Passes.Dead_code.program simplify in
  let schedule = Passes.Schedule.program dead_code in
  let layout_frames, analysis = Passes.Layout_frames.program_with_analysis schedule in
  let allocate_homes = Passes.Allocate_homes.program ~ctx ~analysis layout_frames in
  let assign_homes = Passes.Assign_homes.program allocate_homes in
  let legalize = Passes.Legalize.program ~ctx assign_homes in
  let calling_convention = Passes.Calling_convention.program ~ctx legalize in
  {
    initial;
    passes = [
      { name = "simplify"; program = simplify };
      { name = "dead_code"; program = dead_code };
      { name = "schedule"; program = schedule };
      { name = "layout_frames"; program = layout_frames };
      { name = "allocate_homes"; program = allocate_homes };
      { name = "assign_homes"; program = assign_homes };
      { name = "legalize"; program = legalize };
      { name = "calling_convention"; program = calling_convention }
    ];
    final = calling_convention
  }

let lower_program_with_trace = fun ~ctx (program: Source.Program.t) ->
  Target.Program.{
    module_name = program.module_name;
    procedures = List.map program.procedures ~fn:lower_procedure;
    exports = List.map program.exports ~fn:lower_export
  }
  |> trace_program ~ctx

let lower_program = fun ~ctx program -> (lower_program_with_trace ~ctx program).final
