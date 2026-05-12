(** This pass pushes cheap copied values forward through structured [MIR]. It
    keeps a forward environment of copies, rewrites later uses to point at the
    cheaper source operand, invalidates destinations when they are overwritten,
    and only keeps branch knowledge when both sides agree. The effect is that
    literals, symbol addresses, and simple register aliases stop bouncing
    through temporary names, which in turn exposes more dead work for the next
    cleanup pass. *)
open Std
module Program = Types.Program
module Procedure = Types.Procedure
module Instruction = Types.Instruction
module Operand = Types.Operand
module Callee = Types.Callee
module Literal = Types.Literal

type env = (string * Operand.t) list

let find = fun env name ->
  env |> List.find
    ~fn:(fun (bound_name, _) ->
      String.equal bound_name name) |> Option.map ~fn:(fun (_, operand) -> operand)

let remove = fun env name ->
  List.filter env ~fn:(fun (bound_name, _) -> not (String.equal bound_name name))

let add = fun env name operand -> (name, operand) :: remove env name

let propagatable_operand = fun operand ->
  match operand with
  | Operand.Register _
  | Operand.Symbol_address _
  | Operand.Literal _ -> true
  | Operand.Global _ -> false

let operand_equal = fun left right ->
  match (left, right) with
  | (Operand.Register left, Operand.Register right) -> String.equal left right
  | (Operand.Global left, Operand.Global right) -> String.equal left right
  | (Operand.Symbol_address left, Operand.Symbol_address right) -> String.equal left right
  | (Operand.Literal left, Operand.Literal right) -> left = right
  | _ -> false

let rec depends_on = fun env target operand ->
  match operand with
  | Operand.Register name ->
      String.equal name target || (
        match find env name with
        | Some operand -> depends_on env target operand
        | None -> false
      )
  | Operand.Global _
  | Operand.Symbol_address _
  | Operand.Literal _ -> false

let invalidate = fun env name ->
  remove env name |> List.filter ~fn:(fun (_, operand) -> not (depends_on env name operand))

let resolve_operand = fun env operand ->
  let rec loop seen operand =
    match operand with
    | Operand.Register name ->
        if List.exists (String.equal name) seen then
          operand
        else
          (
            match find env name with
            | Some next when propagatable_operand next -> loop (name :: seen) next
            | _ -> operand
          )
    | Operand.Global _
    | Operand.Symbol_address _
    | Operand.Literal _ -> operand
  in
  loop [] operand

let rewrite_callee = fun env callee ->
  match callee with
  | Callee.Direct _ -> callee
  | Callee.Indirect operand -> Callee.Indirect (resolve_operand env operand)

let rewrite_operands = fun env operands -> List.map operands ~fn:(resolve_operand env)

let rec rewrite_instruction = fun env instruction ->
  match instruction with
  | Instruction.Move { dst; src } ->
      let src = resolve_operand env src in
      if operand_equal src (Operand.Register dst) then
        (env, [])
      else
        let env = invalidate env dst in
        let env =
          if propagatable_operand src then
            add env dst src
          else
            env
        in
        (env, [ Instruction.Move { dst; src } ])
  | Instruction.Store_global { symbol; src } ->
      let src = resolve_operand env src in
      (env, [ Instruction.Store_global { symbol; src } ])
  | Instruction.Call { dst; callee; arguments } ->
      let callee = rewrite_callee env callee in
      let arguments = rewrite_operands env arguments in
      let env =
        match dst with
        | Some dst -> invalidate env dst
        | None -> env
      in
      (env, [ Instruction.Call { dst; callee; arguments } ])
  | Instruction.If_then_else if_then_else ->
      let condition = resolve_operand env if_then_else.condition in
      let then_env, then_ = rewrite_instructions env if_then_else.then_ in
      let else_env, else_ = rewrite_instructions env if_then_else.else_ in
      let env = merge_env then_env else_env in
      (
        match condition with
        | Operand.Literal (Literal.Bool true) -> (then_env, then_)
        | Operand.Literal (Literal.Bool false) -> (else_env, else_)
        | _ when then_ = [] && else_ = [] -> (env, [])
        | _ -> (env, [ Instruction.If_then_else Instruction.{ condition; then_; else_ } ])
      )
  | Instruction.Return operand ->
      let operand = Option.map operand ~fn:(resolve_operand env) in
      (env, [ Instruction.Return operand ])
  | Instruction.Comment _ ->
      (env, [])

and rewrite_instructions = fun env instructions ->
  List.fold_left instructions ~init:(env, [])
    ~fn:(fun (env, acc) instruction ->
      let env, rewritten = rewrite_instruction env instruction in
      (env, acc @ rewritten))

and merge_env = fun left right ->
  left |> List.filter_map
    ~fn:(fun (name, _) ->
      let left_resolved = resolve_operand left (Operand.Register name) in
      match resolve_operand right (Operand.Register name) with
      | right_resolved when operand_equal left_resolved right_resolved ->
          if propagatable_operand left_resolved then
            Some (name, left_resolved)
          else
            None
      | _ -> None)

let rewrite_procedure = fun (procedure: Procedure.t) ->
  let _, body = rewrite_instructions [] procedure.body in
  { procedure with body }

let program = fun (program: Program.t) ->
  { program with procedures = List.map program.procedures ~fn:rewrite_procedure }
