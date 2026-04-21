(** This pass does a deliberately conservative form of common-subexpression
    elimination for [MIR]. It remembers pure materializations of literals and
    symbol addresses, and when the same materialization appears again it
    rewrites the second one into a copy from the first destination register.
    Branch information only survives when both sides agree on the same
    available value. That keeps the optimization honest on our current MIR:
    we reduce repeated pure setup work without pretending global loads or calls
    are safe to reuse. *)
open Std
module Program = Types.Program
module Procedure = Types.Procedure
module Instruction = Types.Instruction
module Operand = Types.Operand
module Literal = Types.Literal

type key =
  | Literal of Literal.t
  | Symbol_address of string

type env = (key * string) list

let key_of_operand = fun operand ->
  match operand with
  | Operand.Literal literal -> Some (Literal literal)
  | Operand.Symbol_address name -> Some (Symbol_address name)
  | Operand.Register _
  | Operand.Global _ -> None

let key_equal = fun left right ->
  match (left, right) with
  | (Literal left, Literal right) -> left = right
  | (Symbol_address left, Symbol_address right) -> String.equal left right
  | _ -> false

let remove_register = fun env name ->
  List.filter env ~fn:(fun (_, current) -> not (String.equal current name))

let remove_key = fun env key -> List.filter env ~fn:(fun (current, _) -> not (key_equal current key))

let bind = fun env key name -> (key, name) :: remove_key (remove_register env name) key

let lookup = fun env key ->
  env
  |> List.find ~fn:(fun (current, _) -> key_equal current key)
  |> Option.map ~fn:(fun (_, name) -> name)

let merge_env = fun left right ->
  left
  |> List.filter_map
    ~fn:(fun (key, left_name) ->
      right
      |> List.find
        ~fn:(fun (other_key, right_name) -> key_equal key other_key && String.equal left_name right_name)
      |> Option.map ~fn:(fun _ -> (key, left_name)))

let rec rewrite_instruction = fun env instruction ->
  match instruction with
  | Instruction.Move { dst; src } -> (
      match key_of_operand src with
      | Some key -> (
          match lookup env key with
          | Some existing when not (String.equal existing dst) ->
              let env = remove_register env dst in
              (bind env key dst, Instruction.Move { dst; src = Operand.Register existing })
          | _ -> (bind env key dst, instruction)
        )
      | None -> (remove_register env dst, instruction)
    )
  | Instruction.Store_global _ ->
      (env, instruction)
  | Instruction.Call { dst; _ } -> (
      match dst with
      | Some dst -> (remove_register env dst, instruction)
      | None -> (env, instruction)
    )
  | Instruction.If_then_else if_then_else ->
      let then_env, then_ = rewrite_instructions env if_then_else.then_ in
      let else_env, else_ = rewrite_instructions env if_then_else.else_ in
      (
        merge_env then_env else_env,
        Instruction.If_then_else Instruction.{ condition = if_then_else.condition; then_; else_ }
      )
  | Instruction.Return _
  | Instruction.Comment _ ->
      (env, instruction)

and rewrite_instructions = fun env instructions ->
  List.fold_left instructions ~init:(env, [])
    ~fn:(fun (env, acc) instruction ->
      let env, instruction = rewrite_instruction env instruction in
      (env, acc @ [ instruction ]))

let rewrite_procedure = fun (procedure: Procedure.t) ->
  let _, body = rewrite_instructions [] procedure.body in
  { procedure with body }

let program = fun (program: Program.t) ->
  { program with procedures = List.map program.procedures ~fn:rewrite_procedure }
