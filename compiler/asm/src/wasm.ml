open Std
open Std.Data
module Doc = Doc

module Value_type = struct
  type t =
    | I32
    | I64
    | F32
    | F64
    | Func_ref
    | Extern_ref

  let to_string = fun value_type ->
    match value_type with
    | I32 -> "i32"
    | I64 -> "i64"
    | F32 -> "f32"
    | F64 -> "f64"
    | Func_ref -> "funcref"
    | Extern_ref -> "externref"

  let to_json = fun value_type -> Json.string (to_string value_type)
end

module Result_type = struct
  type t = Value_type.t list

  let to_string = fun result_type -> result_type |> List.map ~fn:Value_type.to_string |> String.concat " "

  let to_json = fun result_type -> Json.array (List.map result_type ~fn:Value_type.to_json)
end

module Memarg = struct
  type t = {
    offset: int;
    align: int option;
  }

  let make = fun ?(offset = 0) ?align () -> { offset; align }

  let to_string = fun memarg ->
    let parts =
      if memarg.offset = 0 then
        []
      else
        [ format Format.[ str "offset="; int memarg.offset ] ]
    in
    let parts =
      match memarg.align with
      | None -> parts
      | Some align -> parts @ [ format Format.[ str "align="; int align ] ]
    in
    String.concat " " parts

  let to_json = fun memarg ->
    Json.obj
      [
        ("offset", Json.int memarg.offset);
        ("align", Option.map memarg.align ~fn:Json.int |> Option.unwrap_or ~default:Json.null);
      ]
end

module Instruction = struct
  type t =
    | Nop
    | Unreachable
    | Drop
    | Return
    | Else
    | End
    | Local_get of string
    | Local_set of string
    | Local_tee of string
    | Global_get of string
    | Global_set of string
    | Call of string
    | Call_indirect of { params: Result_type.t; results: Result_type.t }
    | I32_const of int
    | I64_const of int64
    | F32_const of float
    | F64_const of float
    | I32_eqz
    | I32_eq
    | I32_ne
    | I32_lt_s
    | I32_gt_s
    | I32_le_s
    | I32_ge_s
    | I32_add
    | I32_sub
    | I32_mul
    | I32_load of Memarg.t
    | I32_store of Memarg.t
    | Block of { label: string option; results: Result_type.t }
    | Loop of { label: string option; results: Result_type.t }
    | If of { label: string option; results: Result_type.t }
    | Br of string
    | Br_if of string

  let wasm_name = fun name ->
    if String.starts_with ~prefix:"$" name then
      name
    else
      format Format.[ str "$"; str name ]

  let render = fun opcode operands ->
    match operands with
    | [] -> opcode
    | _ -> format Format.[ str opcode; str " "; str (String.concat " " operands) ]

  let render_block_signature = fun ~label ~results ->
    let parts =
      match label with
      | None -> []
      | Some label -> [ wasm_name label ]
    in
    let parts =
      match results with
      | [] -> parts
      | results -> parts
      @ [ format Format.[ str "(result "; str (Result_type.to_string results); str ")"; ] ]
    in
    parts

  let memarg_suffix = fun memarg ->
    let suffix = Memarg.to_string memarg in
    if String.equal suffix "" then
      []
    else
      [ suffix ]

  let call_indirect_operands = fun ~params ~results ->
    let type_parts =
      match params with
      | [] -> []
      | params -> [ format Format.[ str "(param "; str (Result_type.to_string params); str ")" ] ]
    in
    let result_parts =
      match results with
      | [] -> []
      | results -> [ format Format.[ str "(result "; str (Result_type.to_string results); str ")" ] ]
    in
    type_parts @ result_parts

  let to_string = fun instruction ->
    match instruction with
    | Nop -> "nop"
    | Unreachable -> "unreachable"
    | Drop -> "drop"
    | Return -> "return"
    | Else -> "else"
    | End -> "end"
    | Local_get name -> render "local.get" [ wasm_name name ]
    | Local_set name -> render "local.set" [ wasm_name name ]
    | Local_tee name -> render "local.tee" [ wasm_name name ]
    | Global_get name -> render "global.get" [ wasm_name name ]
    | Global_set name -> render "global.set" [ wasm_name name ]
    | Call name -> render "call" [ wasm_name name ]
    | Call_indirect { params; results } -> render
      "call_indirect"
      (call_indirect_operands ~params ~results)
    | I32_const value -> render "i32.const" [ string_of_int value ]
    | I64_const value -> render "i64.const" [ Int64.to_string value ]
    | F32_const value -> render "f32.const" [ string_of_float value ]
    | F64_const value -> render "f64.const" [ string_of_float value ]
    | I32_eqz -> "i32.eqz"
    | I32_eq -> "i32.eq"
    | I32_ne -> "i32.ne"
    | I32_lt_s -> "i32.lt_s"
    | I32_gt_s -> "i32.gt_s"
    | I32_le_s -> "i32.le_s"
    | I32_ge_s -> "i32.ge_s"
    | I32_add -> "i32.add"
    | I32_sub -> "i32.sub"
    | I32_mul -> "i32.mul"
    | I32_load memarg -> render "i32.load" (memarg_suffix memarg)
    | I32_store memarg -> render "i32.store" (memarg_suffix memarg)
    | Block { label; results } -> render "block" (render_block_signature ~label ~results)
    | Loop { label; results } -> render "loop" (render_block_signature ~label ~results)
    | If { label; results } -> render "if" (render_block_signature ~label ~results)
    | Br label -> render "br" [ wasm_name label ]
    | Br_if label -> render "br_if" [ wasm_name label ]

  let to_json = fun instruction -> Json.string (to_string instruction)
end

type t = Instruction.t Doc.Document.t

let to_string = fun document -> Doc.Document.to_string ~instruction_to_string:Instruction.to_string document

let to_json = fun document -> Doc.Document.to_json ~instruction_to_json:Instruction.to_json document
