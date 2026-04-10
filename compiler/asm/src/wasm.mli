open Std.Data

module Doc = Doc

module Value_type: sig
  type t =
    | I32
    | I64
    | F32
    | F64
    | Func_ref
    | Extern_ref
  val to_string: t -> string

  val to_json: t -> Json.t
end

module Result_type: sig
  type t = Value_type.t list
  val to_string: t -> string

  val to_json: t -> Json.t
end

module Memarg: sig
  type t = {
    offset: int;
    align: int option;
  }
  val make: ?offset:int -> ?align:int -> unit -> t

  val to_string: t -> string

  val to_json: t -> Json.t
end

module Instruction: sig
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
  val to_string: t -> string

  val to_json: t -> Json.t
end

type t = Instruction.t Doc.Document.t
val to_string: t -> string

val to_json: t -> Json.t
