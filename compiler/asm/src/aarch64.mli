module Doc = Doc

module Register: sig
  type t =
    | X of int
    | W of int
    | Sp
    | Fp
    | Lr
  val x: int -> t

  val w: int -> t

  val sp: t

  val fp: t

  val lr: t

  val to_string: t -> string
end

module Address: sig
  type t =
    | Offset of { base: Register.t; offset: int }
    | Pre_index of { base: Register.t; offset: int }
    | Post_index of { base: Register.t; offset: int }
  val offset: base:Register.t -> offset:int -> t

  val pre_index: base:Register.t -> offset:int -> t

  val post_index: base:Register.t -> offset:int -> t

  val to_string: t -> string
end

module Instruction: sig
  type t =
    | Mov of { dst: Register.t; src: Register.t }
    | Movz of { dst: Register.t; imm: int; shift: int }
    | Movk of { dst: Register.t; imm: int; shift: int }
    | Add of { dst: Register.t; lhs: Register.t; rhs: Register.t }
    | Add_imm of { dst: Register.t; lhs: Register.t; imm: int }
    | Add_symbol of { dst: Register.t; lhs: Register.t; symbol: string }
    | Add_pageoff of { dst: Register.t; lhs: Register.t; symbol: string }
    | Sub_imm of { dst: Register.t; lhs: Register.t; imm: int }
    | Adrp_symbol of { dst: Register.t; symbol: string }
    | Adrp of { dst: Register.t; symbol: string }
    | Ldr of { dst: Register.t; address: Address.t }
    | Str of { src: Register.t; address: Address.t }
    | Stp of { src1: Register.t; src2: Register.t; address: Address.t }
    | Ldp of { dst1: Register.t; dst2: Register.t; address: Address.t }
    | Bl of string
    | Blr of Register.t
    | Ret
  val to_string: t -> string

  val to_json: t -> Std.Data.Json.t
end

type t = Instruction.t Doc.Document.t
val to_string: t -> string
