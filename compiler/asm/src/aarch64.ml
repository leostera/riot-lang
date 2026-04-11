open Std
module Doc = Doc
open Std.Data

module Register = struct
  type t =
    | X of int
    | W of int
    | Sp
    | Fp
    | Lr

  let x = fun index -> X index

  let w = fun index -> W index

  let sp = Sp

  let fp = Fp

  let lr = Lr

  let to_string = fun register ->
    match register with
    | X index -> format Format.[ str "x"; int index ]
    | W index -> format Format.[ str "w"; int index ]
    | Sp -> "sp"
    | Fp -> "x29"
    | Lr -> "x30"
end

module Address = struct
  type t =
    | Offset of { base: Register.t; offset: int }
    | Pre_index of { base: Register.t; offset: int }
    | Post_index of { base: Register.t; offset: int }

  let offset = fun ~base ~offset -> Offset { base; offset }

  let pre_index = fun ~base ~offset -> Pre_index { base; offset }

  let post_index = fun ~base ~offset -> Post_index { base; offset }

  let to_string = fun address ->
    match address with
    | Offset { base; offset } -> format
      Format.[ str "["; str (Register.to_string base); str ", #"; int offset; str "]" ]
    | Pre_index { base; offset } -> format
      Format.[ str "["; str (Register.to_string base); str ", #"; int offset; str "]!" ]
    | Post_index { base; offset } -> format
      Format.[ str "["; str (Register.to_string base); str "], #"; int offset ]
end

module Instruction = struct
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
    | B of string
    | Bl of string
    | Blr of Register.t
    | Cbz of { src: Register.t; label: string }
    | Ret

  let render = fun opcode operands ->
    match operands with
    | [] -> opcode
    | _ -> format Format.[ str opcode; str " "; str (String.concat ", " operands) ]

  let to_string = fun instruction_ ->
    match instruction_ with
    | Mov { dst; src } -> render "mov" [ Register.to_string dst; Register.to_string src ]
    | Movz { dst; imm; shift } -> render
      "movz"
      [
        Register.to_string dst;
        format Format.[ str "#"; int imm ];
        format Format.[ str "lsl #"; int shift ];
      ]
    | Movk { dst; imm; shift } -> render
      "movk"
      [
        Register.to_string dst;
        format Format.[ str "#"; int imm ];
        format Format.[ str "lsl #"; int shift ];
      ]
    | Add { dst; lhs; rhs } -> render
      "add"
      [ Register.to_string dst; Register.to_string lhs; Register.to_string rhs ]
    | Add_imm { dst; lhs; imm } -> render
      "add"
      [ Register.to_string dst; Register.to_string lhs; format Format.[ str "#"; int imm ] ]
    | Add_symbol { dst; lhs; symbol } -> render
      "add"
      [ Register.to_string dst; Register.to_string lhs; symbol ]
    | Add_pageoff { dst; lhs; symbol } -> render
      "add"
      [
        Register.to_string dst;
        Register.to_string lhs;
        format Format.[ str symbol; str "@PAGEOFF" ]
      ]
    | Sub_imm { dst; lhs; imm } -> render
      "sub"
      [ Register.to_string dst; Register.to_string lhs; format Format.[ str "#"; int imm ] ]
    | Adrp_symbol { dst; symbol } -> render "adrp" [ Register.to_string dst; symbol ]
    | Adrp { dst; symbol } -> render
      "adrp"
      [ Register.to_string dst; format Format.[ str symbol; str "@PAGE" ] ]
    | Ldr { dst; address } -> render "ldr" [ Register.to_string dst; Address.to_string address ]
    | Str { src; address } -> render "str" [ Register.to_string src; Address.to_string address ]
    | Stp { src1; src2; address } -> render
      "stp"
      [ Register.to_string src1; Register.to_string src2; Address.to_string address ]
    | Ldp { dst1; dst2; address } -> render
      "ldp"
      [ Register.to_string dst1; Register.to_string dst2; Address.to_string address ]
    | B label -> render "b" [ label ]
    | Bl symbol -> render "bl" [ symbol ]
    | Blr register -> render "blr" [ Register.to_string register ]
    | Cbz { src; label } -> render "cbz" [ Register.to_string src; label ]
    | Ret -> render "ret" []

  let to_json = fun instruction_ -> Json.string (to_string instruction_)
end

type t = Instruction.t Doc.Document.t

let to_string = fun document -> Doc.Document.to_string ~instruction_to_string:Instruction.to_string document
