module Doc = Doc

module Register: sig
  type t =
    | Rax
    | Rbx
    | Rcx
    | Rdx
    | Rsp
    | Rbp
  val to_string: t -> string
end

module Instruction: sig
  type t =
    | Raw of string
  val to_string: t -> string

  val to_json: t -> Std.Data.Json.t
end

type t = Instruction.t Doc.Document.t
val to_string: t -> string
