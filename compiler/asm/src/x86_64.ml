module Doc = Doc
open Std.Data

module Register = struct
  type t =
    | Rax
    | Rbx
    | Rcx
    | Rdx
    | Rsp
    | Rbp

  let to_string = fun register ->
    match register with
    | Rax -> "rax"
    | Rbx -> "rbx"
    | Rcx -> "rcx"
    | Rdx -> "rdx"
    | Rsp -> "rsp"
    | Rbp -> "rbp"
end

module Instruction = struct
  type t =
    | Raw of string

  let to_string = fun instruction ->
    match instruction with
    | Raw text -> text

  let to_json = fun instruction -> Json.string (to_string instruction)
end

type t = Instruction.t Doc.Document.t

let to_string = fun document -> Doc.Document.to_string ~instruction_to_string:Instruction.to_string document
