module Doc = Doc

module Symbol: sig
  val global: string -> string
end

module Reference: sig
  val absolute: string -> string

  val lo12: string -> string
end

module Directive: sig
  val text_section: unit -> 'instruction Doc.Item.t

  val rodata_section: unit -> 'instruction Doc.Item.t

  val data_section: unit -> 'instruction Doc.Item.t

  val globl: string -> 'instruction Doc.Item.t

  val type_function: string -> 'instruction Doc.Item.t

  val size: string -> string -> 'instruction Doc.Item.t
end
