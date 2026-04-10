module Doc = Doc

module Symbol: sig
  val global: string -> string
end

module Reference: sig
  val absolute: string -> string

  val page: string -> string

  val pageoff: string -> string
end

module Directive: sig
  val text_section: unit -> 'instruction Doc.Item.t

  val cstring_section: unit -> 'instruction Doc.Item.t

  val data_section: unit -> 'instruction Doc.Item.t

  val globl: string -> 'instruction Doc.Item.t

  val p2align: int -> 'instruction Doc.Item.t

  val quad: string -> 'instruction Doc.Item.t

  val asciz: string -> 'instruction Doc.Item.t
end
