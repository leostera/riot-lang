open Std


type get_symbol = { kind : Symbol.kind option; name : string }

type find_symbols = {
  kind : Symbol.kind option;
  pattern : string;
  limit : int option;
}

type get_result = Symbol.t option
type find_result = Symbol.t list

val get_package_symbols : Poneglyph.t -> package_name:string -> string list
val get_symbol : Poneglyph.t -> Symbol.reference -> Symbol.t option
val count_symbols : Poneglyph.t -> int
