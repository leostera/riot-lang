open Std
open Syn

module TypingContext = Typing_context
module Typings = File

type typing_context = TypingContext.t

let make_typing_context = fun () -> TypingContext.empty

let check = fun ?(typing_context = make_typing_context ()) ~source:_ source_file ->
  Core.check_source_file ~typing_context source_file
