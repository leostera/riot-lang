type 'a t = 'a eff = ..

(* TODO: uncomment me

   exception Unhandled : 'a t -> exn
*)
exception Continuation_already_resumed

external perform: 'a t -> 'a = "%perform"

module Deep: sig
  type nonrec ('a, 'b) continuation = ('a, 'b) continuation

  val continue: ('a, 'b) continuation -> 'a -> 'b

  val discontinue: ('a, 'b) continuation -> exn -> 'b

  val discontinue_with_backtrace: ('a, 'b) continuation -> exn -> Exception.raw_backtrace -> 'b

  type ('a, 'b) handler = {
    retc: 'a -> 'b;
    exnc: exn -> 'b;
    effc: 'c. 'c t -> (('c, 'b) continuation -> 'b) option;
  }

  val match_with: ('c -> 'a) -> 'c -> ('a, 'b) handler -> 'b

  type 'a effect_handler = {
    effc: 'b. 'b t -> (('b, 'a) continuation -> 'a) option;
  }

  val try_with: ('b -> 'a) -> 'b -> 'a effect_handler -> 'a

  external get_callstack: ('a, 'b) continuation -> int -> Exception.raw_backtrace =
    "caml_get_continuation_callstack"
end

module Shallow: sig
  type ('a, 'b) continuation

  val fiber: ('a -> 'b) -> ('a, 'b) continuation

  type ('a, 'b) handler = {
    retc: 'a -> 'b;
    exnc: exn -> 'b;
    effc: 'c. 'c t -> (('c, 'a) continuation -> 'b) option;
  }

  val continue_with: ('c, 'a) continuation -> 'c -> ('a, 'b) handler -> 'b

  val discontinue_with: ('c, 'a) continuation -> exn -> ('a, 'b) handler -> 'b

  val discontinue_with_backtrace:
    ('a, 'b) continuation ->
    exn ->
    Exception.raw_backtrace ->
    ('b, 'c) handler ->
    'c

  external get_callstack: ('a, 'b) continuation -> int -> Exception.raw_backtrace =
    "caml_get_continuation_callstack"
end
