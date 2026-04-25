open Prelude

type 'a t = 'a eff = ..

external perform: 'a t -> 'a = "%perform"

(* TODO: uncomment me

  type exn += Unhandled: 'a t -> exn
  *)
exception Continuation_already_resumed

module Unsafe = struct
  type value

  external repr: 'a -> value = "%identity"

  external field: value -> int -> value = "%obj_field"

  external tag: value -> int = "caml_obj_tag" [@@noalloc]

  external register_named_value: string -> value -> unit = "caml_register_named_value"

  let object_tag = 248

  let register_exception = fun name exn ->
    let value = repr exn in
    let slot =
      match Int.compare (tag value) object_tag with
      | Order.EQ -> value
      | Order.LT | Order.GT -> field value 0
    in
    register_named_value name slot
end

type _ t +=
  | Should_not_see_this__ : unit t

(* TODO: uncomment me
let _ = Unsafe.register_exception "Effect.Unhandled" (Unhandled Should_not_see_this__)
*)
let _ = Unsafe.register_exception "Effect.Continuation_already_resumed" Continuation_already_resumed

type ('a, 'b) stack [@@immediate]

type last_fiber [@@immediate]

external resume: ('a, 'b) stack -> ('c -> 'a) -> 'c -> last_fiber -> 'b = "%resume"

external runstack: ('a, 'b) stack -> ('c -> 'a) -> 'c -> 'b = "%runstack"

external raise_with_backtrace: exn -> Exception.raw_backtrace -> 'a = "%raise_with_backtrace"

module Deep = struct
  type nonrec ('a, 'b) continuation = ('a, 'b) continuation

  external take_cont_noexc: ('a, 'b) continuation -> ('a, 'b) stack = "caml_continuation_use_noexc" [@@noalloc]

  external alloc_stack: ('a -> 'b) -> (exn -> 'b) -> ('c t -> ('c, 'b) continuation -> last_fiber -> 'b) -> ('a, 'b) stack = "caml_alloc_stack"

  external cont_last_fiber: ('a, 'b) continuation -> last_fiber = "%field1"

  let continue = fun k value ->
    resume (take_cont_noexc k)
      (
        fun x -> x
      )
      value
      (cont_last_fiber k)

  let discontinue = fun k exn ->
    resume (take_cont_noexc k)
      (
        fun err -> raise err
      )
      exn
      (cont_last_fiber k)

  let discontinue_with_backtrace = fun k exn backtrace ->
    resume (take_cont_noexc k)
      (
        fun err -> raise_with_backtrace err backtrace
      )
      exn
      (cont_last_fiber k)

  type ('a, 'b) handler = {
    retc: 'a -> 'b;
    exnc: exn -> 'b;
    effc: 'c. 'c t -> (('c, 'b) continuation -> 'b) option;
  }

  external reperform: 'a t -> ('a, 'b) continuation -> last_fiber -> 'b = "%reperform"

  let match_with = fun computation value handler ->
    let effc eff continuation last_fiber =
      match handler.effc eff with
      | Some fn -> fn continuation
      | None -> reperform eff continuation last_fiber
    in
    let stack = alloc_stack handler.retc handler.exnc effc in runstack stack computation value

  type 'a effect_handler = {
    effc: 'b. 'b t -> (('b, 'a) continuation -> 'a) option;
  }

  let try_with = fun computation value handler ->
    let effc eff continuation last_fiber =
      match handler.effc eff with
      | Some fn -> fn continuation
      | None -> reperform eff continuation last_fiber
    in
    let stack =
      alloc_stack
        (
          fun x -> x
        )
        (
          fun exn -> raise exn
        )
        effc
    in
    runstack stack computation value

  external get_callstack: ('a, 'b) continuation -> int -> Exception.raw_backtrace = "caml_get_continuation_callstack"
end

module Shallow = struct
  type ('a, 'b) continuation

  external alloc_stack: ('a -> 'b) -> (exn -> 'b) -> ('c t -> ('c, 'b) continuation -> last_fiber -> 'b) -> ('a, 'b) stack = "caml_alloc_stack"

  external cont_last_fiber: ('a, 'b) continuation -> last_fiber = "%field1"

  let fiber: type a b. (a -> b) -> (a, b) continuation = fun fn ->
    let module M = struct
      type _ t +=
        | Initial_setup__ : a t
    end in
    let exception Initial of (a, b) continuation in
    let run () = fn (perform M.Initial_setup__) in
    let impossible () = System_error.panic "Effect.Shallow.fiber: impossible control flow" in
    let effc eff continuation _last_fiber =
      match eff with
      | M.Initial_setup__ -> raise (Initial continuation)
      | _ -> impossible ()
    in
    let stack =
      alloc_stack
        (
          fun _ -> impossible ()
        )
        (
          fun _ -> impossible ()
        )
        effc
    in
    match runstack stack run () with
    | exception Initial continuation -> continuation
    | _ -> impossible ()

  type ('a, 'b) handler = {
    retc: 'a -> 'b;
    exnc: exn -> 'b;
    effc: 'c. 'c t -> (('c, 'a) continuation -> 'b) option;
  }

  external update_handler: ('a, 'b) continuation -> ('b -> 'c) -> (exn -> 'c) -> ('d t -> ('d, 'b) continuation -> last_fiber -> 'c) -> ('a, 'c) stack = "caml_continuation_use_and_update_handler_noexc" [@@noalloc]

  external reperform: 'a t -> ('a, 'b) continuation -> last_fiber -> 'c = "%reperform"

  let continue_gen = fun continuation resume_fn value handler ->
    let effc eff next_continuation last_fiber =
      match handler.effc eff with
      | Some fn -> fn next_continuation
      | None -> reperform eff next_continuation last_fiber
    in
    let last_fiber = cont_last_fiber continuation in
    let stack = update_handler continuation handler.retc handler.exnc effc in resume stack resume_fn value last_fiber

  let continue_with = fun continuation value handler ->
    continue_gen continuation
      (
        fun x -> x
      )
      value
      handler

  let discontinue_with = fun continuation exn handler ->
    continue_gen continuation
      (
        fun err -> raise err
      )
      exn
      handler

  let discontinue_with_backtrace = fun continuation exn backtrace handler ->
    continue_gen continuation
      (
        fun err -> raise_with_backtrace err backtrace
      )
      exn
      handler

  external get_callstack: ('a, 'b) continuation -> int -> Exception.raw_backtrace = "caml_get_continuation_callstack"
end
