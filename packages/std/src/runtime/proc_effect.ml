open Kernel

(* Timeout type for blocking operations *)

type timeout =
  | Infinity
  | After of float

type 'msg selection =
  | Select of 'msg
  | Skip

type 'msg selector = Message.t -> 'msg selection

type _ Effect.t +=
  | Receive: {
      selector: 'msg selector;
      timeout: timeout;
    } -> 'msg Effect.t

type _ Effect.t +=
  | Yield: unit Effect.t

(* I/O Effects *)

type _ Effect.t +=
  | Syscall: {
      name: string;
      interest: Kernel.Async.Interest.t;
      source: Kernel.Async.Source.t;
      timeout: timeout;
    } -> unit Effect.t
