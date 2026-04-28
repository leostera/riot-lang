open Kernel

(**
   Timeout values for receive and syscall effects. [After seconds] uses
   seconds.
*)
type timeout =
  | Infinity
  | After of float
(** Selection outcome for mailbox receive selectors. *)
type 'msg selection =
  | Select of 'msg
  | Skip
(** Mailbox selector function. *)
type 'msg selector = Message.t -> 'msg selection
(**
   Receive the next message selected by [selector], or abort when the
   timeout expires.
*)
type _ Effect.t +=
  | Receive: {
      selector: 'msg selector;
      timeout: timeout;
    } -> 'msg Effect.t
(** Cooperatively yield control back to the scheduler. *)
type _ Effect.t +=
  | Yield: unit Effect.t
(**
   Wait for an async source to become ready for the requested interest, or
   abort when the timeout expires.
*)
type _ Effect.t +=
  | Syscall: {
      name: string;
      interest: Kernel.Async.Interest.t;
      source: Kernel.Async.Source.t;
      timeout: timeout;
    } -> unit Effect.t
