open Kernel

exception Unwind

type ('a, 'b) continuation
type error_info = {
  exn : exn;
  backtrace : Kernel.Exception.raw_backtrace;
}
type 'a t =
  | Finished of ('a, error_info) result
  | Suspended : ('a, 'b) continuation * 'a Effect.t -> 'b t
  | Unhandled : ('a, 'b) continuation * 'a -> 'b t
type 'a step =
  | Continue of 'a
  | Discontinue of exn
  | Reperform : 'a Effect.t -> 'a step
  | Delay : 'a step
  | Suspend : 'a step
  | Yield : unit step
  | Terminate : 'a step
type ('a, 'b) step_callback = ('a step -> 'b t) -> 'a Effect.t -> 'b t
type perform = {
  perform : 'a 'b. ('a, 'b) step_callback;
} [@@unboxed]
val make : ('a -> 'b) -> 'a Effect.t -> 'b t

val run : consume_reduction:(unit -> bool) -> perform:perform -> 'a t -> 'a t option

val is_finished : 'a t -> bool

val unwind : id:string -> 'a t -> unit
