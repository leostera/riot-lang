(** FIXME: turn sync.ml into ./std/src/sync/* so each submodule can be a separate file *)

(** Synchronization primitives owned by `std`.

    These wrappers sit above the compiler/runtime-provided low-level mutable and
    concurrency primitives. They are runtime substrate, not kernel surface. *)
open Kernel

module Atomic: sig
  type 'value t = 'value Kernel.Sync.Atomic.t
  val make: 'value -> 'value t

  val get: 'value t -> 'value

  val set: 'value t -> 'value -> unit

  val exchange: 'value t -> 'value -> 'value

  val compare_and_set: 'value t -> 'value -> 'value -> bool

  val fetch_and_add: int t -> int -> int
end

module Cell: sig
  type 'a t
  val create: 'a -> 'a t

  val get: 'a t -> 'a

  val ( ! ): 'a t -> 'a

  val set: 'a t -> 'a -> unit

  val ( := ): 'a t -> 'a -> unit

  val update: 'a t -> ('a -> 'a) -> unit

  val incr: int t -> unit

  val decr: int t -> unit

  val replace: 'a t -> 'a -> 'a

  val take: 'a t -> default:'a -> 'a

  val swap: 'a t -> 'a t -> unit

  val compare_and_swap: 'a t -> 'a -> 'a -> bool

  val equal: 'a t -> 'a t -> bool
end

module Mutex: sig
  type t = Kernel.Sync.Mutex.t
  val create: unit -> t

  val lock: t -> unit

  val unlock: t -> unit

  val try_lock: t -> bool
end

module Condition: sig
  type t = Kernel.Sync.Condition.t
  val create: unit -> t

  val wait: t -> Mutex.t -> unit

  val signal: t -> unit

  val broadcast: t -> unit
end

module OnceCell: sig
  type 'a t
  val create: unit -> 'a t

  val get: 'a t -> 'a option

  val take: 'a t -> 'a option

  val get_or_init: 'a t -> (unit -> 'a) -> 'a

  val get_or_try_init: 'a t -> (unit -> ('a, 'e) result) -> ('a, 'e) result

  val set: 'a t -> 'a -> (unit, [
      `AlreadyInitialized
    ]) result

  val is_initialized: 'a t -> bool
end

module LazyCell: sig
  type 'a t
  val create: (unit -> 'a) -> 'a t

  val get: 'a t -> 'a

  val force: 'a t -> 'a

  val is_initialized: 'a t -> bool

  val take: 'a t -> 'a option
end

module RefCell: sig
  type 'a t
  type 'a borrow
  type 'a borrow_mut
  exception BorrowError of string

  exception BorrowMutError of string

  val create: 'a -> 'a t

  val borrow: 'a t -> 'a borrow

  val release_borrow: 'a borrow -> unit

  val borrow_mut: 'a t -> 'a borrow_mut

  val get_mut: 'a borrow_mut -> 'a

  val set_mut: 'a borrow_mut -> 'a -> unit

  val release_borrow_mut: 'a borrow_mut -> unit

  val with_borrow: 'a t -> ('a -> 'b) -> 'b

  val with_borrow_mut: 'a t -> ((unit -> 'a) -> ('a -> unit) -> 'b) -> 'b

  val try_borrow: 'a t -> ('a borrow, string) result

  val try_borrow_mut: 'a t -> ('a borrow_mut, string) result

  val get_unchecked: 'a t -> 'a

  val set_unchecked: 'a t -> 'a -> unit

  val is_borrowed: 'a t -> bool

  val borrow_count: 'a t -> int
end
