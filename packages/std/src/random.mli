(**
   # Random

   Pseudo-random sampling with a secure standard RNG and composable
   distributions.

   `Std.Random` exposes one default global RNG through `Rng.standard`, plus a
   distribution API for composing larger random values.

   The default standard RNG is intended to be cryptographically secure. When
   `seed` is omitted, it is initialized from `Kernel.Random.Source`.
*)
type error =
  | Entropy of Kernel.Random.Source.error
  | InvalidIntBound of { bound: int }
  | InvalidIntRange of { min: int; max: int }
  | InvalidInt32Bound of { bound: int32 }
  | InvalidInt32Range of { min: int32; max: int32 }
  | InvalidInt64Bound of { bound: int64 }
  | InvalidInt64Range of { min: int64; max: int64 }
  | InvalidFloatRange of { min: float; max: float }
  | InvalidProbability of { probability: float }
  | EmptyPopulation
  | InvalidSampleSize of { requested: int; available: int }

val error_to_string: error -> string

module Rng : sig
  type t

  (**
     Use `make ~state ~fill_bytes` to package a custom RNG implementation for
     use with `Std.Random`.

     `fill_bytes state out` must overwrite `out` fully and may mutate `state`
     internally.
  *)
  val make: state:'state -> fill_bytes:('state -> bytes -> unit) -> t

  (**
     Use `standard ?seed ()` to build the secure standard RNG.

     - `standard ~seed ()` is deterministic
     - `standard ()` is seeded from `Kernel.Random.Source`
  *)
  val standard: ?seed:string -> unit -> (t, error) Result.t
end

type 'value distribution

(**
   Use `init ?seed ()` to replace the default global RNG used when `~rng` is
   omitted from samplers.
*)
val init: ?seed:string -> unit -> (unit, error) Result.t

(** Use `sample ?rng distribution` to draw one value from `distribution`. *)
val sample: ?rng:Rng.t -> 'value distribution -> ('value, error) Result.t

module Distribution : sig
  type 'value t = 'value distribution

  val sample: ?rng:Rng.t -> 'value t -> ('value, error) Result.t

  val map: ('a -> 'b) -> 'a t -> 'b t

  val map2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val tuple: 'a t -> 'b t -> ('a * 'b) t

  val option: 'a t -> 'a option t

  val list: len:int -> 'a t -> 'a list t

  val repeated: count:int -> 'a t -> 'a list t

  val bool: bool t

  val char: char t

  val standard_int: int t

  val standard_int32: int32 t

  val standard_int64: int64 t

  val standard_float: float t

  val bits: int t

  val bits32: int32 t

  val bits64: int64 t

  val int: int -> int t

  val int_range: min:int -> max:int -> int t

  val int32: int32 -> int32 t

  val int32_range: min:int32 -> max:int32 -> int32 t

  val int64: int64 -> int64 t

  val int64_range: min:int64 -> max:int64 -> int64 t

  val float: float -> float t

  val float_range: min:float -> max:float -> float t

  val bernoulli: p:float -> bool t

  val one_of: 'a list -> 'a t

  val one_of_array: 'a array -> 'a t

  val one_of_vec: 'a Collections.Vector.t -> 'a t

  val choose_n: 'a list -> int -> 'a list t

  val choose_n_array: 'a array -> int -> 'a array t

  val choose_n_vec: 'a Collections.Vector.t -> int -> 'a Collections.Vector.t t
end

val bits: ?rng:Rng.t -> unit -> (int, error) Result.t

val bits32: ?rng:Rng.t -> unit -> (int32, error) Result.t

val bits64: ?rng:Rng.t -> unit -> (int64, error) Result.t

val bool: ?rng:Rng.t -> unit -> (bool, error) Result.t

val char: ?rng:Rng.t -> unit -> (char, error) Result.t

val int: ?rng:Rng.t -> int -> (int, error) Result.t

val int_range: ?rng:Rng.t -> min:int -> max:int -> unit -> (int, error) Result.t

val int32: ?rng:Rng.t -> int32 -> (int32, error) Result.t

val int32_range: ?rng:Rng.t -> min:int32 -> max:int32 -> unit -> (int32, error) Result.t

val int64: ?rng:Rng.t -> int64 -> (int64, error) Result.t

val int64_range: ?rng:Rng.t -> min:int64 -> max:int64 -> unit -> (int64, error) Result.t

val float: ?rng:Rng.t -> float -> (float, error) Result.t

val float_range: ?rng:Rng.t -> min:float -> max:float -> unit -> (float, error) Result.t

val one_of: ?rng:Rng.t -> 'a list -> ('a, error) Result.t

val one_of_array: ?rng:Rng.t -> 'a array -> ('a, error) Result.t

val one_of_vec: ?rng:Rng.t -> 'a Collections.Vector.t -> ('a, error) Result.t

val choose_n: ?rng:Rng.t -> 'a list -> int -> ('a list, error) Result.t

val choose_n_array: ?rng:Rng.t -> 'a array -> int -> ('a array, error) Result.t

val choose_n_vec: ?rng:Rng.t -> 'a Collections.Vector.t -> int -> ('a Collections.Vector.t, error) Result.t
