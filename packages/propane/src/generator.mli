open Std

(** Generator module for creating random values.
    
    Inspired by PropEr's proper_gen.erl, this module provides combinators
    for building random value generators.
    
    A generator is a function that takes a random state and produces a value.
    Generators can be combined using various combinators to create complex
    generators from simple ones.
    
    {1 Quick Examples}
    
    {[
      (* Simple generators *)
      Generator.int                        (* Random ints *)
      Generator.string                     (* Random strings *)
      Generator.bool                       (* Random booleans *)
      
      (* Bounded generators *)
      Generator.int_range 1 10             (* Ints from 1 to 10 *)
      Generator.char_range 'a' 'z'         (* Lowercase letters *)
      
      (* Collection generators *)
      Generator.list Generator.int         (* Lists of ints *)
      Generator.pair gen1 gen2             (* Pairs *)
      Generator.option Generator.string    (* Optional strings *)
      
      (* Combinators *)
      Generator.map (fun x -> x * 2) int   (* Transform values *)
      Generator.one_of [gen1; gen2; gen3]  (* Pick randomly *)
      Generator.frequency [                (* Weighted choice *)
        (9, small_gen);                    (* 90% probability *)
        (1, large_gen);                    (* 10% probability *)
      ]
    ]}
    
    {1 Building Custom Generators}
    
    {[
      (* Generate points in 2D space *)
      type point = { x: int; y: int }
      
      let point_gen =
        Generator.map
          (fun (x, y) -> { x; y })
          (Generator.pair (Generator.int_range 0 100)
                          (Generator.int_range 0 100))
      
      (* Generate email addresses *)
      let email_gen =
        Generator.map
          (fun (name, domain) -> name ^ "@" ^ domain ^ ".com")
          (Generator.pair Generator.string_lowercase
                          Generator.string_lowercase)
      
      (* Generate trees recursively *)
      let tree_gen =
        Generator.fix (fun self depth ->
          if depth = 0 then
            Generator.map (fun v -> Leaf v) Generator.int
          else
            Generator.one_of [
              Generator.map (fun v -> Leaf v) Generator.int;
              Generator.map2 (fun l r -> Node (l, r))
                (self (depth - 1)) (self (depth - 1));
            ]
        ) 5
    ]}
*)
(** {1 Core Types} *)

type 'value t
(** A generator that produces random values of type ['value]. *)
(** {1 Constants} *)

val return: 'value -> 'value t

(** [return v] creates a generator that always returns [v]. *)
val exactly: 'value -> 'value t

(** Alias for {!return}. *)
(** {1 Transformations} *)

val map: ('a -> 'b) -> 'a t -> 'b t

(** [map f gen] transforms the values produced by [gen] using [f]. *)
val map2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

(** [map2 f gen1 gen2] combines two generators. *)
val map3: ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

(** [map3 f gen1 gen2 gen3] combines three generators. *)
val and_then: 'a t -> ('a -> 'b t) -> 'b t

(** [and_then gen f] creates a dependent generator. First generates a value
    using [gen], then uses that value to select the next generator via [f]. *)
(** {1 Choice Combinators} *)

val one_of: 'value t list -> 'value t

(** [one_of gens] randomly selects one of the generators from [gens] with
    equal probability.
    
    {[
      (* Generate either a small or large number *)
      Generator.one_of [
        Generator.int_range 1 10;
        Generator.int_range 100 1000;
      ]
      
      (* Generate different shapes *)
      Generator.one_of [
        Generator.return Circle;
        Generator.return Square;
        Generator.return Triangle;
      ]
    ]}
    
    @raise Invalid_argument if the list is empty. *)
val frequency: (int * 'value t) list -> 'value t

(** [frequency weighted_gens] randomly selects a generator based on weights.
    Each element is [(weight, gen)] where weight is a positive integer.
    
    Useful for generating realistic distributions:
    
    {[
      (* Generate mostly small numbers, occasionally large *)
      Generator.frequency [
        (9, Generator.int_range 0 10);      (* 90% probability *)
        (1, Generator.int_range 100 1000);  (* 10% probability *)
      ]
      
      (* Generate mostly successful results *)
      Generator.frequency [
        (95, Generator.map (fun x -> Ok x) value_gen);
        (5, Generator.map (fun e -> Error e) error_gen);
      ]
    ]}
    
    @raise Invalid_argument if the list is empty or contains non-positive weights. *)
(** {1 Size Control} *)

val sized: (int -> 'value t) -> 'value t

(** [sized f] creates a generator that receives the current size parameter.
    The size typically controls the complexity of generated values. *)
val resize: int -> 'value t -> 'value t

(** [resize n gen] runs [gen] with size parameter set to [n]. *)
(** {1 Recursive Generators} *)

val delay: (unit -> 'value t) -> 'value t

(** [delay f] defers the construction of a generator.
    Useful for creating recursive generators. *)
val fix: ((int -> 'value t) -> (int -> 'value t)) -> int -> 'value t

(** [fix f] creates a recursive size-bounded generator.
    The function [f] receives itself as an argument, allowing recursion.
    The integer parameter is the size bound. *)
(** {1 Primitive Generators} *)

(** {2 Integers} *)

val int: int t

(** Generates random integers uniformly distributed. *)
val int32: int32 t

(** Generates random int32 values uniformly distributed. *)
val int64: int64 t

(** Generates random int64 values uniformly distributed. *)
val int_range: int -> int -> int t

(** [int_range low high] generates integers in the range [low] to [high] inclusive.
    @raise Invalid_argument if [low > high]. *)
val int32_range: int32 -> int32 -> int32 t

(** [int32_range low high] generates int32 values in the range [low] to [high] inclusive. *)
val int64_range: int64 -> int64 -> int64 t

(** [int64_range low high] generates int64 values in the range [low] to [high] inclusive. *)
val int_bound: int -> int t

(** [int_bound n] generates integers from 0 to [n] inclusive.
    @raise Invalid_argument if [n < 0]. *)
val small_int: int t

(** Generates small integers (typically 0-100). *)
val big_int: int t

(** Generates larger integers. *)
val positive_int: int t

(** Generates positive integers (>= 0). *)
val negative_int: int t

(** Generates negative integers (<= 0). *)
val non_zero_int: int t

(** Generates non-zero integers. *)
(** {2 Floats} *)

val float: float t

(** Generates random floats. *)
val float_range: float -> float -> float t

(** [float_range low high] generates floats in [low, high]. *)
val float_positive: float t

(** Generates positive floats. *)
val float_negative: float t

(** Generates negative floats. *)
(** {2 Booleans} *)

val bool: bool t

(** Generates random booleans with equal probability. *)
val weighted_bool: int -> int -> bool t

(** [weighted_bool weight_true weight_false] generates booleans with
    the given weight distribution. *)
(** {2 Characters} *)

val char: char t

(** Generates random characters. *)
val char_range: char -> char -> char t

(** [char_range low high] generates chars in [low, high] inclusive. *)
val char_lowercase: char t

(** Generates lowercase letters a-z. *)
val char_uppercase: char t

(** Generates uppercase letters A-Z. *)
val char_digit: char t

(** Generates digit characters 0-9. *)
val char_printable: char t

(** Generates printable ASCII characters. *)
val char_whitespace: char t

(** Generates whitespace characters. *)
(** {2 Runes - Unicode support} *)

val rune: Unicode.Rune.t t

(** Generates random Unicode runes. *)
val rune_range: Unicode.Rune.t -> Unicode.Rune.t -> Unicode.Rune.t t

(** [rune_range low high] generates runes in the given range. *)
val rune_printable: Unicode.Rune.t t

(** Generates printable Unicode runes. *)
(** {2 Strings} *)

val string: string t

(** Generates random strings. *)
val string_of: char t -> string t

(** [string_of char_gen] generates strings using [char_gen] for characters. *)
val string_size: int t -> char t -> string t

(** [string_size size_gen char_gen] generates strings with length from [size_gen]. *)
val string_printable: string t

(** Generates printable strings. *)
val string_lowercase: string t

(** Generates lowercase strings. *)
val string_uppercase: string t

(** Generates uppercase strings. *)
(** {1 Collection Generators} *)

val list: 'value t -> 'value list t

(** [list gen] generates lists of values from [gen]. *)
val list_size: int t -> 'value t -> 'value list t

(** [list_size size_gen gen] generates lists with length from [size_gen]. *)
val list_repeat: int -> 'value t -> 'value list t

(** [list_repeat n gen] generates lists of exactly [n] elements. *)
val non_empty_list: 'value t -> 'value list t

(** [non_empty_list gen] generates non-empty lists. *)
val array: 'value t -> 'value array t

(** [array gen] generates arrays. *)
val array_size: int t -> 'value t -> 'value array t

(** [array_size size_gen gen] generates arrays with length from [size_gen]. *)
(** {2 Std Collections} *)

val vector: 'value t -> 'value Collections.Vector.t t

(** Generate Vectors. *)
val vector_size: int t -> 'value t -> 'value Collections.Vector.t t

val hashmap: 'key t -> 'value t -> ('key, 'value) Collections.HashMap.t t

(** Generate HashMaps. *)
val hashmap_size: int t -> 'key t -> 'value t -> ('key, 'value) Collections.HashMap.t t

val hashset: 'value t -> 'value Collections.HashSet.t t

(** Generate HashSets. *)
val hashset_size: int t -> 'value t -> 'value Collections.HashSet.t t

val queue: 'value t -> 'value Collections.Queue.t t

(** Generate Queues. *)
val queue_size: int t -> 'value t -> 'value Collections.Queue.t t

val deque: 'value t -> 'value Collections.Deque.t t

(** Generate Deques. *)
val deque_size: int t -> 'value t -> 'value Collections.Deque.t t

val heap: 'value t -> 'value Collections.Heap.t t

(** Generate Heaps. *)
val heap_size: int t -> 'value t -> 'value Collections.Heap.t t

(** {1 Tuple Generators} *)
val pair: 'a t -> 'b t -> ('a * 'b) t

(** Generate pairs. *)
val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** Generate triples. *)
val quad: 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

(** Generate quadruples. *)
(** {1 Option & Result Generators} *)

val option: 'value t -> 'value option t

(** Generate optional values. *)
val weighted_option: int -> int -> 'value t -> 'value option t

(** [weighted_option weight_some weight_none gen] generates options with weights. *)
val result: 'value t -> 'error t -> ('value, 'error) result t

(** Generate result values. *)
val weighted_result: int -> int -> 'value t -> 'error t -> ('value, 'error) result t

(** [weighted_result weight_ok weight_error ok_gen error_gen] generates results with weights. *)
(** {1 Low-level Interface} *)

val generate: Random.State.t -> 'value t -> 'value

(** [generate rnd gen] runs the generator with the given random state.
    This is the low-level interface - most users should use the Property module. *)
val generate_with_size: Random.State.t -> int -> 'value t -> 'value

(** [generate_with_size rnd size gen] runs a sized generator. *)
