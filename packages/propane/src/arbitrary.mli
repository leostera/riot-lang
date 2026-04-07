open Std

(** Describe how to generate, shrink, and print values for a property.

    Use an arbitrary when Propane needs the full story for a type:
    how to make values, how to simplify a failing example, and how to show
    the result in an error report.

    Example:
    ```ocaml
    type point = { x: int; y: int }

    let point =
      Arbitrary.make
        ~shrink:(fun { x; y } ->
          List.map (fun x -> { x; y }) (Shrinker.int x)
          @ List.map (fun y -> { x; y }) (Shrinker.int y))
        ~print:(fun { x; y } -> Printf.sprintf "{ x = %d; y = %d }" x y)
        Generator.(map (fun (x, y) -> { x; y }) (pair int int))
    ```
*)

type 'value t = {
  (** Generator used to create candidate test values. *)
  gen: 'value Generator.t;
  (** Shrinker used to minimize a failing example. *)
  shrink: 'value Shrinker.t option;
  (** Printer used in counter-example output. *)
  print: 'value Printer.t option;
  (** Optional size metric used to compare candidate shrink results. *)
  small: ('value -> int) option;
}

(** Complete specification for a value type used in property testing. *)

(** Build an arbitrary from its components.

    Use [make] when the built-in arbitraries are close, but you need custom
    shrinking, printing, or size measurement for a domain type.
*)
val make:
  (** Shrinker used to minimize failing values. *)
  ?shrink:'value Shrinker.t ->
  (** Printer used in failure output. *)
  ?print:'value Printer.t ->
  (** Size metric used to compare smaller candidates. *)
  ?small:('value -> int) ->
  (** Generator used to produce test values. *)
  'value Generator.t ->
  'value t

(** Generate, shrink, and print integers. *)
val int: int t

(** Generate, shrink, and print [int32] values. *)
val int32: int32 t

(** Generate, shrink, and print [int64] values. *)
val int64: int64 t

(** Generate, shrink, and print booleans. Failing examples shrink toward [false]. *)
val bool: bool t

(** Generate, shrink, and print floating-point values. *)
val float: float t

(** Generate, shrink, and print characters. *)
val char: char t

(** Generate, shrink, and print Unicode runes. *)
val rune: Unicode.Rune.t t

(** Generate, shrink, and print strings. *)
val string: string t

(** Lift an arbitrary over element values into an arbitrary over lists.

    Use this for APIs that consume ordered sequences and where shrinking should
    try both dropping elements and simplifying individual entries.
*)
val list: 'value t -> 'value list t

(** Arbitrary for arrays built from an element arbitrary. *)
val array: 'value t -> 'value array t

(** Arbitrary for [Collections.Vector.t] values built from an element arbitrary. *)
val vector: 'value t -> 'value Collections.Vector.t t

(** Arbitrary for hash maps built from key and value arbitraries. *)
val hashmap: 'key t -> 'value t -> ('key, 'value) Collections.HashMap.t t

(** Arbitrary for hash sets built from an element arbitrary. *)
val hashset: 'value t -> 'value Collections.HashSet.t t

(** Arbitrary for queues built from an element arbitrary. *)
val queue: 'value t -> 'value Collections.Queue.t t

(** Arbitrary for deques built from an element arbitrary. *)
val deque: 'value t -> 'value Collections.Deque.t t

(** Arbitrary for heaps built from an element arbitrary. *)
val heap: 'value t -> 'value Collections.Heap.t t

(** Combine two arbitraries into one for pairs.

    Example:
    ```ocaml
    let coordinates = Arbitrary.pair Arbitrary.int Arbitrary.int
    ```
*)
val pair: 'a t -> 'b t -> ('a * 'b) t

(** Combine three arbitraries into one for triples. *)
val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** Arbitrary for optional values.

    Failing values may shrink from [Some x] to [None], or to a smaller
    [Some x'] when the element arbitrary supports shrinking.
*)
val option: 'value t -> 'value option t

(** Arbitrary for result values. *)
val result: 'value t -> 'error t -> ('value, 'error) result t

(** Transform an arbitrary through an isomorphism.

    Use [map to_ from arb] when you already have an arbitrary for a simpler
    representation and want to reuse it for a wrapper type.

    Example:
    ```ocaml
    type port = Port of int

    let port =
      Arbitrary.map
        (fun n -> Port n)
        (fun (Port n) -> n)
        Arbitrary.int
    ```
*)
val map: ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

(** Replace only the generator of an arbitrary.

    Use this when the shrinking and printing behavior is already right, but the
    value distribution should change.
*)
val map_gen: 'value Generator.t -> 'value t -> 'value t
