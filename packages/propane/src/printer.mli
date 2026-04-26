open Std

(**
   Render property-test values into readable counter-examples.

   Printers matter when a property fails. A good printer tells you what value
   broke the property without needing to instrument the test manually.

   Example:
   ```ocaml
   let user =
     Printer.pair Printer.string Printer.int

   user ("leo", 3) = "(\"leo\", 3)"
   ```
*)
type 'value t = 'value -> string

(** Function that renders a value into a diagnostic string. *)

(** Print integers. *)
val int: int t

(** Print [int32] values. *)
val int32: int32 t

(** Print [int64] values. *)
val int64: int64 t

(**
   Print floating-point values.

   Use [precision] when you want stable output in snapshots or counter-example
   reports.
*)
val float: ?precision:int -> float t

(** Print booleans as [true] or [false]. *)
val bool: bool t

(** Print characters using OCaml character syntax. *)
val char: char t

(** Print Unicode runes. *)
val rune: Unicode.Rune.t t

(** Print strings with escaping suitable for failure reports. *)
val string: string t

(** Print lists using the given element printer. *)
val list: 'value t -> 'value list t

(** Print arrays using the given element printer. *)
val array: 'value t -> 'value array t

(** Print vectors using the given element printer. *)
val vector: 'value t -> 'value Collections.Vector.t t

(** Print hash maps using the given key and value printers. *)
val hashmap: 'key t -> 'value t -> ('key, 'value) Collections.HashMap.t t

(** Print hash sets using the given element printer. *)
val hashset: 'value t -> 'value Collections.HashSet.t t

(** Print queues using the given element printer. *)
val queue: 'value t -> 'value Collections.Queue.t t

(** Print deques using the given element printer. *)
val deque: 'value t -> 'value Collections.Deque.t t

(** Print heaps using the given element printer. *)
val heap: 'value t -> 'value Collections.Heap.t t

(**
   Print pairs.

   Example:
   ```ocaml
   let show = Printer.pair Printer.int Printer.string in
   show (1, "ok") = "(1, \"ok\")"
   ```
*)
val pair: 'a t -> 'b t -> ('a * 'b) t

(** Print triples. *)
val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** Print optional values as [None] or [Some ...]. *)
val option: 'value t -> 'value option t

(** Print result values as [Ok ...] or [Error ...]. *)
val result: 'value t -> 'error t -> ('value, 'error) result t
