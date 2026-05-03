(**
   Optional values.

   Type `Option` represents an optional value: every `Option` is either
   [`Some`] and contains a value, or [`None`], and does not. `Option` types are
   very common in OCaml code, as they have a number of uses:

   - Initial values
   - Return values for functions that are not defined over their entire input
     range (partial functions)
   - Return value for otherwise reporting simple errors, where [`None`] is
     returned on error
   - Optional fields
   - Optional function arguments
   - Nullable pointers (using [`Option`] is safer than using null pointers)

   ## Examples

   Basic usage:

   ```ocaml open Std

   let divide x y = if y = 0 then None else Some (x / y)

   (* Pattern matching *) match divide 10 2 with | Some result -> Printf.printf
   "Result: %d\n" result | None -> Printf.printf "Cannot divide by zero\n"

   (* Using combinators *) let result = divide 10 2 |> Option.and_then (fun x
   -> divide x 2) |> Option.map (fun x -> x * 10) |> Option.unwrap_or
   ~default:0 ```

   ## Method Overview

   `Option` provides a wide variety of methods for functional composition:

   - **Querying**: [`is_some`], [`is_none`], [`is_some_and`]
   - **Transforming**: [`map`], [`map_or`], [`map_or_else`]
   - **Extracting**: [`unwrap`], [`unwrap_or`], [`expect`], [`unwrap_or_else`]
   - **Chaining**: [`and_then`], [`or_else`], [`and_`], [`or_`], [`xor`]
   - **Converting**: [`ok_or`], [`ok_or_else`], [`to_list`], [`transpose`]

   ## The Question Mark Operator

   OCaml doesn't have Rust's `?` operator for [`Option`], but you can achieve
   similar early returns with [`and_then`] for chaining operations that might
   fail.

   ## Representation

   OCaml's `option` is a regular variant type with two constructors. It has no
   runtime overhead compared to manually checking for null or special values.
*)
type 'a t = 'a option =
  | None
  | Some of 'a

(** The Option type - either [`Some`] value or [`None`] *)

(**
   Creates a [`Some`] value.

   ## Examples

   ```ocaml let x = Option.some 5 in assert (x = Some 5) ```
*)
val some: 'a -> 'a t

(**
   The [`None`] value.

   ## Examples

   ```ocaml let x : int option = Option.none in assert (x = None) ```
*)
val none: 'a t

(**
   Returns `true` if two options are equal using the provided equality function.

   ## Examples

   ```ocaml
   let eq = Option.equal Int.equal in
   assert (eq (Some 5) (Some 5));
   assert (not (eq (Some 5) (Some 10)));
   assert (not (eq (Some 5) None));
   assert (eq None None)
   ```
*)
val equal: 'a t -> 'a t -> fn:('a -> 'a -> bool) -> bool

(**
   Returns `true` if the option is a [`Some`] value.

   ## Examples

   ```ocaml let x = Some 2 in assert (Option.is_some x);

   let y = None in assert (not (Option.is_some y)) ```
*)
val is_some: 'a t -> bool

(**
   Returns `true` if the option is a [`None`] value.

   ## Examples

   ```ocaml let x = Some 2 in assert (not (Option.is_none x));

   let y = None in assert (Option.is_none y) ```
*)
val is_none: 'a t -> bool

(**
   Returns `true` if the option is [`Some`] and the value matches the
   predicate.

   ## Examples

   ```ocaml let x = Some 2 in assert (Option.is_some_and (fun x -> x > 1) x);
   assert (not (Option.is_some_and (fun x -> x > 5) x));

   let y = None in assert (not (Option.is_some_and (fun x -> x > 1) y)) ```
*)
val is_some_and: 'a t -> fn:('a -> bool) -> bool

(**
   Returns `true` if the option is [`None`] or the value matches the predicate.

   ## Examples

   ```ocaml let is_small = Option.is_none_or (fun x -> x < 10) in

   assert (is_small None); assert (is_small (Some 5)); assert (not (is_small
   (Some 20))) ```
*)
val is_none_or: 'a t -> fn:('a -> bool) -> bool

(**
   Maps an `Option<'a>` to `Option<'b>` by applying a function to the contained
   value.

   ## Examples

   ```ocaml let maybe_string = Some "Hello, World!" in let maybe_len =
   Option.map String.length maybe_string in assert (maybe_len = Some 13);

   let none_string : string option = None in let none_len = Option.map
   String.length none_string in assert (none_len = None) ```
*)
val map: 'a t -> fn:('a -> 'b) -> 'b t

(**
   Returns the result of applying function to [`Some`] value, or default if
   [`None`].

   Arguments are passed in order of increasing likelihood of being evaluated.

   ## Examples

   ```ocaml let x = Some "foo" in assert (Option.map_or ~default:42
   String.length x = 3);

   let y = None in assert (Option.map_or ~default:42 String.length y = 42) ```
*)

(**
   Returns result of applying function to [`Some`] value, or computes default
   if [`None`].

   Alias for [`map_or_else`].

   ## Examples

   ```ocaml let k = 10 in

   let x = Some 4 in assert (Option.map_or_default ~default:(fun () -> 2 * k)
   (fun v -> v * v) x = 16);

   let y = None in assert (Option.map_or_default ~default:(fun () -> 2 * k)
   (fun v -> v * v) y = 20) ```
*)
val map_or: 'a t -> default:'b -> fn:('a -> 'b) -> 'b

(**
   Maps an `Option<'a>` to `'b` by applying function to [`Some`], or computing
   default.

   ## Examples

   ```ocaml let k = 21 in

   let x = Some "foo" in assert (Option.map_or_else ~default:(fun () -> 2 * k)
   String.length x = 3);

   let y = None in assert (Option.map_or_else ~default:(fun () -> 2 * k)
   String.length y = 42) ```
*)
val map_or_default: 'a t -> default:(unit -> 'b) -> fn:('a -> 'b) -> 'b

val map_or_else: 'a t -> default:(unit -> 'b) -> fn:('a -> 'b) -> 'b

(**
   Returns [`None`] if the first option is [`None`], otherwise returns the
   second option.

   ## Examples

   ```ocaml let x = Some 2 in let y = None in assert (Option.and_ x y = None);

   let x = None in let y = Some "foo" in assert (Option.and_ x y = None);

   let x = Some 2 in let y = Some "foo" in assert (Option.and_ x y = Some
   "foo");

   let x = None in let y : string option = None in assert (Option.and_ x y =
   None) ```
*)
val and_: 'a t -> 'b t -> 'b t

(**
   Returns [`None`] if the option is [`None`], otherwise calls function with
   the wrapped value and returns the result.

   Some call this operation "flatmap" or "bind".

   ## Examples

   ```ocaml let sq_if_positive x = if x > 0 then Some (x * x) else None in

   assert (Option.and_then (Some 2) sq_if_positive = Some 4); assert
   (Option.and_then (Some 0) sq_if_positive = None); assert (Option.and_then
   None sq_if_positive = None)

   (* Chaining fallible operations *) let result = Sys.getenv_opt "CONFIG_PATH"
   |> Option.and_then (fun path -> try Some (Fs.read (Path.v path) |>
   Result.unwrap) with _ -> None) |> Option.and_then parse_config ```
*)
val and_then: 'a t -> fn:('a -> 'b t) -> 'b t

(**
   Returns the first option if [`Some`], otherwise returns the second option.

   ## Examples

   ```ocaml let x = Some 2 in let y = None in assert (Option.or_ x y = Some 2);

   let x = None in let y = Some 100 in assert (Option.or_ x y = Some 100);

   let x = Some 2 in let y = Some 100 in assert (Option.or_ x y = Some 2) ```
*)
val or_: 'a t -> 'a t -> 'a t

(**
   Returns the option if [`Some`], otherwise calls function and returns its
   result.

   ## Examples

   ```ocaml let nobody = fun () -> None in let vikings = fun () -> Some
   "vikings" in

   assert (Option.or_else (Some "barbarians") vikings = Some "barbarians");
   assert (Option.or_else None vikings = Some "vikings"); assert
   (Option.or_else None nobody = None) ```
*)
val or_else: 'a t -> fn:(unit -> 'a t) -> 'a t

(**
   Returns [`Some`] if exactly one of the options is [`Some`], otherwise
   [`None`].

   ## Examples

   ```ocaml let x = Some 2 in let y = None in assert (Option.xor x y = Some 2);

   let x = None in let y = Some 3 in assert (Option.xor x y = Some 3);

   let x = Some 4 in let y = Some 5 in assert (Option.xor x y = None);

   let x = None in let y = None in assert (Option.xor x y = None) ```
*)
val xor: 'a t -> 'a t -> 'a t

(**
   Returns the contained [`Some`] value, consuming the option.

   ## Panics

   Panics if the value is [`None`].

   ## Examples

   ```ocaml let x = Some "air" in assert (Option.unwrap x = "air")

   (* This will panic: *) let y = None in Option.unwrap y (* panic: "Called
   Option.unwrap on None" *) ```

   ## Note

   Generally, prefer [`expect`] which provides a more helpful panic message, or
   [`unwrap_or`] / [`unwrap_or_else`] for non-panicking alternatives.
*)

(**
   Returns the contained [`Some`] value or a provided default.

   Arguments are evaluated eagerly; if you are passing the result of a function
   call, consider [`unwrap_or_else`], which is lazily evaluated.

   ## Examples

   ```ocaml assert (Option.unwrap_or ~default:"bike" (Some "car") = "car");
   assert (Option.unwrap_or ~default:"bike" None = "bike") ```
*)
val unwrap: 'a t -> 'a

(**
   Returns the contained [`Some`] value or computes it from a closure.

   ## Examples

   ```ocaml let k = 10 in assert (Option.unwrap_or_else ~fn:(fun () -> 2 * k)
   (Some 4) = 4); assert (Option.unwrap_or_else ~fn:(fun () -> 2 * k) None =
   20) ```
*)
val unwrap_or: 'a t -> default:'a -> 'a

(**
   Returns the contained [`Some`] value, consuming the option.

   ## Panics

   Panics if the value is [`None`] with a custom panic message provided by
   `msg`.

   ## Examples

   ```ocaml let x = Some "value" in assert (Option.expect ~msg:"fruits are
   healthy" x = "value")

   (* This will panic with custom message: *) let y = None in Option.expect
   ~msg:"fruits are healthy" y (* panic: "fruits are healthy" *) ```

   ## Recommended Message Style

   We recommend that `expect` messages describe the reason you expect the
   `Option` to be `Some`:

   ```ocaml let config = Sys.getenv_opt "CONFIG_FILE" |> Option.expect
   ~msg:"env variable CONFIG_FILE should be set by wrapper script" ```
*)
val unwrap_or_else: 'a t -> fn:(unit -> 'a) -> 'a

(**
   Consumes the option, panicking if it is [`Some`].

   ## Panics

   Panics if the value is a [`Some`], with a panic message including the value.

   ## Examples

   ```ocaml Option.unwrap_none None; (* Does nothing *)

   (* This will panic: *) Option.unwrap_none (Some "value") (* panic: "Called
   Option.unwrap_none on Some: value" *) ```
*)
val expect: msg:string -> 'a t -> 'a

val unwrap_none: 'a t -> unit

(** Calls function on Some value if present, returns unchanged option *)
val inspect: 'a t -> fn:('a -> unit) -> 'a t

(** Calls function on Some value if present *)
val for_each: 'a t -> fn:('a -> unit) -> unit

(** Convert Some to Ok, None to Error *)
val ok_or: error:'e -> 'a t -> ('a, 'e) Result.t

(** Convert Some to Ok, None to Error computed from function *)
val ok_or_else: error:(unit -> 'e) -> 'a t -> ('a, 'e) Result.t

(** Alias for ok_or *)
val to_result: error:'e -> 'a t -> ('a, 'e) Result.t

(** Convert Some to single-element list, None to empty list *)
val to_list: 'a t -> 'a list

(** Transpose Option of Result to Result of Option *)
val transpose: ('a, 'e) Result.t t -> ('a t, 'e) Result.t

(** Returns Some if the value matches predicate, otherwise None *)
val filter: 'a t -> fn:('a -> bool) -> 'a t

(** Flatten nested Options *)
val flatten: 'a t t -> 'a t

(** Combine two options into an option of a tuple *)
val zip: 'a t -> 'b t -> ('a * 'b) t

(** Combine two options with a function *)
val zip_with: 'a t -> 'b t -> fn:('a -> 'b -> 'c) -> 'c t

(** Unzip an option of a tuple into a tuple of options *)
val unzip: ('a * 'b) t -> 'a t * 'b t

(** Convert list of Options to Option of list, returns None if any is None *)
val all: 'a t list -> 'a list t
