(**
   Error handling with the Result type.

   `Result<T, E>` is the type used for returning and propagating errors. It is
   an enum with the variants [`Ok(T)`], representing success and containing a
   value, and [`Error(E)`], representing error and containing an error value.

   ## Examples

   Basic usage:

   ```ocaml open Std

   (* A function that might fail *) let divide x y = if y = 0 then Error
   "Division by zero" else Ok (x / y)

   (* Pattern matching on results *) match divide 10 2 with | Ok result ->
   println "Result: %d" result | Error msg -> println "Error: %s" msg

   (* Chaining operations *) let result = divide 10 2 |> Result.and_then (fun x
   -> divide x 2) |> Result.map (fun x -> x * 10) ```

   ## Method Overview

   In addition to working with pattern matching, `Result` provides a wide
   variety of methods for functional composition:

   - **Querying**: [`is_ok`], [`is_err`], [`is_ok_and`], [`is_err_and`]
   - **Transforming**: [`map`], [`map_err`], [`map_or`], [`map_or_else`]
   - **Extracting**: [`unwrap`], [`unwrap_or`], [`expect`], [`unwrap_or_else`]
   - **Chaining**: [`and_then`], [`or_else`], [`or_`]
   - **Converting**: [`ok_value`], [`err_value`], [`to_option`], [`transpose`]

   ## The Question Mark Operator

   OCaml doesn't have Rust's `?` operator, but you can achieve similar early
   returns with [`and_then`] or monadic let operators if available.

   ## Working with Results

   ```ocaml (* Provide a default value *) let port = Env.get "PORT" |>
   Result.and_then Int.of_string |> Result.unwrap_or ~default:8080

   (* Transform errors *) let config = Fs.read (Path.v "config.json") |>
   Result.map_err (fun _ -> "Config file not found") |> Result.and_then
   parse_json |> Result.expect ~msg:"Failed to load configuration" ```
*)
type ('a, 'e) t = ('a, 'e) Kernel.result =
  | Ok of 'a
  | Error of 'e

(**
   The Result type - either [`Ok`] with a success value or [`Error`] with
   an error value
*)

(**
   Creates an [`Ok`] value.

   ## Examples

   ```ocaml let success = Result.ok 42 (* success = Ok 42 *) ```
*)
val ok: 'a -> ('a, 'e) t

(**
   Creates an [`Error`] value.

   ## Examples

   ```ocaml let failure = Result.err "Something went wrong" (* failure = Error
   "Something went wrong" *) ```
*)
val err: 'e -> ('a, 'e) t

(**
   Returns `true` if the result is [`Ok`].

   ## Examples

   ```ocaml let x = Ok 2 in assert (Result.is_ok x);

   let y = Error "Error message" in assert (not (Result.is_ok y)) ```
*)
val is_ok: ('a, 'e) t -> bool

(**
   Returns `true` if the result is [`Error`] (alias for [`is_err`]).

   ## Examples

   ```ocaml if Result.is_error response then Log.error "Request failed" ```
*)
val is_error: ('a, 'e) t -> bool

(**
   Returns `true` if the result is [`Error`].

   ## Examples

   ```ocaml let x = Ok 2 in assert (not (Result.is_err x));

   let y = Error "Error message" in assert (Result.is_err y) ```
*)
val is_err: ('a, 'e) t -> bool

(**
   Returns `true` if the result is [`Ok`] and the value matches the predicate.

   ## Examples

   ```ocaml let x = Ok 2 in assert (Result.is_ok_and (fun v -> v > 1) x);
   assert (not (Result.is_ok_and (fun v -> v > 5) x));

   let y = Error "Error" in assert (not (Result.is_ok_and (fun v -> v > 0) y))
   ```
*)
val is_ok_and: ('a -> bool) -> ('a, 'e) t -> bool

(**
   Returns `true` if the result is [`Error`] and the error matches the
   predicate.

   ## Examples

   ```ocaml let is_not_found = Result.is_err_and (fun e -> String.contains e
   "not found" ) in

   let x = Error "File not found" in assert (is_not_found x);

   let y = Ok 123 in assert (not (is_not_found y)) ```
*)
val is_err_and: ('e -> bool) -> ('a, 'e) t -> bool

(**
   Maps a `Result<'a, 'e>` to `Result<'b, 'e>` by applying a function to the
   [`Ok`] value.

   This function can be used to compose the results of two functions.

   ## Examples

   ```ocaml let parse_int s = try Ok (int_of_string s) with _ -> Error "Not a
   number"

   let double = Result.map (fun x -> x * 2) in

   assert (double (parse_int "21") = Ok 42); assert (double (parse_int "abc") =
   Error "Not a number") ```
*)
val map: ('a, 'e) t -> fn:('a -> 'b) -> ('b, 'e) t

(**
   Maps a `Result<'a, 'e>` to `Result<'a, 'f>` by applying a function to the
   [`Error`] value.

   ## Examples

   ```ocaml let stringify_error = Result.map_err string_of_int in

   let x = Ok 2 in assert (stringify_error x = Ok 2);

   let y = Error 13 in assert (stringify_error y = Error "13") ```
*)
val map_err: ('a, 'e) t -> fn:('e -> 'f) -> ('a, 'f) t

(**
   Returns the result of applying function to [`Ok`] value, or default if
   [`Error`].

   Arguments are passed in order of increasing likelihood of being evaluated,
   so `default` is passed first.

   ## Examples

   ```ocaml let x = Ok "foo" in assert (Result.map_or ~default:42 String.length
   x = 3);

   let y = Error "bar" in assert (Result.map_or ~default:42 String.length y =
   42) ```
*)
val map_or: ('a, 'e) t -> default:'b -> fn:('a -> 'b) -> 'b

(**
   Maps a `Result<'a, 'e>` to `'b` by applying fallback function to [`Error`],
   or function to [`Ok`] value.

   This function can be used to unpack a successful result while handling an
   error.

   ## Examples

   ```ocaml let k = 21 in

   let x = Ok "foo" in assert (Result.map_or_else ~default:(fun _ -> k * 2)
   String.length x = 3);

   let y = Error "bar" in assert (Result.map_or_else ~default:(fun e ->
   String.length e) String.length y = 3) ```
*)
val map_or_else: ('a, 'e) t -> default:('e -> 'b) -> fn:('a -> 'b) -> 'b

(**
   Calls function on [`Ok`] value if present, short-circuits on [`Error`].

   Often used to chain fallible operations. This is sometimes called "flatmap"
   or "bind" in other languages.

   ## Examples

   ```ocaml let parse_int s = try Ok (int_of_string s) with _ -> Error "not an
   integer"

   let divide x y = if y = 0 then Error "division by zero" else Ok (x / y)

   let result = parse_int "10" |> Result.and_then (fun x -> divide x 2) |>
   Result.and_then (fun x -> divide x 2) (* result = Ok 2 *)

   let failed = parse_int "abc" |> Result.and_then (fun x -> divide x 2) (*
   failed = Error "not an integer" - second operation never runs *) ```
*)
val and_then: ('a, 'e) t -> fn:('a -> ('b, 'e) t) -> ('b, 'e) t

(**
   Returns the first result if [`Ok`], otherwise returns the second result.

   ## Examples

   ```ocaml let x = Ok 2 in let y = Error "late error" in assert (Result.or_ x
   y = Ok 2);

   let x = Error "early error" in let y = Ok 2 in assert (Result.or_ x y = Ok
   2);

   let x = Error "early" in let y = Error "late" in assert (Result.or_ x y =
   Error "late") ```
*)
val or_: ('a, 'e) t -> ('a, 'e) t -> ('a, 'e) t

(**
   Calls function on [`Error`] value if present, passes through [`Ok`].

   This function can be used for control flow based on result values.

   ## Examples

   ```ocaml let sq x = Ok (x * x) in let err x = Error x in

   assert (Result.or_else (Ok 2) sq = Ok 2); assert (Result.or_else (Ok 2) err
   = Ok 2); assert (Result.or_else (Error 3) sq = Ok 9); assert (Result.or_else
   (Error 3) err = Error 3) ```
*)
val or_else: ('a, 'e) t -> fn:('e -> ('a, 'f) t) -> ('a, 'f) t

(**
   Returns the contained [`Ok`] value, consuming the result.

   ## Panics

   Panics if the value is an [`Error`], with a panic message including the
   error's content.

   ## Examples

   ```ocaml let x = Ok 2 in assert (Result.unwrap x = 2)

   (* This will panic: *) let y = Error "emergency failure" in Result.unwrap y
   (* panic: "Called Result.unwrap on Error: emergency failure" *) ```

   ## Note

   Generally, prefer [`expect`] which provides a more helpful panic message, or
   [`unwrap_or`] / [`unwrap_or_else`] for non-panicking alternatives.
*)

(**
   Returns the contained [`Ok`] value or a provided default.

   Arguments are evaluated eagerly; if you are passing the result of a function
   call, consider [`unwrap_or_else`], which is lazily evaluated.

   ## Examples

   ```ocaml let default = 2 in let x = Ok 9 in assert (Result.unwrap_or
   ~default x = 9);

   let y = Error "error" in assert (Result.unwrap_or ~default y = 2) ```
*)
val unwrap: ('a, 'e) t -> 'a

(**
   Returns the contained [`Ok`] value or computes it from a closure.

   ## Examples

   ```ocaml let count s = String.length s in assert (Result.unwrap_or_else
   ~fn:(fun () -> 2) (Ok 9) = 9); assert (Result.unwrap_or_else ~fn:(fun () ->
   count "foo") (Error "bar") = 3) ```
*)
val unwrap_or: ('a, 'e) t -> default:'a -> 'a

(**
   Returns the contained [`Error`] value, consuming the result.

   ## Panics

   Panics if the value is an [`Ok`], with a panic message including the success
   value.

   ## Examples

   ```ocaml let x = Error "emergency failure" in assert (Result.unwrap_err x =
   "emergency failure")

   (* This will panic: *) let y = Ok 2 in Result.unwrap_err y (* panic: "Called
   Result.unwrap_err on Ok: 2" *) ```
*)
val unwrap_or_else: ('a, 'e) t -> fn:(unit -> 'a) -> 'a

(**
   Returns the contained [`Ok`] value, consuming the result.

   ## Panics

   Panics if the value is an [`Error`], with the provided message and the
   error's content.

   ## Examples

   ```ocaml let config = Fs.read (Path.v "config.json") |> Result.expect
   ~msg:"Config file is required for the application" ```

   ## Recommended Message Style

   We recommend that `expect` messages describe the reason you expect the
   `Result` to be `Ok`:

   ```ocaml let process_config path = Fs.read path |> Result.expect
   ~msg:"config file should exist at startup" ```

   **Hint**: If you're having trouble choosing a message, consider using
   [`unwrap`] instead.
*)
val unwrap_err: ('a, 'e) t -> 'e

(**
   Returns the contained [`Error`] value, consuming the result.

   ## Panics

   Panics if the value is an [`Ok`], with the provided message and the success
   value.

   ## Examples

   ```ocaml let x = Error "emergency failure" in assert (Result.expect_err
   ~msg:"Testing errors" x = "emergency failure") ```
*)
val expect: msg:string -> ('a, 'e) t -> 'a

(**
   Converts from `Result<'a, 'e>` to [`Option<'a>`].

   Converts [`Ok(v)`] to [`Some(v)`] and [`Error(_)`] to [`None`].

   ## Examples

   ```ocaml let x = Ok 2 in assert (Result.ok_value x = Some 2);

   let y = Error "Nothing here" in assert (Result.ok_value y = None) ```
*)
val expect_err: msg:string -> ('a, 'e) t -> 'e

val ok_value: ('a, 'e) t -> 'a option

(**
   Converts from `Result<'a, 'e>` to [`Option<'e>`].

   Converts [`Error(e)`] to [`Some(e)`] and [`Ok(_)`] to [`None`].

   ## Examples

   ```ocaml let x = Ok 2 in assert (Result.err_value x = None);

   let y = Error "Nothing here" in assert (Result.err_value y = Some "Nothing
   here") ```
*)
val err_value: ('a, 'e) t -> 'e option

(**
   Calls the provided closure on the contained [`Ok`] value (if any).

   Returns the original result unchanged. Useful for debugging or side effects.

   ## Examples

   ```ocaml let parse_int s = try Ok (int_of_string s) with _ -> Error "not an
   integer"

   let result = parse_int "4" |> Result.inspect (fun x -> Printf.printf
   "Original: %d\n" x) |> Result.map (fun x -> x * x) |> Result.inspect (fun x
   -> Printf.printf "Squared: %d\n" x) (* Prints: Original: 4 Squared: 16
   Returns: Ok 16 *) ```
*)
val inspect: ('a -> unit) -> ('a, 'e) t -> ('a, 'e) t

(**
   Calls the provided closure on the contained [`Error`] value (if any).

   Returns the original result unchanged. Useful for logging errors.

   ## Examples

   ```ocaml Fs.read (Path.v "config.json") |> Result.inspect_err (fun e ->
   Log.warning "Failed to load config: %s" e) |> Result.unwrap_or
   ~default:default_config ```
*)
val inspect_err: ('e -> unit) -> ('a, 'e) t -> ('a, 'e) t

(**
   Calls function on [`Ok`] value if present, otherwise does nothing.

   ## Examples

   ```ocaml let x = Ok 7 in Result.iter (fun v -> Printf.printf "Got: %d\n" v)
   x; (* Prints: Got: 7 *)

   let y = Error "nothing" in Result.iter (fun v -> Printf.printf "Got: %d\n"
   v) y (* Prints nothing *) ```
*)
val iter: ('a, 'e) t -> fn:('a -> unit) -> unit

(**
   Calls function on [`Error`] value if present, otherwise does nothing.

   ## Examples

   ```ocaml let log_error = Result.iter_err (fun e -> Log.error "Operation
   failed: %s" e ) in

   log_error (Error "disk full"); (* Logs error *) log_error (Ok 123) (* Does
   nothing *) ```
*)
val iter_err: ('a, 'e) t -> fn:('e -> unit) -> unit

(**
   Converts from `Result<'a, 'e>` to [`Option<'a>`].

   Alias for [`ok_value`].

   ## Examples

   ```ocaml let x = Ok 2 in assert (Result.to_option x = Some 2);

   let y = Error "Nothing here" in assert (Result.to_option y = None) ```
*)
val to_option: ('a, 'e) t -> 'a option

(**
   Converts from [`Option<'a>`] to `Result<'a, 'e>`.

   [`Some(v)`] becomes [`Ok(v)`] and [`None`] becomes [`Error(error)`].

   ## Examples

   ```ocaml let x = Some "value" in assert (Result.of_option ~error:"no value"
   x = Ok "value");

   let y = None in assert (Result.of_option ~error:"no value" y = Error "no
   value")

   (* Common pattern with environment variables *) let port = Sys.getenv_opt
   "PORT" |> Result.of_option ~error:"PORT not set" |> Result.and_then (fun s
   -> try Ok (int_of_string s) with _ -> Error "PORT must be a number") ```
*)
val from_option: error:'e -> 'a option -> ('a, 'e) t

(**
   Transposes a `Result` of an `Option` into an `Option` of a `Result`.

   [`Ok(None)`] becomes [`None`]. [`Ok(Some(v))`] becomes [`Some(Ok(v))`].
   [`Error(e)`] becomes [`Some(Error(e))`].

   ## Examples

   ```ocaml let x = Ok (Some 5) in assert (Result.transpose x = Some (Ok 5));

   let y = Ok None in assert (Result.transpose y = None);

   let z = Error "error" in assert (Result.transpose z = Some (Error "error"))
   ```
*)
val transpose: ('a option, 'e) t -> ('a, 'e) t option

(**
   Converts from `Result<Result<'a, 'e>, 'e>` to `Result<'a, 'e>`.

   ## Examples

   ```ocaml let x = Ok (Ok 2) in assert (Result.flatten x = Ok 2);

   let y = Ok (Error "late error") in assert (Result.flatten y = Error "late
   error");

   let z = Error "early error" in assert (Result.flatten z = Error "early
   error")

   (* Flattening is the reverse of nesting *) let nested = Ok (Ok 5) in assert
   (Result.flatten nested = Ok 5) ```
*)
val flatten: (('a, 'e) t, 'e) t -> ('a, 'e) t

(**
   Converts a list of `Result`s into a single `Result` containing a list.

   If all results are [`Ok`], returns [`Ok`] with list of values. If any result
   is [`Error`], returns the first error encountered.

   ## Examples

   ```ocaml let results = [Ok 1; Ok 2; Ok 3] in assert (Result.all results = Ok
   [1; 2; 3]);

   let with_error = [Ok 1; Error "oops"; Ok 3; Error "another"] in assert
   (Result.all with_error = Error "oops")

   (* Common pattern: validate multiple inputs *) let parse_numbers strings =
   strings |> List.map (fun s -> try Ok (int_of_string s) with _ -> Error
   (Printf.sprintf "'%s' is not a number" s)) |> Result.all ```
*)
val all: ('a, 'e) t list -> ('a list, 'e) t

(**
   Combines two `Result`s into a single `Result` containing a tuple.

   If both are [`Ok`], returns [`Ok`] with tuple of values.
   If either is [`Error`], returns the first error.

   ## Examples

   ```ocaml
   let x = Ok 5 in
   let y = Ok "hello" in
   assert (Result.both x y = Ok (5, "hello"));

   let z = Error "failed" in
   assert (Result.both x z = Error "failed");
   assert (Result.both z x = Error "failed")

   (* Validate two required fields *)
   let validate_credentials username password =
     Result.both
       (validate_username username)
       (validate_password password)
     |> Result.map (fun (u, p) -> {username = u; password = p})
   ```
*)
val both: ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

(**
   Applies one of two functions depending on the result variant.

   This is equivalent to pattern matching but can be more convenient in
   point-free style or when building combinators.

   ## Examples

   ```ocaml let to_string = Result.fold ~ok:string_of_int ~error:(fun e ->
   Printf.sprintf "Error: %s" e) in

   assert (to_string (Ok 42) = "42"); assert (to_string (Error "failed") =
   "Error: failed")

   (* Convert any result to an exit code *) let to_exit_code = Result.fold
   ~ok:(fun _ -> 0) ~error:(fun _ -> 1) ```
*)
val fold: ('a, 'e) t -> ok:('a -> 'c) -> error:('e -> 'c) -> 'c

module Syntax: sig
  val ( let* ): ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end
