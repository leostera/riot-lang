open Global

(**
   JSON parsing and serialization.

   A simple JSON library for parsing, generating, and manipulating JSON data.
   Designed for RPC communication and configuration files.

   ## Examples

   Basic parsing and serialization:

   ```ocaml
   open Std.Data

   (* Parse JSON *)
   let json_str = {|{"name": "Alice", "age": 30, "active": true}|} in
   match Json.from_string json_str with
   | Ok json ->
       (* Extract fields *)
       let name = Json.get_field "name" json
         |> Option.and_then Json.get_string in
       let age = Json.get_field "age" json
         |> Option.and_then Json.get_int in
       Printf.printf "Name: %s, Age: %d\n"
         (Option.unwrap name) (Option.unwrap age)
   | Error err ->
       Log.error "Parse error: %s" (Json.error_to_string err)
   ```

   Building JSON programmatically:

   ```ocaml
   let user_json = Json.obj [
     ("id", Json.int 123);
     ("name", Json.string "Bob");
     ("email", Json.string "bob@example.com");
     ("tags", Json.array [
       Json.string "admin";
       Json.string "verified"
     ]);
     ("metadata", Json.obj [
       ("created_at", Json.string "2024-01-01");
       ("login_count", Json.int 42)
     ])
   ] in

   let json_string = Json.to_string user_json
   (* {"id":123,"name":"Bob",...} *)
   ```

   Working with arrays:

   ```ocaml
   let json = Json.from_string {|[1, 2, 3, 4, 5]|} |> Result.unwrap in
   match Json.get_array json with
   | Some items ->
       let sum = List.fold_left (fun acc item ->
         match Json.get_int item with
         | Some n -> acc + n
         | None -> acc
       ) 0 items in
       Printf.printf "Sum: %d\n" sum
   | None -> ()
   ```

   ## Error Handling

   Parse errors include position information for debugging:

   ```ocaml
   match Json.from_string {|{"bad": json}|} with
   | Ok _ -> ()
   | Error (Invalid_literal { expected; position; found }) ->
       Printf.printf "Expected %s at position %d, found: %s\n"
         expected position found
   | Error err ->
       Printf.printf "Error: %s\n" (Json.error_to_string err)
   ```
*)

(**
   JSON value representation. Supports all standard JSON types: null,
   booleans, numbers (int/float), strings, arrays, and objects.
*)
type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Object of (string * t) list
  | Embed of t
(** JSON parsing errors with position information for debugging. *)
type error =
  | Unterminated_string of { position: int }
  | Invalid_literal of { expected: string; position: int; found: string }
  | Invalid_number of { position: int; text: string }
  | Expected_comma_or_bracket of {
      kind: string;
      position: int;
      found: char option;
    }
  | Expected_string_key of {
      position: int;
      found: char option;
    }
  | Expected_colon of {
      position: int;
      found: char option;
    }
  | Unexpected_end_of_input of { expected: string }
  | Unexpected_character of { position: int; character: char; expected: string }
  | Extra_input_after_value of { position: int }
  | Unknown_error of string

(**
   Parses a JSON string into a [t] value.

   ## Examples

   ```ocaml
   Json.from_string {|{"key": "value"}|}  (* Ok (Object [...]) *)
   Json.from_string {|[1, 2, 3]|}  (* Ok (Array [...]) *)
   Json.from_string {|true|}  (* Ok (Bool true) *)
   Json.from_string {|invalid}|}  (* Error (Invalid_literal {...}) *)
   ```

   ## Error Cases

   Returns [Error] for:
   - Malformed JSON syntax
   - Unexpected end of input
   - Invalid escape sequences in strings
   - Invalid number formats
*)
val from_string: string -> (t, error) result

(**
   Serializes a JSON value to a compact string (no pretty-printing).

   ## Examples

   ```ocaml
   let json = Json.obj [("a", Json.int 1); ("b", Json.bool true)] in
   Json.to_string json  (* {"a":1,"b":true} *)

   Json.to_string (Json.array [Json.int 1; Json.int 2])  (* [1,2] *)
   ```
*)
val to_string: t -> string

(**
   Serializes a JSON value with stable two-space indentation.

   This preserves array/object field order from the input value, unlike any
   higher-level canonicalization helpers that may sort keys first.

   ## Examples

   ```ocaml
   let json =
     Json.obj
       [
         ("name", Json.string "Alice");
         ("tags", Json.array [ Json.string "admin"; Json.string "verified" ]);
       ]
   in
   Json.to_string_pretty json
   (* {
        "name": "Alice",
        "tags": [
          "admin",
          "verified"
        ]
      } *)
   ```
*)
val to_string_pretty: ?depth:int -> t -> string

(**
   Converts a parse error to a human-readable error message.

   ## Examples

   ```ocaml match Json.from_string bad_input with | Ok _ -> () | Error err ->
   Log.error "JSON parse failed: %s" (Json.error_to_string err) ```
*)
val error_to_string: error -> string

(**
   Creates a JSON null value.

   ## Examples

   ```ocaml Json.null (* Null *) Json.to_string Json.null (* "null" *) ```
*)
val null: t

(**
   Creates a JSON boolean value.

   ## Examples

   ```ocaml Json.bool true (* Bool true *) Json.bool false (* Bool false *) ```
*)
val bool: bool -> t

(**
   Creates a JSON integer value.

   ## Examples

   ```ocaml Json.int 42 (* Int 42 *) Json.int (-100) (* Int (-100) *) ```
*)
val int: int -> t

(**
   Creates a JSON floating-point value.

   ## Examples

   ```ocaml Json.float 3.14 (* Float 3.14 *) Json.float (-0.5) (* Float (-0.5)
   *) ```
*)
val float: float -> t

(**
   Creates a JSON string value.

   ## Examples

   ```ocaml Json.string "hello" (* String "hello" *) Json.to_string
   (Json.string "test") (* "\"test\"" *) ```
*)
val string: string -> t

(**
   Creates a JSON array from a list of values.

   ## Examples

   ```ocaml Json.array [Json.int 1; Json.int 2; Json.int 3] (* Array
   [Int 1; Int 2; Int 3] *)

   Json.array [] (* Array [] *) ```
*)
val array: t list -> t

(**
   Creates a JSON object from key-value pairs.

   ## Examples

   ```ocaml Json.obj
   [ ("name", Json.string "Alice"); ("age", Json.int 30); ("active", Json.bool
    true) ] (* Object [("name", String "Alice"); ...] *)

   Json.obj [] (* Object [] - empty object *) ```
*)
val obj: (string * t) list -> t

(**
   Extracts a field from a JSON object by key name. Returns [None] if the value
   is not an object or the field doesn't exist.

   ## Examples

   ```ocaml let json = Json.obj [("x", Json.int 10); ("y", Json.int 20)] in
   Json.get_field "x" json (* Some (Int 10) *) Json.get_field "z" json (* None
   \- field doesn't exist *)

   Json.get_field "x" (Json.int 5) (* None - not an object *) ```
*)
val get_field: string -> t -> t option

(**
   Extracts a string value. Returns [None] if not a string.

   ## Examples

   ```ocaml Json.get_string (Json.string "hello") (* Some "hello" *)
   Json.get_string (Json.int 42) (* None *) Json.get_string Json.null (* None
   *) ```
*)
val get_string: t -> string option

(**
   Extracts an integer value. Returns [None] if not an integer.

   ## Examples

   ```ocaml Json.get_int (Json.int 42) (* Some 42 *) Json.get_int (Json.float
   3.14) (* None - is a float *) Json.get_int (Json.string "42") (* None - is a
   string *) ```
*)
val get_int: t -> int option

(**
   Extracts a boolean value. Returns [None] if not a boolean.

   ## Examples

   ```ocaml Json.get_bool (Json.bool true) (* Some true *) Json.get_bool
   (Json.int 1) (* None - not a bool *) ```
*)
val get_bool: t -> bool option

(**
   Extracts an array as a list of values. Returns [None] if not an array.

   ## Examples

   ```ocaml let json = Json.array [Json.int 1; Json.int 2] in Json.get_array
   json (* Some [Int 1; Int 2] *)

   Json.get_array (Json.string "test") (* None *) ```
*)
val get_array: t -> t list option

(**
   Extracts an object as a list of key-value pairs. Returns [None] if not an
   object.

   ## Examples

   ```ocaml let json = Json.obj [("a", Json.int 1)] in Json.get_object json (*
   Some [("a", Int 1)] *)

   Json.get_object (Json.array []) (* None *) ```
*)
val get_object: t -> (string * t) list option

(**
   Computes deep differences between two JSON values.

   Recursively compares nested structures and returns a list of all differences
   with their paths. Each diff includes:
   - The path to the difference (e.g., ["user"; "address"; "city"])
   - The type of change (Added, Removed, or Changed)

   ## Examples

   ```ocaml (* Primitive diff *) Json.diff (Json.int 1) (Json.int 2) (*
   [{ path = []; change = Changed (Int 1, Int 2) }] *)

   (* Object diff *) let o1 = Json.obj [("a", Json.int 1); ("b", Json.int 2)]
   in let o2 = Json.obj
   [("a", Json.int 1); ("b", Json.int 3); ("c", Json.int 4)] in Json.diff o1 o2
   (*
   [ { path = ["b"]; change = Changed (Int 2, Int 3) }; { path = ["c"]; change
    = Added (Int 4) } ] *)

   (* Nested object diff *) let o1 = Json.obj
   [("user", Json.obj [("age", Json.int 30)])] in let o2 = Json.obj
   [("user", Json.obj [("age", Json.int 31)])] in Json.diff o1 o2 (*
   [{ path = ["user"; "age"]; change = Changed (Int 30, Int 31) }] *)

   (* Array diff *) let a1 = Json.array [Json.int 1; Json.int 2; Json.int 3] in
   let a2 = Json.array [Json.int 1; Json.int 99; Json.int 3] in Json.diff a1 a2
   (* [{ path = ["1"]; change = Changed (Int 2, Int 99) }] *) ```

   ## Filtering Results

   Use {!Diff} helper functions to filter results:

   ```ocaml let diff = Json.diff o1 o2 in let additions = Diff.additions diff
   in let removals = Diff.removals diff in let changes = Diff.changes diff in
   let user_changes = Diff.at_path ["user"] diff ```
*)
val diff: t -> t -> t Diff.change list
