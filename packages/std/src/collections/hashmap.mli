(** # Collections.HashMap - Fast hash table implementation

    A hash table with O(1) average case for insert, remove, and lookup
    operations. This is similar to Rust's HashMap, providing a safe API with
    Option-based lookups instead of exceptions.

    ## Examples

    Basic usage:

    ```ocaml open Std.Collections

    let scores = HashMap.create () in let _ = HashMap.insert scores "Alice" 100
    in let _ = HashMap.insert scores "Bob" 87 in

    match HashMap.get scores "Alice" with | Some score -> Printf.printf "Alice
    scored %d\n" score | None -> Printf.printf "Alice not found\n" ```

    ## When to use HashMap

    - Use `HashMap` when you need fast lookups by key (O(1) average)
    - Use `Vector` when you need indexed/ordered access
    - Use `List` for simple, immutable sequences
    - Use `HashSet` when you only need to track presence, not values *)

type ('k, 'v) t
(** The type of hash maps from keys of type `'k` to values of type `'v`.

    The implementation uses a growing hash table with Robin Hood hashing for
    good cache performance. *)

(** # Creation *)

val create : unit -> ('k, 'v) t
(** Creates a new empty HashMap with default capacity.

    The map will automatically grow as needed when elements are added.

    ## Examples

    ```ocaml let map = HashMap.create () (* Ready to store any type of key-value
    pairs *)

    let users : (string, user) HashMap.t = HashMap.create () (* Type annotations
    can help with inference *) ``` *)

val with_capacity : int -> ('k, 'v) t
(** Creates a new empty HashMap with specified initial capacity.

    Use this when you know approximately how many elements you'll store to avoid
    resizing overhead during insertions.

    ## Examples

    ```ocaml (* Processing a known dataset of 10000 users *) let users =
    HashMap.with_capacity 10000 in List.iter (fun u -> HashMap.insert users u.id
    u |> ignore ) user_list ```

    ## Performance

    Pre-sizing prevents rehashing during growth, which is important for bulk
    insertions. The capacity is a hint - the map may still resize if needed. *)

val of_list : ('k * 'v) list -> ('k, 'v) t
(** Creates a HashMap from a list of key-value pairs.

    If duplicate keys exist, later values override earlier ones.

    ## Examples

    ```ocaml let config = HashMap.of_list
    [ ("host", "localhost"); ("port", "8080"); ("debug", "true") ]

    (* Handles duplicates - last value wins *) let map = HashMap.of_list
    [ ("a", 1); ("b", 2);  ("a", 3)  (* "a" will map to 3 *) ] ``` *)

(** # Basic Operations *)

val insert : ('k, 'v) t -> 'k -> 'v -> 'v option
(** Inserts a key-value pair into the map.

    Returns `Some previous_value` if the key already existed, `None` otherwise.
    This allows you to detect overwrites.

    ## Examples

    ```ocaml let cache = HashMap.create () in

    (* First insertion returns None *) assert (HashMap.insert cache "user:1"
    "Alice" = None);

    (* Overwriting returns previous value *) assert (HashMap.insert cache
    "user:1" "Alicia" = Some "Alice");

    (* Common pattern: increment counter *) let increment counts word = let
    current = HashMap.get counts word |> Option.unwrap_or ~default:0 in
    HashMap.insert counts word (current + 1) |> ignore ```

    ## Complexity

    O(1) average case, O(n) worst case during resize *)

val get : ('k, 'v) t -> 'k -> 'v option
(** Looks up a value by key.

    Returns `Some value` if key exists, `None` otherwise. Never raises
    exceptions.

    ## Examples

    ```ocaml let settings = HashMap.create () in HashMap.insert settings "theme"
    "dark" |> ignore;

    (* Safe lookup with pattern matching *) match HashMap.get settings "theme"
    with | Some theme -> Printf.printf "Theme: %s\n" theme | None ->
    Printf.printf "Using default theme\n"

    (* Using Option utilities for defaults *) let port = HashMap.get settings
    "port" |> Option.and_then int_of_string_opt |> Option.unwrap_or
    ~default:8080 ```

    ## Complexity

    O(1) average case *)

val remove : ('k, 'v) t -> 'k -> 'v option
(** Removes a key from the map.

    Returns `Some value` if the key existed, `None` otherwise.

    ## Examples

    ```ocaml let sessions = HashMap.create () in HashMap.insert sessions
    "user123" session_data |> ignore;

    (* Remove and use the old value *) match HashMap.remove sessions "user123"
    with | Some session -> cleanup_session session | None -> () (* Already
    removed *) ```

    ## Complexity

    O(1) average case *)

val contains_key : ('k, 'v) t -> 'k -> bool
(** Checks if a key exists in the map.

    ## Examples

    ```ocaml let users = HashMap.create () in HashMap.insert users "alice"
    user_alice |> ignore;

    if HashMap.contains_key users "alice" then print_endline "Alice is
    registered" else print_endline "Alice not found" ```

    ## Complexity

    O(1) average case *)

val len : ('k, 'v) t -> int
(** Returns the number of key-value pairs in the map.

    ## Examples

    ```ocaml let map = HashMap.create () in assert (HashMap.len map = 0);

    HashMap.insert map "a" 1 |> ignore; HashMap.insert map "b" 2 |> ignore;
    assert (HashMap.len map = 2); ``` *)

val is_empty : ('k, 'v) t -> bool
(** Checks if the map contains no elements.

    ## Examples

    ```ocaml let map = HashMap.create () in assert (HashMap.is_empty map);

    HashMap.insert map "key" "value" |> ignore; assert (not (HashMap.is_empty
    map)); ``` *)

val clear : ('k, 'v) t -> unit
(** Removes all elements from the map.

    The map's capacity is not affected.

    ## Examples

    ```ocaml let cache = HashMap.create () in (* ... add many items ... *)

    HashMap.clear cache; (* Reset for reuse *) assert (HashMap.is_empty cache);
    ``` *)

(** # Iteration *)

val keys : ('k, 'v) t -> 'k list
(** Returns a list of all keys in the map.

    The order is unspecified and may change between calls.

    ## Examples

    ```ocaml let ages = HashMap.of_list [("Alice", 30); ("Bob", 25)] in let
    names = HashMap.keys ages in (* names is ["Alice"; "Bob"] or
    ["Bob"; "Alice"] *)

    (* Check if any key matches a condition *) let has_admin = HashMap.keys
    users |> List.exists (fun k -> String.starts_with ~prefix:"admin_" k) ``` *)

val values : ('k, 'v) t -> 'v list
(** Returns a list of all values in the map.

    The order is unspecified. Values may appear multiple times if different keys
    map to the same value.

    ## Examples

    ```ocaml let scores = HashMap.of_list [("Alice", 100); ("Bob", 100)] in let
    all_scores = HashMap.values scores in (* all_scores is [100; 100] *)

    (* Calculate statistics *) let average_score scores = let values =
    HashMap.values scores in let sum = List.fold_left (+) 0 values in
    float_of_int sum /. float_of_int (List.length values) ``` *)

val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
(** Applies a function to each key-value pair.

    The iteration order is unspecified.

    ## Examples

    ```ocaml let print_config config = HashMap.iter (fun key value ->
    Printf.printf "%s = %s\n" key value ) config

    (* Side effects like logging *) HashMap.iter (fun user_id session ->
    Log.debug "Active session: %s" user_id; update_last_seen user_id )
    active_sessions ``` *)

val fold : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc
(** Folds over all key-value pairs with an accumulator.

    The iteration order is unspecified.

    ## Examples

    ```ocaml (* Sum all values *) let total = HashMap.fold (fun _ value acc ->
    acc + value ) scores 0

    (* Build a reverse mapping *) let reverse_map map = HashMap.fold (fun key
    value acc -> HashMap.insert acc value key |> ignore; acc ) map
    (HashMap.create ())

    (* Collect matching entries *) let find_high_scores scores threshold =
    HashMap.fold (fun name score acc -> if score > threshold then (name, score)
    :: acc else acc ) scores [] ``` *)

val to_list : ('k, 'v) t -> ('k * 'v) list
(** Converts the map to a list of key-value pairs.

    The order is unspecified.

    ## Examples

    ```ocaml let map = HashMap.of_list [("a", 1); ("b", 2)] in let pairs =
    HashMap.to_list map in (* pairs is [("a", 1); ("b", 2)] or
    [("b", 2); ("a", 1)] *)

    (* Sort by key *) let sorted_pairs = HashMap.to_list map |> List.sort (fun
    (k1, _) (k2, _) -> String.compare k1 k2)

    (* Filter and convert *) let active_users = HashMap.to_list sessions |>
    List.filter (fun (_, session) -> session.active) |> List.map fst ``` *)

(** # Entry API *)

(** Entry type for advanced key manipulation.

    Provides efficient in-place updates without multiple lookups. *)
type ('k, 'v) entry =
  | Occupied of 'v ref  (** Key exists with mutable reference to value *)
  | Vacant  (** Key does not exist *)

val entry : ('k, 'v) t -> 'k -> ('k, 'v) entry
(** Gets the entry for a key for in-place manipulation.

    This allows efficient updates without multiple lookups.

    ## Examples

    ```ocaml let map = HashMap.create () in

    match HashMap.entry map "counter" with | Occupied r -> r := !r + 1 (*
    Increment existing *) | Vacant -> HashMap.insert map "counter" 1 |> ignore
    ``` *)

val or_insert : ('k, 'v) t -> 'k -> 'v -> 'v
(** Inserts a default value if key is absent, returns the value.

    Useful for getting a mutable reference to a value, inserting a default if
    needed.

    ## Examples

    ```ocaml (* Ensure key exists with default *) let counts = HashMap.create ()
    in let count = HashMap.or_insert counts "apples" 0 in (* count is now 0, and
    "apples" -> 0 is in map *)

    (* Common pattern: grouped data *) let group_by_key items key_fn = let
    groups = HashMap.create () in List.iter (fun item -> let key = key_fn item
    in let list = HashMap.or_insert groups key [] in HashMap.insert groups key
    (item :: list) |> ignore ) items; groups ``` *)

val and_modify : ('k, 'v) t -> 'k -> ('v -> 'v) -> unit
(** Modifies the value if the key exists.

    No effect if the key is absent.

    ## Examples

    ```ocaml let scores = HashMap.of_list [("Alice", 100); ("Bob", 87)] in

    (* Add bonus points if player exists *) HashMap.and_modify scores "Alice"
    (fun s -> s + 10); (* Alice now has 110 points *)

    HashMap.and_modify scores "Charlie" (fun s -> s + 10); (* No effect -
    Charlie doesn't exist *)

    (* Chain with or_insert for upsert behavior *) let upsert map key f default
    = HashMap.and_modify map key f; if not (HashMap.contains_key map key) then
    HashMap.insert map key default |> ignore ``` *)

val to_mut_iter : ('k, 'v) t -> ('k * 'v) Iter.MutIterator.t
(** Returns a mutable iterator over the map's key-value pairs.

    ## Examples

    ```ocaml let map = HashMap.create () in HashMap.insert map "a" 1 |> ignore;
    HashMap.insert map "b" 2 |> ignore; let iter = HashMap.to_mut_iter map in
    ``` *)
