(** Complex Key Type Property Tests for SwissTable
    
    Tests Swisstable with real-world OCaml types:
    - Records (user profiles, coordinates)
    - Variants (events, states, commands)
    - Tuples (composite keys)
    - Nested structures (complex business objects)
    
    These tests validate that polymorphic hashing works correctly
    with complex types, not just simple integers. *)
open Std
open Propane

(** {1 Complex Type Definitions} *)

(* Record types *)

type user = {
  id: int;
  name: string;
}

type point = {
  x: int;
  y: int;
}

type person = {
  age: int;
  email: string;
  active: bool;
}

(* Variant types *)

type event =
  | Click of int * int
  | KeyPress of char
  | Scroll of int
  | Resize of int * int

type status =
  | Pending
  | Running of int
  | Complete of string
  | Failed of string

(* Nested types *)

type customer = {
  user: user;
  location: point;
  status: status;
}

(** {1 Custom Generators} *)

(* User generator *)

let user_gen =
  Generator.map
    (fun ((id, name)) -> { id; name })
    (Generator.pair (Generator.int_range 0 1_000) Generator.string)

let user_arb =
  Arbitrary.make ~print:(fun u -> "{id=" ^ Int.to_string u.id ^ "; name=" ^ u.name ^ "}") user_gen

(* Point generator *)

let point_gen =
  Generator.map
    (fun ((x, y)) -> { x; y })
    (Generator.pair (Generator.int_range (-100) 100) (Generator.int_range (-100) 100))

let point_arb =
  Arbitrary.make ~print:(fun p -> "(" ^ Int.to_string p.x ^ "," ^ Int.to_string p.y ^ ")") point_gen

(* Person generator *)

let person_gen =
  Generator.map
    (fun ((age, email, active)) -> { age; email; active })
    (Generator.triple (Generator.int_range 0 120) Generator.string Generator.bool)

let person_arb =
  Arbitrary.make
    ~print:(fun p ->
      "{age="
      ^ Int.to_string p.age
      ^ "; email="
      ^ p.email
      ^ "; active="
      ^ Bool.to_string p.active
      ^ "}")
    person_gen

(* Event generator *)

let event_gen = Generator.one_of
  [
    Generator.map
      (fun ((x, y)) -> Click (x, y))
      (Generator.pair (Generator.int_range 0 1_000) (Generator.int_range 0 1_000));
    Generator.map (fun c -> KeyPress c) Generator.char;
    Generator.map (fun n -> Scroll n) (Generator.int_range (-100) 100);
    Generator.map
      (fun ((w, h)) -> Resize (w, h))
      (Generator.pair (Generator.int_range 0 2_000) (Generator.int_range 0 2_000));
  ]

let event_arb =
  Arbitrary.make
    ~print:(
      function
      | Click (x, y) -> "Click(" ^ Int.to_string x ^ "," ^ Int.to_string y ^ ")"
      | KeyPress c -> "KeyPress('" ^ String.make ~len:1 ~char:c ^ "')"
      | Scroll n -> "Scroll(" ^ Int.to_string n ^ ")"
      | Resize (w, h) -> "Resize(" ^ Int.to_string w ^ "," ^ Int.to_string h ^ ")"
    )
    event_gen

(* Status generator *)

let status_gen = Generator.frequency
  [
    (1, Generator.return Pending);
    (2, Generator.map (fun n -> Running n) (Generator.int_range 0 100));
    (1, Generator.map (fun s -> Complete s) Generator.string);
    (1, Generator.map (fun s -> Failed s) Generator.string);
  ]

let status_arb =
  Arbitrary.make
    ~print:(
      function
      | Pending -> "Pending"
      | Running n -> "Running(" ^ Int.to_string n ^ ")"
      | Complete s -> "Complete(" ^ s ^ ")"
      | Failed s -> "Failed(" ^ s ^ ")"
    )
    status_gen

(* Customer generator (nested) *)

let customer_gen =
  Generator.map
    (fun ((user, location, status)) -> { user; location; status })
    (Generator.triple user_gen point_gen status_gen)

let customer_arb =
  Arbitrary.make
    ~print:(fun c ->
      "{user="
      ^ Int.to_string c.user.id
      ^ "; location=("
      ^ Int.to_string c.location.x
      ^ ","
      ^ Int.to_string c.location.y
      ^ ")}")
    customer_gen

(** {1 Record Key Properties} *)

(* Property 1: User record keys - insert/get *)

let user_key_insert_get_prop =
  property "record keys (user): insert then get" Arbitrary.(pair user_arb int)
    (fun ((user, value)) ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map user value in
      match Swisstable.get map user with
      | Some v -> v = value
      | None -> fail "User key not found after insert")

(* Property 2: User record keys - multiple users *)

let user_key_multiple_prop =
  property "record keys (user): multiple distinct users" (Arbitrary.list
    (Arbitrary.pair user_arb Arbitrary.int))
    (fun pairs ->
      assume (Collections.List.length pairs <= 50);
      let map = Swisstable.create () in
      (* Insert all users *)
      List.iter
        (fun ((user, value)) ->
          let _ = Swisstable.insert map user value in
          ())
        pairs;
      (* Build reference to handle duplicates *)
      let ref_map = Collections.HashMap.create () in
      List.iter
        (fun ((user, value)) -> Collections.HashMap.insert ref_map ~key:user ~value |> ignore)
        pairs;
      (* Verify all accessible *)
      Collections.HashMap.for_each
        ref_map
        ~fn:(fun user expected_value ->
          match Swisstable.get map user with
          | Some actual_value ->
              if not (actual_value = expected_value) then
                fail "User value mismatch"
          | None -> fail "User key not found");
      true)

(* Property 3: Point record keys *)

let point_key_prop =
  property "record keys (point): insert then get" Arbitrary.(pair point_arb string)
    (fun ((point, value)) ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map point value in
      match Swisstable.get map point with
      | Some v -> v = value
      | None -> fail "Point key not found after insert")

(* Property 4: Person record keys with all fields *)

let person_key_prop =
  property "record keys (person): all fields matter" Arbitrary.(triple person_arb int int)
    (fun ((person, val1, val2)) ->
      let map = Swisstable.create () in
      (* Insert original person *)
      let _ = Swisstable.insert map person val1 in
      (* Create slightly different person *)
      let different_person = { person with age = person.age + 1 } in
      let _ = Swisstable.insert map different_person val2 in
      (* Both should be accessible with correct values *)
      let r1 = Swisstable.get map person in
      let r2 = Swisstable.get map different_person in
      r1 = Some val1 && r2 = Some val2)

(** {1 Variant Key Properties} *)

(* Property 5: Event variant keys *)

let event_key_prop =
  property "variant keys (event): insert then get" Arbitrary.(pair event_arb int)
    (fun ((event, value)) ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map event value in
      match Swisstable.get map event with
      | Some v -> v = value
      | None -> fail "Event key not found after insert")

(* Property 6: Multiple different events *)

let event_key_multiple_prop =
  property "variant keys (event): multiple distinct events" (Arbitrary.list
    (Arbitrary.pair event_arb Arbitrary.int))
    (fun pairs ->
      assume (Collections.List.length pairs <= 30);
      let map = Swisstable.create () in
      let ref_map = Collections.HashMap.create () in
      (* Insert all events *)
      List.iter
        (fun ((event, value)) ->
          let _ = Swisstable.insert map event value in
          let _ = Collections.HashMap.insert ref_map ~key:event ~value in
          ())
        pairs;
      (* Verify lengths match *)
      if not (Swisstable.len map = Collections.HashMap.length ref_map) then
        fail "Event map lengths differ";
      Collections.HashMap.for_each
        ref_map
        ~fn:(fun event expected ->
          match Swisstable.get map event with
          | Some actual when actual = expected -> ()
          | _ -> fail "Event value mismatch");
      true)

(* Property 7: Status variant keys *)

let status_key_prop =
  property "variant keys (status): insert then get" Arbitrary.(pair status_arb string)
    (fun ((status, value)) ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map status value in
      match Swisstable.get map status with
      | Some v -> v = value
      | None -> fail "Status key not found after insert")

(** {1 Tuple Key Properties} *)

(* Property 8: Simple tuple keys (int * int) *)

let tuple_int_int_prop =
  property "tuple keys (int * int): insert then get" Arbitrary.(pair (pair int int) string)
    (fun ((key, value)) ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map key value in
      match Swisstable.get map key with
      | Some v -> v = value
      | None -> fail "Tuple key not found after insert")

(* Property 9: Triple tuple keys *)

let tuple_triple_prop =
  property "tuple keys (int * string * bool): insert then get" Arbitrary.(pair
    (triple int string bool)
    int)
    (fun ((key, value)) ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map key value in
      match Swisstable.get map key with
      | Some v -> v = value
      | None -> fail "Triple tuple key not found after insert")

(* Property 10: Mixed tuple keys (user * int) *)

let tuple_user_int_prop =
  property "tuple keys (user * int): composite key" Arbitrary.(pair (pair user_arb int) string)
    (fun ((key, value)) ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map key value in
      match Swisstable.get map key with
      | Some v -> v = value
      | None -> fail "User-int tuple key not found")

(** {1 Nested Structure Properties} *)

(* Property 11: Customer (deeply nested) *)

let customer_key_prop =
  property "nested keys (customer): insert then get" Arbitrary.(pair customer_arb int)
    (fun ((customer, value)) ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map customer value in
      match Swisstable.get map customer with
      | Some v -> v = value
      | None -> fail "Customer key not found after insert")

(* Property 12: Multiple customers *)

let customer_key_multiple_prop =
  property "nested keys (customer): multiple distinct customers" (Arbitrary.list
    (Arbitrary.pair customer_arb Arbitrary.int))
    (fun pairs ->
      assume (Collections.List.length pairs <= 30);
      let map = Swisstable.create () in
      let ref_map = Collections.HashMap.create () in
      (* Insert all *)
      List.iter
        (fun ((customer, value)) ->
          let _ = Swisstable.insert map customer value in
          let _ = Collections.HashMap.insert ref_map ~key:customer ~value in
          ())
        pairs;
      (* Verify equivalence *)
      if not (Swisstable.len map = Collections.HashMap.length ref_map) then
        fail "Customer map lengths differ";
      Collections.HashMap.for_each
        ref_map
        ~fn:(fun customer expected ->
          match Swisstable.get map customer with
          | Some actual when actual = expected -> ()
          | _ -> fail "Customer value mismatch");
      true)

(** {1 Hash Collision Properties} *)

(* Property 13: Small point range forces collisions *)

let collision_point_prop =
  property "hash collisions (small point range): correctness maintained" (Arbitrary.list
    (Arbitrary.pair Arbitrary.int Arbitrary.int))
    (fun pairs ->
      assume (Collections.List.length pairs <= 50);
      (* Use small point range (0-9) to force collisions *)
      let small_pairs =
        List.map
          pairs
          ~fn:(fun ((k, v)) ->
            let x =
              if k < 0 then
                (-k) mod 10
              else
                k mod 10
            in
            ({ x; y = x }, v))
      in
      let map = Swisstable.create () in
      let ref_map = Collections.HashMap.create () in
      (* Insert all *)
      List.iter
        (fun ((p, v)) ->
          let _ = Swisstable.insert map p v in
          let _ = Collections.HashMap.insert ref_map ~key:p ~value:v in
          ())
        small_pairs;
      (* Verify lengths match *)
      if not (Swisstable.len map = Collections.HashMap.length ref_map) then
        fail "Lengths differ after collision test";
      Collections.HashMap.for_each
        ref_map
        ~fn:(fun p v ->
          match Swisstable.get map p with
          | Some v' when v' = v -> ()
          | _ -> fail "Point not found or value mismatch in collision test");
      true)

(** {1 Mixed Operations with Complex Keys} *)

(* Property 14: Insert, remove, reinsert with records *)

let record_mixed_ops_prop =
  property "record keys: insert/remove/reinsert cycle" Arbitrary.(pair user_arb int)
    (fun ((user, value)) ->
      let map = Swisstable.create () in
      (* Insert *)
      let r1 = Swisstable.insert map user value in
      if not (r1 = None) then
        fail "Expected None on first insert";
      let r2 = Swisstable.remove map user in
      if not (r2 = Some value) then
        fail "Expected Some(value) on remove";
      let r3 = Swisstable.insert map user (value + 1) in
      if not (r3 = None) then
        fail "Expected None on reinsert";
      Swisstable.get map user = Some (value + 1))

(* Property 15: Overwrite with variant keys *)

let variant_overwrite_prop =
  property "variant keys: overwrite behavior" Arbitrary.(triple event_arb int int)
    (fun ((event, val1, val2)) ->
      let map = Swisstable.create () in
      (* Insert first value *)
      let r1 = Swisstable.insert map event val1 in
      (* Overwrite with second value *)
      let r2 = Swisstable.insert map event val2 in
      r1 = None && r2 = Some val1 && Swisstable.get map event = Some val2)

(** {1 Test Suite} *)

let tests = [
  user_key_insert_get_prop;
  user_key_multiple_prop;
  point_key_prop;
  person_key_prop;
  event_key_prop;
  event_key_multiple_prop;
  status_key_prop;
  tuple_int_int_prop;
  tuple_triple_prop;
  tuple_user_int_prop;
  customer_key_prop;
  customer_key_multiple_prop;
  collision_point_prop;
  record_mixed_ops_prop;
  variant_overwrite_prop;
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"swisstable-complex-key-tests" ~tests ~args)
    ~args:Env.args
    ()
