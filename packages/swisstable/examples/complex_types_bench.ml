open Std
open Std.Bench

module HashMap = Std.Collections.HashMap

(* ========================================================================
   * Type Definitions for Complex Keys/Values
   * ========================================================================
*)
(* Simple user record *)
type user = { id: int; name: string; email: string }

(* Address record *)
type address = { street: string; city: string; zip: int }

(* Event variants *)
type event =
  | Click of { x: int; y: int }
  | KeyPress of string
  | Scroll of int
  | MouseMove of { x: int; y: int; button: int }

(* Order status *)
type order_status =
  | Pending
  | Processing
  | Shipped of { tracking: string; carrier: string }
  | Delivered

(* Complex order record *)
type order = {
  order_id: int;
  customer_name: string;
  items: (string * int * float) list;
  (* name, quantity, price *)
  total: float;
  status: order_status;
}

(* Nested customer structure *)
type customer = { user: user; address: address; order_count: int }

(* ========================================================================
   * Benchmark Configuration
   * ========================================================================
*)
let small_config = { iterations = 100; warmup = 10 }

let medium_config = { iterations = 50; warmup = 5 }

let large_config = { iterations = 20; warmup = 2 }

(* ========================================================================
   * Record Keys Benchmarks
   * ========================================================================
*)
(* HashMap: Insert with user record keys *)
let bench_hashmap_record_keys_insert = fun () ->
  let map = HashMap.create () in
  for i = 0 to 9_999 do
    let user = { id = i; name = "user_" ^ string_of_int i; email = "user" ^ string_of_int i ^ "@example.com" } in ignore (HashMap.insert map ~key:user ~value:i)
  done

(* Swisstable: Insert with user record keys *)
let bench_swisstable_record_keys_insert = fun () ->
  let map = Swisstable.create () in
  for i = 0 to 9_999 do
    let user = { id = i; name = "user_" ^ string_of_int i; email = "user" ^ string_of_int i ^ "@example.com" } in ignore (Swisstable.insert map user i)
  done

(* HashMap: Get with user record keys *)
let bench_hashmap_record_keys_get = fun () ->
  let map = HashMap.create () in
  for i = 0 to 9_999 do
    let user = { id = i; name = "user_" ^ string_of_int i; email = "user" ^ string_of_int i ^ "@example.com" } in ignore (HashMap.insert map ~key:user ~value:i)
  done;
  for i = 0 to 999 do
    let user = { id = i * 10; name = "user_" ^ string_of_int (i * 10); email = "user" ^ string_of_int (i * 10) ^ "@example.com" } in ignore (HashMap.get map ~key:user)
  done

(* Swisstable: Get with user record keys *)
let bench_swisstable_record_keys_get = fun () ->
  let map = Swisstable.create () in
  for i = 0 to 9_999 do
    let user = { id = i; name = "user_" ^ string_of_int i; email = "user" ^ string_of_int i ^ "@example.com" } in ignore (Swisstable.insert map user i)
  done;
  for i = 0 to 999 do
    let user = { id = i * 10; name = "user_" ^ string_of_int (i * 10); email = "user" ^ string_of_int (i * 10) ^ "@example.com" } in ignore (Swisstable.get map user)
  done

(* ========================================================================
   * Variant Keys Benchmarks
   * ========================================================================
*)
(* HashMap: Insert with variant keys *)
let bench_hashmap_variant_keys_insert = fun () ->
  let map = HashMap.create () in
  for i = 0 to 9_999 do
    let event =
      match i mod 4 with
      | 0 -> Click { x = i; y = i * 2 }
      | 1 -> KeyPress ("key_" ^ string_of_int i)
      | 2 -> Scroll i
      | _ -> MouseMove { x = i; y = i * 2; button = i mod 3 }
    in
    ignore (HashMap.insert map ~key:event ~value:i)
  done

(* Swisstable: Insert with variant keys *)
let bench_swisstable_variant_keys_insert = fun () ->
  let map = Swisstable.create () in
  for i = 0 to 9_999 do
    let event =
      match i mod 4 with
      | 0 -> Click { x = i; y = i * 2 }
      | 1 -> KeyPress ("key_" ^ string_of_int i)
      | 2 -> Scroll i
      | _ -> MouseMove { x = i; y = i * 2; button = i mod 3 }
    in
    ignore (Swisstable.insert map event i)
  done

(* HashMap: Get with variant keys *)
let bench_hashmap_variant_keys_get = fun () ->
  let map = HashMap.create () in
  for i = 0 to 9_999 do
    let event =
      match i mod 4 with
      | 0 -> Click { x = i; y = i * 2 }
      | 1 -> KeyPress ("key_" ^ string_of_int i)
      | 2 -> Scroll i
      | _ -> MouseMove { x = i; y = i * 2; button = i mod 3 }
    in
    ignore (HashMap.insert map ~key:event ~value:i)
  done;
  for i = 0 to 999 do
    let event =
      match i mod 4 with
      | 0 -> Click { x = i * 10; y = (i * 10) * 2 }
      | 1 -> KeyPress ("key_" ^ string_of_int (i * 10))
      | 2 -> Scroll (i * 10)
      | _ -> MouseMove { x = i * 10; y = (i * 10) * 2; button = (i * 10) mod 3 }
    in
    ignore (HashMap.get map ~key:event)
  done

(* Swisstable: Get with variant keys *)
let bench_swisstable_variant_keys_get = fun () ->
  let map = Swisstable.create () in
  for i = 0 to 9_999 do
    let event =
      match i mod 4 with
      | 0 -> Click { x = i; y = i * 2 }
      | 1 -> KeyPress ("key_" ^ string_of_int i)
      | 2 -> Scroll i
      | _ -> MouseMove { x = i; y = i * 2; button = i mod 3 }
    in
    ignore (Swisstable.insert map event i)
  done;
  for i = 0 to 999 do
    let event =
      match i mod 4 with
      | 0 -> Click { x = i * 10; y = (i * 10) * 2 }
      | 1 -> KeyPress ("key_" ^ string_of_int (i * 10))
      | 2 -> Scroll (i * 10)
      | _ -> MouseMove { x = i * 10; y = (i * 10) * 2; button = (i * 10) mod 3 }
    in
    ignore (Swisstable.get map event)
  done

(* ========================================================================
   * Tuple Keys Benchmarks
   * ========================================================================
*)
(* HashMap: Insert with tuple keys (common for coordinates, multi-part keys) *)
let bench_hashmap_tuple_keys_insert = fun () ->
  let map = HashMap.create () in
  for i = 0 to 9_999 do
    let key = i, i mod 100, "category_" ^ string_of_int (i mod 10) in ignore (HashMap.insert map ~key ~value:i)
  done

(* Swisstable: Insert with tuple keys *)
let bench_swisstable_tuple_keys_insert = fun () ->
  let map = Swisstable.create () in
  for i = 0 to 9_999 do
    let key = i, i mod 100, "category_" ^ string_of_int (i mod 10) in ignore (Swisstable.insert map key i)
  done

(* HashMap: Get with tuple keys *)
let bench_hashmap_tuple_keys_get = fun () ->
  let map = HashMap.create () in
  for i = 0 to 9_999 do
    let key = i, i mod 100, "category_" ^ string_of_int (i mod 10) in ignore (HashMap.insert map ~key ~value:i)
  done;
  for i = 0 to 999 do
    let key = i * 10, (i * 10) mod 100, "category_" ^ string_of_int ((i * 10) mod 10) in ignore (HashMap.get map ~key)
  done

(* Swisstable: Get with tuple keys *)
let bench_swisstable_tuple_keys_get = fun () ->
  let map = Swisstable.create () in
  for i = 0 to 9_999 do
    let key = i, i mod 100, "category_" ^ string_of_int (i mod 10) in ignore (Swisstable.insert map key i)
  done;
  for i = 0 to 999 do
    let key = i * 10, (i * 10) mod 100, "category_" ^ string_of_int ((i * 10) mod 10) in ignore (Swisstable.get map key)
  done

(* ========================================================================
   * Complex Values Benchmarks (int keys, complex values)
   * ========================================================================
*)
(* HashMap: Insert with complex order values *)
let bench_hashmap_complex_values_insert = fun () ->
  let map = HashMap.create () in
  for i = 0 to 9_999 do
    let order = {
      order_id = i;
      customer_name = "customer_" ^ string_of_int i;
      items = [
        "item1", 2, 9.99;
        "item2", 1, 19.99;
        "item3", 3, 5.99;
      ];
      total = 39.97 +. float_of_int (i mod 100);
      status = if i mod 3 = 0 then
        Delivered
      else
        if i mod 3 = 1 then
          Shipped { tracking = "TRK" ^ string_of_int i; carrier = "FedEx" }
        else Pending
    }
    in
    ignore (HashMap.insert map ~key:i ~value:order)
  done

(* Swisstable: Insert with complex order values *)
let bench_swisstable_complex_values_insert = fun () ->
  let map = Swisstable.create () in
  for i = 0 to 9_999 do
    let order = {
      order_id = i;
      customer_name = "customer_" ^ string_of_int i;
      items = [
        "item1", 2, 9.99;
        "item2", 1, 19.99;
        "item3", 3, 5.99;
      ];
      total = 39.97 +. float_of_int (i mod 100);
      status = if i mod 3 = 0 then
        Delivered
      else
        if i mod 3 = 1 then
          Shipped { tracking = "TRK" ^ string_of_int i; carrier = "FedEx" }
        else Pending
    }
    in
    ignore (Swisstable.insert map i order)
  done

(* HashMap: Get with complex values *)
let bench_hashmap_complex_values_get = fun () ->
  let map = HashMap.create () in
  for i = 0 to 9_999 do
    let order = {
      order_id = i;
      customer_name = "customer_" ^ string_of_int i;
      items = [
        "item1", 2, 9.99;
        "item2", 1, 19.99;
      ];
      total = 39.97;
      status = Pending
    }
    in
    ignore (HashMap.insert map ~key:i ~value:order)
  done;
  for i = 0 to 999 do ignore (HashMap.get map ~key:(i * 10)) done

(* Swisstable: Get with complex values *)
let bench_swisstable_complex_values_get = fun () ->
  let map = Swisstable.create () in
  for i = 0 to 9_999 do
    let order = {
      order_id = i;
      customer_name = "customer_" ^ string_of_int i;
      items = [
        "item1", 2, 9.99;
        "item2", 1, 19.99;
      ];
      total = 39.97;
      status = Pending
    }
    in
    ignore (Swisstable.insert map i order)
  done;
  for i = 0 to 999 do ignore (Swisstable.get map (i * 10)) done

(* ========================================================================
   * Nested Structures Benchmarks
   * ========================================================================
*)
(* HashMap: Insert with nested customer keys *)
let bench_hashmap_nested_keys_insert = fun () ->
  let map = HashMap.create () in
  for i = 0 to 9_999 do
    let customer = { user = { id = i; name = "user_" ^ string_of_int i; email = "user" ^ string_of_int i ^ "@example.com" }; address = { street = string_of_int i ^ " Main St"; city = "City"; zip = 10_000 + i }; order_count = i mod 50 } in ignore (HashMap.insert map ~key:customer ~value:i)
  done

(* Swisstable: Insert with nested customer keys *)
let bench_swisstable_nested_keys_insert = fun () ->
  let map = Swisstable.create () in
  for i = 0 to 9_999 do
    let customer = { user = { id = i; name = "user_" ^ string_of_int i; email = "user" ^ string_of_int i ^ "@example.com" }; address = { street = string_of_int i ^ " Main St"; city = "City"; zip = 10_000 + i }; order_count = i mod 50 } in ignore (Swisstable.insert map customer i)
  done

(* ========================================================================
   * Main Benchmark Suite
   * ========================================================================
*)
let benchmarks = Bench.[
  compare_with_config ~config:medium_config "record keys: insert 10k users" [ make_case "HashMap" bench_hashmap_record_keys_insert; make_case "Swisstable" bench_swisstable_record_keys_insert ];
  compare_with_config ~config:medium_config "record keys: get from 10k users" [ make_case "HashMap" bench_hashmap_record_keys_get; make_case "Swisstable" bench_swisstable_record_keys_get ];
  compare_with_config ~config:medium_config "variant keys: insert 10k events" [ make_case "HashMap" bench_hashmap_variant_keys_insert; make_case "Swisstable" bench_swisstable_variant_keys_insert ];
  compare_with_config ~config:medium_config "variant keys: get from 10k events" [ make_case "HashMap" bench_hashmap_variant_keys_get; make_case "Swisstable" bench_swisstable_variant_keys_get ];
  compare_with_config ~config:medium_config "tuple keys: insert 10k items" [ make_case "HashMap" bench_hashmap_tuple_keys_insert; make_case "Swisstable" bench_swisstable_tuple_keys_insert ];
  compare_with_config ~config:medium_config "tuple keys: get from 10k items" [ make_case "HashMap" bench_hashmap_tuple_keys_get; make_case "Swisstable" bench_swisstable_tuple_keys_get ];
  compare_with_config ~config:medium_config "complex values: insert 10k orders" [ make_case "HashMap" bench_hashmap_complex_values_insert; make_case "Swisstable" bench_swisstable_complex_values_insert ];
  compare_with_config ~config:medium_config "complex values: get from 10k orders" [ make_case "HashMap" bench_hashmap_complex_values_get; make_case "Swisstable" bench_swisstable_complex_values_get ];
  compare_with_config ~config:large_config "nested keys: insert 10k customers" [ make_case "HashMap" bench_hashmap_nested_keys_insert; make_case "Swisstable" bench_swisstable_nested_keys_insert ];
]

let main ~args =
  println "HashMap vs Swisstable - Complex Types Performance\n";
  Bench.Cli.main ~name:"Complex Types Benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
