open Std

module HashMap = Collections.HashMap
module Iterator = Iter.Iterator
module MutIterator = Iter.MutIterator

type 'a box = { mutable value: 'a }

let box = fun value -> { value }

let sort_ints = fun values -> List.sort values ~compare:Int.compare

let sort_pairs = fun values ->
  List.sort
    values
    ~compare:(fun (left_key, _) (right_key, _) -> Int.compare left_key right_key)

let test_create = fun _ctx ->
  let map = HashMap.create () in
  if HashMap.is_empty map && Int.equal (HashMap.length map) 0 then
    Ok ()
  else
    Error "expected HashMap.create to start empty"

let test_with_capacity = fun _ctx ->
  let map = HashMap.with_capacity ~size:32 in
  if HashMap.is_empty map && HashMap.bucket_count map >= 32 then
    Ok ()
  else
    Error "expected HashMap.with_capacity to start empty with at least the requested capacity"

let test_from_list_unique = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); (2, "b"); ] in
  if HashMap.get map ~key:1 = Some "a" && HashMap.get map ~key:2 = Some "b" then
    Ok ()
  else
    Error "expected from_list to preserve unique key/value pairs"

let test_from_list_duplicate_keeps_last = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); (1, "b"); ] in
  if HashMap.get map ~key:1 = Some "b" then
    Ok ()
  else
    Error "expected duplicate keys to keep the last value"

let test_insert_new = fun _ctx ->
  let map = HashMap.create () in
  let previous = HashMap.insert map ~key:1 ~value:"a" in
  if
    Option.is_none previous && Int.equal (HashMap.length map) 1 && HashMap.get map ~key:1 = Some "a"
  then
    Ok ()
  else
    Error "expected insert new key to return None and increment length"

let test_insert_existing = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); ] in
  if HashMap.insert map ~key:1 ~value:"b" = Some "a" && Int.equal (HashMap.length map) 1 then
    Ok ()
  else
    Error "expected insert existing key to return old value without changing length"

let test_get_existing = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); ] in
  if HashMap.get map ~key:1 = Some "a" then
    Ok ()
  else
    Error "expected get existing key = Some value"

let test_get_missing = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); ] in
  if HashMap.get map ~key:2 = None then
    Ok ()
  else
    Error "expected get missing key = None"

let test_remove_existing = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); ] in
  let removed = HashMap.remove map ~key:1 in
  if
    Option.is_some removed && Int.equal (HashMap.length map) 0 && HashMap.get map ~key:1 = None
  then
    Ok ()
  else
    Error "expected remove existing key to delete entry"

let test_remove_missing = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); ] in
  if HashMap.remove map ~key:2 = None then
    Ok ()
  else
    Error "expected remove missing key = None"

let test_has_key = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); ] in
  if HashMap.has_key map ~key:1 && not (HashMap.has_key map ~key:2) then
    Ok ()
  else
    Error "expected has_key to reflect membership"

let test_length_after_overwrite = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); ] in
  ignore (HashMap.insert map ~key:1 ~value:"b");
  if Int.equal (HashMap.length map) 1 then
    Ok ()
  else
    Error "expected length to count distinct keys"

let test_clear = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); (2, "b"); ] in
  HashMap.clear map;
  if HashMap.is_empty map && HashMap.to_list map = [] then
    Ok ()
  else
    Error "expected clear to remove all entries"

let test_keys = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); (2, "b"); ] in
  if sort_ints (HashMap.keys map) = [ 1; 2 ] then
    Ok ()
  else
    Error "expected keys to list every key exactly once"

let test_values = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); (2, "b"); ] in
  if List.sort (HashMap.values map) ~compare:String.compare = [ "a"; "b" ] then
    Ok ()
  else
    Error "expected values to list every current value"

let test_for_each = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); (2, "b"); ] in
  let seen = box [] in
  HashMap.for_each map ~fn:(fun key value -> seen.value <- (key, value) :: seen.value);
  if sort_pairs seen.value = [ (1, "a"); (2, "b"); ] then
    Ok ()
  else
    Error "expected for_each to visit each entry exactly once"

let test_fold_left = fun _ctx ->
  let map = HashMap.from_list [ (1, 2); (2, 3); ] in
  if Int.equal (HashMap.fold_left map ~init:0 ~fn:(fun acc _key value -> acc + value)) 5 then
    Ok ()
  else
    Error "expected fold_left to accumulate over all entries"

let test_to_list_roundtrip = fun _ctx ->
  let original = HashMap.from_list [ (1, "a"); (2, "b"); ] in
  let rebuilt = HashMap.from_list (HashMap.to_list original) in
  if sort_pairs (HashMap.to_list rebuilt) = [ (1, "a"); (2, "b"); ] then
    Ok ()
  else
    Error "expected to_list/from_list to preserve key/value mapping"

let test_entry_missing = fun _ctx ->
  let map = HashMap.create () in
  match HashMap.entry map ~key:1 with
  | HashMap.Vacant -> Ok ()
  | HashMap.Occupied _ -> Error "expected missing entry to be Vacant"

let test_entry_existing = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); ] in
  match HashMap.entry map ~key:1 with
  | HashMap.Occupied "a" -> Ok ()
  | _ -> Error "expected existing entry to be Occupied"

let test_compute_insert = fun _ctx ->
  let map = HashMap.create () in
  let previous = HashMap.compute map ~key:1 ~fn:(fun value -> HashMap.Insert ("a", value)) in
  if previous = None && HashMap.get map ~key:1 = Some "a" then
    Ok ()
  else
    Error "expected compute insert to return previous value and insert"

let test_compute_remove = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); ] in
  let removed =
    HashMap.compute
      map
      ~key:1
      ~fn:(fun value ->
        match value with
        | None -> HashMap.Abort None
        | Some existing -> HashMap.Remove (Some existing))
  in
  if removed = Some "a" && HashMap.get map ~key:1 = None && HashMap.is_empty map then
    Ok ()
  else
    Error "expected compute remove to return removed value and delete"

let test_compute_abort = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); ] in
  let value = HashMap.compute map ~key:1 ~fn:(fun current -> HashMap.Abort current) in
  if value = Some "a" && HashMap.get map ~key:1 = Some "a" then
    Ok ()
  else
    Error "expected compute abort to return current value without mutation"

let test_iter = fun _ctx ->
  let items = Iterator.to_list (HashMap.iter (HashMap.from_list [ (1, "a"); (2, "b"); ])) in
  if sort_pairs items = [ (1, "a"); (2, "b"); ] then
    Ok ()
  else
    Error "expected iter to yield every entry once"

let test_mut_iter_after_removals = fun _ctx ->
  let map = HashMap.from_list [ (1, "a"); (2, "b"); (3, "c"); ] in
  ignore (HashMap.remove map ~key:2);
  let items = MutIterator.to_list (HashMap.mut_iter map) in
  if sort_pairs items = [ (1, "a"); (3, "c"); ] then
    Ok ()
  else
    Error "expected mut_iter to yield only live entries"

let tests =
  Test.[
    case "HashMap.create starts empty" test_create;
    case "HashMap.with_capacity starts empty" test_with_capacity;
    case "HashMap.from_list preserves unique pairs" test_from_list_unique;
    case "HashMap.from_list keeps the last duplicate value" test_from_list_duplicate_keeps_last;
    case "HashMap.insert new key returns None and increments length" test_insert_new;
    case "HashMap.insert existing key returns old value" test_insert_existing;
    case "HashMap.get existing key returns Some" test_get_existing;
    case "HashMap.get missing key returns None" test_get_missing;
    case "HashMap.remove existing key deletes entry" test_remove_existing;
    case "HashMap.remove missing key returns None" test_remove_missing;
    case "HashMap.has_key reflects membership" test_has_key;
    case "HashMap.length counts distinct keys" test_length_after_overwrite;
    case "HashMap.clear removes all entries" test_clear;
    case "HashMap.keys lists every key once" test_keys;
    case "HashMap.values lists every current value" test_values;
    case "HashMap.for_each visits each entry once" test_for_each;
    case "HashMap.fold_left accumulates values" test_fold_left;
    case "HashMap.to_list and from_list roundtrip the mapping" test_to_list_roundtrip;
    case "HashMap.entry missing key is Vacant" test_entry_missing;
    case "HashMap.entry existing key is Occupied" test_entry_existing;
    case "HashMap.compute can insert from current value" test_compute_insert;
    case "HashMap.compute can remove from current value" test_compute_remove;
    case "HashMap.compute can abort without mutation" test_compute_abort;
    case "HashMap.iter yields every entry once" test_iter;
    case "HashMap.mut_iter yields only live entries" test_mut_iter_after_removals;
  ]

let main ~args = Test.Cli.main ~name:"hashmap" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
