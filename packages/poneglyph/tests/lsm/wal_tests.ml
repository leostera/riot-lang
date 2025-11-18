open Std
open Poneglyph.Storage.Lsm
open Propane

(* Define let* operator for Gen monad *)
let (let*) = Generator.and_then

let () = Random.init 42

let setup_test_dir () =
  let test_dir = "/tmp/wal_test_" ^ string_of_int (Random.int 1000000) in
  ignore (Fs.create_dir_all (Path.v test_dir));
  test_dir

let cleanup_test_dir dir = ignore (Fs.remove_dir_all (Path.v dir))

let make_test_path dir name = dir ^ "/" ^ name ^ ".wal"

module Bytes = Kernel.IO.Bytes

let make_key entity_id_int attr_id_int value_int =
  (* Create a simple EAVT key for testing *)
  let key : Key.eavt_key = {
    entity_id = Int64.of_int entity_id_int;
    attr_id = Int64.of_int attr_id_int;
    value_kind = Encoding.VK_Int;
    value_repr = Int64.of_int value_int;
    tx_id = 1L;
    fact_id = 1L;
  } in
  Key.encode_eavt key

let make_string_value s =
  Bytes.of_string s

let make_int_value i =
  Bytes.of_string (string_of_int i)

(* ============================= Unit Tests ============================= *)

let test_create_new_wal () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_create" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      match Wal.close wal with
      | Error err ->
          cleanup_test_dir dir;
          Error ("close failed: " ^ err)
      | Ok () ->
          cleanup_test_dir dir;
          Ok ())

let test_append_single_entry () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_append_single" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let key = make_key 1 1 100 in
      let value = make_string_value "Alice" in

      match Wal.append wal ~key ~value with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append: " ^ err)
      | Ok () -> (
          match Wal.close wal with
          | Error err ->
              cleanup_test_dir dir;
              Error ("close: " ^ err)
          | Ok () ->
              cleanup_test_dir dir;
              Ok ()))

let test_replay_single () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_replay_single" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let key = make_key 1 1 100 in
      let value = make_string_value "Alice" in

      match Wal.append wal ~key ~value with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append: " ^ err)
      | Ok () -> (
          match Wal.close wal with
          | Error err ->
              cleanup_test_dir dir;
              Error ("close: " ^ err)
          | Ok () -> (
              match Wal.open_existing ~path with
              | Error err ->
                  cleanup_test_dir dir;
                  Error ("reopen: " ^ err)
              | Ok wal -> (
                  match Wal.replay wal with
                  | Error err ->
                      ignore (Wal.close wal);
                      cleanup_test_dir dir;
                      Error ("replay: " ^ err)
                  | Ok entries -> (
                      if List.length entries != 1 then (
                        ignore (Wal.close wal);
                        cleanup_test_dir dir;
                        Error "expected 1 entry")
                      else
                        match List.hd entries with
                        | Wal.Put (k, v) ->
                            if not (Bytes.equal k key) then (
                              ignore (Wal.close wal);
                              cleanup_test_dir dir;
                              Error "key mismatch")
                            else if not (Bytes.equal v value) then (
                              ignore (Wal.close wal);
                              cleanup_test_dir dir;
                              Error "value mismatch")
                            else (
                              ignore (Wal.close wal);
                              cleanup_test_dir dir;
                              Ok ())
                        | Wal.Delete _ ->
                            ignore (Wal.close wal);
                            cleanup_test_dir dir;
                            Error "expected Put, got Delete")))))

let test_append_multiple_entries () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_append_multiple" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let entries =
        [
          (make_key 1 1 100, make_string_value "Alice");
          (make_key 2 2 100, make_int_value 30);
          (make_key 3 3 100, make_string_value "NYC");
        ]
      in

      let rec append_all entries =
        match entries with
        | [] -> Ok ()
        | (key, value) :: rest -> (
            match Wal.append wal ~key ~value with
            | Error err -> Error err
            | Ok () -> append_all rest)
      in

      match append_all entries with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append: " ^ err)
      | Ok () -> (
          match Wal.close wal with
          | Error err ->
              cleanup_test_dir dir;
              Error ("close: " ^ err)
          | Ok () ->
              cleanup_test_dir dir;
              Ok ()))

let test_replay_multiple () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_replay_multiple" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let entries =
        [
          (make_key 1 1 100, make_string_value "Alice");
          (make_key 2 2 100, make_int_value 30);
          (make_key 3 3 100, make_string_value "NYC");
        ]
      in

      let rec append_all entries =
        match entries with
        | [] -> Ok ()
        | (key, value) :: rest -> (
            match Wal.append wal ~key ~value with
            | Error err -> Error err
            | Ok () -> append_all rest)
      in

      match append_all entries with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append: " ^ err)
      | Ok () -> (
          match Wal.close wal with
          | Error err ->
              cleanup_test_dir dir;
              Error ("close: " ^ err)
          | Ok () -> (
              match Wal.open_existing ~path with
              | Error err ->
                  cleanup_test_dir dir;
                  Error ("reopen: " ^ err)
              | Ok wal -> (
                  match Wal.replay wal with
                  | Error err ->
                      ignore (Wal.close wal);
                      cleanup_test_dir dir;
                      Error ("replay: " ^ err)
                  | Ok replayed ->
                      if List.length replayed != List.length entries then (
                        ignore (Wal.close wal);
                        cleanup_test_dir dir;
                        Error ("expected " ^ string_of_int (List.length entries) ^
                               " entries, got " ^ string_of_int (List.length replayed)))
                      else (
                        ignore (Wal.close wal);
                        cleanup_test_dir dir;
                        Ok ())))))

let test_append_delete () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_append_delete" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let key = make_key 1 1 100 in

      match Wal.append_delete wal ~key with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append: " ^ err)
      | Ok () -> (
          match Wal.close wal with
          | Error err ->
              cleanup_test_dir dir;
              Error ("close: " ^ err)
          | Ok () ->
              cleanup_test_dir dir;
              Ok ()))

let test_replay_delete () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_replay_delete" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let key = make_key 1 1 100 in

      match Wal.append_delete wal ~key with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append: " ^ err)
      | Ok () -> (
          match Wal.close wal with
          | Error err ->
              cleanup_test_dir dir;
              Error ("close: " ^ err)
          | Ok () -> (
              match Wal.open_existing ~path with
              | Error err ->
                  cleanup_test_dir dir;
                  Error ("reopen: " ^ err)
              | Ok wal -> (
                  match Wal.replay wal with
                  | Error err ->
                      ignore (Wal.close wal);
                      cleanup_test_dir dir;
                      Error ("replay: " ^ err)
                  | Ok entries -> (
                      if List.length entries != 1 then (
                        ignore (Wal.close wal);
                        cleanup_test_dir dir;
                        Error "expected 1 entry")
                      else
                        match List.hd entries with
                        | Wal.Delete k ->
                            if not (Bytes.equal k key) then (
                              ignore (Wal.close wal);
                              cleanup_test_dir dir;
                              Error "key mismatch")
                            else (
                              ignore (Wal.close wal);
                              cleanup_test_dir dir;
                              Ok ())
                        | Wal.Put _ ->
                            ignore (Wal.close wal);
                            cleanup_test_dir dir;
                            Error "expected Delete, got Put")))))

let test_mixed_operations () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_mixed" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let key1 = make_key 1 1 100 in
      let key2 = make_key 2 2 100 in
      let value1 = make_string_value "Alice" in

      match Wal.append wal ~key:key1 ~value:value1 with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append1: " ^ err)
      | Ok () -> (
          match Wal.append_delete wal ~key:key2 with
          | Error err ->
              ignore (Wal.close wal);
              cleanup_test_dir dir;
              Error ("append2: " ^ err)
          | Ok () -> (
              match Wal.close wal with
              | Error err ->
                  cleanup_test_dir dir;
                  Error ("close: " ^ err)
              | Ok () -> (
                  match Wal.open_existing ~path with
                  | Error err ->
                      cleanup_test_dir dir;
                      Error ("reopen: " ^ err)
                  | Ok wal -> (
                      match Wal.replay wal with
                      | Error err ->
                          ignore (Wal.close wal);
                          cleanup_test_dir dir;
                          Error ("replay: " ^ err)
                      | Ok entries ->
                          if List.length entries != 2 then (
                            ignore (Wal.close wal);
                            cleanup_test_dir dir;
                            Error "expected 2 entries")
                          else (
                            ignore (Wal.close wal);
                            cleanup_test_dir dir;
                            Ok ()))))))

let test_truncate () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_truncate" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let key = make_key 1 1 100 in
      let value = make_string_value "Alice" in

      match Wal.append wal ~key ~value with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append: " ^ err)
      | Ok () -> (
          match Wal.truncate wal with
          | Error err ->
              ignore (Wal.close wal);
              cleanup_test_dir dir;
              Error ("truncate: " ^ err)
          | Ok () -> (
              match Wal.replay wal with
              | Error err ->
                  ignore (Wal.close wal);
                  cleanup_test_dir dir;
                  Error ("replay: " ^ err)
              | Ok entries ->
                  if List.length entries != 0 then (
                    ignore (Wal.close wal);
                    cleanup_test_dir dir;
                    Error "expected 0 entries after truncate")
                  else (
                    ignore (Wal.close wal);
                    cleanup_test_dir dir;
                    Ok ()))))

let test_replay_empty () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_replay_empty" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      match Wal.replay wal with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("replay: " ^ err)
      | Ok entries ->
          if List.length entries != 0 then (
            ignore (Wal.close wal);
            cleanup_test_dir dir;
            Error "expected 0 entries")
          else (
            ignore (Wal.close wal);
            cleanup_test_dir dir;
            Ok ()))

let test_replay_preserves_order () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_order" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let entries =
        [
          (make_key 1 1 100, make_string_value "Alice");
          (make_key 2 2 100, make_int_value 30);
          (make_key 3 3 100, make_string_value "NYC");
        ]
      in

      let rec append_all entries =
        match entries with
        | [] -> Ok ()
        | (key, value) :: rest -> (
            match Wal.append wal ~key ~value with
            | Error err -> Error err
            | Ok () -> append_all rest)
      in

      match append_all entries with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append: " ^ err)
      | Ok () -> (
          match Wal.close wal with
          | Error err ->
              cleanup_test_dir dir;
              Error ("close: " ^ err)
          | Ok () -> (
              match Wal.open_existing ~path with
              | Error err ->
                  cleanup_test_dir dir;
                  Error ("reopen: " ^ err)
              | Ok wal -> (
                  match Wal.replay wal with
                  | Error err ->
                      ignore (Wal.close wal);
                      cleanup_test_dir dir;
                      Error ("replay: " ^ err)
                  | Ok replayed ->
                      let rec check_order original replayed =
                        match (original, replayed) with
                        | [], [] -> true
                        | (k1, v1) :: rest1, Wal.Put (k2, v2) :: rest2 ->
                            if Bytes.equal k1 k2 && Bytes.equal v1 v2 then
                              check_order rest1 rest2
                            else false
                        | _ -> false
                      in
                      if not (check_order entries replayed) then (
                        ignore (Wal.close wal);
                        cleanup_test_dir dir;
                        Error "order mismatch")
                      else (
                        ignore (Wal.close wal);
                        cleanup_test_dir dir;
                        Ok ())))))

(* ============================= Batch Atomicity Tests ============================= *)

let test_append_batch_empty () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_batch_empty" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      match Wal.append_batch wal [] with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append_batch: " ^ err)
      | Ok () -> (
          match Wal.close wal with
          | Error err ->
              cleanup_test_dir dir;
              Error ("close: " ^ err)
          | Ok () ->
              cleanup_test_dir dir;
              Ok ()))

let test_append_batch_single () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_batch_single" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let key = make_key 1 1 100 in
      let value = make_string_value "Alice" in
      let entries = [Wal.Put (key, value)] in

      match Wal.append_batch wal entries with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append_batch: " ^ err)
      | Ok () -> (
          match Wal.close wal with
          | Error err ->
              cleanup_test_dir dir;
              Error ("close: " ^ err)
          | Ok () ->
              cleanup_test_dir dir;
              Ok ()))

let test_append_batch_multiple () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_batch_multiple" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let entries = [
        Wal.Put (make_key 1 1 100, make_string_value "Alice");
        Wal.Put (make_key 2 2 100, make_int_value 30);
        Wal.Delete (make_key 3 3 100);
        Wal.Put (make_key 4 4 100, make_string_value "NYC");
      ] in

      match Wal.append_batch wal entries with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append_batch: " ^ err)
      | Ok () -> (
          match Wal.close wal with
          | Error err ->
              cleanup_test_dir dir;
              Error ("close: " ^ err)
          | Ok () ->
              cleanup_test_dir dir;
              Ok ()))

let test_batch_replay_atomicity () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_batch_replay" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let entries = [
        Wal.Put (make_key 1 1 100, make_string_value "Alice");
        Wal.Put (make_key 2 2 100, make_int_value 30);
        Wal.Delete (make_key 3 3 100);
      ] in

      match Wal.append_batch wal entries with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append_batch: " ^ err)
      | Ok () -> (
          match Wal.close wal with
          | Error err ->
              cleanup_test_dir dir;
              Error ("close: " ^ err)
          | Ok () -> (
              match Wal.open_existing ~path with
              | Error err ->
                  cleanup_test_dir dir;
                  Error ("reopen: " ^ err)
              | Ok wal -> (
                  match Wal.replay wal with
                  | Error err ->
                      ignore (Wal.close wal);
                      cleanup_test_dir dir;
                      Error ("replay: " ^ err)
                  | Ok replayed ->
                      if List.length replayed != List.length entries then (
                        ignore (Wal.close wal);
                        cleanup_test_dir dir;
                        Error ("expected " ^ string_of_int (List.length entries) ^
                               " entries, got " ^ string_of_int (List.length replayed)))
                      else (
                        ignore (Wal.close wal);
                        cleanup_test_dir dir;
                        Ok ())))))

let test_tagged_batch_basic () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_tagged_batch" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let entries = [
        Wal.TaggedPut (Wal.EAVT, make_key 1 1 100, make_string_value "Alice");
        Wal.TaggedPut (Wal.AVET, make_key 2 2 100, make_int_value 30);
        Wal.TaggedPut (Wal.FACT, make_key 3 3 100, make_string_value "fact");
        Wal.TaggedDelete (Wal.SOURCE, make_key 4 4 100);
      ] in

      match Wal.append_batch_tagged wal entries with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append_batch_tagged: " ^ err)
      | Ok () -> (
          match Wal.close wal with
          | Error err ->
              cleanup_test_dir dir;
              Error ("close: " ^ err)
          | Ok () ->
              cleanup_test_dir dir;
              Ok ()))

let test_tagged_batch_replay () =
  let dir = setup_test_dir () in
  let path = make_test_path dir "test_tagged_replay" in

  match Wal.create ~path with
  | Error err ->
      cleanup_test_dir dir;
      Error err
  | Ok wal -> (
      let entries = [
        Wal.TaggedPut (Wal.EAVT, make_key 1 1 100, make_string_value "Alice");
        Wal.TaggedPut (Wal.AVET, make_key 2 2 100, make_int_value 30);
        Wal.TaggedPut (Wal.FACT, make_key 3 3 100, make_string_value "fact");
        Wal.TaggedDelete (Wal.SOURCE, make_key 4 4 100);
      ] in

      match Wal.append_batch_tagged wal entries with
      | Error err ->
          ignore (Wal.close wal);
          cleanup_test_dir dir;
          Error ("append_batch_tagged: " ^ err)
      | Ok () -> (
          match Wal.close wal with
          | Error err ->
              cleanup_test_dir dir;
              Error ("close: " ^ err)
          | Ok () -> (
              match Wal.open_existing ~path with
              | Error err ->
                  cleanup_test_dir dir;
                  Error ("reopen: " ^ err)
              | Ok wal -> (
                  match Wal.replay_tagged wal with
                  | Error err ->
                      ignore (Wal.close wal);
                      cleanup_test_dir dir;
                      Error ("replay_tagged: " ^ err)
                  | Ok replayed ->
                      if List.length replayed != List.length entries then (
                        ignore (Wal.close wal);
                        cleanup_test_dir dir;
                        Error ("expected " ^ string_of_int (List.length entries) ^
                               " entries, got " ^ string_of_int (List.length replayed)))
                      else (
                        (* Verify tags are preserved *)
                        let check_tags = List.for_all2 (fun orig repl ->
                          match orig, repl with
                          | Wal.TaggedPut (t1, _, _), Wal.TaggedPut (t2, _, _) -> t1 = t2
                          | Wal.TaggedDelete (t1, _), Wal.TaggedDelete (t2, _) -> t1 = t2
                          | _ -> false
                        ) entries replayed in
                        if not check_tags then (
                          ignore (Wal.close wal);
                          cleanup_test_dir dir;
                          Error "tag mismatch")
                        else (
                          ignore (Wal.close wal);
                          cleanup_test_dir dir;
                          Ok ()))))))


(* ============================= Property Tests ============================= *)

(* Generators for WAL entries *)
let arb_key = Arbitrary.int64
let arb_value = Arbitrary.(map Bytes.of_string Bytes.to_string string)

let arb_wal_entry =
  Arbitrary.map
    (fun (e_id, a_id, v_int) ->
      let key = make_key e_id a_id v_int in
      let value = make_string_value ("val" ^ string_of_int v_int) in
      (key, value))
    (fun (_key, _value) -> (0, 0, 0))  (* Dummy inverse - we don't shrink *)
    Arbitrary.(triple int int int)

let prop_append_replay_roundtrip =
  property "WAL replay returns all appended entries"
    Arbitrary.(list arb_wal_entry)
    (fun entries ->
      if List.length entries = 0 then assume_fail ();
      
      let dir = setup_test_dir () in
      let path = make_test_path dir "prop_roundtrip" in
      
      match Wal.create ~path with
      | Error _ ->
          cleanup_test_dir dir;
          assume_fail ()
      | Ok wal ->
          let rec append_all es =
            match es with
            | [] -> Ok ()
            | (key, value) :: rest ->
                match Wal.append wal ~key ~value with
                | Error _ -> Error ()
                | Ok () -> append_all rest
          in
          
          match append_all entries with
          | Error _ ->
              ignore (Wal.close wal);
              cleanup_test_dir dir;
              assume_fail ()
          | Ok () ->
              match Wal.close wal with
              | Error _ ->
                  cleanup_test_dir dir;
                  assume_fail ()
              | Ok () ->
                  match Wal.open_existing ~path with
                  | Error _ ->
                      cleanup_test_dir dir;
                      assume_fail ()
                  | Ok wal2 ->
                      match Wal.replay wal2 with
                      | Error _ ->
                          ignore (Wal.close wal2);
                          cleanup_test_dir dir;
                          false
                      | Ok replayed ->
                          ignore (Wal.close wal2);
                          cleanup_test_dir dir;
                          List.length replayed = List.length entries)

(* ============================= Test Suite ============================= *)

let tests =
  Test.[
    case "Create new WAL" test_create_new_wal;
    case "Append single entry" test_append_single_entry;
    case "Replay single entry" test_replay_single;
    case "Append multiple entries" test_append_multiple_entries;
    case "Replay multiple entries" test_replay_multiple;
    case "Append delete" test_append_delete;
    case "Replay delete" test_replay_delete;
    case "Mixed operations" test_mixed_operations;
    case "Truncate" test_truncate;
    case "Replay empty" test_replay_empty;
    case "Replay preserves order" test_replay_preserves_order;
    case "Append batch empty" test_append_batch_empty;
    case "Append batch single" test_append_batch_single;
    case "Append batch multiple" test_append_batch_multiple;
    case "Batch replay atomicity" test_batch_replay_atomicity;
    case "Tagged batch basic" test_tagged_batch_basic;
    case "Tagged batch replay" test_tagged_batch_replay;
    prop_append_replay_roundtrip;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"poneglyph/lsm/wal" ~tests ~args)
    ~args:Env.args ()
