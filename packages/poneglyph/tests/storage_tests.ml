(** Tests for storage backends - inmemory and file *)

open Std
open Poneglyph

let test_inmemory_storage () =
  let graph = create () in

  let entity = Uri.of_string "test:entity:1" in
  let attr = Uri.of_string "test:value" in
  let source = Uri.of_string "test:source:storage-test" in

  (* State a fact *)
  let facts =
    [
      Fact.make ~source ~entity ~attribute:attr ~value:(Fact.Int 42)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
    ]
  in

  let tx_id = state graph facts in
  if tx_id <= 0 then
    Error "Transaction ID should be positive"
  else
    (* Retrieve it *)
    match get graph ~entity ~attr with
    | Some (Fact.Int 42) ->
      (* Check existence *)
      if not (exists graph entity) then
        Error "Entity should exist"
      else if exists graph (Uri.of_string "test:nonexistent") then
        Error "Nonexistent entity should not exist"
      else
        Ok ()
    | _ -> Error "Expected Int 42"

let test_file_storage () =
  let db_path = "/tmp/poneglyph_test_storage.db" in
  (* Clean up first if file exists *)
  let _ = match Fs.exists (Path.v db_path) with
    | Ok true -> Fs.remove_file (Path.v db_path)
    | _ -> Ok ()
  in

  (* Create and populate *)
  let graph = create_persistent db_path in

  let entity = Uri.of_string "test:entity:persistent" in
  let attr = Uri.of_string "test:data" in
  let source = Uri.of_string "test:source:persistence-test" in

  let facts =
    [
      Fact.make ~source ~entity ~attribute:attr ~value:(Fact.String "persisted")
        ~stated_at:(Datetime.now ()) ~tx_id:0;
    ]
  in

  let _ = state graph facts in
  
  (* Check the file was created *)
  let file_exists = match Fs.exists (Path.v db_path) with
    | Ok true -> true
    | _ -> false
  in
  
  if not file_exists then
    Error "File was not created after stating facts"
  else
    (* Read the file content to debug *)
    let file_content = match Fs.read (Path.v db_path) with
      | Ok content -> content
      | Error _ -> ""
    in
    
    (* Load in new graph *)
    let loaded = load db_path in
    
    (* Check if facts were loaded at all *)
    let loaded_facts = get_all_facts loaded ~entity in

    let cleanup () = let _ = Fs.remove_file (Path.v db_path) in () in
    
    if List.length loaded_facts = 0 then begin
      cleanup ();
      Error ("No facts loaded from file. File content: " ^ String.sub file_content 0 (min 100 (String.length file_content)))
    end else
      match get loaded ~entity ~attr with
      | Some (Fact.String "persisted") ->
        if not (exists loaded entity) then begin
          cleanup ();
          Error "Entity should exist after loading"
        end else begin
          cleanup ();
          Ok ()
        end
      | Some v ->
        cleanup ();
        Error ("Wrong value type loaded")
      | None ->
        cleanup ();
        Error "Data was not persisted correctly"

let test_retraction () =
  let graph = create () in

  let entity = Uri.of_string "test:entity:retract" in
  let attr = Uri.of_string "test:value" in
  let source = Uri.of_string "test:source:retraction-test" in

  let facts =
    [
      Fact.make ~source ~entity ~attribute:attr ~value:(Fact.Int 100)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
    ]
  in

  let _ = state graph facts in

  (* Fact should exist *)
  match get graph ~entity ~attr with
  | Some (Fact.Int 100) ->
    (* Get the fact URI *)
    let all_facts = get_all_facts graph ~entity in
    if List.length all_facts = 0 then
      Error "Should have at least one fact"
    else
      let fact = List.hd all_facts in

      (* Retract it *)
      retract graph ~fact_uri:fact.Fact.fact_uri;

      (* Should no longer be current *)
      (match get graph ~entity ~attr with
      | None ->
        (* But should still be in history *)
        let all = get_all_facts graph ~entity in
        if List.length all != 1 then
          Error "Should have exactly one fact in history"
        else if not (List.hd all).Fact.retracted then
          Error "Fact should be marked as retracted"
        else
          Ok ()
      | _ -> Error "Fact should be None after retraction")
  | _ -> Error "Fact should exist before retraction"

let tests =
  Test.[
    case "Inmemory storage" test_inmemory_storage;
    case "File storage" test_file_storage;
    case "Retraction" test_retraction;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"poneglyph/storage" ~tests ~args)
    ~args:Env.args ()
