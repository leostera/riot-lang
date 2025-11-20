open Std
open Std.Collections

type state = {
  graph : Poneglyph.t;
  packages : (string, Model.Package_info.t) HashMap.t;
  mutable last_tx_id : UUID.t;
}

(** Main server loop - dispatches to handler functions *)
let rec loop state =
  let selector msg =
    match msg with
    | Messages.CodeDbRequest req -> `select req
    | _ -> `skip
  in

  match receive ~selector () with
  | Messages.AddPackage data -> handle_add_package state data
  | Messages.AddModule data -> handle_add_module state data
  | Messages.GetSymbol data -> handle_get_symbol state data

(** Handler for AddPackage message *)
and handle_add_package state (data : Messages.add_package) =
  Log.debug
    (String.concat ""
       [ "[CodeDB] Adding package "; Model.Package_name.to_string data.package_name ]);
  let _ =
    HashMap.insert state.packages
      (Model.Package_name.to_string data.package_name)
      (Model.Package_info.make ~name:data.package_name ~path:data.package_path)
  in
  loop state

(** Handler for AddModule message *)
and handle_add_module state (data : Messages.add_module) =
  Log.debug
    (String.concat ""
       [
         "[CodeDB] Adding module ";
         Model.Module_name.to_string data.module_name;
         " from ";
         Path.to_string data.source_file;
       ]);

  (* Look up the package info *)
  let package_info =
    match HashMap.get state.packages (Model.Package_name.to_string data.package_name) with
    | Some pkg -> pkg
    | None ->
        Log.warn
          (String.concat ""
             [
               "[CodeDB] Package not found: ";
               Model.Package_name.to_string data.package_name;
               ", creating placeholder";
             ]);
        Model.Package_info.make ~name:data.package_name ~path:(Path.v ".")
  in

  (* Read source file and compute SHA256 *)
  let full_source_path = Path.(package_info.path / data.source_file) in
  let content, source_sha256 =
    match Fs.read_to_string full_source_path with
    | Ok content ->
        let hash = Crypto.Sha256.hash_string content in
        let sha256 = Crypto.Digest.hex hash in
        (content, sha256)
    | Error _err ->
        let err_msg =
          String.concat ""
            [
              "[CodeDB] FATAL: Could not read source file: ";
              Path.to_string full_source_path;
              " (package=";
              Model.Package_name.to_string data.package_name;
              ", source=";
              Path.to_string data.source_file;
              ")";
            ]
        in
        Log.error err_msg;
        panic err_msg
  in

  (* Create File entity *)
  let file =
    Model.File.make ~path:full_source_path ~sha256:source_sha256
      ~size:(String.length content) ()
  in

  (* Create files record - determine if it's implementation or interface based on extension *)
  let files = 
    match Path.extension full_source_path with
    | Some ".mli" -> Model.Symbol.{ implementation = None; interface = Some file }
    | _ -> Model.Symbol.{ implementation = Some file; interface = None }
  in

  (* Create Symbol entity with files *)
  let symbol =
    Model.Symbol.make ~kind:Model.Symbol.Module ~name:data.module_name
      ~package:package_info ~files
  in

  (* Generate new transaction ID using UUIDv7 for time-ordering *)
  let tx_id = UUID.v7_monotonic () in

  (* Convert to facts and state them (includes file facts + relationship) *)
  let facts = Model.Symbol.to_facts ~tx_id symbol in
  let _ = Poneglyph.state state.graph facts in

  (* Update last_tx_id so queries can see the new data *)
  state.last_tx_id <- tx_id;

  loop state

(** Handler for GetSymbol message *)
and handle_get_symbol state (data : Messages.get_symbol) =
  let name_str =
    match data.sym with
    | Model.Symbol.Module n -> Model.Module_name.to_string n
    | Model.Symbol.Value n -> Model.Value_name.to_string n
    | Model.Symbol.Type n -> Model.Type_name.to_string n
    | Model.Symbol.Interface n -> Model.Module_name.to_string n
  in
  Log.debug (String.concat "" [ "[CodeDB] Getting symbol "; name_str ]);
  (* TODO: Use last_tx_id for consistent reads once Query API supports it *)
  let result = Model.Query.get_symbol state.graph data.sym in
  send data.caller
    (Messages.CodeDbResponse (Messages.GetSymbolResponse { ref = data.ref; result }));
  loop state

let start_link ?(data_dir = ".codedb.pone") () =
  spawn_link (fun () ->
    Log.info "[CodeDB] Server starting";
    
    let graph = match Poneglyph.open_exclusive ~data_dir () with
      | Error e ->
          Log.error ("[CodeDB] Failed to open database: " ^ e);
          panic ("Failed to open Poneglyph database: " ^ e)
      | Ok g ->
          Log.info ("[CodeDB] Opened database: " ^ data_dir);
          g
    in
    
    let state = {
      graph;
      packages = HashMap.create ();
      last_tx_id = UUID.v7_monotonic ();
    } in
    Log.info "[CodeDB] Server ready";
    loop state
  )
