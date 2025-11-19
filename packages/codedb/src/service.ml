open Std

(** Message types for internal server *)
type Message.t +=
  | IndexFile of Path.t
  | MarkFileDeleted of Path.t
  | RegisterWatcher of Pid.t

(** Internal server - Main server loop for handling indexing requests *)
let start_internal_server config =
  (* Open Poneglyph database *)
  let db_path = Config.db_path config in
  let db =
    Poneglyph.open_exclusive ~data_dir:(Path.to_string db_path) ()
    |> Result.expect ~msg:"Failed to open Poneglyph database"
  in

  (* Index workspace *)
  Indexer.Graph_indexer.index_workspace ~config ~db;

  (* Message loop - handle file change events *)
  let rec loop () =
    let selector = function
      | IndexFile _ | MarkFileDeleted _ | RegisterWatcher _ as msg -> `select msg
      | _ -> `skip
    in
    match receive ~selector () with
    | IndexFile file_path ->
        Indexer.Graph_indexer.index_file ~config ~db ~file_path;
        loop ()
    | MarkFileDeleted file_path ->
        Indexer.Graph_indexer.mark_file_deleted ~config ~db ~file_path;
        loop ()
    | RegisterWatcher _watcher_pid ->
        (* File watcher registered - just acknowledge *)
        loop ()
    | _ -> loop ()
  in
  loop ()

(** File watcher service - monitors file changes *)
let start_file_watcher config internal_server_pid =
  if not (Config.watch config) then (
    println "\n🔍 File watcher: disabled\n";
    let rec loop () =
      sleep (Time.Duration.from_secs 3600);
      loop ()
    in
    loop ())
  else
    try
      let workspace_root = Config.workspace_root config in
      let packages_path = Path.(workspace_root / v "packages") in
      
      println "\n🔍 File watcher: monitoring packages/ for changes\n";
      
      let _watcher = 
        Fs.FileWatcher.start_link ~root:workspace_root ()
      in
      
      (* Register with internal server *)
      send internal_server_pid (RegisterWatcher (self ()));
      
      println "🔍 File watcher: waiting for events...\n";
      
      (* Forward file events to internal server *)
      let rec loop () =
        let selector = function
          | Fs.FileWatcher.FileEvents events -> `select events
          | _ -> `skip
        in
        
        match receive ~selector () with
        | events ->
            (* Filter and forward relevant events *)
            let event_count = List.length events in
            if event_count > 0 then
              Log.debug (String.concat "" [ "Received "; Int.to_string event_count; " file events" ]);
            
            List.iter (fun (event : Fs.Event.t) ->
              let path_str = Path.to_string event.path in
              let workspace_root_str = Path.to_string workspace_root in
              
              (* Compute relative path for display *)
              let relative_path = 
                if String.starts_with ~prefix:workspace_root_str path_str then
                  let prefix_len = String.length workspace_root_str in
                  let relative = String.sub path_str prefix_len (String.length path_str - prefix_len) in
                  (* Remove leading slash if present *)
                  if String.length relative > 0 && String.get relative 0 = '/' then
                    String.sub relative 1 (String.length relative - 1)
                  else relative
                else path_str
              in
              
              let packages_path_str = Path.to_string packages_path in
              
              (* Only process files in packages/ directory *)
              if String.starts_with ~prefix:packages_path_str path_str then
                (* Check if this is a relevant source file *)
                let is_relevant =
                  (* OCaml source files *)
                  String.ends_with ~suffix:".ml" path_str ||
                  String.ends_with ~suffix:".mli" path_str ||
                  (* Build configuration *)
                  String.ends_with ~suffix:"tusk.toml" path_str ||
                  (* Native code *)
                  String.ends_with ~suffix:".c" path_str ||
                  String.ends_with ~suffix:".h" path_str ||
                  String.ends_with ~suffix:".cpp" path_str ||
                  String.ends_with ~suffix:".hpp" path_str ||
                  (* Rust FFI *)
                  String.ends_with ~suffix:".rs" path_str ||
                  (* Build scripts *)
                  String.ends_with ~suffix:"Cargo.toml" path_str
                in
                
                if is_relevant then
                  match event.kind with
                  | Fs.Event.Created | Fs.Event.Modified ->
                      println (String.concat "" [ "📝 File changed: "; relative_path ]);
                      send internal_server_pid (IndexFile event.path)
                  | Fs.Event.Deleted ->
                      println (String.concat "" [ "🗑️  File deleted: "; relative_path ]);
                      send internal_server_pid (MarkFileDeleted event.path)
                  | _ -> ()
            ) events;
            loop ()
      in
      loop ()
    with
    | exn ->
        println ("\n⚠️  File watcher failed to start: " ^ Exception.to_string exn);
        println "Falling back to manual indexing mode\n";
        let rec loop () =
          sleep (Time.Duration.from_secs 3600);
          loop ()
        in
        loop ()

(** Start the Codedb service supervisor with all child processes *)
let start config =
  (* Start internal server first to get its PID *)
  let internal_server_pid = spawn (fun () -> start_internal_server config) in
  
  (* Child specs *)
  let internal_server_spec =
    Supervisor.child_spec ~id:"codedb_internal_server"
      ~start:(fun () -> internal_server_pid)
      ~restart:Permanent ()
  in

  let file_watcher_spec =
    Supervisor.child_spec ~id:"codedb_file_watcher"
      ~start:(fun () -> spawn (fun () -> start_file_watcher config internal_server_pid))
      ~restart:Permanent ()
  in

  (* Start supervisor *)
  Supervisor.start_link ~strategy:OneForOne
    ~intensity:{ max_restarts = 5; window = Time.Duration.from_secs 10 }
    ~children:[ internal_server_spec; file_watcher_spec ]
    ()
