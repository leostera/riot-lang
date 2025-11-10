open Global
  open Process
  open Sync
  module List = Collections.List

type t = Pid.t

type config = {
  paths : Path.t list;
  ignore_patterns : string list;
  file_extensions : (string list) option;
  latency : float;
}

type event = {
  path : Path.t;
  kind : event_kind;
}

and event_kind =
  | Created
  | Modified
  | Deleted
  | Renamed
  | Metadata

type Message.t += 
  | FileWatchEvent of event
  | StopWatcher

let default_config ~paths = {
  paths;
  ignore_patterns = [];
  file_extensions = None;
  latency = 0.1;  (* 100ms *)
}

let should_ignore ~ignore_patterns:_ _path =
  (* Simplified for now - no pattern matching to avoid dependencies *)
  false

let start ~config ~owner_pid =
  spawn (fun () ->
    let watcher = 
      Kernel.Fs.Events.create ()
      |> Result.expect ~msg:"Failed to create file watcher"
    in
    
    (* Watch all configured paths *)
    List.iter (fun path ->
      let path_str = Path.to_string path in
      let _ = 
        Kernel.Fs.Events.watch watcher ~path:path_str ~latency:config.latency
        |> Result.expect ~msg:("Failed to watch: " ^ path_str)
      in
      ()
    ) config.paths;
    
    (* Event loop *)
    let rec loop () =
      (* Check for stop message with timeout *)
      let selector msg = match msg with
        | StopWatcher -> `select StopWatcher
        | _ -> `skip
      in
      
      match receive ~timeout:(Time.Duration.from_secs_float 0.010) ~selector () with
      | StopWatcher ->
          (* Clean up watcher *)
          let _ = Kernel.Fs.Events.stop watcher in
          Ok ()
      | exception Receive_timeout ->
          (* No stop message, continue with file events *)
          (match Kernel.Fs.Events.read_events watcher with
          | Error _ -> 
              sleep (Time.Duration.from_secs_float 0.050)
          | Ok events ->
              (* Filter and send events *)
              List.iter (fun (kernel_event : Kernel.Fs.Events.event) ->
                let path = Path.of_string kernel_event.path 
                           |> Result.expect ~msg:"Invalid path from kernel" in
                
                let allowed = 
                  match config.file_extensions with
                  | None -> true
                  | Some exts ->
                      match Path.extension path with
                      | None -> false  (* No extension *)
                      | Some ext ->
                          let rec mem x = function
                            | [] -> false
                            | h :: t -> h = x || mem x t
                          in
                          mem ext exts
                in
                if not (should_ignore ~ignore_patterns:config.ignore_patterns path) && allowed
                then
                  let kind = 
                    match Kernel.Fs.Events.decode_event_kind kernel_event.flags with
                    | Kernel.Fs.Events.Created -> Created
                    | Kernel.Fs.Events.Modified -> Modified
                    | Kernel.Fs.Events.Deleted -> Deleted
                    | Kernel.Fs.Events.Renamed -> Renamed
                    | Kernel.Fs.Events.Metadata -> Metadata
                  in
                  send owner_pid (FileWatchEvent { path; kind })
              ) events;
              
              sleep (Time.Duration.from_secs_float 0.050));
          loop ()
      | _ ->
          (* Ignore other messages *)
          loop ()
    in
    
    loop ()
  )

let stop watcher_pid =
  send watcher_pid StopWatcher
