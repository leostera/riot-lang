open Std

module List = Collections.List
module Queue = Collections.Queue
module DynamicWorkerPool = WorkerPool.DynamicWorkerPool

type error =
  | File_system of {
      path: Path.t option;
      cause: Fs.error;
    }
  | Invalid_glob of {
      path: Path.t;
      line: int;
      input: string;
      message: string;
      offset: int option;
    }

type frame = {
  custom: Gitignore.t list;
  ignore: Gitignore.t option;
  git_ignore: Gitignore.t option;
}

type t = {
  roots: Path.t list;
  concurrency: int;
  sort: bool;
  follow_symlinks: bool;
  hidden: bool;
  parents: bool;
  ignore: bool;
  git_ignore: bool;
  custom_ignore_filenames: string list;
  ignore_patterns: Gitignore.t;
  overrides: Gitignore.t;
}

let basename_is_hidden = fun entry ->
  let name = Fs.Walker.FileItem.name entry in
  String.length name > 0 && Char.equal (String.get_unchecked name ~at:0) '.'

let trim_frames = fun frames depth ->
  let rec loop index = fun __tmp1 ->
    match __tmp1 with
    | [] -> []
    | frame :: rest ->
        if index < depth then
          frame :: loop (index + 1) rest
        else
          []
  in
  loop 0 frames

let load_ignore_file = fun path ->
  match Gitignore.from_file ~syntax:Gitignore.Ignore_file path with
  | Ok rules -> Ok rules
  | Error (Gitignore.File_system cause) -> Error (File_system { path = Some path; cause })
  | Error (Gitignore.Invalid_glob {
             line;
             input;
             message;
             offset;
           }) ->
      Error (
        Invalid_glob {
          path;
          line;
          input;
          message;
          offset;
        }
      )

let load_frame = fun config entry ->
  let dir = Fs.Walker.FileItem.path entry in
  let rec load_custom acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | name :: rest ->
        let path = Path.(dir / Path.v name) in
        begin
          match load_ignore_file path with
          | Ok None -> load_custom acc rest
          | Ok (Some matcher) -> load_custom (matcher :: acc) rest
          | Error _ as err -> err
        end
  in
  begin
    match load_custom [] config.custom_ignore_filenames with
    | Error _ as err -> err
    | Ok custom -> (
        let load_named enabled name =
          if enabled then
            load_ignore_file Path.(dir / Path.v name)
          else
            Ok None
        in
        match load_named config.ignore ".ignore" with
        | Error _ as err -> err
        | Ok ignore -> (
            match load_named config.git_ignore ".gitignore" with
            | Error _ as err -> err
            | Ok git_ignore -> Ok { custom; ignore; git_ignore }
          )
      )
  end

let match_across_frames = fun config frames path ~is_dir ->
  let frames =
    if config.parents then
      List.reverse frames
    else
      match List.reverse frames with
      | [] -> []
      | frame :: _ -> [ frame ]
  in
  let rec match_group getter =
    let rec find_in_frames = fun __tmp1 ->
      match __tmp1 with
      | [] -> Match.None_
      | frame :: rest ->
          let rec find_in_matchers = fun __tmp1 ->
            match __tmp1 with
            | [] -> find_in_frames rest
            | matcher :: matchers ->
                let match_ = Gitignore.matched matcher ~path ~is_dir in
                if Match.is_none match_ then
                  find_in_matchers matchers
                else
                  match_
          in
          find_in_matchers (getter frame)
    in
    find_in_frames frames
  in
  let custom_match = match_group (fun frame -> List.reverse frame.custom) in
  let ignore_match =
    if Match.is_none custom_match then
      match_group
        (fun frame ->
          match frame.ignore with
          | None -> []
          | Some matcher -> [ matcher ])
    else
      Match.None_
  in
  let git_ignore_match =
    if Match.is_none custom_match && Match.is_none ignore_match then
      match_group
        (fun frame ->
          match frame.git_ignore with
          | None -> []
          | Some matcher -> [ matcher ])
    else
      Match.None_
  in
  custom_match
  |> Match.or_else ignore_match
  |> Match.or_else git_ignore_match

let decision_for_entry = fun config frames entry ->
  let path = Fs.Walker.FileItem.path entry in
  let is_dir =
    match Fs.Walker.FileItem.kind entry with
    | Fs.Walker.Directory -> true
    | Fs.Walker.File
    | Fs.Walker.Symlink
    | Fs.Walker.Other -> false
  in
  let override_match = Gitignore.matched config.overrides ~path ~is_dir in
  if not (Match.is_none override_match) then
    Ok override_match
  else
    let extra_ignore_match = Gitignore.matched config.ignore_patterns ~path ~is_dir in
    if not (Match.is_none extra_ignore_match) then
      Ok extra_ignore_match
    else (
      let ignore_match = match_across_frames config frames path ~is_dir in
      if Match.is_ignore ignore_match then
        Ok Match.Ignore
      else if Match.is_whitelist ignore_match then
        Ok Match.Whitelist
      else if config.hidden && Fs.Walker.FileItem.depth entry > 0 && basename_is_hidden entry then
        Ok Match.Ignore
      else
        Ok Match.None_
    )

let create = fun
  ~roots
  ?(concurrency = Thread.available_parallelism)
  ?(sort = false)
  ?(follow_symlinks = false)
  ?(hidden = true)
  ?(parents = true)
  ?(ignore = true)
  ?(git_ignore = true)
  ?(custom_ignore_filenames = [])
  ?(ignore_patterns = [])
  ?(overrides = [])
  () ->
  let concurrency = Int.max 1 concurrency in
  let root =
    match roots with
    | [] -> Path.v "."
    | root :: _ -> root
  in
  let make_glob_error label (error: Gitignore.parse_error) =
    Glob.Invalid_glob {
      input = label ^ " line " ^ Int.to_string error.line ^ ": " ^ error.input;
      message = error.message;
      offset = error.offset;
    }
  in
  match Gitignore.from_lines ~root ~syntax:Gitignore.Ignore_file ignore_patterns with
  | Error error -> Error (make_glob_error "ignore pattern" error)
  | Ok ignore_pattern_matcher -> (
      match Gitignore.from_lines ~root ~syntax:Gitignore.Override overrides with
      | Error error -> Error (make_glob_error "override" error)
      | Ok override_matcher ->
          Ok {
            roots;
            concurrency;
            sort;
            follow_symlinks;
            hidden;
            parents;
            ignore;
            git_ignore;
            custom_ignore_filenames;
            ignore_patterns = ignore_pattern_matcher;
            overrides = override_matcher;
          }
    )

let join_path_string = fun dir_path name ->
  if String.equal dir_path "" then
    name
  else if Char.equal (String.get_unchecked dir_path ~at:(String.length dir_path - 1)) '/' then
    dir_path ^ name
  else
    dir_path ^ "/" ^ name

let entry_kind_of_metadata = fun metadata ->
  match Fs.Metadata.file_type metadata with
  | Regular -> Fs.Walker.File
  | Directory -> Fs.Walker.Directory
  | Symlink -> Fs.Walker.Symlink
  | Block
  | Character
  | Fifo
  | Socket
  | Unknown -> Fs.Walker.Other

let metadata_for_path_string = fun config path_string ->
  match Path.from_string path_string with
  | Error _ -> Error (IO.Unknown_error ("Invalid path " ^ path_string))
  | Ok path ->
      if config.follow_symlinks then
        Fs.metadata path
      else
        Fs.symlink_metadata path

let child_entry = fun config ~dir_path ~depth (raw: Fs.ReadDir.entry) ->
  let { Fs.ReadDir.path; kind } = raw in
  let name = Path.to_string path in
  let path_string = join_path_string dir_path name in
  match kind with
  | Fs.ReadDir.RegularFile ->
      Ok (Fs.Walker.FileItem.make ~path_string ~name ~depth ~kind:Fs.Walker.File)
  | Fs.ReadDir.Directory ->
      Ok (Fs.Walker.FileItem.make ~path_string ~name ~depth ~kind:Fs.Walker.Directory)
  | Fs.ReadDir.SymbolicLink ->
      if config.follow_symlinks then
        metadata_for_path_string config path_string
        |> fun result ->
          Result.map
            result
            ~fn:(fun metadata ->
              Fs.Walker.FileItem.make
                ~path_string
                ~name
                ~depth
                ~kind:(entry_kind_of_metadata metadata))
      else
        Ok (Fs.Walker.FileItem.make ~path_string ~name ~depth ~kind:Fs.Walker.Symlink)
  | Fs.ReadDir.CharacterDevice
  | Fs.ReadDir.BlockDevice
  | Fs.ReadDir.NamedPipe
  | Fs.ReadDir.Socket ->
      Ok (Fs.Walker.FileItem.make ~path_string ~name ~depth ~kind:Fs.Walker.Other)
  | Fs.ReadDir.Unknown ->
      metadata_for_path_string config path_string
      |> fun result ->
        Result.map
          result
          ~fn:(fun metadata ->
            Fs.Walker.FileItem.make
              ~path_string
              ~name
              ~depth
              ~kind:(entry_kind_of_metadata metadata))

let root_entry = fun _config root ->
  let path_string = Path.to_string root in
  Fs.symlink_metadata root
  |> fun result ->
    Result.map
      result
      ~fn:(fun metadata ->
        Fs.Walker.FileItem.make
          ~path_string
          ~name:(Path.basename root)
          ~depth:0
          ~kind:(entry_kind_of_metadata metadata))
    |> fun result ->
      Result.map_err
        result
        ~fn:(fun cause -> File_system { path = Some root; cause })

let should_descend_root = fun entry ->
  match Fs.Walker.FileItem.kind entry with
  | Fs.Walker.Directory -> true
  | Fs.Walker.Symlink ->
      Fs.Walker.FileItem.path entry
      |> Fs.is_dir
      |> Result.unwrap_or ~default:false
  | Fs.Walker.File
  | Fs.Walker.Other -> false

let compare_entry = fun left right ->
  String.compare
    (Fs.Walker.FileItem.path_string left)
    (Fs.Walker.FileItem.path_string right)

let read_child_entries = fun config entry ->
  let dir_path = Fs.Walker.FileItem.path_string entry in
  let dir = Fs.Walker.FileItem.path entry in
  match Fs.ReadDir.open_dir dir with
  | Error cause -> Error (File_system { path = Some dir; cause })
  | Ok handle ->
      let rec loop acc =
        match Fs.ReadDir.next handle with
        | None ->
            let _ = Fs.ReadDir.close handle in
            let entries = List.reverse acc in
            if config.sort then
              Ok (List.sort entries ~compare:compare_entry)
            else
              Ok entries
        | Some raw -> (
            match child_entry config ~dir_path ~depth:(Fs.Walker.FileItem.depth entry + 1) raw with
            | Ok child -> loop (child :: acc)
            | Error cause ->
                let _ = Fs.ReadDir.close handle in
                Error (File_system { path = Some dir; cause })
          )
      in
      loop []

let sequential_walk = fun config ~f ->
  let frames = ref [] in
  let deferred_error = ref None in
  let wrapped entry =
    frames := trim_frames !frames (Fs.Walker.FileItem.depth entry);
    match decision_for_entry config !frames entry with
    | Error err ->
        deferred_error := Some err;
        Fs.Walker.Stop
    | Ok match_ when Match.is_ignore match_ -> (
        match Fs.Walker.FileItem.kind entry with
        | Fs.Walker.Directory -> Fs.Walker.Skip_subtree
        | Fs.Walker.File
        | Fs.Walker.Symlink
        | Fs.Walker.Other -> Fs.Walker.Continue
      )
    | Ok _ -> (
        match Fs.Walker.FileItem.kind entry with
        | Fs.Walker.Directory -> (
            match load_frame config entry with
            | Ok frame ->
                frames := !frames @ [ frame ];
                f entry
            | Error err ->
                deferred_error := Some err;
                Fs.Walker.Stop
          )
        | Fs.Walker.File
        | Fs.Walker.Symlink
        | Fs.Walker.Other -> f entry
      )
  in
  match Fs.Walker.walk
    ~roots:config.roots
    ~sort:config.sort
    ~follow_symlinks:config.follow_symlinks
    ~f:wrapped
    () with
  | Ok () -> (
      match !deferred_error with
      | Some err -> Error err
      | None -> Ok ()
    )
  | Error cause -> Error (File_system { path = None; cause })

type task = {
  dir: Fs.Walker.FileItem.t;
  frames: frame list;
}

type task_result = {
  child_tasks: task list;
  stop: bool;
}

type walk_message =
  | Scan_finished of task_result
  | Scan_failed of error
  | Walk_completed of {
      coordinator: Pid.t;
      result: (unit, error) result;
    }

type Message.t +=
  | Ignore_walk of walk_message

type state = {
  owner: Pid.t;
  coordinator: Pid.t;
  pool: task DynamicWorkerPool.t;
  idle_workers: task DynamicWorkerPool.worker Queue.t;
  pending_tasks: task Queue.t;
  mutable in_flight: int;
  mutable stop: bool;
  mutable error: error option;
}

let process_directory_task = fun config task f ->
  match read_child_entries config task.dir with
  | Error err -> Error err
  | Ok entries ->
      let rec loop child_tasks = fun __tmp1 ->
        match __tmp1 with
        | [] -> Ok { child_tasks = List.reverse child_tasks; stop = false }
        | entry :: rest ->
            match decision_for_entry config task.frames entry with
            | Error err -> Error err
            | Ok match_ when Match.is_ignore match_ -> loop child_tasks rest
            | Ok _ -> (
                match Fs.Walker.FileItem.kind entry with
                | Fs.Walker.Directory -> (
                    match load_frame config entry with
                    | Error err -> Error err
                    | Ok frame -> (
                        match f entry with
                        | Fs.Walker.Stop ->
                            Ok { child_tasks = List.reverse child_tasks; stop = true }
                        | Fs.Walker.Skip_subtree -> loop child_tasks rest
                        | Fs.Walker.Continue ->
                            loop
                              ({ dir = entry; frames = task.frames @ [ frame ] } :: child_tasks)
                              rest
                      )
                  )
                | Fs.Walker.File
                | Fs.Walker.Symlink
                | Fs.Walker.Other -> (
                    match f entry with
                    | Fs.Walker.Stop -> Ok { child_tasks = List.reverse child_tasks; stop = true }
                    | Fs.Walker.Skip_subtree
                    | Fs.Walker.Continue -> loop child_tasks rest
                  )
              )
      in
      loop [] entries

let dispatch_available = fun state ->
  let rec loop () =
    if state.stop || Option.is_some state.error then
      ()
    else
      match (Queue.front state.idle_workers, Queue.front state.pending_tasks) with
      | (Some _, Some _) -> (
          match (Queue.pop state.idle_workers, Queue.pop state.pending_tasks) with
          | (Some worker, Some task) ->
              state.in_flight <- state.in_flight + 1;
              DynamicWorkerPool.send_task state.pool worker task;
              loop ()
          | _ -> panic "ignore walker dispatch queue desynchronized"
        )
      | _ -> ()
  in
  loop ()

let finish_result = fun state ->
  match state.error with
  | Some err -> Error err
  | None -> Ok ()

let maybe_finish = fun state ->
  dispatch_available state;
  if state.in_flight = 0 && (state.stop || Queue.is_empty state.pending_tasks) then (
    send
      state.owner
      (Ignore_walk (Walk_completed {
        coordinator = state.coordinator;
        result = finish_result state;
      }));
    true
  ) else
    false

let enqueue_root_task = fun config state ~f root ->
  match root_entry config root with
  | Error err ->
      if Option.is_none state.error then
        state.error <- Some err;
      state.stop <- true
  | Ok entry -> (
      match decision_for_entry config [] entry with
      | Error err ->
          if Option.is_none state.error then
            state.error <- Some err;
          state.stop <- true
      | Ok match_ when Match.is_ignore match_ -> ()
      | Ok _ ->
          if should_descend_root entry then
            match load_frame config entry with
            | Error err ->
                if Option.is_none state.error then
                  state.error <- Some err;
                state.stop <- true
            | Ok frame -> (
                match f entry with
                | Fs.Walker.Stop -> state.stop <- true
                | Fs.Walker.Skip_subtree -> ()
                | Fs.Walker.Continue ->
                    Queue.push state.pending_tasks ~value:{ dir = entry; frames = [ frame ] }
              )
          else
            match f entry with
            | Fs.Walker.Stop -> state.stop <- true
            | Fs.Walker.Skip_subtree
            | Fs.Walker.Continue -> ()
    )

type event =
  | Worker_ready of task DynamicWorkerPool.worker
  | Walk_event of walk_message

let rec coordinator_loop = fun state ->
  if maybe_finish state then
    Ok ()
  else
    let selector msg =
      match msg with
      | DynamicWorkerPool.WorkerReady worker -> (
          match Ref.type_equal state.pool.task_ref (DynamicWorkerPool.get_worker_task_ref worker) with
          | Some Type.Equal -> Select (Worker_ready worker)
          | None -> Skip
        )
      | Ignore_walk inner -> Select (Walk_event inner)
      | _ -> Skip
    in
    match receive ~selector () with
    | Worker_ready worker ->
        Queue.push state.idle_workers ~value:worker;
        coordinator_loop state
    | Walk_event (Scan_finished { child_tasks; stop }) ->
        state.in_flight <- state.in_flight - 1;
        if stop then
          state.stop <- true
        else
          List.for_each child_tasks ~fn:(fun task -> Queue.push state.pending_tasks ~value:task);
        coordinator_loop state
    | Walk_event (Scan_failed err) ->
        state.in_flight <- state.in_flight - 1;
        if Option.is_none state.error then
          state.error <- Some err;
        state.stop <- true;
        coordinator_loop state
    | Walk_event (Walk_completed _) -> panic "ignore walker received nested completion event"

let parallel_walk = fun config ~f ->
  let owner = self () in
  let init () =
    let coordinator = self () in
    let worker_fn ~owner ~task =
      match process_directory_task config task f with
      | Ok result -> send owner (Ignore_walk (Scan_finished result))
      | Error err -> send owner (Ignore_walk (Scan_failed err))
    in
    let pool =
      DynamicWorkerPool.start ~concurrency:config.concurrency ~owner:coordinator ~worker_fn ()
    in
    let state = {
      owner;
      coordinator;
      pool;
      idle_workers = Queue.create ();
      pending_tasks = Queue.create ();
      in_flight = 0;
      stop = false;
      error = None;
    }
    in
    List.for_each config.roots ~fn:(enqueue_root_task config state ~f);
    coordinator_loop state
  in
  let coordinator = spawn init in
  let selector msg =
    match msg with
    | Ignore_walk (Walk_completed { coordinator = sender; result }) when Pid.equal
      sender
      coordinator -> Select result
    | _ -> Skip
  in
  receive ~selector ()

let walk = fun config ~f ->
  if config.concurrency <= 1 then
    sequential_walk config ~f
  else
    parallel_walk config ~f

let to_list = fun config ->
  let lock = Sync.Mutex.create () in
  let items = ref [] in
  walk
    config
    ~f:(fun entry ->
      Sync.Mutex.lock lock;
      begin
        match items := entry :: !items with
        | () -> Sync.Mutex.unlock lock
        | exception exn ->
            Sync.Mutex.unlock lock;
            raise exn
      end;
      Fs.Walker.Continue)
  |> fun result -> Result.map result ~fn:(fun () -> List.reverse !items)
