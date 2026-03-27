open Std
open Std.Iter
open Std.Collections

type run_mode =
  | Check
  | Verify

type file_status =
  | Already_formatted
  | Needs_formatting
  | Would_reformat
  | Unsafe_to_format
  | Failed

type file_result = {
  file : Path.t;
  status : file_status;
  needs_formatting : bool;
  error : string option;
  duration : Time.Duration.t;
}

type summary = {
  total_files : int;
  already_formatted : int;
  needs_formatting : int;
  would_reformat : int;
  unsafe_to_format : int;
  failed_files : int;
  duration : Time.Duration.t;
}

type run_result = { files : file_result list; summary : summary }

type Message.t +=
  | ScannerDiscovered of { scanner_ref : unit Ref.t; file : Path.t }
  | ScannerComplete of unit Ref.t
  | DispatchFileChecked of {
      result_ref : file_result Ref.t;
      result : file_result;
    }
  | StreamFileResult of { run_ref : unit Ref.t; result : file_result }
  | StreamCompleted of unit Ref.t

let is_ocaml_source path =
  let path = Path.to_string path in
  String.ends_with ~suffix:".ml" path || String.ends_with ~suffix:".mli" path

let should_skip_directory path =
  let basename = Path.basename path in
  String.starts_with ~prefix:"." basename
  || String.equal basename "_build"
  || String.equal basename "target"

let compare_paths left right =
  String.compare (Path.to_string left) (Path.to_string right)

let rec walk_dir dir =
  match Fs.read_dir dir with
  | Error _ -> []
  | Ok entries ->
      entries
      |> MutIterator.to_list
      |> List.concat_map (fun entry ->
             let entry_path = Path.(dir / entry) in
             match Fs.is_dir entry_path with
             | Ok true ->
                 if should_skip_directory entry_path then
                   []
                 else
                   walk_dir entry_path
             | Ok false | Error _ ->
                 if is_ocaml_source entry_path then
                   [ entry_path ]
                 else
                   [])

let collect_ocaml_files ?(should_ignore = fun _ -> false) ~roots () =
  roots
  |> List.concat_map (fun root ->
         if should_ignore root then
           []
         else
         match Fs.is_dir root with
         | Ok true -> walk_dir root
         | Ok false | Error _ ->
             if is_ocaml_source root then
               [ root ]
             else
               [])
  |> List.filter (fun path -> not (should_ignore path))
  |> List.sort_uniq compare_paths

let syntax_hash (result : Syn.Parser.parse_result) =
  let buffer = IO.Buffer.create 1024 in
  let rec write_element = function
    | Syn.Ceibo.Green.Token _ as element -> (
        match Syn.Ceibo.Green.kind element with
        | Syn.SyntaxKind.WHITESPACE -> ()
        | kind ->
            IO.Buffer.add_string buffer "T(";
            IO.Buffer.add_string buffer (Syn.SyntaxKind.to_string kind);
            IO.Buffer.add_string buffer ":";
            IO.Buffer.add_string buffer
              (Syn.Ceibo.Green.text element |> Option.expect ~msg:"token text");
            IO.Buffer.add_string buffer ")")
    | Syn.Ceibo.Green.Node node as element ->
        IO.Buffer.add_string buffer "N(";
        IO.Buffer.add_string buffer
          (Syn.SyntaxKind.to_string (Syn.Ceibo.Green.kind element));
        IO.Buffer.add_string buffer "[";
        Array.iter write_element (Syn.Ceibo.Green.children node);
        IO.Buffer.add_string buffer "])"
  in
  write_element (Syn.Ceibo.Green.Node result.tree);
  IO.Buffer.contents buffer |> Crypto.hash_string |> Crypto.Digest.hex

let is_trivia_kind = function
  | Syn.SyntaxKind.WHITESPACE -> true
  | _ -> false

let is_comment_like_kind = function
  | Syn.SyntaxKind.COMMENT
  | Syn.SyntaxKind.DOCSTRING ->
      true
  | _ ->
      false

let is_redundant_paren_inner_kind = function
  | Syn.SyntaxKind.IDENT_EXPR
  | Syn.SyntaxKind.PATH_EXPR
  | Syn.SyntaxKind.CONSTRUCTOR_EXPR
  | Syn.SyntaxKind.POLY_VARIANT_EXPR
  | Syn.SyntaxKind.INT_LITERAL
  | Syn.SyntaxKind.FLOAT_LITERAL
  | Syn.SyntaxKind.STRING_LITERAL
  | Syn.SyntaxKind.CHAR_LITERAL
  | Syn.SyntaxKind.BOOL_LITERAL
  | Syn.SyntaxKind.UNIT_LITERAL
  | Syn.SyntaxKind.LIST_EXPR
  | Syn.SyntaxKind.ARRAY_EXPR
  | Syn.SyntaxKind.RECORD_EXPR
  | Syn.SyntaxKind.RECORD_UPDATE_EXPR
  | Syn.SyntaxKind.TUPLE_EXPR
  | Syn.SyntaxKind.PAREN_EXPR ->
      true
  | _ ->
      false

let semantic_hash (result : Syn.Parser.parse_result) =
  let buffer = IO.Buffer.create 1024 in
  let rec should_skip_token ~parent_kind token =
    let token_kind = Syn.Ceibo.Green.kind (Syn.Ceibo.Green.Token token) in
    let token_text =
      Syn.Ceibo.Green.text (Syn.Ceibo.Green.Token token)
      |> Option.expect ~msg:"green token text"
    in
    if is_trivia_kind token_kind then
      true
    else
      match parent_kind with
      | Some Syn.SyntaxKind.PAREN_EXPR ->
          String.equal token_text "(" || String.equal token_text ")"
      | Some Syn.SyntaxKind.LIST_EXPR ->
          String.equal token_text "["
          || String.equal token_text "]"
          || String.equal token_text ";"
      | Some Syn.SyntaxKind.ARRAY_EXPR ->
          String.equal token_text "[|"
          || String.equal token_text "|]"
          || String.equal token_text ";"
      | _ ->
          false
  and redundant_paren_child node =
    let has_comment_like =
          Array.exists
        (function
          | Syn.Ceibo.Green.Token token ->
              let token_kind = Syn.Ceibo.Green.kind (Syn.Ceibo.Green.Token token) in
              is_comment_like_kind token_kind
          | Syn.Ceibo.Green.Node _ ->
              false)
        (Syn.Ceibo.Green.children node)
    in
    if has_comment_like then
      None
    else
      let meaningful_children =
        Syn.Ceibo.Green.children node
        |> Array.to_list
        |> List.filter (function
               | Syn.Ceibo.Green.Token token ->
                   let token_kind = Syn.Ceibo.Green.kind (Syn.Ceibo.Green.Token token) in
                   let token_text =
                     Syn.Ceibo.Green.text (Syn.Ceibo.Green.Token token)
                     |> Option.expect ~msg:"green token text"
                   in
                   not (is_trivia_kind token_kind)
                   && not (String.equal token_text "(" || String.equal token_text ")")
               | Syn.Ceibo.Green.Node _ ->
                   true)
      in
      match meaningful_children with
      | [ Syn.Ceibo.Green.Node inner as child ]
        when
          is_redundant_paren_inner_kind
            (Syn.Ceibo.Green.kind (Syn.Ceibo.Green.Node inner)) ->
          Some child
      | [ Syn.Ceibo.Green.Token token as child ]
        when
          is_redundant_paren_inner_kind
            (Syn.Ceibo.Green.kind (Syn.Ceibo.Green.Token token)) ->
          Some child
      | _ ->
          None
  and write_token token =
    let token_kind = Syn.Ceibo.Green.kind (Syn.Ceibo.Green.Token token) in
    let token_text =
      Syn.Ceibo.Green.text (Syn.Ceibo.Green.Token token)
      |> Option.expect ~msg:"green token text"
    in
    IO.Buffer.add_string buffer "T(";
    IO.Buffer.add_string buffer (Syn.SyntaxKind.to_string token_kind);
    IO.Buffer.add_string buffer ":";
    IO.Buffer.add_string buffer token_text;
    IO.Buffer.add_string buffer ")"
  and write_child ~parent_kind = function
    | Syn.Ceibo.Green.Token token ->
        if not (should_skip_token ~parent_kind token) then
          write_token token
    | Syn.Ceibo.Green.Node _ as element ->
        write_element element
  and write_node node =
    let node_kind = Syn.Ceibo.Green.kind (Syn.Ceibo.Green.Node node) in
    IO.Buffer.add_string buffer "N(";
    IO.Buffer.add_string buffer (Syn.SyntaxKind.to_string node_kind);
    IO.Buffer.add_string buffer "[";
    Array.iter (write_child ~parent_kind:(Some node_kind)) (Syn.Ceibo.Green.children node);
    IO.Buffer.add_string buffer "])"
  and write_element = function
    | Syn.Ceibo.Green.Token token ->
        let token_kind = Syn.Ceibo.Green.kind (Syn.Ceibo.Green.Token token) in
        if not (is_trivia_kind token_kind) then
          write_token token
    | Syn.Ceibo.Green.Node node ->
        let node_kind = Syn.Ceibo.Green.kind (Syn.Ceibo.Green.Node node) in
        if Syn.SyntaxKind.(node_kind = PAREN_EXPR) then
          match redundant_paren_child node with
          | Some child ->
              write_child ~parent_kind:(Some Syn.SyntaxKind.PAREN_EXPR) child
          | None ->
              write_node node
        else
          write_node node
  in
  write_node result.tree;
  IO.Buffer.contents buffer |> Crypto.hash_string |> Crypto.Digest.hex

let finalize file start ~status ~needs_formatting ~error =
  {
    file;
    status;
    needs_formatting;
    error;
    duration = Time.Instant.elapsed start;
  }

let format_file ~mode file =
  let start = Time.Instant.now () in
  match Fs.read file with
  | Error _ ->
      finalize file start ~status:Failed ~needs_formatting:false
        ~error:(Some ("Failed to read " ^ Path.to_string file))
  | Ok source ->
      let parsed = Syn.parse ~filename:file source in
      match Format_core.format parsed with
      | Ok formatted ->
          let result =
            if String.equal source formatted then
              finalize file start ~status:Already_formatted
                ~needs_formatting:false ~error:None
            else
              match mode with
              | Check ->
                  finalize file start ~status:Needs_formatting
                    ~needs_formatting:true ~error:None
              | Verify ->
                  let original_hash = semantic_hash parsed in
                  let reparsed = Syn.parse ~filename:file formatted in
                  let formatted_hash = semantic_hash reparsed in
                  if String.equal original_hash formatted_hash then
                    finalize file start ~status:Would_reformat
                      ~needs_formatting:true ~error:None
                  else
                    finalize file start ~status:Unsafe_to_format
                      ~needs_formatting:true
                      ~error:
                        (Some
                           ("semantic-hash mismatch after formatting (original: "
                          ^ original_hash ^ ", formatted: " ^ formatted_hash
                          ^ ")"))
          in
          result
      | Error err ->
          finalize file start ~status:Failed ~needs_formatting:false
            ~error:(Some (Format_core.format_error_to_string err))

let check_file file = format_file ~mode:Check file
let verify_file file = format_file ~mode:Verify file

type scanner_state = {
  owner : Pid.t;
  scanner_ref : unit Ref.t;
  should_ignore : Path.t -> bool;
  seen : string HashSet.t;
  mutable pending : Path.t list;
}

let sorted_directory_entries dir =
  match Fs.read_dir dir with
  | Error _ -> []
  | Ok entries ->
      entries
      |> MutIterator.to_list
      |> List.map (fun entry -> Path.(dir / entry))
      |> List.sort compare_paths

let rec next_discovered_file state =
  match state.pending with
  | [] -> None
  | path :: rest ->
      state.pending <- rest;
      let path_string = Path.to_string path in
      if HashSet.contains state.seen path_string then
        next_discovered_file state
      else (
        let _ = HashSet.insert state.seen path_string in
        match Fs.is_dir path with
        | Ok true ->
            if should_skip_directory path || state.should_ignore path then
              next_discovered_file state
            else (
              state.pending <- sorted_directory_entries path @ state.pending;
              next_discovered_file state)
        | Ok false | Error _ ->
            if is_ocaml_source path && not (state.should_ignore path) then
              Some path
            else
              next_discovered_file state)

let rec scanner_loop state =
  match next_discovered_file state with
  | Some file ->
      send state.owner (ScannerDiscovered { scanner_ref = state.scanner_ref; file });
      scanner_loop state
  | None ->
      send state.owner (ScannerComplete state.scanner_ref);
      Ok ()

let start_scanner ~owner ~roots ~scanner_ref ~should_ignore =
  let seen = HashSet.create () in
  let state =
    {
      owner;
      scanner_ref;
      should_ignore;
      seen;
      pending = List.sort compare_paths roots;
    }
  in
  spawn (fun () -> scanner_loop state)

type dispatch_state = {
  owner : Pid.t;
  run_ref : unit Ref.t;
  scanner_ref : unit Ref.t;
  pool : Path.t WorkerPool.DynamicWorkerPool.t;
  result_ref : file_result Ref.t;
  pending_files : Path.t Queue.t;
  idle_workers : Path.t WorkerPool.DynamicWorkerPool.worker Queue.t;
  mutable tasks_in_flight : int;
  mutable discovery_complete : bool;
}

let dispatch_ready_workers state =
  let rec loop () =
    match (Queue.front state.idle_workers, Queue.front state.pending_files) with
    | Some _, Some _ ->
        let worker =
          Queue.pop state.idle_workers
          |> Option.expect ~msg:"idle worker should exist"
        in
        let file =
          Queue.pop state.pending_files
          |> Option.expect ~msg:"pending file should exist"
        in
        state.tasks_in_flight <- state.tasks_in_flight + 1;
        WorkerPool.DynamicWorkerPool.send_task state.pool worker file;
        loop ()
    | _ -> ()
  in
  loop ()

let is_dispatch_complete state =
  state.discovery_complete
  && state.tasks_in_flight = 0
  && Queue.is_empty state.pending_files

let rec dispatch_loop state =
  if is_dispatch_complete state then (
    send state.owner (StreamCompleted state.run_ref);
    Ok ())
  else
    let selector :
        [
          `WorkerReady of Path.t WorkerPool.DynamicWorkerPool.worker
        | `ScannerDiscovered of Path.t
        | `ScannerComplete
        | `FileChecked of file_result
        ]
        selector =
     function
      | WorkerPool.DynamicWorkerPool.WorkerReady worker -> (
          match
            Ref.type_equal state.pool.task_ref
              (WorkerPool.DynamicWorkerPool.get_worker_task_ref worker)
          with
          | Some Type.Equal -> `select (`WorkerReady worker)
          | None -> `skip)
      | ScannerDiscovered { scanner_ref; file }
        when Ref.equal state.scanner_ref scanner_ref ->
          `select (`ScannerDiscovered file)
      | ScannerComplete scanner_ref when Ref.equal state.scanner_ref scanner_ref ->
          `select `ScannerComplete
      | DispatchFileChecked { result_ref; result }
        when Ref.equal state.result_ref result_ref ->
          `select (`FileChecked result)
      | _ -> `skip
    in
    match receive ~selector () with
    | `WorkerReady worker ->
        Queue.push state.idle_workers worker;
        dispatch_ready_workers state;
        dispatch_loop state
    | `ScannerDiscovered file ->
        Queue.push state.pending_files file;
        dispatch_ready_workers state;
        dispatch_loop state
    | `ScannerComplete ->
        state.discovery_complete <- true;
        dispatch_loop state
    | `FileChecked result ->
        state.tasks_in_flight <- max 0 (state.tasks_in_flight - 1);
        send state.owner (StreamFileResult { run_ref = state.run_ref; result });
        dispatch_ready_workers state;
        dispatch_loop state

let start_dispatcher ~owner ~run_ref ~concurrency ~roots ~should_ignore ~check_fn =
  let dispatcher_owner = self () in
  let scanner_ref = Ref.make () in
  let result_ref = Ref.make () in
  let worker_fn ~owner ~task =
    let result = check_fn task in
    send owner (DispatchFileChecked { result_ref; result })
  in
  let _scanner =
    start_scanner ~owner:dispatcher_owner ~roots ~scanner_ref ~should_ignore
  in
  let pool =
    WorkerPool.DynamicWorkerPool.start ~concurrency ~owner:dispatcher_owner
      ~worker_fn ()
  in
  let state =
    {
      owner;
      run_ref;
      scanner_ref;
      pool;
      result_ref;
      pending_files = Queue.create ();
      idle_workers = Queue.create ();
      tasks_in_flight = 0;
      discovery_complete = false;
    }
  in
  dispatch_loop state

let summarize ~duration files =
  List.fold_left
    (fun acc result ->
      match result.status with
      | Failed ->
          { acc with total_files = acc.total_files + 1; failed_files = acc.failed_files + 1 }
      | Needs_formatting ->
          {
            acc with
            total_files = acc.total_files + 1;
            needs_formatting = acc.needs_formatting + 1;
          }
      | Would_reformat ->
          {
            acc with
            total_files = acc.total_files + 1;
            would_reformat = acc.would_reformat + 1;
          }
      | Unsafe_to_format ->
          {
            acc with
            total_files = acc.total_files + 1;
            unsafe_to_format = acc.unsafe_to_format + 1;
          }
      | Already_formatted ->
          {
            acc with
            total_files = acc.total_files + 1;
            already_formatted = acc.already_formatted + 1;
          })
    {
      total_files = 0;
      already_formatted = 0;
      needs_formatting = 0;
      would_reformat = 0;
      unsafe_to_format = 0;
      failed_files = 0;
      duration;
    }
    files

let run_streaming ~mode ?(concurrency = System.available_parallelism)
    ?(should_ignore = fun _ -> false) ~roots ~on_result () =
  let concurrency = max 1 concurrency in
  let run_ref = Ref.make () in
  let owner = self () in
  let start = Time.Instant.now () in
  let check_fn =
    match mode with
    | Check -> check_file
    | Verify -> verify_file
  in
  let _dispatcher =
    spawn (fun () ->
        start_dispatcher ~owner ~run_ref ~concurrency ~roots ~should_ignore
          ~check_fn)
  in
  let rec collect results_rev =
    let selector :
        [ `FileResult of file_result | `Completed ] selector = function
      | StreamFileResult { run_ref = msg_ref; result } when Ref.equal run_ref msg_ref
        -> `select (`FileResult result)
      | StreamCompleted msg_ref when Ref.equal run_ref msg_ref ->
          `select `Completed
      | _ -> `skip
    in
    match receive ~selector () with
    | `FileResult result ->
        on_result result;
        collect (result :: results_rev)
    | `Completed ->
        let files = List.rev results_rev in
        let duration = Time.Instant.elapsed start in
        { files; summary = summarize ~duration files }
  in
  collect []

let run_checks_streaming ?concurrency ?should_ignore ~roots ~on_result () =
  run_streaming ~mode:Check ?concurrency ?should_ignore ~roots ~on_result ()

let run_verify_streaming ?concurrency ?should_ignore ~roots ~on_result () =
  run_streaming ~mode:Verify ?concurrency ?should_ignore ~roots ~on_result ()

let run_batch ~mode ?(concurrency = System.available_parallelism)
    ?(should_ignore = fun _ -> false) files =
  let concurrency = max 1 concurrency in
  let start = Time.Instant.now () in
  let check_fn =
    match mode with
    | Check -> check_file
    | Verify -> verify_file
  in
  let files =
    files
    |> List.filter (fun path -> not (should_ignore path))
    |> List.sort compare_paths
  in
  let results =
    WorkerPool.SimpleWorkerPool.run ~concurrency ~tasks:files ~fn:check_fn ()
    |> List.map snd
    |> List.sort (fun left right -> compare_paths left.file right.file)
  in
  let duration = Time.Instant.elapsed start in
  { files = results; summary = summarize ~duration results }

let run_checks ?concurrency ?should_ignore files =
  run_batch ~mode:Check ?concurrency ?should_ignore files

let run_verify ?concurrency ?should_ignore files =
  run_batch ~mode:Verify ?concurrency ?should_ignore files
