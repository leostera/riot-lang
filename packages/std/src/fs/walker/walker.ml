open Global
open Iter
open Collections

type entry_kind =
  | File
  | Directory
  | Symlink
  | Other

type entry = {
  path: Path.t;
  depth: int;
  kind: entry_kind;
}

type error = {
  path: Path.t option;
  depth: int;
  cause: Common.error;
}

type file_item = (entry, error) result

type create_error =
  | MinDepthCannotBeMoreThanMaxDepth of {
      min_depth: int;
      max_depth: int;
    }

type step =
  | Continue
  | Skip_subtree
  | Stop

type t = {
  roots: Path.t list;
  sort: bool;
  follow_symlinks: bool;
  follow_root_links: bool;
  max_open: int;
  min_depth: int;
  max_depth: int;
  contents_first: bool;
  accept_entry: entry -> bool;
}

type dir_list =
  | Opened of {
      depth: int;
      dir_path: Path.t;
      handle: ReadDir.t;
    }
  | Closed of {
      entries: file_item Vector.t;
      mutable index: int;
    }

type iterator_state = {
  opts: t;
  mutable roots: Path.t list;
  stack_list: dir_list Vector.t;
  deferred_dirs: entry Vector.t;
  mutable oldest_opened: int;
}

let create_error_message = function
  | MinDepthCannotBeMoreThanMaxDepth { min_depth; max_depth } ->
      "Walker min_depth cannot be greater than max_depth (min_depth="
      ^ string_of_int min_depth
      ^ ", max_depth="
      ^ string_of_int max_depth
      ^ ")"

let create = fun
  ~roots
  ?(sort = false)
  ?(follow_symlinks = false)
  ?(follow_root_links = true)
  ?(max_open = 10)
  ?(min_depth = 0)
  ?(max_depth = max_int)
  ?(contents_first = false)
  () ->
  let max_open = if max_open <= 0 then 1 else max_open in
  if min_depth > max_depth then
    Error (MinDepthCannotBeMoreThanMaxDepth { min_depth; max_depth })
  else
    let roots =
      if sort then
        List.sort Path.compare roots
      else
        roots
    in
    Ok {
      roots;
      sort;
      follow_symlinks;
      follow_root_links;
      max_open;
      min_depth;
      max_depth;
      contents_first;
      accept_entry = (fun _ -> true);
    }

let entry_kind_of_metadata = fun metadata ->
  match Metadata.file_type metadata with
  | `Regular -> File
  | `Directory -> Directory
  | `Symlink -> Symlink
  | `Block
  | `Character
  | `Fifo
  | `Socket -> Other

let metadata_for_path = fun ~follow_symlinks path ->
  let path_str = Path.to_string path in
  if follow_symlinks then
    Kernel.Fs.File.stat path_str |> Common.convert_kernel_result
  else
    Kernel.Fs.File.lstat path_str |> Common.convert_kernel_result

let make_error = fun ?path ~depth cause -> { path; depth; cause }

let entry_for_path = fun opts ~depth path ->
  match metadata_for_path ~follow_symlinks:opts.follow_symlinks path with
  | Ok metadata ->
      Ok { path; depth; kind = entry_kind_of_metadata metadata }
  | Error cause ->
      Error (make_error ~path ~depth cause)

let compare_item_path = fun left right ->
  match (left, right) with
  | Ok (left: entry), Ok (right: entry) ->
      String.compare (Path.to_string left.path) (Path.to_string right.path)
  | Error (left: error), Error (right: error) ->
      let left = Option.map Path.to_string left.path |> Option.unwrap_or ~default:"" in
      let right = Option.map Path.to_string right.path |> Option.unwrap_or ~default:"" in
      String.compare left right
  | Error _, Ok _ -> -1
  | Ok _, Error _ -> 1

let next_dir_entry = fun opts ~depth ~dir_path handle ->
  match ReadDir.next handle with
  | None -> None
  | Some relative -> Some (entry_for_path opts ~depth:(depth + 1) Path.(dir_path / relative))

let next_dir_list = fun opts dir_list ->
  match dir_list with
  | Opened { depth; dir_path; handle } ->
      next_dir_entry opts ~depth ~dir_path handle
  | Closed state ->
      if state.index >= Vector.len state.entries then
        None
      else
        match Vector.get state.entries state.index with
        | Some item ->
            state.index <- state.index + 1;
            Some item
        | None ->
            None

let close_dir_list = fun opts dir_list ->
  match dir_list with
  | Closed _ -> dir_list
  | Opened { depth; dir_path; handle } ->
      let entries = Vector.create () in
      let rec drain () =
        match next_dir_entry opts ~depth ~dir_path handle with
        | Some item ->
            Vector.push entries item;
            drain ()
        | None ->
            ()
      in
      drain ();
      Closed { entries; index = 0 }

let sort_dir_list = fun dir_list ->
  match dir_list with
  | Opened _ -> dir_list
  | Closed state ->
      Vector.sort_by state.entries compare_item_path;
      dir_list

let skippable = fun opts depth -> depth < opts.min_depth || depth > opts.max_depth

let maybe_directory_target = fun path ->
  match metadata_for_path ~follow_symlinks:true path with
  | Ok metadata -> Metadata.is_dir metadata
  | Error _ -> false

let push = fun state (dent: entry) ->
  let open_handle_budget = Vector.len state.stack_list - state.oldest_opened in
  let () =
    if open_handle_budget = state.opts.max_open then
      match Vector.get state.stack_list state.oldest_opened with
      | Some dir_list ->
          Vector.set
            state.stack_list
            state.oldest_opened
            (close_dir_list state.opts dir_list)
      | None -> ()
  in
  match ReadDir.create dent.path with
  | Error cause ->
      Error (make_error ~path:dent.path ~depth:dent.depth cause)
  | Ok handle ->
      let dir_list =
        Opened {
          depth = dent.depth;
          dir_path = dent.path;
          handle;
        }
      in
      let dir_list =
        if state.opts.sort then
          dir_list |> close_dir_list state.opts |> sort_dir_list
        else
          dir_list
      in
      Vector.push state.stack_list dir_list;
      if open_handle_budget = state.opts.max_open then
        state.oldest_opened <- state.oldest_opened + 1;
      Ok ()

let pop = fun state ->
  ignore (Vector.pop state.stack_list);
  state.oldest_opened <- min state.oldest_opened (Vector.len state.stack_list)

let skip_current_dir = fun state ->
  if not (Vector.is_empty state.stack_list) then
    pop state

let rec maybe_emit_deferred = fun state ->
  if state.opts.contents_first && Vector.len state.stack_list < Vector.len state.deferred_dirs then
    match Vector.pop state.deferred_dirs with
    | Some dent when skippable state.opts dent.depth ->
        maybe_emit_deferred state
    | Some dent ->
        Some (Ok dent)
    | None ->
        None
  else
    None

let handle_entry = fun state (dent: entry) ->
  if not (state.opts.accept_entry dent) then
    None
  else
    let is_normal_dir = match dent.kind with Directory -> true | _ -> false in
    let should_follow_root_link =
      dent.depth = 0
      && state.opts.follow_root_links
      && (match dent.kind with
        | Symlink -> maybe_directory_target dent.path
        | _ -> false)
    in
    if is_normal_dir || should_follow_root_link then
      match push state dent with
      | Error err -> Some (Error err)
      | Ok () ->
          if is_normal_dir && state.opts.contents_first then (
            Vector.push state.deferred_dirs dent;
            None
          ) else if skippable state.opts dent.depth then
            None
          else
            Some (Ok dent)
    else if skippable state.opts dent.depth then
      None
    else
      Some (Ok dent)

let root_item = fun state root ->
  match entry_for_path { state.opts with follow_symlinks = false } ~depth:0 root with
  | Error err -> Some (Error err)
  | Ok (dent: entry) -> handle_entry state dent

let make_state = fun opts ->
  {
    opts;
    roots = opts.roots;
    stack_list = Vector.create ();
    deferred_dirs = Vector.create ();
    oldest_opened = 0;
  }

let rec next_item = fun state ->
  match maybe_emit_deferred state with
  | Some item -> Some item
  | None ->
      if Vector.is_empty state.stack_list then
        match state.roots with
        | [] -> None
        | root :: rest ->
            state.roots <- rest;
            begin
              match root_item state root with
              | Some item -> Some item
              | None -> next_item state
            end
      else if Vector.len state.stack_list > state.opts.max_depth then (
        pop state;
        next_item state
      ) else
        let current_idx = Vector.len state.stack_list - 1 in
        match Vector.get state.stack_list current_idx with
        | None -> None
        | Some dir_list -> (
            match next_dir_list state.opts dir_list with
            | None ->
                pop state;
                next_item state
            | Some (Error err) ->
                Some (Error err)
            | Some (Ok dent) ->
                begin
                  match handle_entry state dent with
                  | Some item -> Some item
                  | None -> next_item state
                end
          )

let filter_entry = fun walker ~f ->
  {
    walker with
    accept_entry = (fun entry -> walker.accept_entry entry && f entry);
  }

let into_iter = fun opts ->
  let module Base = struct
    type state = iterator_state

    type item = file_item

    let next = fun state -> (next_item state, state)

    let size = fun state ->
      List.length state.roots + Vector.len state.stack_list + Vector.len state.deferred_dirs
  end in
  Iterator.make (module Base) (make_state opts)

let walk = fun ~roots ?(sort = true) ?follow_symlinks ~f () ->
  match create ~roots ~sort ?follow_symlinks () with
  | Error err ->
      Error (IO.Unknown_error (create_error_message err))
  | Ok walker ->
      let state = make_state walker in
      let rec loop () =
        match next_item state with
        | None -> Ok ()
        | Some (Error err) -> Error err.cause
        | Some (Ok dent) ->
            match f dent with
            | Stop -> Ok ()
            | Continue -> loop ()
            | Skip_subtree ->
                if match dent.kind with Directory -> true | _ -> false then
                  skip_current_dir state;
                loop ()
      in
      loop ()

let to_list = fun ~roots ?(sort = true) ?follow_symlinks ?(include_directories = true) () ->
  match create ~roots ~sort ?follow_symlinks () with
  | Error err ->
      Error (IO.Unknown_error (create_error_message err))
  | Ok walker ->
      let iter = into_iter walker in
      let items = ref [] in
      let rec loop iter =
        match Iterator.next iter with
        | None, _ -> Ok (List.rev !items)
        | Some (Error err), _ -> Error err.cause
        | Some (Ok dent), iter' ->
            begin
              match dent.kind with
              | Directory when not include_directories -> ()
              | _ -> items := dent :: !items
            end;
            loop iter'
      in
      loop iter
