open Global
open Iter
open Collections

type entry_kind =
  | File
  | Directory
  | Symlink
  | Other

type path_repr =
  | Full_path of string
  | Joined_path of { dir_path: string; name: string }

type entry = {
  path_repr: path_repr;
  name: string;
  depth: int;
  kind: entry_kind;
  mutable path_string_cache: string option;
  mutable path_cache: Path.t option;
}

let join_path_string = fun dir_path name ->
  if String.equal dir_path "" then
    name
  else if dir_path.[String.length dir_path - 1] = '/' then
    dir_path ^ name
  else
    dir_path ^ "/" ^ name

let basename_string = fun path ->
  if String.equal path "" then
    ""
  else if String.equal path "/" then
    "/"
  else
    let parts = String.split_on_char '/' path in
    match List.rev parts with
    | [] -> ""
    | last :: _ ->
        if String.equal last "" then
          "/"
        else
          last

let path_string_of_entry = fun item ->
  match item.path_string_cache with
  | Some path_string -> path_string
  | None ->
      let path_string =
        match item.path_repr with
        | Full_path path_string -> path_string
        | Joined_path { dir_path; name } -> join_path_string dir_path name
      in
      item.path_string_cache <- Some path_string;
      path_string

module FileItem = struct
  type t = entry

  let make = fun ~path_string ~name ~depth ~kind ->
    {
      path_repr = Full_path path_string;
      name;
      depth;
      kind;
      path_string_cache = Some path_string;
      path_cache = None;
    }

  let path = fun item ->
    match item.path_cache with
    | Some path -> path
    | None ->
        let path_string = path_string_of_entry item in
        let path = Path.of_string path_string
        |> Result.expect ~msg:(("Invalid walker path " ^ path_string)) in
        item.path_cache <- Some path;
        path

  let path_string = path_string_of_entry

  let name = fun item -> item.name

  let depth = fun item -> item.depth

  let kind = fun item -> item.kind
end

type error = {
  path: Path.t option;
  depth: int;
  cause: Common.error;
}

type file_item = (FileItem.t, error) result

type create_error =
  | MinDepthCannotBeMoreThanMaxDepth of { min_depth: int; max_depth: int }

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
  | Opened of { depth: int; dir_path: string; handle: ReadDir.t }
  | Closed of { entries: file_item Vector.t; mutable index: int }

type iterator_state = {
  opts: t;
  mutable roots: Path.t list;
  stack_list: dir_list Vector.t;
  deferred_dirs: entry Vector.t;
  mutable oldest_opened: int;
}

let create_error_message = function
  | MinDepthCannotBeMoreThanMaxDepth { min_depth; max_depth } -> "Walker min_depth cannot be greater than max_depth (min_depth="
  ^ string_of_int min_depth
  ^ ", max_depth="
  ^ string_of_int max_depth
  ^ ")"

let create ~roots ?(sort = false) ?(follow_symlinks = false) ?(follow_root_links = true) ?(max_open = 10) ?(min_depth = 0) ?(max_depth = max_int) ?(contents_first = false) () =
  let max_open =
    if max_open <= 0 then
      1
    else
      max_open
  in
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

let metadata_for_path_string = fun ~follow_symlinks path_string ->
  if follow_symlinks then
    Kernel.Fs.File.stat path_string |> Common.convert_kernel_result
  else
    Kernel.Fs.File.lstat path_string |> Common.convert_kernel_result

let make_error = fun ?path ~depth cause -> { path; depth; cause }

let path_option_of_string = fun path_string ->
  match Path.of_string path_string with
  | Ok path -> Some path
  | Error _ -> None

let full_path_entry = fun ~depth ~kind path_string ->
  let path_cache =
    match Path.of_string path_string with
    | Ok path -> Some path
    | Error _ -> None
  in
  {
    path_repr = Full_path path_string;
    name = basename_string path_string;
    depth;
    kind;
    path_string_cache = Some path_string;
    path_cache;
  }

let joined_entry = fun ~depth ~kind ~dir_path ~name ->
  {
    path_repr = Joined_path { dir_path; name };
    name;
    depth;
    kind;
    path_string_cache = None;
    path_cache = None;
  }

let entry_for_path_string = fun opts ~depth path_string ->
  match metadata_for_path_string ~follow_symlinks:opts.follow_symlinks path_string with
  | Ok metadata -> Ok (full_path_entry ~depth ~kind:(entry_kind_of_metadata metadata) path_string)
  | Error cause -> Error (make_error ?path:(path_option_of_string path_string) ~depth cause)

let root_entry_for_path = fun opts ~depth path ->
  let path_string = Path.to_string path in
  match metadata_for_path_string ~follow_symlinks:opts.follow_symlinks path_string with
  | Ok metadata ->
      Ok {
        path_repr = Full_path path_string;
        name = Path.basename path;
        depth;
        kind = entry_kind_of_metadata metadata;
        path_string_cache = Some path_string;
        path_cache = Some path;
      }
  | Error cause -> Error (make_error ~path ~depth cause)

let hinted_entry_for_name = fun opts ~depth ~dir_path ~name kind ->
  match kind with
  | ReadDir.Regular ->
      Ok (joined_entry ~depth ~kind:File ~dir_path ~name)
  | ReadDir.Directory ->
      Ok (joined_entry ~depth ~kind:Directory ~dir_path ~name)
  | ReadDir.Symlink ->
      let path_string = join_path_string dir_path name in
      if opts.follow_symlinks then
        entry_for_path_string opts ~depth path_string
      else
        Ok (joined_entry ~depth ~kind:Symlink ~dir_path ~name)
  | ReadDir.Other ->
      Ok (joined_entry ~depth ~kind:Other ~dir_path ~name)
  | ReadDir.Unknown ->
      let path_string = join_path_string dir_path name in
      entry_for_path_string opts ~depth path_string

let compare_item_path = fun left right ->
  match (left, right) with
  | Ok (left: entry), Ok (right: entry) ->
      String.compare (FileItem.path_string left) (FileItem.path_string right)
  | Error (left: error), Error (right: error) ->
      let left = Option.map Path.to_string left.path |> Option.unwrap_or ~default:"" in
      let right = Option.map Path.to_string right.path |> Option.unwrap_or ~default:"" in
      String.compare left right
  | Error _, Ok _ ->
      (-1)
  | Ok _, Error _ ->
      1

let next_dir_entry = fun opts ~depth ~dir_path handle ->
  match ReadDir.next_raw_entry handle with
  | None -> None
  | Some relative -> Some (hinted_entry_for_name
    opts
    ~depth:((depth + 1))
    ~dir_path
    ~name:relative.name
    relative.kind)

let next_dir_list = fun opts dir_list ->
  match dir_list with
  | Opened { depth; dir_path; handle } -> next_dir_entry opts ~depth ~dir_path handle
  | Closed state ->
      if state.index >= Vector.len state.entries then
        None
      else
        match Vector.get state.entries state.index with
        | Some item ->
            state.index <- state.index + 1;
            Some item
        | None -> None

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
        | None -> ()
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

let maybe_directory_target = fun path_string ->
  match metadata_for_path_string ~follow_symlinks:true path_string with
  | Ok metadata -> Metadata.is_dir metadata
  | Error _ -> false

let push = fun state (dent: entry) ->
  let open_handle_budget = Vector.len state.stack_list - state.oldest_opened in
  (
    if open_handle_budget = state.opts.max_open then
      match Vector.get state.stack_list state.oldest_opened with
      | Some dir_list -> Vector.set
        state.stack_list
        state.oldest_opened
        (close_dir_list state.opts dir_list)
      | None -> ()
  );
  let dir_path = FileItem.path_string dent in
  match ReadDir.create_string dir_path with
  | Error cause -> Error (make_error ?path:(path_option_of_string dir_path) ~depth:dent.depth cause)
  | Ok handle ->
      let dir_list = Opened { depth = dent.depth; dir_path; handle } in
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
    | Some dent when skippable state.opts dent.depth -> maybe_emit_deferred state
    | Some dent -> Some (Ok dent)
    | None -> None
  else
    None

let handle_entry = fun state (dent: entry) ->
  if not (state.opts.accept_entry dent) then
    None
  else
    let is_normal_dir =
      match dent.kind with
      | Directory -> true
      | _ -> false
    in
    let should_follow_root_link =
      dent.depth = 0
      && state.opts.follow_root_links
      && (
        match dent.kind with
        | Symlink -> maybe_directory_target (FileItem.path_string dent)
        | _ -> false
      )
    in
    if is_normal_dir || should_follow_root_link then
      match push state dent with
      | Error err -> Some (Error err)
      | Ok () ->
          if is_normal_dir && state.opts.contents_first then
            (
              Vector.push state.deferred_dirs dent;
              None
            )
          else if skippable state.opts dent.depth then
            None
          else
            Some (Ok dent)
    else if skippable state.opts dent.depth then
      None
    else
      Some (Ok dent)

let root_item = fun state root ->
  match root_entry_for_path { state.opts with follow_symlinks = false } ~depth:0 root with
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
      else if Vector.len state.stack_list > state.opts.max_depth then
        (
          pop state;
          next_item state
        )
      else
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
            | Some (Ok dent) -> begin
                match handle_entry state dent with
                | Some item -> Some item
                | None -> next_item state
              end
          )

let filter_entry = fun walker ~f ->
  { walker with accept_entry = (fun entry -> walker.accept_entry entry && f entry) }

let into_iter = fun opts ->
  let module Base = struct
    type state = iterator_state

    type item = file_item

    let next = fun state -> (next_item state, state)

    let size = fun state ->
      List.length state.roots + Vector.len state.stack_list + Vector.len state.deferred_dirs
  end in
  Iterator.make (module Base) (make_state opts)

let walk ~roots ?(sort = true) ?follow_symlinks ~f () =
  match create ~roots ~sort ?follow_symlinks () with
  | Error err -> Error (IO.Unknown_error (create_error_message err))
  | Ok walker ->
      let state = make_state walker in
      let rec loop () =
        match next_item state with
        | None ->
            Ok ()
        | Some (Error err) ->
            Error err.cause
        | Some (Ok dent) ->
            match f dent with
            | Stop ->
                Ok ()
            | Continue ->
                loop ()
            | Skip_subtree ->
                let should_skip =
                  match FileItem.kind dent with
                  | Directory -> true
                  | _ -> false
                in
                if should_skip then
                  skip_current_dir state;
                loop ()
      in
      loop ()

let to_list ~roots ?(sort = true) ?follow_symlinks ?(include_directories = true) () =
  match create ~roots ~sort ?follow_symlinks () with
  | Error err -> Error (IO.Unknown_error (create_error_message err))
  | Ok walker ->
      let iter = into_iter walker in
      let items = ref [] in
      let rec loop iter =
        match Iterator.next iter with
        | None, _ ->
            Ok (List.rev !items)
        | Some (Error err), _ ->
            Error err.cause
        | Some (Ok dent), iter' ->
            (
              match FileItem.kind dent with
              | Directory when not include_directories -> ()
              | _ -> items := dent :: !items
            );
            loop iter'
      in
      loop iter
