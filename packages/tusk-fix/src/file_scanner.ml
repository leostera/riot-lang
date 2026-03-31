open Std
open Std.Collections

type t = {
  roots: Path.t list;
  exclude_patterns: string list;
  should_ignore: Path.t -> bool;
}

type state = {
  scanner: t;
  owner: Pid.t option;
  seen: string HashSet.t;
  mutable pending: Path.t list;
}

let create_many = fun ~roots ?(exclude_patterns = [ "."; "_build"; "target" ]) ?(should_ignore = fun _ ->
    false) () ->
    {roots; exclude_patterns; should_ignore}

let create = fun ~root ?exclude_patterns ?should_ignore () ->
    create_many ~roots:[ root ] ?exclude_patterns ?should_ignore ()

let is_non_source_test_path = fun path ->
    let path_str = Path.to_string path in
    String.contains path_str "/tests/fixtures/"
    || String.contains path_str "/tests/generated/"
    || String.contains path_str "/tests/diagnostics/"

let is_ocaml_source = fun path ->
    let path_str = Path.to_string path in
    String.ends_with ~suffix:".ml" path_str || String.ends_with ~suffix:".mli" path_str

let should_exclude = fun scanner path ->
    let dir_name = Path.basename path in
    List.exists
      (fun pattern -> String.starts_with ~prefix:pattern dir_name || String.equal pattern dir_name)
      scanner.exclude_patterns

let sorted_directory_entries = fun dir ->
    match Fs.read_dir dir with
    | Error _ -> []
    | Ok entries ->
        entries
        |> Iter.MutIterator.to_list
        |> List.map (fun entry -> Path.(dir / entry))
        |> List.sort
          (fun left right ->
            String.compare (Path.to_string left) (Path.to_string right))

let compare_paths = fun left right ->
    String.compare (Path.to_string left) (Path.to_string right)

let init_state = fun ?owner scanner ->
    {scanner; owner; seen = HashSet.create (); pending = List.sort compare_paths scanner.roots; }

let rec next_discovered_file = fun state ->
    match state.pending with
    | [] -> None
    | path :: rest ->
        state.pending <- rest;
        let path_string = Path.to_string path in
        if HashSet.contains state.seen path_string then
          next_discovered_file state
        else
          (
            let _ = HashSet.insert state.seen path_string in
            match Fs.is_dir path with
            | Ok true ->
                if
                  should_exclude state.scanner path
                  || is_non_source_test_path path
                  || state.scanner.should_ignore path
                then
                  next_discovered_file state
                else (
                  state.pending <- sorted_directory_entries path @ state.pending;
                  next_discovered_file state
                )
            | Ok false
            | Error _ ->
                if
                  is_ocaml_source path
                  && not (is_non_source_test_path path)
                  && not (state.scanner.should_ignore path)
                then
                  Some path
                else
                  next_discovered_file state
          )

let scan = fun scanner ->
    let state = init_state scanner in
    let rec collect acc =
      match next_discovered_file state with
      | Some file -> collect (file :: acc)
      | None -> List.rev acc
    in
    collect []

let rec scanner_loop = fun state ->
    match next_discovered_file state with
    | Some file ->
        let owner = Option.expect ~msg:"streaming file scanner requires an owner" state.owner in
        send owner (Messages.ScannerDiscovered file);
        scanner_loop state
    | None ->
        let owner = Option.expect ~msg:"streaming file scanner requires an owner" state.owner in
        send owner Messages.ScannerComplete;
        Ok ()

let start = fun ~owner scanner ->
    let state = init_state ~owner scanner in
    spawn (fun () -> scanner_loop state)
