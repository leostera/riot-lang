open Std

type t = { root : Path.t; exclude_patterns : string list }

let create ~root ?(exclude_patterns = [ "."; "_build"; "target" ]) () =
  { root; exclude_patterns }

let should_exclude t dir_name =
  List.exists
    (fun pattern ->
      String.starts_with ~prefix:pattern dir_name
      || String.equal pattern dir_name)
    t.exclude_patterns

let rec walk_dir t dir =
  match Fs.read_dir dir with
  | Error _ -> []
  | Ok entries ->
      let entry_list = Iter.MutIterator.to_list entries in
      List.concat_map
        (fun entry ->
          let entry_path = Path.(dir / entry) in
          match Fs.is_dir entry_path with
          | Ok true ->
              let dir_name = Path.to_string entry in
              if should_exclude t dir_name then []
              else walk_dir t entry_path
          | Ok false | Error _ ->
              let path_str = Path.to_string entry_path in
              if
                String.ends_with ~suffix:".ml" path_str
                || String.ends_with ~suffix:".mli" path_str
              then [ entry_path ]
              else [])
        entry_list

let scan t = walk_dir t t.root
