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
}

let create_many = fun ~roots ?(exclude_patterns = [ "."; "_build"; "target" ]) ?(should_ignore = fun _ ->
  false) () ->
  { roots; exclude_patterns; should_ignore }

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

let init_state = fun ?owner scanner -> { scanner; owner; seen = HashSet.create () }

let handle_entry = fun state (entry: Std.Fs.Walker.FileItem.t) on_file ->
  let path = Std.Fs.Walker.FileItem.path entry in
  let path_string = Path.to_string path in
  if HashSet.contains state.seen path_string then
    Std.Fs.Walker.Skip_subtree
  else
    (
      let _ = HashSet.insert state.seen path_string in
      match Std.Fs.Walker.FileItem.kind entry with
      | Directory ->
          if
            should_exclude state.scanner path
            || is_non_source_test_path path
            || state.scanner.should_ignore path
          then
            Std.Fs.Walker.Skip_subtree
          else
            Std.Fs.Walker.Continue
      | File ->
          if
            is_ocaml_source path
            && not (is_non_source_test_path path)
            && not (state.scanner.should_ignore path)
          then
            on_file path;
          Std.Fs.Walker.Continue
      | Symlink
      | Other ->
          Std.Fs.Walker.Continue
    )

let scan = fun scanner ->
  let state = init_state scanner in
  let files = ref [] in
  let _ =
    Std.Fs.Walker.walk
      ~roots:scanner.roots
      ~f:(fun entry -> handle_entry state entry (fun path -> files := path :: !files))
      ()
  in
  List.rev !files

let start = fun ~owner scanner ->
  let state = init_state ~owner scanner in
  spawn
    (fun () ->
      let _ =
        Std.Fs.Walker.walk
          ~roots:scanner.roots
          ~f:(fun entry ->
            handle_entry state entry (fun path -> send owner (Messages.ScannerDiscovered path)))
          ()
      in
      send owner Messages.ScannerComplete;
      Ok ())
