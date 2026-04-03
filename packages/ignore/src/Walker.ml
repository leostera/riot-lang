open Std
module List = Collections.List

type error =
  | File_system of { path: Path.t option; cause: Fs.error }
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
  sort: bool;
  follow_symlinks: bool;
  hidden: bool;
  parents: bool;
  ignore: bool;
  git_ignore: bool;
  custom_ignore_filenames: string list;
  overrides: Gitignore.t;
}

let basename_is_hidden = fun entry ->
  let name = Fs.Walker.FileItem.name entry in
  String.length name > 0 && Char.equal name.[0] '.'

let trim_frames = fun frames depth ->
  let rec loop index =
    function
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
  | Error (Gitignore.File_system cause) ->
      Error (File_system { path = Some path; cause })
  | Error (Gitignore.Invalid_glob { line; input; message; offset }) ->
      Error (Invalid_glob { path; line; input; message; offset })

let load_frame = fun config entry ->
  let dir = Fs.Walker.FileItem.path entry in
  let rec load_custom acc =
    function
    | [] -> Ok (List.rev acc)
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
      List.rev frames
    else
      match List.rev frames with
      | [] -> []
      | frame :: _ -> [ frame ]
  in
  let rec match_group getter =
    let rec find_in_frames =
      function
      | [] -> Match.None_
      | frame :: rest ->
          let rec find_in_matchers =
            function
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
  let custom_match = match_group (fun frame -> List.rev frame.custom) in
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
  custom_match |> Match.or_else ignore_match |> Match.or_else git_ignore_match

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
    let ignore_match = match_across_frames config frames path ~is_dir in
    if Match.is_ignore ignore_match then
      Ok Match.Ignore
    else if Match.is_whitelist ignore_match then
      Ok Match.Whitelist
    else if config.hidden && Fs.Walker.FileItem.depth entry > 0 && basename_is_hidden entry then
      Ok Match.Ignore
    else
      Ok Match.None_

let create = fun ~roots ?(sort = false) ?(follow_symlinks = false) ?(hidden = true) ?(parents = true) ?(ignore = true) ?(git_ignore = true) ?(custom_ignore_filenames = []) ?(overrides = []) () ->
  let root =
    match roots with
    | [] -> Path.v "."
    | root :: _ -> root
  in
  match Gitignore.of_lines ~root ~syntax:Gitignore.Override overrides with
  | Error { line; input; message; offset } ->
      Error (Glob.Invalid_glob { input = "line " ^ string_of_int line ^ ": " ^ input; message; offset })
  | Ok override_matcher ->
      Ok
        {
          roots;
          sort;
          follow_symlinks;
          hidden;
          parents;
          ignore;
          git_ignore;
          custom_ignore_filenames;
          overrides = override_matcher;
        }

let walk = fun config ~f ->
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
  match Fs.Walker.walk ~roots:config.roots ~sort:config.sort ~follow_symlinks:config.follow_symlinks ~f:wrapped () with
  | Ok () -> (
      match !deferred_error with
      | Some err -> Error err
      | None -> Ok ()
    )
  | Error cause ->
      Error (File_system { path = None; cause })

let to_list = fun config ->
  let items = ref [] in
  walk config
    ~f:(fun entry ->
      items := entry :: !items;
      Fs.Walker.Continue)
  |> Result.map (fun () -> List.rev !items)
