open Std

type t = {
  locator: string;
  selector: string;
  repository_root: Path.t;
  origin_url: string;
  package_subdir: Path.t option;
}

type error =
  | NotGitRepository of { path: Path.t }
  | MissingOriginRemote of { path: Path.t }
  | InvalidRepositoryRoot of { path: string; error: Path.error }
  | PackageOutsideRepository of { package_root: Path.t; repository_root: Path.t }
  | UnsupportedRemoteUrl of { url: string }
  | GitCommandFailed of { command: string; status: int; stdout: string; stderr: string }
  | GitCommandSpawnFailed of { command: string; error: Command.error }

let path_error_message = function
  | Path.InvalidUtf8 { path } -> "invalid utf8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> "invalid utf8 from " ^ syscall ^ ": " ^ path
  | Path.SystemError msg -> msg

let command_error_message = function
  | Command.SystemError error -> error

let message = function
  | NotGitRepository { path } ->
      "package root '" ^ Path.to_string path ^ "' is not inside a git repository"
  | MissingOriginRemote { path } ->
      "git repository at '" ^ Path.to_string path ^ "' is missing remote 'origin'"
  | InvalidRepositoryRoot { path; error } ->
      "git returned invalid repository root '" ^ path ^ "': " ^ path_error_message error
  | PackageOutsideRepository { package_root; repository_root } ->
      "package root '"
      ^ Path.to_string package_root
      ^ "' is not inside repository root '"
      ^ Path.to_string repository_root
      ^ "'"
  | UnsupportedRemoteUrl { url } ->
      "unsupported git remote URL for publish provenance: " ^ url
  | GitCommandFailed { command; status; stdout; stderr } ->
      let detail =
        if String.equal stderr "" then
          stdout
        else
          stderr
      in
      "git command '" ^ command ^ "' failed (exit " ^ Int.to_string status ^ "): " ^ detail
  | GitCommandSpawnFailed { command; error } ->
      "failed to spawn git command '" ^ command ^ "': " ^ command_error_message error

let run_git = fun ~cwd args ->
  let command = Command.make
    "env"
    ~args:([
      "-u";
      "GIT_DIR";
      "-u";
      "GIT_WORK_TREE";
      "-u";
      "GIT_INDEX_FILE";
      "git";
      "-C";
      Path.to_string cwd;
    ]
    @ args) in
  match Command.output command with
  | Error error ->
      Error (GitCommandSpawnFailed { command = Command.to_string command; error })
  | Ok output when not (Int.equal output.status 0) ->
      let stderr = String.trim output.stderr in
      if String.contains stderr "not a git repository" then
        Error (NotGitRepository { path = cwd })
      else if String.contains stderr "No such remote 'origin'" then
        Error (MissingOriginRemote { path = cwd })
      else
        Error (GitCommandFailed {
          command = Command.to_string command;
          status = output.status;
          stdout = output.stdout;
          stderr = output.stderr
        })
  | Ok output ->
      Ok (String.trim output.stdout)

let strip_git_suffix = fun url ->
  if String.ends_with ~suffix:".git" url then
    String.sub url ~offset:0 ~len:(String.length url - 4)
  else
    url

let split_once = fun ~on text ->
  match String.index_of text ~char:on with
  | None -> None
  | Some idx -> Some (
    String.sub text ~offset:0 ~len:idx,
    String.sub text ~offset:(idx + 1) ~len:(String.length text - idx - 1)
  )

let normalize_remote_url = fun raw_url ->
  let url = raw_url |> String.trim |> strip_git_suffix in
  let mk_locator host path =
    let path =
      if String.starts_with ~prefix:"/" path then
        String.sub path ~offset:1 ~len:(String.length path - 1)
      else
        path
    in
    if String.equal host "" || String.equal path "" then
      Error (UnsupportedRemoteUrl { url = raw_url })
    else
      Ok (host ^ "/" ^ path)
  in
  if String.starts_with ~prefix:"https://" url then
    let rest = String.sub url ~offset:8 ~len:(String.length url - 8) in
    match split_once ~on:'/' rest with
    | Some (host, path) -> mk_locator host path
    | None -> Error (UnsupportedRemoteUrl { url = raw_url })
  else if String.starts_with ~prefix:"http://" url then
    let rest = String.sub url ~offset:7 ~len:(String.length url - 7) in
    match split_once ~on:'/' rest with
    | Some (host, path) -> mk_locator host path
    | None -> Error (UnsupportedRemoteUrl { url = raw_url })
  else if String.starts_with ~prefix:"ssh://git@" url then
    let rest = String.sub url ~offset:10 ~len:(String.length url - 10) in
    match split_once ~on:'/' rest with
    | Some (host, path) -> mk_locator host path
    | None -> Error (UnsupportedRemoteUrl { url = raw_url })
  else if String.starts_with ~prefix:"git@" url then
    let rest = String.sub url ~offset:4 ~len:(String.length url - 4) in
    match split_once ~on:':' rest with
    | Some (host, path) -> mk_locator host path
    | None -> Error (UnsupportedRemoteUrl { url = raw_url })
  else
    Error (UnsupportedRemoteUrl { url = raw_url })

let package_subdir_from_root = fun ~repository_root ~package_root ->
  if Path.equal repository_root package_root then
    Ok None
  else
    match Path.strip_prefix package_root ~prefix:repository_root with
    | Ok subdir -> Ok (Some subdir)
    | Error _ -> Error (PackageOutsideRepository { package_root; repository_root })

let discover = fun ~package_root ->
  let package_root =
    match Fs.canonicalize package_root with
    | Ok canonical -> canonical
    | Error _ -> package_root
  in
  match run_git ~cwd:package_root [ "rev-parse"; "--show-toplevel" ] with
  | Error _ as err -> err
  | Ok repository_root_str -> (
      match Path.from_string repository_root_str with
      | Error err -> Error (InvalidRepositoryRoot { path = repository_root_str; error = err })
      | Ok repository_root -> (
          match package_subdir_from_root ~repository_root ~package_root with
          | Error _ as err -> err
          | Ok package_subdir -> (
              match run_git ~cwd:package_root [ "remote"; "get-url"; "origin" ] with
              | Error _ as err -> err
              | Ok origin_url -> (
                  match normalize_remote_url origin_url with
                  | Error _ as err -> err
                  | Ok repository_locator -> (
                      match run_git ~cwd:package_root [ "rev-parse"; "HEAD" ] with
                      | Error _ as err -> err
                      | Ok selector ->
                          let locator =
                            match package_subdir with
                            | None -> repository_locator
                            | Some subdir -> repository_locator ^ "/" ^ Path.to_string subdir
                          in
                          Ok {
                            locator;
                            selector;
                            repository_root;
                            origin_url;
                            package_subdir;
                          }
                    )
                )
            )
        )
    )
