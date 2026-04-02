open Std

type spec = {
  source_locator: string;
  ref_: string option;
}

type locator = {
  host: string;
  owner: string;
  repo: string;
  subdir: Path.t option;
}

type materialized = {
  source_locator: string;
  ref_: string;
  repository_root: Path.t;
  package_root: Path.t;
}

type error =
  | InvalidSourceSpec of { source: string; error: string }
  | UnsupportedSourceHost of { source: string; host: string }
  | CachedRepositoryInvalid of { path: Path.t }
  | PackageRootMissing of { path: Path.t }
  | GitCommandFailed of { command: string; status: int; stdout: string; stderr: string }
  | GitCommandSpawnFailed of { command: string; error: string }

let ( let* ) = Result.and_then

let message = function
  | InvalidSourceSpec { source; error } ->
      "invalid source dependency '" ^ source ^ "': " ^ error
  | UnsupportedSourceHost { source; host } ->
      "source dependency '" ^ source ^ "' uses unsupported host '" ^ host ^ "'"
  | CachedRepositoryInvalid { path } ->
      "cached git dependency repository at '" ^ Path.to_string path ^ "' is not a git checkout"
  | PackageRootMissing { path } ->
      "materialized source dependency is missing package root '" ^ Path.to_string path ^ "'"
  | GitCommandFailed { command; status; stdout; stderr } ->
      let detail =
        if String.equal (String.trim stderr) "" then
          String.trim stdout
        else
          String.trim stderr
      in
      "git command '" ^ command ^ "' failed (exit " ^ Int.to_string status ^ "): " ^ detail
  | GitCommandSpawnFailed { command; error } ->
      "failed to spawn git command '" ^ command ^ "': " ^ error

let split_once = fun ~on text ->
  match String.index_opt text on with
  | None -> None
  | Some idx -> Some (
    String.sub text 0 idx,
    String.sub text (idx + 1) (String.length text - idx - 1)
  )

let normalize_source_locator = fun raw ->
  let raw = String.trim raw in
  let raw =
    if String.starts_with ~prefix:"https://" raw then
      String.sub raw 8 (String.length raw - 8)
    else if String.starts_with ~prefix:"http://" raw then
      String.sub raw 7 (String.length raw - 7)
    else
      raw
  in
  if String.ends_with ~suffix:".git" raw then
    String.sub raw 0 (String.length raw - 4)
  else
    raw

let parse_spec = fun raw ->
  let trimmed = String.trim raw in
  match String.split_on_char '#' trimmed with
  | [ locator ] -> Ok { source_locator = normalize_source_locator locator; ref_ = None }
  | [locator;ref_] ->
      Ok {
        source_locator = normalize_source_locator locator;
        ref_ =
          if String.equal (String.trim ref_) "" then
            None
          else
            Some (String.trim ref_);
      }
  | _ -> Error (InvalidSourceSpec { source = raw; error = "expected at most one #ref suffix" })

let parse_source_locator = fun source_locator ->
  let normalized = normalize_source_locator source_locator in
  match String.split_on_char '/' normalized with
  | host :: owner :: repo :: rest when not (String.equal host "")
  && not (String.equal owner "")
  && not (String.equal repo "") ->
      if not (String.equal host "github.com") then
        Error (UnsupportedSourceHost { source = source_locator; host })
      else
        let subdir =
          match rest with
          | [] -> None
          | _ -> Some (Path.v (String.concat "/" rest))
        in
        Ok { host; owner; repo; subdir }
  | _ -> Error (InvalidSourceSpec {
    source = source_locator;
    error = "expected github.com/<owner>/<repo>[/path/to/package]"
  })

let run_git = fun ?cwd args ->
  let args =
    match cwd with
    | Some cwd -> [ "-C"; Path.to_string cwd ] @ args
    | None -> args
  in
  let command = Command.make "env"
    ~args:((
      [
        "-u";
        "GIT_DIR";
        "-u";
        "GIT_WORK_TREE";
        "-u";
        "GIT_INDEX_FILE";
        "-u";
        "GIT_COMMON_DIR";
        "-u";
        "GIT_OBJECT_DIRECTORY";
        "-u";
        "GIT_ALTERNATE_OBJECT_DIRECTORIES";
        "-u";
        "GIT_IMPLICIT_WORK_TREE";
        "git";
      ] @ args
    ))
  in
  match Command.output command with
  | Error (Command.SystemError error) -> Error (GitCommandSpawnFailed {
    command = Command.to_string command;
    error
  })
  | Ok output when not (Int.equal output.status 0) -> Error (GitCommandFailed {
    command = Command.to_string command;
    status = output.status;
    stdout = output.stdout;
    stderr = output.stderr
  })
  | Ok output -> Ok (String.trim output.stdout)

let remote_url_of_locator = fun ({ host; owner; repo; _ }: locator) ->
  "https://" ^ host ^ "/" ^ owner ^ "/" ^ repo ^ ".git"

let checkout_target = fun ~repo_dir ~ref_ ->
  match run_git ~cwd:repo_dir [ "rev-parse"; "--verify"; "--quiet"; ref_ ] with
  | Ok _ -> ref_
  | Error _ -> (
      match run_git ~cwd:repo_dir [ "rev-parse"; "--verify"; "--quiet"; "origin/" ^ ref_ ] with
      | Ok _ -> "origin/" ^ ref_
      | Error _ -> ref_
    )

let sync_checkout = fun ~repo_dir ~remote_url ~ref_ ->
  let repo_git_dir = Path.(repo_dir / Path.v ".git") in
  let parent =
    match Path.parent repo_dir with
    | Some parent -> parent
    | None -> Path.v "."
  in
  let* has_repo_root = Fs.exists repo_dir
  |> Result.map_error
    (fun err -> GitCommandSpawnFailed { command = "fs.exists"; error = IO.error_message err }) in
  if has_repo_root then
    let* has_git_dir = Fs.exists repo_git_dir
    |> Result.map_error
      (fun err -> GitCommandSpawnFailed { command = "fs.exists"; error = IO.error_message err }) in
    if not has_git_dir then
      Error (CachedRepositoryInvalid { path = repo_dir })
    else
      let target = checkout_target ~repo_dir ~ref_ in
      let checkout_current () = run_git ~cwd:repo_dir [ "checkout"; "--quiet"; "--force"; target ]
      |> Result.map (fun _ -> ()) in
      match checkout_current () with
      | Ok () -> Ok ()
      | Error _ ->
          let* _ = run_git ~cwd:repo_dir [ "remote"; "set-url"; "origin"; remote_url ] in
          let* _ = run_git ~cwd:repo_dir [ "fetch"; "--quiet"; "--tags"; "origin" ] in
          run_git ~cwd:repo_dir [ "checkout"; "--quiet"; "--force"; checkout_target ~repo_dir ~ref_ ]
          |> Result.map (fun _ -> ())
  else
    let* () = Fs.create_dir_all parent
    |> Result.map_error
      (fun err ->
        GitCommandSpawnFailed { command = "fs.create_dir_all"; error = IO.error_message err }) in
    let* _ = run_git ~cwd:parent [ "clone"; "--quiet"; remote_url; Path.to_string repo_dir ] in
    run_git ~cwd:repo_dir [ "checkout"; "--quiet"; "--force"; checkout_target ~repo_dir ~ref_ ]
    |> Result.map (fun _ -> ())

let materialize = fun ~source_locator ~ref_ () ->
  let* locator = parse_source_locator source_locator in
  let repository_root = Riot_model.Riot_dirs.git_registry_repo_dir
    ~host:locator.host
    ~owner:locator.owner
    ~repo:locator.repo in
  let ref_ = Option.unwrap_or ~default:"main" ref_ in
  let* () = sync_checkout ~repo_dir:repository_root ~remote_url:(remote_url_of_locator locator) ~ref_ in
  let package_root =
    match locator.subdir with
    | Some subdir -> Path.(repository_root / subdir)
    | None -> repository_root
  in
  let* exists = Fs.exists package_root
  |> Result.map_error
    (fun err -> GitCommandSpawnFailed { command = "fs.exists"; error = IO.error_message err }) in
  if not exists then
    Error (PackageRootMissing { path = package_root })
  else
    Ok { source_locator; ref_; repository_root; package_root }
