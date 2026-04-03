open Std

let current_dir = fun () -> Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"

let set_current_dir = fun path ->
  Env.set_current_dir path
  |> Result.expect ~msg:(("Failed to change directory to " ^ Path.to_string path))

let with_cwd = fun ?cwd fn ->
  match cwd with
  | None -> fn ()
  | Some cwd ->
      let original = current_dir () in
      set_current_dir cwd;
      try
        let result = fn () in
        set_current_dir original;
        result
      with
      | exn ->
          set_current_dir original;
          raise exn

let default_path = fun () ->
  let cwd = current_dir () in
  let workspace_root =
    match Fix_config.load_scope ~cwd with
    | Some scope -> Fix_config.workspace_root scope
    | None -> cwd
  in
  let packages_dir = Path.(workspace_root / Path.v "packages") in
  if Fs.is_dir packages_dir |> Result.unwrap_or ~default:false then
    packages_dir
  else
    cwd

let resolve_target = fun matches ->
  match ArgParser.get_path matches "path" with
  | Some path -> path
  | None -> default_path ()

let relative_to_cwd = fun path ->
  let cwd = current_dir () in
  match Path.strip_prefix path ~prefix:cwd with
  | Ok rel_path -> Path.to_string rel_path
  | Error _ -> Path.to_string path

let diagnostic_count = fun result ->
  Runner.(List.length result.parse_diagnostics + List.length result.diagnostics)

let rec take = fun n xs ->
  if n <= 0 then
    []
  else
    match xs with
    | [] -> []
    | x :: rest -> x :: take (n - 1) rest

let clip_result_to_limit = fun remaining result ->
  if remaining <= 0 then
    { result with Runner.parse_diagnostics = []; diagnostics = [] }
  else
    let parse_count = List.length Runner.(result.parse_diagnostics) in
    if parse_count >= remaining then
      Runner.{
        result
        with parse_diagnostics = take remaining result.parse_diagnostics;
        diagnostics = []
      }
    else
      Runner.{ result with diagnostics = take (remaining - parse_count) result.diagnostics }
