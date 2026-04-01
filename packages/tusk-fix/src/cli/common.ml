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
  List.length result.Runner.parse_diagnostics + List.length result.diagnostics

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
    let parse_count = List.length result.Runner.parse_diagnostics in
    if parse_count >= remaining then
      {
        result
        with Runner.parse_diagnostics = take remaining result.parse_diagnostics;
        diagnostics = []
      }
    else
      { result with Runner.diagnostics = take (remaining - parse_count) result.diagnostics }

let args_of_matches = fun matches ->
  let args = ref [] in
  let push_flag enabled flag =
    if enabled then
      args := !args @ [ flag ]
  in
  let push_option name render =
    match render with
    | Some value -> args := !args @ [ name; value ]
    | None -> ()
  in
  push_flag (ArgParser.get_flag matches "list-rules") "--list-rules";
  push_flag (ArgParser.get_flag matches "list-diagnostics") "--list-diagnostics";
  push_flag (ArgParser.get_flag matches "json") "--json";
  push_flag (ArgParser.get_flag matches "apply") "--apply";
  if not (ArgParser.get_flag matches "apply") then
    push_flag true "--check";
  push_option "--limit"
    (ArgParser.get_int matches "limit" |> Option.map Int.to_string);
  push_option "--explain" (ArgParser.get_one matches "explain");
  (
    match ArgParser.get_path matches "path" with
    | Some path -> args := !args @ [ Path.to_string path ]
    | None -> ()
  );
  !args
