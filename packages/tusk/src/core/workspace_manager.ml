open Model
(** Workspace manager - caches workspace and avoids repeated scanning *)

type cached_workspace = {
  workspace : Workspace.t;
  root : string;
  last_scanned : Std.Datetime.t;
}
(** Cached workspace state *)

(** Global workspace cache *)
let workspace_cache = ref None

(** How long to cache workspace before rescanning (in seconds) *)
let cache_ttl = 30.0

(** Check if current directory is within the cached workspace *)
let is_within_workspace current_dir cached_root =
  String.starts_with ~prefix:cached_root current_dir

(** Check if cache is still valid *)
let is_cache_valid cached =
  let now = Std.Datetime.now () in
  let elapsed =
    Std.Datetime.to_timestamp now -. Std.Datetime.to_timestamp cached.last_scanned
  in
  elapsed < cache_ttl

(** Get workspace, using cache when possible *)
let get_workspace ~root =
  match !workspace_cache with
  | Some cached
    when is_within_workspace root cached.root && is_cache_valid cached ->
      (* Cache hit - reuse existing workspace *)
      cached.workspace
  | _ ->
      (* Cache miss or expired - scan workspace *)
      let path = Std.Path.of_string root |> Std.Result.unwrap in
      let workspace =
        match Workspace.scan path with
        | Ok ws -> ws
        | Error _ -> failwith "Failed to scan workspace"
      in
      workspace_cache :=
        Some
          {
            workspace;
            root = Std.Path.to_string workspace.root;
            last_scanned = Std.Datetime.now ();
          };
      workspace

(** Clear the workspace cache (useful for testing or when workspace changes) *)
let clear_cache () = workspace_cache := None

(** Get the current cached workspace root, if any *)
let get_cached_root () =
  match !workspace_cache with Some cached -> Some cached.root | None -> None

(** Scan workspace from a given path *)
let scan path = Workspace.scan path
