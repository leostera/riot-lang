open Std
open Std.Iter
open Std.Collections
open Riot_model

module ContentStore = Contentstore.Store

(** Store - Content-addressable storage for build artifacts * *)
module Manifest = Manifest

let ( let* ) result fn = Result.and_then result ~fn

let package_artifacts_namespace =
  Contentstore.Namespace.from_parts [ "package-artifacts" ]
  |> Result.expect ~msg:"riot-store package artifacts namespace should be valid"

let action_artifacts_namespace =
  Contentstore.Namespace.from_parts [ "action-artifacts" ]
  |> Result.expect ~msg:"riot-store action artifacts namespace should be valid"

let plans_namespace =
  Contentstore.Namespace.from_parts [ "plans" ]
  |> Result.expect ~msg:"riot-store plans namespace should be valid"

type node_payload_namespace =
  | DependencyResolution
  | ExternalDependencyReady
  | ToolchainReady
  | SourceAnalysis
  | ModulePlans
  | ActionSpecs

let node_payload_namespace_to_string = fun __tmp1 ->
  match __tmp1 with
  | DependencyResolution -> "dependency-resolution"
  | ExternalDependencyReady -> "external-dependency-ready"
  | ToolchainReady -> "toolchain-ready"
  | SourceAnalysis -> "source-analysis"
  | ModulePlans -> "module-plans"
  | ActionSpecs -> "action-specs"

let node_payload_namespace = fun namespace ->
  Contentstore.Namespace.from_parts [ node_payload_namespace_to_string namespace ]
  |> Result.expect ~msg:"riot-store node payload namespace should be valid"

type t = {
  package_store: ContentStore.t;
  action_store: ContentStore.t;
  plan_store: ContentStore.t;
  dependency_resolution_store: ContentStore.t;
  external_dependency_store: ContentStore.t;
  toolchain_ready_store: ContentStore.t;
  source_analysis_store: ContentStore.t;
  module_plan_store: ContentStore.t;
  action_spec_store: ContentStore.t;
  package_cache: artifact_cache;
  action_cache: artifact_cache;
}

and artifact_cache = {
  artifacts: (string, Artifact.t) ConcurrentHashMap.t;
}

type error =
  | HashNotFound of {
      hash: Crypto.hash;
    }
  | LoadManifestFailed of {
      path: Path.t;
      cause: string;
    }
  | CreateTargetDirFailed of {
      path: Path.t;
      cause: Fs.error;
    }
  | CreateParentDirFailed of {
      path: Path.t;
      cause: Fs.error;
    }
  | ReadSourceMetadataFailed of {
      path: Path.t;
      cause: Fs.error;
    }
  | CopyArtifactFailed of {
      src: Path.t;
      dst: Path.t;
      cause: Fs.error;
    }
  | SetCopiedArtifactPermissionsFailed of {
      src: Path.t;
      dst: Path.t;
      cause: Fs.error;
    }
  | CreateTempDirFailed of {
      path: Path.t;
      cause: Fs.error;
    }
  | CheckSourceExistsFailed of {
      path: Path.t;
      cause: Fs.error;
    }
  | DeclaredOutputMissing of {
      path: Path.t;
    }
  | DeclaredOutputOutsideSandbox of {
      path: Path.t;
      sandbox_dir: Path.t;
    }
  | MetadataReadFailed of {
      path: Path.t;
      cause: Fs.error;
    }
  | SaveManifestFailed of {
      path: Path.t;
      cause: string;
    }
  | CommitArtifactsFailed of {
      source_dir: Path.t;
      destination_dir: Path.t;
      cause: string;
    }
  | SavePlanBundleFailed of {
      hash: Crypto.hash;
      cause: string;
    }
  | SaveNodePayloadFailed of {
      namespace: node_payload_namespace;
      hash: Crypto.hash;
      cause: string;
    }
  | ExportPathMustBeRelative of {
      path: Path.t;
    }
  | CreatePackageOutputDirFailed of {
      path: Path.t;
      cause: Fs.error;
    }
  | CopyExportFailed of {
      src: Path.t;
      dst: Path.t;
      cause: Fs.error;
    }
  | ExportSourceMissing of {
      path: Path.t;
    }

type export_entry = Manifest.export_entry = {
  name: string;
  path: Path.t;
  action_hash: string;
}

let error_message = fun __tmp1 ->
  match __tmp1 with
  | HashNotFound { hash } -> "Hash not found in store: " ^ Crypto.Digest.hex hash
  | LoadManifestFailed { path; cause } ->
      "Failed to load manifest: " ^ Path.to_string path ^ " (" ^ cause ^ ")"
  | CreateTargetDirFailed { path; cause } ->
      "Failed to create target directory: "
      ^ Path.to_string path
      ^ " ("
      ^ IO.error_message cause
      ^ ")"
  | CreateParentDirFailed { path; cause } ->
      "Failed to create parent directory: "
      ^ Path.to_string path
      ^ " ("
      ^ IO.error_message cause
      ^ ")"
  | ReadSourceMetadataFailed { path; cause } ->
      "Failed to read source metadata: " ^ Path.to_string path ^ " (" ^ IO.error_message cause ^ ")"
  | CopyArtifactFailed { src; dst; cause } ->
      "Failed to copy file: "
      ^ Path.to_string src
      ^ " -> "
      ^ Path.to_string dst
      ^ " ("
      ^ IO.error_message cause
      ^ ")"
  | SetCopiedArtifactPermissionsFailed { src; dst; cause } ->
      "Failed to preserve copied file permissions: "
      ^ Path.to_string src
      ^ " -> "
      ^ Path.to_string dst
      ^ " ("
      ^ IO.error_message cause
      ^ ")"
  | CreateTempDirFailed { path; cause } ->
      "Failed to create temp directory: "
      ^ Path.to_string path
      ^ " ("
      ^ IO.error_message cause
      ^ ")"
  | CheckSourceExistsFailed { path; cause } ->
      "Failed to check source path: " ^ Path.to_string path ^ " (" ^ IO.error_message cause ^ ")"
  | DeclaredOutputMissing { path } -> "Declared output was not created: " ^ Path.to_string path
  | DeclaredOutputOutsideSandbox { path; sandbox_dir } ->
      "Declared output is outside the sandbox: "
      ^ Path.to_string path
      ^ " (sandbox: "
      ^ Path.to_string sandbox_dir
      ^ ")"
  | MetadataReadFailed { path; cause } ->
      "Failed to get metadata for " ^ Path.to_string path ^ " (" ^ IO.error_message cause ^ ")"
  | SaveManifestFailed { path; cause } ->
      "Failed to save manifest: " ^ Path.to_string path ^ " (" ^ cause ^ ")"
  | CommitArtifactsFailed { source_dir; destination_dir; cause } ->
      "Failed to commit artifact directory: "
      ^ Path.to_string source_dir
      ^ " -> "
      ^ Path.to_string destination_dir
      ^ " ("
      ^ cause
      ^ ")"
  | SavePlanBundleFailed { hash; cause } ->
      "Failed to save plan bundle for " ^ Crypto.Digest.hex hash ^ " (" ^ cause ^ ")"
  | SaveNodePayloadFailed { namespace; hash; cause } ->
      "Failed to save "
      ^ node_payload_namespace_to_string namespace
      ^ " node payload for "
      ^ Crypto.Digest.hex hash
      ^ " ("
      ^ cause
      ^ ")"
  | ExportPathMustBeRelative { path } -> "Export path must be relative: " ^ Path.to_string path
  | CreatePackageOutputDirFailed { path; cause } ->
      "Failed to create package output directory: "
      ^ Path.to_string path
      ^ " ("
      ^ IO.error_message cause
      ^ ")"
  | CopyExportFailed { src; dst; cause } ->
      "Failed to copy export: "
      ^ Path.to_string src
      ^ " -> "
      ^ Path.to_string dst
      ^ " ("
      ^ IO.error_message cause
      ^ ")"
  | ExportSourceMissing { path } ->
      "Export source is missing from the store: "
      ^ Path.to_string path
      ^ " (cache is corrupted; try `riot clean`)"

let create_artifact_cache = fun () -> {
  artifacts = ConcurrentHashMap.with_capacity ~size:4_096;
}

let artifact_cache_key = fun hash -> Crypto.Digest.hex hash

let cached_artifact = fun cache hash ->
  let key = artifact_cache_key hash in
  ConcurrentHashMap.get cache.artifacts ~key

let remember_artifact = fun cache (artifact: Artifact.t) ->
  let key = artifact_cache_key artifact.input_hash in
  ignore (ConcurrentHashMap.insert cache.artifacts ~key ~value:artifact)

let forget_artifact = fun cache hash ->
  let key = artifact_cache_key hash in
  ignore (ConcurrentHashMap.remove cache.artifacts ~key)

let hash_of_hex = fun hex ->
  let hex_nibble ch =
    match ch with
    | '0' .. '9' -> Some (Char.code ch - Char.code '0')
    | 'a' .. 'f' -> Some (10 + Char.code ch - Char.code 'a')
    | 'A' .. 'F' -> Some (10 + Char.code ch - Char.code 'A')
    | _ -> None
  in
  let len = String.length hex in
  if len = 0 || len mod 2 != 0 then
    None
  else
    let bytes = IO.Bytes.create ~size:(len / 2) in
    let rec loop index =
      if index >= len then
        Some (Crypto.Hash.from_bytes bytes)
      else
        match (
          hex_nibble (String.get_unchecked hex ~at:index),
          hex_nibble (String.get_unchecked hex ~at:(index + 1))
        ) with
        | (Some hi, Some lo) ->
            IO.Bytes.set_unchecked
              bytes
              ~at:(index / 2)
              ~char:(Char.from_int_unchecked ((hi lsl 4) lor lo));
            loop (index + 2)
        | _ -> None
    in
    loop 0

let artifact_of_metadata = fun metadata ->
  match (hash_of_hex metadata.Manifest.input_hash, hash_of_hex metadata.output_hash) with
  | (Some input_hash, Some output_hash) ->
      Some Artifact.{
        input_hash;
        output_hash;
        size_bytes = metadata.size_bytes;
        files = [];
        ocamlc_warnings = metadata.ocamlc_warnings;
        exports = metadata.exports;
      }
  | _ -> None

let metadata_of_artifact = fun (artifact: Artifact.t) -> Manifest.{
  input_hash = Crypto.Digest.hex artifact.input_hash;
  output_hash = Crypto.Digest.hex artifact.output_hash;
  size_bytes = artifact.size_bytes;
  ocamlc_warnings = artifact.ocamlc_warnings;
  exports = artifact.exports;
}

let metadata_of_manifest = fun (manifest: Manifest.t) -> Manifest.{
  input_hash = manifest.input_hash;
  output_hash = manifest.output_hash;
  size_bytes = manifest.size_bytes;
  ocamlc_warnings = manifest.ocamlc_warnings;
  exports = manifest.exports;
}

(** Create a store rooted at a specific build lane *)
let create_for_lane = fun ~(workspace:Workspace.t) ~profile ~target ->
  let store_dir =
    Path.(workspace.target_dir_root
    / Path.v profile
    / Path.v (Riot_model.Target.to_string target)
    / Path.v "cache")
  in
  let package_store =
    ContentStore.create
      ~root:Path.(store_dir / Path.v "package-artifacts")
      ~ns:package_artifacts_namespace
      ~policy:Contentstore.Policy.default
  in
  let action_store =
    ContentStore.create
      ~root:Path.(store_dir / Path.v "action-artifacts")
      ~ns:action_artifacts_namespace
      ~policy:Contentstore.Policy.default
  in
  let plan_store =
    ContentStore.create
      ~root:store_dir
      ~ns:plans_namespace
      ~policy:Contentstore.Policy.default
  in
  let node_store namespace =
    ContentStore.create
      ~root:store_dir
      ~ns:(node_payload_namespace namespace)
      ~policy:Contentstore.Policy.default
  in
  {
    package_store;
    action_store;
    plan_store;
    dependency_resolution_store = node_store DependencyResolution;
    external_dependency_store = node_store ExternalDependencyReady;
    toolchain_ready_store = node_store ToolchainReady;
    source_analysis_store = node_store SourceAnalysis;
    module_plan_store = node_store ModulePlans;
    action_spec_store = node_store ActionSpecs;
    package_cache = create_artifact_cache ();
    action_cache = create_artifact_cache ();
  }

(** Create a new store for the given workspace *)
let create = fun ~(workspace:Workspace.t) ->
  create_for_lane
    ~workspace
    ~profile:"debug"
    ~target:(Riot_dirs.host_target ())

(** Get the path for a package artifact hash in the store *)
let get_package_hash_dir = fun store hash -> ContentStore.hash_dir_of store.package_store hash

(** Get the path for an action artifact hash in the store *)
let get_action_hash_dir = fun store hash -> ContentStore.hash_dir_of store.action_store hash

let manifest_path = fun hash_dir -> Path.(hash_dir / Path.v "manifest.bin")

let metadata_path = fun hash_dir -> Path.(hash_dir / Path.v "metadata.bin")

let manifest_cache_key = fun hash -> Std.Crypto.Digest.hex hash

let path_exists = fun path ->
  Fs.exists path
  |> Result.unwrap_or ~default:false

let export_source_path = fun store (entry: export_entry) ->
  if Path.is_absolute entry.path then
    None
  else
    match hash_of_hex entry.action_hash with
    | None -> None
    | Some action_hash -> Some Path.(get_action_hash_dir store action_hash / entry.path)

let manifest_files_exist = fun hash_dir (manifest: Manifest.t) ->
  List.all
    manifest.files
    ~fn:(fun (entry: Manifest.file_entry) -> path_exists Path.(hash_dir / entry.path))

let manifest_exports_exist = fun store (manifest: Manifest.t) ->
  List.all
    manifest.exports
    ~fn:(fun (entry: Manifest.export_entry) ->
      match export_source_path store entry with
      | Some path -> path_exists path
      | None -> false)

let artifact_dir_is_reusable = fun store hash_dir ->
  match Manifest.load ~path:(manifest_path hash_dir) with
  | Ok manifest ->
      manifest_files_exist hash_dir manifest
      && manifest_exports_exist store manifest
  | Error _ -> false

let prepare_artifact_destination = fun store ~hash_dir ~temp_dir ->
  match Fs.exists hash_dir with
  | Ok false -> Ok ()
  | Ok true ->
      if artifact_dir_is_reusable store hash_dir then
        Ok ()
      else
        Fs.remove_dir_all hash_dir
        |> Result.map_err
          ~fn:(fun cause ->
            CommitArtifactsFailed {
              source_dir = temp_dir;
              destination_dir = hash_dir;
              cause = "failed to remove stale artifact directory: " ^ IO.error_message cause;
            })
  | Error cause ->
      Error (
        CommitArtifactsFailed {
          source_dir = temp_dir;
          destination_dir = hash_dir;
          cause = "failed to inspect artifact directory: " ^ IO.error_message cause;
        }
      )

let copy_without_permissions = fun ~src ~dst ~copy_error ->
  Fs.copy ~src ~dst
  |> Result.map_err ~fn:copy_error

let copy_with_permissions = fun ~src ~dst ~copy_error ->
  let* metadata =
    Fs.metadata src
    |> Result.map_err ~fn:(fun cause -> ReadSourceMetadataFailed { path = src; cause })
  in
  let* () =
    Fs.copy ~src ~dst
    |> Result.map_err ~fn:copy_error
  in
  Fs.set_permissions dst (Fs.Metadata.permissions metadata)
  |> Result.map_err ~fn:(fun cause -> SetCopiedArtifactPermissionsFailed { src; dst; cause })

let cleanup_temp_dir = fun temp_dir ->
  match Fs.exists temp_dir with
  | Ok true -> Fs.remove_dir_all temp_dir
  | Ok false
  | Error _ -> Ok ()

let artifact_temp_counter = Sync.Atomic.make 0

let next_artifact_temp_nonce = fun () ->
  Sync.Atomic.fetch_and_add artifact_temp_counter 1

let artifact_temp_dir = fun store hash ->
  let nanos =
    Time.SystemTime.duration_since_epoch ()
    |> Time.Duration.to_nanos
  in
  let pid =
    Process.id ()
    |> Int32.to_string
  in
  let nonce =
    next_artifact_temp_nonce ()
    |> Int.to_string
  in
  let temp_name =
    Std.Crypto.Digest.hex hash ^ ".tmp." ^ pid ^ "." ^ Int64.to_string nanos ^ "." ^ nonce
  in
  Path.(ContentStore.root store.package_store / Path.v temp_name)

let read_opened_file = fun file ->
  let content = Fs.File.read_to_end file in
  let _ = Fs.File.close file in
  content

(** Check if artifacts for a given hash exist in the store *)
let exists = fun store hash ->
  let hash_dir = get_package_hash_dir store hash in
  artifact_dir_is_reusable store hash_dir

(** Promote artifacts from store to target directory *)
let promote_files = fun ~preserve_permissions ~hash_dir ~files ~target_dir ->
  let* () =
    Fs.create_dir_all target_dir
    |> Result.map_err ~fn:(fun cause -> CreateTargetDirFailed { path = target_dir; cause })
  in
  let promote_one (entry: Manifest.file_entry) =
    let src = Path.(hash_dir / entry.path) in
    let dst = Path.(target_dir / entry.path) in
    let dst_parent = Path.dirname dst in
    let* () =
      Fs.create_dir_all dst_parent
      |> Result.map_err ~fn:(fun cause -> CreateParentDirFailed { path = dst_parent; cause })
    in
    let copy_error cause = CopyArtifactFailed { src; dst; cause } in
    if preserve_permissions then
      copy_with_permissions ~src ~dst ~copy_error
    else
      copy_without_permissions ~src ~dst ~copy_error
  in
  List.fold_left
    files
    ~init:(Ok ())
    ~fn:(fun acc entry ->
      let* () = acc in
      promote_one entry)

let promote = fun store input_hash ~target_dir ->
  let hash_dir = get_package_hash_dir store input_hash in
  let manifest_path = manifest_path hash_dir in
  let* manifest =
    match Manifest.load ~path:manifest_path with
    | Ok manifest -> Ok manifest
    | Error cause -> (
        match Fs.exists manifest_path with
        | Ok true -> Error (LoadManifestFailed { path = manifest_path; cause })
        | Ok false
        | Error _ -> Error (HashNotFound { hash = input_hash })
      )
  in
  promote_files ~preserve_permissions:true ~hash_dir ~files:manifest.files ~target_dir

(** Store artifacts from sandbox to content-addressable store *)
let store_artifacts = fun
  content_store store ~package ?(ocamlc_warnings = []) ?(exports = []) input_hash sandbox_dir declared_outputs ->
  let hash_dir = ContentStore.hash_dir_of content_store input_hash in
  let temp_dir =
    let nanos =
      Time.SystemTime.duration_since_epoch ()
      |> Time.Duration.to_nanos
    in
    let pid =
      Process.id ()
      |> Int32.to_string
    in
    let nonce =
      next_artifact_temp_nonce ()
      |> Int.to_string
    in
    let temp_name =
      Std.Crypto.Digest.hex input_hash
      ^ ".tmp."
      ^ pid
      ^ "."
      ^ Int64.to_string nanos
      ^ "."
      ^ nonce
    in
    Path.(ContentStore.root content_store / Path.v temp_name)
  in
  let* () =
    Fs.create_dir_all temp_dir
    |> Result.map_err ~fn:(fun cause -> CreateTempDirFailed { path = temp_dir; cause })
  in
  let validate_exports_relative exports =
    match List.find exports ~fn:(fun (entry: export_entry) -> Path.is_absolute entry.path) with
    | Some entry -> Error (ExportPathMustBeRelative { path = entry.path })
    | None -> Ok ()
  in
  (* Copy declared outputs to store and track what was actually stored *)
  let copy_output output_file =
    let src = Path.(sandbox_dir / Path.v output_file) in
    match Fs.exists src with
    | Ok false -> Error (DeclaredOutputMissing { path = src })
    | Error cause -> Error (CheckSourceExistsFailed { path = src; cause })
    | Ok true ->
        let dst = Path.(temp_dir / Path.v output_file) in
        let dst_parent = Path.dirname dst in
        let* () =
          Fs.create_dir_all dst_parent
          |> Result.map_err ~fn:(fun cause -> CreateParentDirFailed { path = dst_parent; cause })
        in
        let* () =
          copy_with_permissions
            ~src
            ~dst
            ~copy_error:(fun cause -> CopyArtifactFailed { src; dst; cause })
        in
        let* metadata =
          Fs.metadata dst
          |> Result.map_err ~fn:(fun cause -> MetadataReadFailed { path = dst; cause })
        in
        Ok (Some (Path.v output_file, Fs.Metadata.len metadata))
  in
  let rec collect_outputs = fun acc ->
    fun __tmp1 ->
      match __tmp1 with
      | [] -> Ok (List.reverse acc)
      | output_file :: rest -> (
          match copy_output output_file with
          | Error _ as err -> err
          | Ok None -> collect_outputs acc rest
          | Ok (Some entry) -> collect_outputs (entry :: acc) rest
        )
  in
  let result =
    let* () = validate_exports_relative exports in
    let* stored_files_with_sizes = collect_outputs [] declared_outputs in
    let manifest =
      Manifest.create
        ~base_dir:temp_dir
        ~ocamlc_warnings
        ~exports
        ()
        ~package
        ~input_hash:(Std.Crypto.Digest.hex input_hash)
        ~files:(List.reverse stored_files_with_sizes)
    in
    let manifest_path = manifest_path temp_dir in
    let* () =
      Manifest.save manifest ~path:manifest_path
      |> Result.map_err ~fn:(fun cause -> SaveManifestFailed { path = manifest_path; cause })
    in
    let metadata_path = metadata_path temp_dir in
    let metadata = metadata_of_manifest manifest in
    let* () =
      Manifest.save_metadata metadata ~path:metadata_path
      |> Result.map_err ~fn:(fun cause -> SaveManifestFailed { path = metadata_path; cause })
    in
    let* () = prepare_artifact_destination store ~hash_dir ~temp_dir in
    let* () =
      ContentStore.commit_dir content_store ~hash:input_hash ~source_dir:temp_dir
      |> Result.map_err
        ~fn:(fun cause ->
          CommitArtifactsFailed {
            source_dir = temp_dir;
            destination_dir = hash_dir;
            cause = ContentStore.error_message cause;
          })
    in
    let output_hash =
      hash_of_hex manifest.Manifest.output_hash
      |> Option.expect ~msg:"store manifest output_hash should be valid hex"
    in
    Ok Artifact.{
      input_hash;
      output_hash;
      size_bytes = manifest.Manifest.size_bytes;
      files = manifest.Manifest.files;
      ocamlc_warnings;
      exports;
    }
  in
  let _ = cleanup_temp_dir temp_dir in
  result

let load_manifest = fun store ~hash ->
  match Manifest.load ~path:(manifest_path (get_package_hash_dir store hash)) with
  | Ok manifest -> Some manifest
  | Error _ -> None

let artifact_files_exist = fun hash_dir (artifact: Artifact.t) ->
  List.all
    artifact.files
    ~fn:(fun (entry: Manifest.file_entry) -> path_exists Path.(hash_dir / entry.path))

let artifact_exports_exist = fun store (artifact: Artifact.t) ->
  List.all
    artifact.exports
    ~fn:(fun (entry: Manifest.export_entry) ->
      match export_source_path store entry with
      | Some path -> path_exists path
      | None -> false)

let store_trace_enabled = fun () ->
  match Env.get Env.String ~var:"RIOT_STORE_TRACE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let trace_store_get = fun ~hash ~load_duration ~export_check_duration ~hit ->
  if store_trace_enabled () then
    eprintln
      (
        "riot-store get hash="
        ^ Crypto.Digest.hex hash
        ^ " load_us="
        ^ Int.to_string (Time.Duration.to_micros load_duration)
        ^ " export_check_us="
        ^ Int.to_string (Time.Duration.to_micros export_check_duration)
        ^ " hit="
        ^ Bool.to_string hit
      )

(** Simple interface - check if we have cached artifacts for a hash *)
let get_from = fun content_store artifact_cache store hash ~check_files ~check_exports ->
  let load_started_at = Time.Instant.now () in
  match cached_artifact artifact_cache hash with
  | Some artifact ->
      let hash_dir = ContentStore.hash_dir_of content_store hash in
      let export_check_started_at = Time.Instant.now () in
      if
        (not check_files || artifact_files_exist hash_dir artifact)
        && (not check_exports || artifact_exports_exist store artifact)
      then (
        trace_store_get
          ~hash
          ~load_duration:(Time.Instant.duration_since ~earlier:load_started_at (Time.Instant.now ()))
          ~export_check_duration:(Time.Instant.duration_since ~earlier:export_check_started_at (Time.Instant.now ()))
          ~hit:true;
        Some artifact
      ) else (
        forget_artifact artifact_cache hash;
        trace_store_get
          ~hash
          ~load_duration:(Time.Instant.duration_since ~earlier:load_started_at (Time.Instant.now ()))
          ~export_check_duration:(Time.Instant.duration_since ~earlier:export_check_started_at (Time.Instant.now ()))
          ~hit:false;
        None
      )
  | None -> (
      let hash_dir = ContentStore.hash_dir_of content_store hash in
      match Manifest.load ~path:(manifest_path hash_dir) with
  | Ok manifest ->
      let load_duration = Time.Instant.duration_since ~earlier:load_started_at (Time.Instant.now ()) in
      let export_check_started_at = Time.Instant.now () in
      if
        (not check_files || manifest_files_exist hash_dir manifest)
        && (not check_exports || manifest_exports_exist store manifest)
      then
        let export_check_duration =
          Time.Instant.duration_since ~earlier:export_check_started_at (Time.Instant.now ())
        in
        let () = trace_store_get ~hash ~load_duration ~export_check_duration ~hit:true in
        let output_hash =
          hash_of_hex manifest.output_hash
          |> Option.expect ~msg:"store manifest output_hash should be valid hex"
        in
        let artifact = Artifact.{
          input_hash = hash;
          output_hash;
          size_bytes = manifest.size_bytes;
          files = manifest.files;
          ocamlc_warnings = manifest.ocamlc_warnings;
          exports = manifest.exports;
        }
        in
        remember_artifact artifact_cache artifact;
        Some artifact
      else (
        let export_check_duration =
          Time.Instant.duration_since ~earlier:export_check_started_at (Time.Instant.now ())
        in
        let () = trace_store_get ~hash ~load_duration ~export_check_duration ~hit:false in
        None
      )
  | Error _ ->
      let load_duration = Time.Instant.duration_since ~earlier:load_started_at (Time.Instant.now ()) in
      let () =
        trace_store_get
          ~hash
          ~load_duration
          ~export_check_duration:Time.Duration.zero
          ~hit:false
      in
      None
    )

let get_package = fun store hash ->
  get_from store.package_store store.package_cache store hash ~check_files:true ~check_exports:true

let get_package_metadata = fun store hash ->
  let load_started_at = Time.Instant.now () in
  let hash_dir = ContentStore.hash_dir_of store.package_store hash in
  let load_artifact_metadata path =
    Manifest.load_metadata ~path |> Result.to_option
  in
  let load_manifest_metadata () =
    if artifact_dir_is_reusable store hash_dir then
      let* manifest = Manifest.load ~path:(manifest_path hash_dir) in
      let metadata = metadata_of_manifest manifest in
      let* () = Manifest.save_metadata metadata ~path:(metadata_path hash_dir) in
      Ok metadata
    else
      Error "artifact directory is missing a valid current manifest"
  in
  let metadata =
    match load_artifact_metadata (metadata_path hash_dir) with
    | Some metadata -> Some metadata
    | None -> (
        match load_manifest_metadata () with
        | Ok metadata -> Some metadata
        | Error _ -> None
      )
  in
  match metadata with
  | Some metadata -> (
      let load_duration = Time.Instant.duration_since ~earlier:load_started_at (Time.Instant.now ()) in
      let () =
        trace_store_get
          ~hash
          ~load_duration
          ~export_check_duration:Time.Duration.zero
          ~hit:true
      in
      match artifact_of_metadata metadata with
      | Some artifact -> Some artifact
      | None -> None
    )
  | None ->
      let load_duration = Time.Instant.duration_since ~earlier:load_started_at (Time.Instant.now ()) in
      let () =
        trace_store_get
          ~hash
          ~load_duration
          ~export_check_duration:Time.Duration.zero
          ~hit:false
      in
      None

let get_action = fun store hash ->
  get_from store.action_store store.action_cache store hash ~check_files:true ~check_exports:false

(** Simple interface - check if we have cached package artifacts for a hash *)
let get = get_package

(** Save build outputs to the store *)
let save_to = fun
  content_store artifact_cache ?(ocamlc_warnings = []) ?(exports = []) store ~package ~input_hash ~sandbox_dir ~outs ->
  let sandbox_dir = Path.normalize sandbox_dir in
  let relative_output_path out_path =
    let out_path = Path.normalize out_path in
    if Path.is_absolute out_path then
      match Path.strip_prefix out_path ~prefix:sandbox_dir with
      | Ok relative -> Ok relative
      | Error _ -> Error (DeclaredOutputOutsideSandbox { path = out_path; sandbox_dir })
    else
      Ok out_path
  in
  let rec collect_outs acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | out_path :: rest ->
        let* relative = relative_output_path out_path in
        collect_outs (Path.to_string relative :: acc) rest
  in
  let* outs_str = collect_outs [] outs in
  let* artifact =
    store_artifacts
    content_store
    store
    ~package
    ~ocamlc_warnings
    ~exports
    input_hash
    sandbox_dir
    outs_str
  in
  remember_artifact artifact_cache artifact;
  Ok artifact

let save_package = fun
  ?(ocamlc_warnings = []) ?(exports = []) store ~package ~input_hash ~sandbox_dir ~outs ->
  save_to
    store.package_store
    store.package_cache
    ~ocamlc_warnings
    ~exports
    store
    ~package
    ~input_hash
    ~sandbox_dir
    ~outs

let save_action = fun ?(ocamlc_warnings = []) store ~package ~input_hash ~sandbox_dir ~outs ->
  save_to
    store.action_store
    store.action_cache
    ~ocamlc_warnings
    ~exports:[]
    store
    ~package
    ~input_hash
    ~sandbox_dir
    ~outs

let save = save_package

(** Promote cached artifacts to target directory *)
let promote_artifact = fun store artifact ~target_dir ->
  promote
    store
    Artifact.(artifact.input_hash)
    ~target_dir

let promote_action = fun ?(preserve_permissions = true) store input_hash ~target_dir ->
  let hash_dir = get_action_hash_dir store input_hash in
  let manifest_path = manifest_path hash_dir in
  let* manifest =
    match Manifest.load ~path:manifest_path with
    | Ok manifest -> Ok manifest
    | Error cause -> (
        match Fs.exists manifest_path with
        | Ok true -> Error (LoadManifestFailed { path = manifest_path; cause })
        | Ok false
        | Error _ -> Error (HashNotFound { hash = input_hash })
      )
  in
  promote_files ~preserve_permissions ~hash_dir ~files:manifest.files ~target_dir

let promote_action_artifact = fun
  ?(preserve_permissions = true) store (artifact: Artifact.t) ~target_dir ->
  let hash_dir = get_action_hash_dir store artifact.input_hash in
  promote_files ~preserve_permissions ~hash_dir ~files:artifact.files ~target_dir

(** Get absolute paths to artifact files in immutable cache *)
let get_artifact_paths = fun store artifact ->
  let hash_dir = get_package_hash_dir store Artifact.(artifact.input_hash) in
  List.map Artifact.(artifact.files) ~fn:(fun entry -> Path.(hash_dir / entry.Manifest.path))

(** Get the cache directory containing an artifact's files *)
let get_artifact_dir = fun store artifact -> get_package_hash_dir store Artifact.(artifact.input_hash)

let hash_dir_of = fun store hash -> get_package_hash_dir store hash

let action_hash_dir_of = fun store hash -> get_action_hash_dir store hash

let node_payload_store = fun store namespace ->
  match namespace with
  | DependencyResolution -> store.dependency_resolution_store
  | ExternalDependencyReady -> store.external_dependency_store
  | ToolchainReady -> store.toolchain_ready_store
  | SourceAnalysis -> store.source_analysis_store
  | ModulePlans -> store.module_plan_store
  | ActionSpecs -> store.action_spec_store

let save_node_payload = fun store ~namespace ~hash ~payload ->
  ContentStore.save_object (node_payload_store store namespace) ~hash ~content:payload
  |> Result.map_err
    ~fn:(fun cause ->
      SaveNodePayloadFailed { namespace; hash; cause = ContentStore.error_message cause })

let load_node_payload = fun store ~namespace ~hash ->
  match ContentStore.open_object (node_payload_store store namespace) ~hash with
  | Error _ -> None
  | Ok file -> (
      match read_opened_file file with
      | Error _ -> None
      | Ok content -> Some content
    )

let save_plan_bundle = fun store ~hash ~plan ->
  ContentStore.save_object store.plan_store ~hash ~content:(Std.Data.Json.to_string plan)
  |> Result.map_err
    ~fn:(fun cause -> SavePlanBundleFailed { hash; cause = ContentStore.error_message cause })

let load_plan_bundle = fun store ~hash ->
  match ContentStore.open_object store.plan_store ~hash with
  | Error _ -> None
  | Ok file -> (
      match read_opened_file file with
      | Error _ -> None
      | Ok content ->
          Data.Json.from_string content
          |> Result.to_option
    )

let materialize_package_exports = fun store ~exports ~target_dir ->
  let* () =
    Fs.create_dir_all target_dir
    |> Result.map_err ~fn:(fun cause -> CreatePackageOutputDirFailed { path = target_dir; cause })
  in
  let copy_one (entry: export_entry) =
    match export_source_path store entry with
    | None -> Error (ExportPathMustBeRelative { path = entry.path })
    | Some src ->
        let dst = Path.(target_dir / Path.v entry.name) in
        match Fs.exists src with
        | Ok true ->
            copy_with_permissions
              ~src
              ~dst
              ~copy_error:(fun cause -> CopyExportFailed { src; dst; cause })
        | Ok false
        | Error _ -> Error (ExportSourceMissing { path = src })
  in
  List.fold_left
    exports
    ~init:(Ok ())
    ~fn:(fun acc entry ->
      let* () = acc in
      copy_one entry)
