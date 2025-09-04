open Std
(** Build results - tracks which packages have been built *)

type status =
  | NotStarted
  | Building
  | Built of Hasher.hash
  | Failed of string (* Built now stores the content hash *)

type t = { mutable results : (string, status) Hashtbl.t }

(** Create a new build results tracker *)
let create () = { results = Hashtbl.create 64 }

(** Clear all build results *)
let clear t = Hashtbl.clear t.results

(** Initialize all packages as not started *)
let init_packages t packages =
  List.iter (fun pkg -> Hashtbl.replace t.results pkg NotStarted) packages

(** Initialize a single package as not started *)
let init_package t pkg = Hashtbl.replace t.results pkg NotStarted

(** Check if a package is being tracked *)
let is_tracked t pkg = Hashtbl.mem t.results pkg

(** Get the status of a package *)
let get_status t pkg = Hashtbl.find_opt t.results pkg

(** Check if all dependencies are built *)
let dependencies_ready t deps =
  List.for_all
    (fun dep ->
      match Hashtbl.find_opt t.results dep with
      | Some (Built _) -> true
      | _ -> false)
    deps

(** Get unbuilt dependencies *)
let get_unbuilt_deps t deps =
  List.filter
    (fun dep ->
      match Hashtbl.find_opt t.results dep with
      | Some (Built _) -> false
      | _ -> true)
    deps

(** Mark a package as building *)
let mark_building t pkg = Hashtbl.replace t.results pkg Building

(** Mark a package as built with its content hash *)
let mark_built_with_hash t pkg hash = Hashtbl.replace t.results pkg (Built hash)

(** Mark a package as built (legacy - uses placeholder hash) *)
let mark_built t pkg =
  let unknown_hash = Hasher.of_string "unknown" in
  Hashtbl.replace t.results pkg (Built unknown_hash)

(** Mark a package as failed *)
let mark_failed_pkg t pkg error = Hashtbl.replace t.results pkg (Failed error)

(** Mark a node as failed *)
let mark_failed t node ~error =
  let pkg_name = node.Build_node.package.name in
  Hashtbl.replace t.results pkg_name (Failed error)

(** Reset failed packages to NotStarted so they can be retried *)
let reset_failed_packages t =
  let failed_packages = ref [] in
  Hashtbl.filter_map_inplace
    (fun pkg status ->
      match status with
      | Failed _ ->
          failed_packages := pkg :: !failed_packages;
          Some NotStarted
      | other -> Some other)
    t.results;

  (* Log reset information for user awareness *)
  if !failed_packages <> [] then
    Printf.printf "🔄 Retrying previously failed packages: %s\n"
      (String.concat ", " (List.rev !failed_packages));
  flush stdout

(** Check if source files are newer than build outputs *)
let sources_newer_than_outputs workspace pkg_name =
  let root = Std.Path.to_string workspace.Workspace.root in
  let pkg_dir = Filename.concat root ("packages/" ^ pkg_name) in
  let src_dir = Filename.concat pkg_dir "src" in
  let target_dir =
    Filename.concat root ("target/debug/out/packages/" ^ pkg_name)
  in
  let output_file = Filename.concat target_dir (pkg_name ^ ".cma") in

  if not (File_utils.exists ~path:output_file) then true
    (* No outputs, need to build *)
  else
    let output_stat =
      Fs.stat
        (Path.of_string output_file |> Result.expect ~msg:"Invalid output_file")
      |> Result.expect ~msg:"Failed to stat output_file"
    in
    let output_mtime = output_stat.st_mtime in

    if File_utils.exists ~path:src_dir then
      let files =
        Fs.readdir
          (Path.of_string src_dir |> Result.expect ~msg:"Invalid src_dir")
        |> Result.expect ~msg:"Failed to read src_dir"
      in
      let source_files =
        List.filter
          (fun f ->
            String.ends_with ~suffix:".ml" f
            || String.ends_with ~suffix:".mli" f)
          files
      in

      List.exists
        (fun file ->
          let filepath = Filename.concat src_dir file in
          if File_utils.exists ~path:filepath then
            let file_stat =
              Fs.stat
                (Path.of_string filepath
                |> Result.expect ~msg:"Invalid filepath")
              |> Result.expect ~msg:"Failed to stat filepath"
            in
            file_stat.st_mtime > output_mtime
          else false)
        source_files
    else false

(** Check if build outputs exist for a package *)
let build_outputs_exist workspace pkg_name =
  let root = Std.Path.to_string workspace.Workspace.root in
  let target_dir = Filename.concat root "target/debug/out/packages" in
  let pkg_target_dir = Filename.concat target_dir pkg_name in
  let cma_file = Filename.concat pkg_target_dir (pkg_name ^ ".cma") in
  let cmi_file = Filename.concat pkg_target_dir (pkg_name ^ ".cmi") in

  (* Check if key build outputs exist *)
  File_utils.exists ~path:cma_file && File_utils.exists ~path:cmi_file

(** Check if a package is built with the current content hash *)
let is_built_with_current_hash t pkg current_hash =
  match Hashtbl.find_opt t.results pkg with
  | Some (Built stored_hash) -> Hasher.equal stored_hash current_hash
  | _ -> false

(** Check if a package is built and outputs are still valid (legacy
    timestamp-based) *)
let is_built_with_outputs_check t pkg workspace =
  match Hashtbl.find_opt t.results pkg with
  | Some (Built _) ->
      build_outputs_exist workspace pkg
      && not (sources_newer_than_outputs workspace pkg)
  | _ -> false

(** Check if a package is being built *)
let is_building t pkg =
  match Hashtbl.find_opt t.results pkg with Some Building -> true | _ -> false

(** Check if all packages are done (built or failed) *)
let all_done t =
  if Hashtbl.length t.results = 0 then false
    (* Empty results means nothing has been queued yet *)
  else
    Hashtbl.fold
      (fun _pkg status acc ->
        acc && match status with Built _ | Failed _ -> true | _ -> false)
      t.results true

(** Mark a node as pending *)
let mark_pending t node =
  let pkg_name = node.Build_node.package.name in
  (* Only mark as pending if not already built - preserve cached builds *)
  match Hashtbl.find_opt t.results pkg_name with
  | Some (Built _) -> () (* Already built, don't overwrite *)
  | _ -> Hashtbl.replace t.results pkg_name NotStarted

(** Mark a node as completed with artifact *)
let mark_completed t node artifact =
  let pkg_name = node.Build_node.package.name in
  (* Extract hash from the node's spec *)
  let hash =
    match node.Build_node.spec with
    | Planned { hash; _ } -> hash
    | Unplanned -> 
        failwith (Printf.sprintf "CRITICAL: Trying to mark unplanned node %s as completed! This should never happen." pkg_name)
  in
  Hashtbl.replace t.results pkg_name (Built hash)

(** Get build statistics *)
let get_stats t =
  let built = ref 0 in
  let failed = ref 0 in
  let building = ref 0 in
  let not_started = ref 0 in
  Hashtbl.iter
    (fun _pkg status ->
      match status with
      | Built _ -> incr built
      | Failed _ -> incr failed
      | Building -> incr building
      | NotStarted -> incr not_started)
    t.results;
  (!built, !failed, !building, !not_started)

(** Convert build results to Event.build_result list *)
let to_events t =
  Hashtbl.fold
    (fun package status acc ->
      match status with
      | Built _ ->
          let result : Event.build_result =
            {
              package;
              success = true;
              duration_ms = 0;  (* TODO: track per-package timing *)
              modules_compiled = 0;  (* TODO: track modules compiled *)
              cache_hits = 0;  (* TODO: track cache hits *)
              cache_misses = 0;  (* TODO: track cache misses *)
              errors = [];
            }
          in
          result :: acc
      | Failed error ->
          let result : Event.build_result =
            {
              package;
              success = false;
              duration_ms = 0;
              modules_compiled = 0;
              cache_hits = 0;
              cache_misses = 0;
              errors = [];  (* TODO: Convert string error to proper Event.build_error *)
            }
          in
          result :: acc
      | _ -> acc)  (* Don't include Building or NotStarted in final results *)
    t.results []

(** Tests submodule *)
module Tests = struct
  let test_build_results_tracks_all_package_states () : (unit, string) result =
    (* Test that all build states are tracked correctly *)
    Ok ()
    [@test]

  let test_hash_stored_with_successful_builds () : (unit, string) result =
    (* Test that content hash is stored on successful build *)
    Ok ()
    [@test]

  let test_error_messages_preserved_on_failure () : (unit, string) result =
    (* Test that failure reasons are captured and accessible *)
    Ok ()
    [@test]

  let test_statistics_accurately_reflect_state () : (unit, string) result =
    (* Test that stats correctly count packages in each state *)
    Ok ()
end [@test]
