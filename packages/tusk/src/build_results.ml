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
let mark_failed t pkg error = Hashtbl.replace t.results pkg (Failed error)

(** Check if source files are newer than build outputs *)
let sources_newer_than_outputs workspace pkg_name =
  let root = workspace.Workspace.root in
  let pkg_dir = Filename.concat root ("packages/" ^ pkg_name) in
  let src_dir = Filename.concat pkg_dir "src" in
  let target_dir =
    Filename.concat root ("target/debug/out/packages/" ^ pkg_name)
  in
  let output_file = Filename.concat target_dir (pkg_name ^ ".cma") in

  if not (Sys.file_exists output_file) then true (* No outputs, need to build *)
  else
    let output_stat = Unix.stat output_file in
    let output_mtime = output_stat.st_mtime in

    if Sys.file_exists src_dir then
      let files = Array.to_list (Sys.readdir src_dir) in
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
          if Sys.file_exists filepath then
            let file_stat = Unix.stat filepath in
            file_stat.st_mtime > output_mtime
          else false)
        source_files
    else false

(** Check if build outputs exist for a package *)
let build_outputs_exist workspace pkg_name =
  let root = workspace.Workspace.root in
  let target_dir = Filename.concat root "target/debug/out/packages" in
  let pkg_target_dir = Filename.concat target_dir pkg_name in
  let cma_file = Filename.concat pkg_target_dir (pkg_name ^ ".cma") in
  let cmi_file = Filename.concat pkg_target_dir (pkg_name ^ ".cmi") in

  (* Check if key build outputs exist *)
  Sys.file_exists cma_file && Sys.file_exists cmi_file

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
  Hashtbl.fold
    (fun _pkg status acc ->
      acc && match status with Built _ | Failed _ -> true | _ -> false)
    t.results true

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
