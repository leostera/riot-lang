(** Build results - tracks which packages have been built *)

type status = 
  | NotStarted
  | Building
  | Built
  | Failed of string

type t = {
  mutable results : (string, status) Hashtbl.t;
}

(** Create a new build results tracker *)
let create () = {
  results = Hashtbl.create 64;
}

(** Clear all build results *)
let clear t =
  Hashtbl.clear t.results

(** Initialize all packages as not started *)
let init_packages t packages =
  List.iter (fun pkg ->
    Hashtbl.replace t.results pkg NotStarted
  ) packages

(** Check if all dependencies are built *)
let dependencies_ready t deps =
  List.for_all (fun dep ->
    match Hashtbl.find_opt t.results dep with
    | Some Built -> true
    | _ -> false
  ) deps

(** Get unbuilt dependencies *)
let get_unbuilt_deps t deps =
  List.filter (fun dep ->
    match Hashtbl.find_opt t.results dep with
    | Some Built -> false
    | _ -> true
  ) deps

(** Mark a package as building *)
let mark_building t pkg =
  Hashtbl.replace t.results pkg Building

(** Mark a package as built *)
let mark_built t pkg =
  Hashtbl.replace t.results pkg Built

(** Mark a package as failed *)
let mark_failed t pkg error =
  Hashtbl.replace t.results pkg (Failed error)

(** Check if a package is built *)
let is_built t pkg =
  match Hashtbl.find_opt t.results pkg with
  | Some Built -> true
  | _ -> false

(** Check if a package is being built *)
let is_building t pkg =
  match Hashtbl.find_opt t.results pkg with
  | Some Building -> true
  | _ -> false

(** Check if all packages are done (built or failed) *)
let all_done t =
  Hashtbl.fold (fun _pkg status acc ->
    acc && (match status with
      | Built | Failed _ -> true
      | _ -> false)
  ) t.results true

(** Get build statistics *)
let get_stats t =
  let built = ref 0 in
  let failed = ref 0 in
  let building = ref 0 in
  let not_started = ref 0 in
  Hashtbl.iter (fun _pkg status ->
    match status with
    | Built -> incr built
    | Failed _ -> incr failed
    | Building -> incr building
    | NotStarted -> incr not_started
  ) t.results;
  (!built, !failed, !building, !not_started)