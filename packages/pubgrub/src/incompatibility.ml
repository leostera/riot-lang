open Std

type package = string
type version = Version.t

type external_cause =
  | NotRoot of package * version
  | NoVersions of package * version Ranges.t
  | FromDependency of package * version * package * version Ranges.t
  | Custom of package * version Ranges.t * string

type t =
  | External of { terms : Term.t list; cause : external_cause }
  | Derived of {
      terms : Term.t list;
      cause1 : t;
      cause2 : t;
      shared_id : int option;
    }

let create_external terms cause = External { terms; cause }

let create_derived terms cause1 cause2 shared_id =
  Derived { terms; cause1; cause2; shared_id }

let not_root pkg ver =
  let term = Term.negative pkg (Ranges.singleton ver) in
  create_external [ term ] (NotRoot (pkg, ver))

let no_versions pkg ranges =
  let term = Term.positive pkg ranges in
  create_external [ term ] (NoVersions (pkg, ranges))

let from_dependency pkg ver (dep_pkg, dep_ranges) =
  let parent_term = Term.positive pkg (Ranges.singleton ver) in
  if Ranges.is_empty dep_ranges then
    create_external [ parent_term ]
      (FromDependency (pkg, ver, dep_pkg, dep_ranges))
  else
    let dep_term = Term.negative dep_pkg dep_ranges in
    create_external [ parent_term; dep_term ]
      (FromDependency (pkg, ver, dep_pkg, dep_ranges))

let terms incompat =
  match incompat with
  | External { terms; _ } -> terms
  | Derived { terms; _ } -> terms

let get_term incompat pkg =
  let all_terms = terms incompat in
  List.find_opt (fun term -> Term.package term = pkg) all_terms

let is_terminal incompat root_pkg root_ver =
  match incompat with
  | External { terms = []; _ } | Derived { terms = []; _ } ->
      (* Empty incompatibility is always terminal (fundamental contradiction) *)
      true
  | External { terms = [ term ]; cause = NotRoot (pkg, ver) } ->
      pkg = root_pkg && ver = root_ver && Term.is_positive term
      && Term.package term = root_pkg
  | _ -> false

let version_compare a b =
  match Version.compare a b with Lt -> -1 | Eq -> 0 | Gt -> 1

let ranges_equal ~compare_v r1 r2 =
  Ranges.subset_of ~compare_v r1 r2 && Ranges.subset_of ~compare_v r2 r1

let as_dependency incompat =
  match incompat with
  | External { cause = FromDependency (p1, _, p2, _); _ } -> Some (p1, p2)
  | _ -> None

let merge_dependents incompat1 incompat2 =
  match (incompat1, incompat2) with
  | ( External { terms = terms1; cause = FromDependency (p1, v1, dep1, r1) },
      External { terms = terms2; cause = FromDependency (p2, v2, dep2, r2) } )
    when p1 = p2 && dep1 = dep2 ->
      if ranges_equal ~compare_v:version_compare r1 r2 then
        let merged_range =
          Ranges.union ~compare_v:version_compare (Ranges.singleton v1)
            (Ranges.singleton v2)
        in
        let parent_term = Term.negative p1 merged_range in
        let dep_term = Term.positive dep1 r1 in
        Some
          (create_external [ parent_term; dep_term ]
             (FromDependency (p1, v1, dep1, r1)))
      else None
  | _ -> None

let prior_cause incompat satisfier_cause package =
  let incompat_terms = terms incompat in
  let satisfier_terms = terms satisfier_cause in
  Log.info
    "prior_cause: incompat has %d terms, satisfier has %d terms, package=%s"
    (List.length incompat_terms)
    (List.length satisfier_terms)
    package;
  match incompat with
  | External _ | Derived _ ->
      (* Find the term for the package in both incompatibilities and merge them *)
      let incompat_term =
        List.find_opt (fun t -> Term.package t = package) incompat_terms
      in
      let satisfier_term =
        List.find_opt (fun t -> Term.package t = package) satisfier_terms
      in

      let merged_term =
        match (incompat_term, satisfier_term) with
        | Some it, Some st -> Term.intersection it st
        | Some it, None -> it
        | None, Some st -> st
        | None, None -> panic "Package not found in either incompatibility"
      in

      let other_incompat_terms =
        List.filter (fun t -> Term.package t <> package) incompat_terms
      in
      let other_satisfier_terms =
        List.filter (fun t -> Term.package t <> package) satisfier_terms
      in

      let merged_other_terms =
        List.fold_left
          (fun acc incompat_t ->
            let pkg = Term.package incompat_t in
            match
              List.find_opt
                (fun t -> Term.package t = pkg)
                other_satisfier_terms
            with
            | Some satisfier_t ->
                Term.intersection incompat_t satisfier_t :: acc
            | None -> incompat_t :: acc)
          [] other_incompat_terms
      in

      let remaining_satisfier_terms =
        List.filter
          (fun t ->
            not
              (List.exists
                 (fun it -> Term.package it = Term.package t)
                 other_incompat_terms))
          other_satisfier_terms
      in

      let all_terms =
        if Term.is_any merged_term then
          merged_other_terms @ remaining_satisfier_terms
        else (merged_term :: merged_other_terms) @ remaining_satisfier_terms
      in

      Log.info "prior_cause: all_terms has %d terms" (List.length all_terms);

      (* Even if all_terms is empty, create a derived incompatibility *)
      (* An empty incompatibility is terminal (fundamental contradiction) *)
      create_derived all_terms incompat satisfier_cause None
  | _ -> incompat
