open Std

module Log = struct
  let debug _ = ()

  let info _ = ()

  let error _ = ()

  let trace _ = ()
end

type package = string

type version = Version.t

type external_cause =
  | NotRoot of package * version
  | NoVersions of package * version Ranges.t
  | FromDependency of package * version * package * version Ranges.t
  | Custom of package * version Ranges.t * string

type t =
  | External of {
      terms: Term.t list;
      cause: external_cause;
    }
  | Derived of {
      terms: Term.t list;
      cause1: t;
      cause2: t;
      shared_id: int option;
    }

let create_external = fun terms cause -> External { terms; cause }

let create_derived = fun terms cause1 cause2 shared_id ->
  Derived {
    terms;
    cause1;
    cause2;
    shared_id;
  }

let not_root = fun pkg ver ->
  let term = Term.negative pkg (Ranges.singleton ver) in
  create_external [ term ] (NotRoot (pkg, ver))

let no_versions = fun pkg ranges ->
  let term = Term.positive pkg ranges in
  create_external [ term ] (NoVersions (pkg, ranges))

let from_dependency = fun pkg ver (dep_pkg, dep_ranges) ->
  let parent_term = Term.positive pkg (Ranges.singleton ver) in
  if Ranges.is_empty dep_ranges then
    create_external [ parent_term ] (FromDependency (pkg, ver, dep_pkg, dep_ranges))
  else
    let dep_term = Term.negative dep_pkg dep_ranges in
    create_external [ parent_term; dep_term ] (FromDependency (pkg, ver, dep_pkg, dep_ranges))

let terms = fun incompat ->
  match incompat with
  | External { terms; _ } -> terms
  | Derived { terms; _ } -> terms

let get_term = fun incompat pkg ->
  let all_terms = terms incompat in
  List.find all_terms ~fn:(fun term -> Term.package term = pkg)

let version_compare = Version.compare

let is_terminal = fun incompat root_pkg root_ver ->
  match incompat with
  | External { terms = []; _ }
  | Derived { terms = []; _ } ->
      (* Empty incompatibility is always terminal (fundamental contradiction) *)
      true
  | External {
      terms = [ term ];
      cause = NotRoot (pkg, ver);
    } ->
      pkg = root_pkg
      && ver = root_ver
      && not (Term.is_positive term)
      && Term.package term = root_pkg
      && Ranges.contains ~compare_v:version_compare (Term.ranges term) root_ver
  | Derived { terms = [ term ]; _ } ->
      (* Derived incompatibility with only root term is terminal *)
      Term.package term = root_pkg
      && Term.is_positive term
      && Ranges.contains ~compare_v:version_compare (Term.ranges term) root_ver
  | _ -> false

let ranges_equal = fun ~compare_v r1 r2 ->
  Ranges.subset_of ~compare_v r1 r2 && Ranges.subset_of ~compare_v r2 r1

let as_dependency = fun incompat ->
  match incompat with
  | External { cause = FromDependency (p1, _, p2, _); _ } -> Some (p1, p2)
  | _ -> None

let merge_dependents = fun incompat1 incompat2 ->
  match (incompat1, incompat2) with
  | (
    External { terms = terms1; cause = FromDependency (p1, v1, dep1, r1) },
    External { terms = terms2; cause = FromDependency (p2, v2, dep2, r2) }
  ) when p1 = p2 && dep1 = dep2 ->
      if ranges_equal ~compare_v:version_compare r1 r2 then
        let merged_range =
          Ranges.union ~compare_v:version_compare (Ranges.singleton v1) (Ranges.singleton v2)
        in
        let parent_term = Term.negative p1 merged_range in
        let dep_term = Term.positive dep1 r1 in
        Some (create_external [ parent_term; dep_term ] (FromDependency (p1, v1, dep1, r1)))
      else
        None
  | _ -> None

let normalize_terms = fun terms ->
  List.fold_left
    terms
    ~init:[]
    ~fn:(fun acc term ->
      match List.find acc ~fn:(fun existing -> Term.package existing = Term.package term) with
      | None ->
          if Term.is_any term then
            acc
          else
            term :: acc
      | Some existing ->
          let merged = Term.intersection existing term in
          let acc_without_pkg =
            List.filter
              acc
              ~fn:(fun existing -> not (String.equal (Term.package existing) (Term.package term)))
          in
          if Term.is_any merged then
            acc_without_pkg
          else
            merged :: acc_without_pkg)
  |> List.reverse

let prior_cause = fun ?extra_term incompat satisfier_cause package ->
  let incompat_terms = terms incompat in
  let satisfier_terms = terms satisfier_cause in
  Log.info
    ("prior_cause: incompat has "
    ^ Int.to_string (List.length incompat_terms)
    ^ " terms, satisfier has "
    ^ Int.to_string (List.length satisfier_terms)
    ^ " terms, package="
    ^ package);
  Log.info "  incompat terms:";
  List.for_each
    incompat_terms
    ~fn:(fun t ->
      Log.info
        (
          "    " ^ (
            if Term.is_positive t then
              ""
            else
              "NOT "
          ) ^ Term.package t
        ));
  Log.info "  satisfier_cause terms:";
  List.for_each
    satisfier_terms
    ~fn:(fun t ->
      Log.info
        (
          "    " ^ (
            if Term.is_positive t then
              ""
            else
              "NOT "
          ) ^ Term.package t
        ));
  match incompat with
  | External _
  | Derived _ ->
      let incompat_term = List.find incompat_terms ~fn:(fun t -> Term.package t = package) in
      let satisfier_term = List.find satisfier_terms ~fn:(fun t -> Term.package t = package) in
      let resolved_package_term =
        match (incompat_term, satisfier_term) with
        | (Some left, Some right) -> Some (Term.union left right)
        | (Some left, None) -> Some left
        | (None, Some right) -> Some right
        | (None, None) -> panic "Package not found in either incompatibility"
      in
      let add_or_merge_term acc term =
        let pkg = Term.package term in
        match List.find acc ~fn:(fun existing -> Term.package existing = pkg) with
        | Some existing ->
            let merged = Term.intersection existing term in
            merged
            :: List.filter acc ~fn:(fun existing -> not (String.equal (Term.package existing) pkg))
        | None -> term :: acc
      in
      let all_terms =
        List.fold_left
          incompat_terms
          ~init:[]
          ~fn:(fun acc term ->
            if String.equal (Term.package term) package then
              acc
            else
              add_or_merge_term acc term)
      in
      let all_terms =
        List.fold_left
          satisfier_terms
          ~init:all_terms
          ~fn:(fun acc term ->
            if String.equal (Term.package term) package then
              acc
            else
              add_or_merge_term acc term)
      in
      let all_terms =
        match resolved_package_term with
        | Some term when not (Term.is_any term) -> term :: all_terms
        | _ -> all_terms
      in
      let all_terms =
        match extra_term with
        | Some term -> term :: all_terms
        | None -> all_terms
      in
      let all_terms = normalize_terms (List.reverse all_terms) in
      Log.info ("prior_cause: all_terms has " ^ Int.to_string (List.length all_terms) ^ " terms");
      (* Even if all_terms is empty, create a derived incompatibility *)
      (* An empty incompatibility is terminal (fundamental contradiction) *)
      create_derived all_terms incompat satisfier_cause None
