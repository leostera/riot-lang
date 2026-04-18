open Std

module Log = struct
  let debug _ = ()

  let info _ = ()

  let error _ = ()

  let trace _ = ()
end

open Std.Sync
open Std.Sync.Cell

type package = string

type version = Version.t

type decision_level = int

type assignment =
  | Decision of package * version * decision_level * int
  (* global_index *)
  | Derivation of package * version Ranges.t * bool * Incompatibility.t * decision_level * int

(* global_index *)

type same_decision_levels = {
  cause: Incompatibility.t;
  extra_term: Term.t option;
}

type t = {
  assignments: assignment list;
  decisions: (package, version) Collections.HashMap.t;
  decision_level: decision_level;
  next_global_index: int;
}

let empty = fun () ->
  {
    assignments = [];
    decisions = Collections.HashMap.create ();
    decision_level = 0;
    next_global_index = 0
  }

let current_decision_level = fun solution -> solution.decision_level

let version_compare = fun a b ->
  match Version.compare a b with
  | Lt -> (-1)
  | Eq -> 0
  | Gt -> 1

let add_decision = fun solution pkg ver ->
  let new_level = solution.decision_level + 1 in
  let global_index = solution.next_global_index in
  let _ = Collections.HashMap.insert solution.decisions ~key:pkg ~value:ver in
  {
    solution
    with assignments = Decision (pkg, ver, new_level, global_index) :: solution.assignments;
    decision_level = new_level;
    next_global_index = global_index + 1
  }

let add_derivation = fun solution pkg incompat ->
  let global_index = solution.next_global_index in
  let version_compare a b =
    match Version.compare a b with
    | Lt -> (-1)
    | Eq -> 0
    | Gt -> 1
  in
  (* Get the term for this package from the incompatibility and derive ranges *)
  (* RUST: negating gives the constraint - if term is POS, derive complement; if NEG, derive ranges *)
  let term = Incompatibility.get_term incompat pkg in
  let ranges, requires_decision =
    match term with
    | Some t when Term.is_positive t ->
        (* Positive term: must complement to get what's forbidden *)
        (Ranges.complement ~compare_v:version_compare (Term.ranges t), false)
    | Some t ->
        (* Negative term: ranges directly are what's required *)
        (Term.ranges t, true)
    | None ->
        panic
          (
            "Partial_solution.add_derivation: package "
            ^ pkg
            ^ " is missing from incompatibility"
          )
  in
  {
    solution
    with assignments = Derivation
      (pkg, ranges, requires_decision, incompat, solution.decision_level, global_index)
    :: solution.assignments;
    next_global_index = global_index + 1
  }

let get_decision = fun solution pkg -> Collections.HashMap.get solution.decisions ~key:pkg

(* Get the effective constraint for a package, considering both decisions and derivations *)

let get_constraint = fun solution pkg ->
  match get_decision solution pkg with
  | Some ver -> `Decided ver
  | None ->
      let derived_ranges =
        List.fold_left solution.assignments ~acc:None
          ~fn:(fun acc ->
            function
            | Derivation (p, ranges, _, _, _, _) when p = pkg ->
                Some (
                  match acc with
                  | None -> ranges
                  | Some existing -> Ranges.intersection ~compare_v:version_compare existing ranges
                )
            | _ -> acc)
      in
      match derived_ranges with
      | Some ranges -> `Constrained ranges
      | None -> `Undecided

let extract_solution = fun solution ->
  let rec collect acc = function
    | [] -> List.reverse acc
    | Decision (pkg, ver, _, _) :: rest -> collect ((pkg, ver) :: acc) rest
    | Derivation _ :: rest -> collect acc rest
  in
  List.sort
    (collect [] (List.reverse solution.assignments))
    ~compare:(fun (left, _) (right, _) -> String.compare left right)

let pick_highest_priority_pkg = fun solution prioritizer ->
  let seen = Collections.HashSet.create () in
  let candidates = ref [] in
  let package_requires_decision = fun pkg ->
    List.any solution.assignments ~fn:(
      function
      | Derivation (p, _, true, _, _, _) when p = pkg -> true
      | _ -> false
    )
  in
  List.for_each (List.reverse solution.assignments)
    ~fn:(
      function
      | Derivation (pkg, _, _, _, _, gidx) -> (
          match Collections.HashMap.get solution.decisions ~key:pkg with
          | None
            when not (Collections.HashSet.contains seen ~value:pkg)
            && package_requires_decision pkg -> (
              let _ = Collections.HashSet.insert seen ~value:pkg in
              match get_constraint solution pkg with
              | `Constrained ranges ->
                  Log.info ("🔍 pick: found candidate " ^ pkg);
                  candidates := (pkg, ranges, gidx) :: !candidates
              | `Undecided ->
                  ()
              | `Decided _ ->
                  ()
            )
          | None ->
              ()
          | Some _ ->
              Log.info ("🔍 pick: " ^ pkg ^ " already decided, skipping")
        )
      | Decision _ -> ()
    );
  Log.info ("🔍 pick: found " ^ Int.to_string (List.length !candidates) ^ " total candidates");
  match !candidates with
  | [] -> None
  | _ ->
      let scored =
        List.map !candidates
          ~fn:(fun (pkg, ranges, gidx) -> (pkg, ranges, gidx, prioritizer pkg ranges))
      in
      let sorted =
        List.sort scored
          ~compare:(fun (_, _, gidx1, pri1) (_, _, gidx2, pri2) ->
            if pri1 = pri2 then
              compare gidx1 gidx2
            else
              compare pri2 pri1)
      in
      let pkg, _, _, _ = List.get_unchecked sorted ~at:0 in
      Log.info ("🔍 pick: selected " ^ pkg);
      let pkg, ranges, _, _ = List.get_unchecked sorted ~at:0 in
      Some (pkg, ranges)

let backtrack = fun solution target_level ->
  let new_decisions = Collections.HashMap.create () in
  let rec filter_assignments = fun acc ->
    function
    | [] ->
        List.reverse acc
    | Decision (pkg, ver, level, gidx) :: rest when level <= target_level ->
        let _ = Collections.HashMap.insert new_decisions ~key:pkg ~value:ver in
        filter_assignments (Decision (pkg, ver, level, gidx) :: acc) rest
    | Decision (_, _, level, _) :: rest when level > target_level ->
        filter_assignments acc rest
    | Derivation (pkg, ranges, requires_decision, cause, level, gidx) :: rest
      when level <= target_level ->
        (* KEEP derivations at or below target level - they're still valid! *)
        filter_assignments
          (Derivation (pkg, ranges, requires_decision, cause, level, gidx) :: acc)
          rest
    | Derivation (_, _, _, _, level, _) :: rest when level > target_level ->
        (* REMOVE derivations above target level *)
        filter_assignments acc rest
    | _ ->
        filter_assignments acc []
  in
  let new_assignments = filter_assignments [] solution.assignments in
  {
    assignments = new_assignments;
    decisions = new_decisions;
    decision_level = target_level;
    next_global_index = solution.next_global_index
  }

let relation = fun solution incompat ->
  let terms = Incompatibility.terms incompat in
  let satisfied_count = ref 0 in
  let undecided_pkg = ref None in
  let undecided_count = ref 0 in
  let contradicted_pkg = ref None in
  let contradicted_count = ref 0 in
  List.for_each terms
    ~fn:(fun term ->
      let pkg = Term.package term in
      let ranges = Term.ranges term in
      let is_positive = Term.is_positive term in
      match get_constraint solution pkg with
      | `Undecided ->
          incr undecided_count;
          undecided_pkg := Some pkg
      | `Decided ver ->
          let in_range = Ranges.contains ~compare_v:version_compare ranges ver in
          if (is_positive && in_range) || ((not is_positive) && not in_range) then
            incr satisfied_count
          else (
            incr contradicted_count;
            contradicted_pkg := Some pkg
          )
      | `Constrained constrained_ranges ->
          if is_positive then
            if Ranges.is_empty constrained_ranges then
              if Ranges.is_empty ranges then
                incr satisfied_count
              else (
                incr contradicted_count;
                contradicted_pkg := Some pkg
              )
            else if Ranges.is_disjoint ~compare_v:version_compare constrained_ranges ranges then
              (
                incr contradicted_count;
                contradicted_pkg := Some pkg
              )
            else if Ranges.subset_of ~compare_v:version_compare constrained_ranges ranges then
              incr satisfied_count
            else (
              incr undecided_count;
              undecided_pkg := Some pkg
            )
          else if
            Ranges.is_empty
              (Ranges.intersection ~compare_v:version_compare constrained_ranges ranges)
          then
            incr satisfied_count
          else if Ranges.subset_of ~compare_v:version_compare constrained_ranges ranges then
            (
              incr contradicted_count;
              contradicted_pkg := Some pkg
            )
          else (
            incr undecided_count;
            undecided_pkg := Some pkg
          ));
  let total = List.length terms in
  if !satisfied_count = total then
    (
      Log.debug
        ("Incompatibility SATISFIED (all " ^ Int.to_string total ^ " terms' constraints met → conflict!)");
      `Satisfied
    )
  else if !satisfied_count = total - 1 && !undecided_count = 1 then
    match !undecided_pkg with
    | Some pkg ->
        Log.debug ("Incompatibility ALMOST SATISFIED (one undecided: " ^ pkg ^ ")");
        `AlmostSatisfied pkg
    | None -> `Unknown
  else if !contradicted_count > 0 then
    match !contradicted_pkg with
    | Some pkg ->
        Log.debug
          ("Incompatibility CONTRADICTED ("
          ^ Int.to_string !contradicted_count
          ^ "/"
          ^ Int.to_string total
          ^ " terms' constraints unmet, pkg="
          ^ pkg
          ^ ")");
        `Contradicted pkg
    | None -> `Unknown
  else (
    Log.debug
      ("Incompatibility INCONCLUSIVE ("
      ^ Int.to_string !satisfied_count
      ^ " satisfied, "
      ^ Int.to_string !undecided_count
      ^ " undecided, "
      ^ Int.to_string !contradicted_count
      ^ " contradicted)");
    `Unknown
  )

let get_assignment_level = fun solution pkg ->
  let rec find_level = function
    | [] -> None
    | Decision (p, _, level, _) :: _ when p = pkg -> Some level
    | Derivation (p, _, _, _, level, _) :: _ when p = pkg -> Some level
    | _ :: rest -> find_level rest
  in
  find_level solution.assignments

type satisfier_info = {
  cause: Incompatibility.t option;
  global_index: int;
  decision_level: decision_level;
  assignment_ranges: version Ranges.t;
}

(* Port of Rust's satisfier_search algorithm *)

let satisfier_search = fun solution incompat ->
  let chronological = List.reverse solution.assignments in
  let assignment_pkg = function
    | Decision (pkg, _, _, _)
    | Derivation (pkg, _, _, _, _, _) -> pkg
  in
  let assignment_global_index = function
    | Decision (_, _, _, global_index)
    | Derivation (_, _, _, _, _, global_index) -> global_index
  in
  let assignment_level = function
    | Decision (_, _, level, _)
    | Derivation (_, _, _, _, level, _) -> level
  in
  let assignment_cause = function
    | Decision _ -> None
    | Derivation (_, _, _, cause, _, _) -> Some cause
  in
  let assignment_ranges = function
    | Decision (pkg, ver, _, _) -> Term.positive pkg (Ranges.singleton ver)
    | Derivation (pkg, ranges, _, _, _, _) -> Term.positive pkg ranges
  in
  let ranges_satisfy_term assigned_ranges term =
    if Term.is_positive term then
      Ranges.subset_of ~compare_v:version_compare assigned_ranges (Term.ranges term)
    else
      Ranges.is_disjoint ~compare_v:version_compare assigned_ranges (Term.ranges term)
  in
  let assignment_allowed_ranges = fun assignment -> Term.ranges (assignment_ranges assignment) in
  let update_accumulated_ranges = fun accumulated assignment ->
    match assignment with
    | Decision (_, ver, _, _) ->
        Ranges.singleton ver
    | Derivation (_, ranges, _, _, _, _) -> (
        match accumulated with
        | Some accumulated -> Ranges.intersection ~compare_v:version_compare accumulated ranges
        | None -> ranges
      )
  in
  let extra_term_for_partial_satisfier assigned_ranges term =
    let term_allowed_ranges =
      if Term.is_positive term then
        Term.ranges term
      else
        Ranges.complement ~compare_v:version_compare (Term.ranges term)
    in
    let difference = Ranges.intersection
      ~compare_v:version_compare
      assigned_ranges
      (Ranges.complement ~compare_v:version_compare term_allowed_ranges) in
    if Ranges.is_empty difference then
      None
    else
      Some (Term.negative (Term.package term) difference)
  in
  let find_package_satisfier = fun pkg term ->
    let rec loop accumulated = function
      | [] ->
          panic ("No satisfier found for package " ^ pkg)
      | assignment :: rest ->
          if String.equal (assignment_pkg assignment) pkg then
            let accumulated_ranges = update_accumulated_ranges accumulated assignment in
            if ranges_satisfy_term accumulated_ranges term then
              {
                cause = assignment_cause assignment;
                global_index = assignment_global_index assignment;
                decision_level = assignment_level assignment;
                assignment_ranges = assignment_allowed_ranges assignment
              }
            else
              loop (Some accumulated_ranges) rest
          else
            loop accumulated rest
    in
    loop None chronological
  in
  let find_package_previous_satisfier = fun pkg term satisfier_ranges ->
    let rec loop accumulated = function
      | [] ->
          panic ("No previous satisfier found for package " ^ pkg)
      | assignment :: rest ->
          if String.equal (assignment_pkg assignment) pkg then
            let accumulated_ranges = update_accumulated_ranges accumulated assignment in
            let combined_ranges = Ranges.intersection
              ~compare_v:version_compare
              accumulated_ranges
              satisfier_ranges in
            if ranges_satisfy_term combined_ranges term then
              {
                cause = assignment_cause assignment;
                global_index = assignment_global_index assignment;
                decision_level = assignment_level assignment;
                assignment_ranges = assignment_allowed_ranges assignment
              }
            else
              loop (Some accumulated_ranges) rest
          else
            loop accumulated rest
    in
    loop None chronological
  in
  let package_satisfiers =
    List.map
      (Incompatibility.terms incompat)
      ~fn:(fun term -> (term, find_package_satisfier (Term.package term) term))
  in
  let satisfier_term, satisfier_info =
    match package_satisfiers with
    | [] -> panic "No satisfier found for empty incompatibility"
    | first :: rest ->
        List.fold_left
          rest
          ~acc:first
          ~fn:(fun ((current_term, current_info)) ((candidate_term, candidate_info)) ->
            if candidate_info.global_index > current_info.global_index then
              (candidate_term, candidate_info)
            else
              (current_term, current_info))
  in
  let satisfier_pkg = Term.package satisfier_term in
  let _, previous_level =
    List.fold_left
      package_satisfiers
      ~acc:(-1, 1)
      ~fn:(fun ((previous_global_index, previous_level)) ((term, info)) ->
        let info =
          if String.equal (Term.package term) satisfier_pkg then
            find_package_previous_satisfier
              satisfier_pkg
              satisfier_term
              info.assignment_ranges
          else
            info
        in
        if info.global_index > previous_global_index then
          (info.global_index, max info.decision_level 1)
        else
          (previous_global_index, previous_level))
  in
  match satisfier_info.cause with
  | None ->
      (satisfier_pkg, `DifferentDecisionLevels previous_level)
  | Some _ when previous_level < satisfier_info.decision_level ->
      (satisfier_pkg, `DifferentDecisionLevels previous_level)
  | Some cause ->
      let extra_term =
        if ranges_satisfy_term satisfier_info.assignment_ranges satisfier_term then
          None
        else
          extra_term_for_partial_satisfier satisfier_info.assignment_ranges satisfier_term
      in
      (satisfier_pkg, `SameDecisionLevels { cause; extra_term })
