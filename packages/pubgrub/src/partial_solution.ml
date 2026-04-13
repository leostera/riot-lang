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
  | Derivation of package * version Ranges.t * Incompatibility.t * decision_level * int

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
  let ranges =
    match term with
    | Some t when Term.is_positive t ->
        (* Positive term: must complement to get what's forbidden *)
        Ranges.complement ~compare_v:version_compare (Term.ranges t)
    | Some t ->
        (* Negative term: ranges directly are what's required *)
        Term.ranges t
    | None -> Ranges.full
  in
  {
    solution
    with assignments = Derivation (pkg, ranges, incompat, solution.decision_level, global_index)
    :: solution.assignments;
    next_global_index = global_index + 1
  }

let get_decision = fun solution pkg ->
  Collections.HashMap.get solution.decisions ~key:pkg

(* Get the effective constraint for a package, considering both decisions and derivations *)

let get_constraint = fun solution pkg ->
  match get_decision solution pkg with
  | Some ver -> `Decided ver
  | None ->
      let derived_ranges =
        List.fold_left solution.assignments ~acc:None
          ~fn:(fun acc ->
            function
            | Derivation (p, ranges, _, _, _) when p = pkg ->
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

let extract_solution = fun solution -> Collections.HashMap.to_list solution.decisions

let pick_highest_priority_pkg = fun solution prioritizer ->
  let seen = Collections.HashSet.create () in
  let candidates = ref [] in
  List.for_each (List.reverse solution.assignments)
    ~fn:(
      function
      | Derivation (pkg, _, _, _, gidx) -> (
          match Collections.HashMap.get solution.decisions ~key:pkg with
          | None when not (Collections.HashSet.contains seen ~value:pkg) -> (
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
      let sorted =
        List.sort !candidates
          ~compare:(fun ((p1, r1, gidx1)) ((p2, r2, gidx2)) ->
            let pri1 = prioritizer p1 r1 in
            let pri2 = prioritizer p2 r2 in
            if pri1 = pri2 then
              compare gidx1 gidx2
            else
              compare pri2 pri1)
      in
      let pkg, _, _ = List.get_unchecked sorted ~at:0 in
      Log.info ("🔍 pick: selected " ^ pkg);
      let pkg, ranges, _ = List.get_unchecked sorted ~at:0 in
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
    | Derivation (pkg, ranges, cause, level, gidx) :: rest when level <= target_level ->
        (* KEEP derivations at or below target level - they're still valid! *)
        filter_assignments (Derivation (pkg, ranges, cause, level, gidx) :: acc) rest
    | Derivation (_, _, _, level, _) :: rest when level > target_level ->
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
    | Derivation (p, _, _, level, _) :: _ when p = pkg -> Some level
    | _ :: rest -> find_level rest
  in
  find_level solution.assignments

(* Port of Rust's satisfier_search algorithm *)

let satisfier_search = fun solution incompat ->
  let chronological = List.reverse solution.assignments in
  let solution_of_chronological assignments =
    let decisions = Collections.HashMap.create () in
    let decision_level = ref 0 in
    List.for_each assignments
      ~fn:(
        function
        | Decision (pkg, ver, level, _) ->
            let _ = Collections.HashMap.insert decisions ~key:pkg ~value:ver in
            decision_level := max !decision_level level
        | Derivation (_, _, _, level, _) -> decision_level := max !decision_level level
      );
    {
      assignments = List.reverse assignments;
      decisions;
      decision_level = !decision_level;
      next_global_index = List.length assignments
    }
  in
  let assignment_pkg = function
    | Decision (pkg, _, _, _)
    | Derivation (pkg, _, _, _, _) -> pkg
  in
  let assignment_level = function
    | Decision (_, _, level, _)
    | Derivation (_, _, _, level, _) -> level
  in
  let assignment_cause = function
    | Decision _ -> None
    | Derivation (_, _, cause, _, _) -> Some cause
  in
  let assignment_term = function
    | Decision (pkg, ver, _, _) -> Term.positive pkg (Ranges.singleton ver)
    | Derivation (pkg, ranges, _, _, _) -> Term.positive pkg ranges
  in
  let assignment_satisfies_term assignment term =
    let assigned_ranges = Term.ranges (assignment_term assignment) in
    if Term.is_positive term then
      Ranges.subset_of ~compare_v:version_compare assigned_ranges (Term.ranges term)
    else
      Ranges.is_disjoint ~compare_v:version_compare assigned_ranges (Term.ranges term)
  in
  let extra_term_for_partial_satisfier assignment term =
    let assigned_ranges = Term.ranges (assignment_term assignment) in
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
  let rec find_satisfier prefix = function
    | [] ->
        Log.error "No satisfier found in satisfier_search";
        panic
          (
            "No satisfier found in satisfier_search for " ^ (
              Incompatibility.terms incompat |> List.map ~fn:(fun term ->
                  let prefix =
                    if Term.is_positive term then
                      ""
                    else
                      "not "
                  in
                  prefix ^ Term.package term) |> String.concat " && "
            )
          )
    | assignment :: rest ->
        let prefix' = prefix @ [ assignment ] in
        let prefix_solution = solution_of_chronological prefix' in
        if relation prefix_solution incompat = `Satisfied then
          let pkg = assignment_pkg assignment in
          let term =
            match Incompatibility.get_term incompat pkg with
            | Some term -> term
            | None -> panic "Satisfier package not found in incompatibility"
          in
          (prefix', assignment, pkg, term)
        else
          find_satisfier prefix' rest
  in
  let satisfier_prefix, satisfier_assignment, satisfier_pkg, satisfier_term = find_satisfier [] chronological in
  let satisfier_level = assignment_level satisfier_assignment in
  let rec find_previous_satisfier prefix candidate before_satisfier =
    match before_satisfier with
    | [] -> candidate
    | assignment :: rest ->
        let prefix' = prefix @ [ assignment ] in
        let prefix_with_satisfier = prefix' @ [ satisfier_assignment ] in
        let prefix_solution = solution_of_chronological prefix_with_satisfier in
        if relation prefix_solution incompat = `Satisfied then
          find_previous_satisfier prefix' (Some assignment) rest
        else
          find_previous_satisfier prefix' candidate rest
  in
  let before_satisfier = List.reverse (List.tail (List.reverse satisfier_prefix)) in
  let previous_satisfier = find_previous_satisfier [] None before_satisfier in
  let previous_level =
    match previous_satisfier with
    | Some assignment -> max (assignment_level assignment) 1
    | None -> 1
  in
  match (assignment_cause satisfier_assignment, previous_satisfier) with
  | None, _ ->
      (satisfier_pkg, `DifferentDecisionLevels previous_level)
  | Some _, _ when previous_level < satisfier_level ->
      (satisfier_pkg, `DifferentDecisionLevels previous_level)
  | Some cause, _ ->
      let extra_term =
        if assignment_satisfies_term satisfier_assignment satisfier_term then
          None
        else
          extra_term_for_partial_satisfier satisfier_assignment satisfier_term
      in
      (satisfier_pkg, `SameDecisionLevels { cause; extra_term })
