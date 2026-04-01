open Std
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
    next_global_index = 0;
  }

let current_decision_level = fun solution -> solution.decision_level

let add_decision = fun solution pkg ver ->
  let new_level = solution.decision_level + 1 in
  let global_index = solution.next_global_index in
  ignore (Collections.HashMap.insert solution.decisions pkg ver);
  {
    solution
    with assignments = Decision (pkg, ver, new_level, global_index) :: solution.assignments;
    decision_level = new_level;
    next_global_index = global_index + 1;
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
    next_global_index = global_index + 1;
  }

let get_decision = fun solution pkg ->
  Collections.HashMap.get solution.decisions pkg

(* Get the effective constraint for a package, considering both decisions and derivations *)

let get_constraint = fun solution pkg ->
  match get_decision solution pkg with
  | Some ver -> `Decided ver
  | None ->
      (* Check if there's a derivation *)
      let rec find_derivation = function
        | [] -> `Undecided
        | Derivation (p, ranges, _, _, _) :: _ when p = pkg -> `Constrained ranges
        | _ :: rest -> find_derivation rest
      in
      find_derivation solution.assignments

let extract_solution = fun solution -> Collections.HashMap.to_list solution.decisions

let pick_highest_priority_pkg = fun solution prioritizer ->
  let candidates = ref [] in
  List.iter
    (
      function
      | Derivation (pkg, ranges, _, _, _) -> (
          match Collections.HashMap.get solution.decisions pkg with
          | None ->
              if not (Ranges.is_empty ranges) then
                (
                  Log.info ("🔍 pick: found candidate " ^ pkg);
                  candidates := (pkg, ranges) :: !candidates
                )
          | Some _ -> Log.info ("🔍 pick: " ^ pkg ^ " already decided, skipping")
        )
      | Decision _ -> ()
    )
    solution.assignments;
  Log.info ("🔍 pick: found " ^ string_of_int (List.length !candidates) ^ " total candidates");
  match !candidates with
  | [] -> None
  | _ ->
      let sorted =
        List.sort
          (fun ((p1, r1)) ((p2, r2)) ->
            let pri1 = prioritizer p1 r1 in
            let pri2 = prioritizer p2 r2 in
            if pri1 = pri2 then
              String.compare p1 p2
            else
              compare pri2 pri1)
          !candidates
      in
      let pkg, _ = List.hd sorted in
      Log.info ("🔍 pick: selected " ^ pkg);
      Some (List.hd sorted)

let backtrack = fun solution target_level ->
  let new_decisions = Collections.HashMap.create () in
  let rec filter_assignments = fun acc ->
    function
    | [] ->
        List.rev acc
    | Decision (pkg, ver, level, gidx) :: rest when level <= target_level ->
        ignore (Collections.HashMap.insert new_decisions pkg ver);
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
    next_global_index = solution.next_global_index;
  }

let version_compare = fun a b ->
  match Version.compare a b with
  | Lt -> (-1)
  | Eq -> 0
  | Gt -> 1

let relation = fun solution incompat ->
  let terms = Incompatibility.terms incompat in
  let satisfied_count = ref 0 in
  let undecided_pkg = ref None in
  let undecided_count = ref 0 in
  let contradicted_pkg = ref None in
  let contradicted_count = ref 0 in
  List.iter
    (fun term ->
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
          else (
            incr undecided_count;
            undecided_pkg := Some pkg
          ))
    terms;
  let total = List.length terms in
  if !satisfied_count = total then
    (
      Log.debug
        ("Incompatibility SATISFIED (all " ^ string_of_int total ^ " terms' constraints met → conflict!)");
      `Satisfied
    )
  else if !satisfied_count = total - 1 && !undecided_count = 1 then
    match !undecided_pkg with
    | Some pkg ->
        Log.debug ("Incompatibility ALMOST SATISFIED (one undecided: " ^ pkg ^ ")");
        `AlmostSatisfied pkg
    | None -> `Unknown
  else if !contradicted_count = total then
    (
      Log.debug
        ("Incompatibility SATISFIED (all " ^ string_of_int total ^ " terms' constraints unmet → conflict!)");
      `Satisfied
    )
  else if !contradicted_count > 0 then
    match !contradicted_pkg with
    | Some pkg ->
        Log.debug
          ("Incompatibility CONTRADICTED ("
          ^ string_of_int !contradicted_count
          ^ "/"
          ^ string_of_int total
          ^ " terms' constraints unmet, pkg="
          ^ pkg
          ^ ")");
        `Contradicted pkg
    | None -> `Unknown
  else (
    Log.debug
      ("Incompatibility INCONCLUSIVE ("
      ^ string_of_int !satisfied_count
      ^ " satisfied, "
      ^ string_of_int !undecided_count
      ^ " undecided, "
      ^ string_of_int !contradicted_count
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
  let terms = Incompatibility.terms incompat in
  (* Step 1: find_satisfier - for each term, find when it was first satisfied *)
  (* Returns: (package, (cause_option, global_index, decision_level)) *)
  let find_satisfier_for_term term =
    let pkg = Term.package term in
    let term_ranges = Term.ranges term in
    let is_positive = Term.is_positive term in
    (* Search through assignments to find first one that satisfies the term *)
    let rec search = function
      | [] ->
          None
      | Decision (p, ver, level, gidx) :: _ when p = pkg ->
          (* Check if decision satisfies the term *)
          let in_range = Ranges.contains ~compare_v:version_compare term_ranges ver in
          let satisfies = (is_positive && in_range) || ((not is_positive) && not in_range) in
          if satisfies then
            Some (pkg, (None, gidx, level))
          else
            None
      | Derivation (p, ranges, cause, level, gidx) :: rest when p = pkg ->
          (* Check if derivation satisfies the term *)
          (* For positive term: derivation ranges must be subset of term ranges *)
          (* For negative term: derivation ranges must be disjoint from term ranges *)
          let satisfies =
            if is_positive then
              Ranges.subset_of ~compare_v:version_compare ranges term_ranges
            else
              Ranges.is_disjoint ~compare_v:version_compare ranges term_ranges
          in
          if satisfies then
            Some (pkg, (Some cause, gidx, level))
          else
            search rest
      | _ :: rest ->
          search rest
    in
    search solution.assignments
  in
  (* Build satisfied_map for all terms *)
  let satisfied_map = List.filter_map find_satisfier_for_term terms in
  if List.length satisfied_map = 0 then
    (
      Log.error "No satisfiers found in satisfier_search!";
      Log.error ("Incompatibility has " ^ string_of_int (List.length terms) ^ " terms");
      List.iter
        (fun term ->
          Log.error
            ("  Term: "
            ^ Term.package term
            ^ "@ranges (positive="
            ^ string_of_bool (Term.is_positive term)
            ^ ")"))
        terms;
      panic "No satisfiers found in satisfier_search"
    );
  let satisfier_pkg, (satisfier_cause_opt, _satisfier_gidx, satisfier_level) =
    List.fold_left
      (fun acc ((pkg, (cause_opt, gidx, level))) ->
        let _, (_, acc_gidx, _) = acc in
        if gidx > acc_gidx then
          (pkg, (cause_opt, gidx, level))
        else
          acc)
      (List.hd satisfied_map)
      (List.tl satisfied_map)
  in
  (* Step 3: find_previous_satisfier *)
  (* Find max global_index excluding satisfier *)
  let previous_satisfier_level =
    let filtered =
      List.filter (fun ((pkg, _)) -> pkg != satisfier_pkg) satisfied_map
    in
    if List.length filtered = 0 then
      1
      (* No previous satisfier *)
    else
      let _, (_, _, level) =
        List.fold_left
          (fun acc ((pkg, (cause_opt, gidx, level))) ->
            let _, (_, acc_gidx, _) = acc in
            if gidx > acc_gidx then
              (pkg, (cause_opt, gidx, level))
            else
              acc)
          (List.hd filtered)
          (List.tl filtered)
      in
      max level 1
  in
  (* Determine result based on decision levels *)
  if previous_satisfier_level >= satisfier_level then
    (
      (* SameDecisionLevels *)
      match satisfier_cause_opt with
      | Some cause -> (satisfier_pkg, `SameDecisionLevels cause)
      | None ->
          (* Satisfier is a decision, not a derivation - shouldn't happen in this case *)
          Log.error
            "Satisfier is decision at same level, treating as \
           DifferentDecisionLevels";
          (satisfier_pkg, `DifferentDecisionLevels previous_satisfier_level)
    )
  else
    (* DifferentDecisionLevels *)
    (satisfier_pkg, `DifferentDecisionLevels previous_satisfier_level)
