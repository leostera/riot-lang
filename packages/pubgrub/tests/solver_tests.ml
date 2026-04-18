open Std
open Propane

let v = fun major minor patch -> Pubgrub.make_version ~major ~minor ~patch

let order_versions = fun left right ->
  if Pubgrub.version_compare left right <= 0 then
    (left, right)
  else
    (right, left)

let print_range = fun range ->
  Pubgrub.Ranges.to_string ~to_string_v:Pubgrub.version_to_string range

let version_gen = Generator.map3
  (fun major minor patch -> v major minor patch)
  (Generator.int_range 0 3)
  (Generator.int_range 0 3)
  (Generator.int_range 0 3)

let version_arb = Arbitrary.make ~print:Pubgrub.version_to_string version_gen

let simple_range_gen =
  let between_gen = Generator.map2
    (fun left right ->
      let low, high = order_versions left right in
      if Int.equal (Pubgrub.version_compare low high) 0 then
        Pubgrub.singleton low
      else
        Pubgrub.between low high)
    version_gen
    version_gen in
  Generator.one_of
    [
      Generator.return Pubgrub.empty;
      Generator.return Pubgrub.full;
      Generator.map Pubgrub.singleton version_gen;
      Generator.map Pubgrub.higher_than version_gen;
      Generator.map Pubgrub.strictly_higher_than version_gen;
      Generator.map Pubgrub.lower_than version_gen;
      Generator.map Pubgrub.strictly_lower_than version_gen;
      between_gen;
    ]

let range_gen = Generator.frequency
  [
    (6, simple_range_gen);
    (4, Generator.map2
      (Pubgrub.Ranges.union ~compare_v:Pubgrub.version_compare)
      simple_range_gen
      simple_range_gen);
  ]

let range_arb = Arbitrary.make ~print:print_range range_gen

let term_gen = Generator.map3
  (fun positive pkg ranges ->
    if positive then
      Pubgrub.Term.positive pkg ranges
    else
      Pubgrub.Term.negative pkg ranges)
  Generator.bool
  (Generator.one_of [ Generator.return "foo"; Generator.return "bar" ])
  range_gen

let print_term = fun term ->
  (
    if Pubgrub.Term.is_positive term then
      "+"
    else
      "-"
  )
  ^ Pubgrub.Term.package term
  ^ ":"
  ^ print_range (Pubgrub.Term.ranges term)

let term_arb = Arbitrary.make ~print:print_term term_gen

let assert_solution = fun expected_count result ->
  match result with
  | Ok (Pubgrub.Solver.Success solution) ->
      if List.length solution = expected_count then
        Ok ()
      else
        Error ("Wrong number of packages: got "
        ^ (Int.to_string (List.length solution))
        ^ ", expected "
        ^ (Int.to_string expected_count))
  | Ok (Pubgrub.Solver.Failure _) -> Error "Unexpected conflict"
  | Error err -> Error ("Error: " ^ err)

let assert_conflict = fun result ->
  match result with
  | Ok (Pubgrub.Solver.Failure _) -> Ok ()
  | Ok (Pubgrub.Solver.Success _) -> Error "Expected conflict but found solution"
  | Error err -> Error ("Error: " ^ err)

let ranges_equal = fun left right ->
  Pubgrub.Ranges.equal ~compare_v:Pubgrub.version_compare left right

let assert_ranges_equal = fun ~expected ~actual ~message ->
  if ranges_equal expected actual then
    Ok ()
  else
    Error message

let assert_range_membership = fun ~present ~absent ranges ->
  let rec check_present = function
    | [] -> Ok ()
    | version :: rest ->
        if Pubgrub.Ranges.contains ~compare_v:Pubgrub.version_compare ranges version then
          check_present rest
        else
          Error ("Expected range to contain " ^ Pubgrub.version_to_string version)
  in
  let rec check_absent = function
    | [] -> Ok ()
    | version :: rest ->
        if Pubgrub.Ranges.contains ~compare_v:Pubgrub.version_compare ranges version then
          Error ("Expected range to exclude " ^ Pubgrub.version_to_string version)
        else
          check_absent rest
  in
  match check_present present with
  | Ok () -> check_absent absent
  | Error _ as err -> err

let assert_term = fun ~package ~positive ~ranges term ->
  if not (String.equal (Pubgrub.Term.package term) package) then
    Error ("Expected term package " ^ package)
  else if not (Bool.equal (Pubgrub.Term.is_positive term) positive) then
    Error ("Unexpected sign for term " ^ package)
  else
    assert_ranges_equal
      ~expected:ranges
      ~actual:(Pubgrub.Term.ranges term)
      ~message:("Unexpected ranges for term " ^ package)

let assert_term_incompat = fun ~package ~positive ~ranges incompat ->
  match Pubgrub.Incompatibility.get_term incompat package with
  | Some term -> assert_term ~package ~positive ~ranges term
  | None -> Error ("Expected incompatibility term for package " ^ package)

let assert_version_equal = fun ~expected ~actual ~message ->
  if Int.equal (Pubgrub.version_compare expected actual) 0 then
    Ok ()
  else
    Error
      (message
      ^ ": got "
      ^ Pubgrub.version_to_string actual
      ^ ", expected "
      ^ Pubgrub.version_to_string expected)

let assert_choose_none = fun result ->
  match result with
  | Ok None -> Ok ()
  | Ok (Some version) ->
      Error ("Expected no version but got " ^ Pubgrub.version_to_string version)
  | Error err -> Error ("Unexpected provider error: " ^ err)

let assert_choose_version = fun ~expected result ->
  match result with
  | Ok (Some actual) ->
      assert_version_equal
        ~expected
        ~actual
        ~message:"Unexpected chosen version"
  | Ok None ->
      Error "Expected a chosen version but got none"
  | Error err -> Error ("Unexpected provider error: " ^ err)

let assert_count_versions = fun ~expected result ->
  match result with
  | Ok actual when Int.equal actual expected -> Ok ()
  | Ok actual ->
      Error
        ("Unexpected version count: got "
        ^ Int.to_string actual
        ^ ", expected "
        ^ Int.to_string expected)
  | Error err -> Error ("Unexpected provider error: " ^ err)

let rec equal_dependencies = fun expected actual ->
  match (expected, actual) with
  | [], [] -> true
  | (expected_pkg, expected_ranges) :: expected_rest, (actual_pkg, actual_ranges) :: actual_rest ->
      String.equal expected_pkg actual_pkg
      && ranges_equal expected_ranges actual_ranges
      && equal_dependencies expected_rest actual_rest
  | _ -> false

let assert_dependencies = fun ~expected result ->
  match result with
  | Ok (Pubgrub.Provider.Available actual) when equal_dependencies expected actual -> Ok ()
  | Ok (Pubgrub.Provider.Available _) ->
      Error "Unexpected dependency list"
  | Ok (Pubgrub.Provider.Unavailable reason) ->
      Error ("Expected available dependencies but got unavailable: " ^ reason)
  | Error err -> Error ("Unexpected provider error: " ^ err)

let assert_unavailable = fun ~expected result ->
  match result with
  | Ok (Pubgrub.Provider.Unavailable actual) when String.equal actual expected -> Ok ()
  | Ok (Pubgrub.Provider.Unavailable actual) ->
      Error ("Unexpected unavailable reason: " ^ actual)
  | Ok (Pubgrub.Provider.Available _) ->
      Error "Expected unavailable provider result"
  | Error err -> Error ("Unexpected provider error: " ^ err)

let assert_relation = fun expected actual ->
  match (expected, actual) with
  | `Satisfied, `Satisfied -> Ok ()
  | `Unknown, `Unknown -> Ok ()
  | `AlmostSatisfied expected_pkg, `AlmostSatisfied actual_pkg when String.equal expected_pkg actual_pkg -> Ok ()
  | `Contradicted expected_pkg, `Contradicted actual_pkg when String.equal expected_pkg actual_pkg -> Ok ()
  | _ -> Error "Unexpected relation result"

let assert_constraint = fun ~expected actual ->
  match (expected, actual) with
  | `Undecided, `Undecided -> Ok ()
  | `Decided expected, `Decided actual ->
      assert_version_equal ~expected ~actual ~message:"Unexpected decided version"
  | `Constrained expected, `Constrained actual ->
      assert_ranges_equal ~expected ~actual ~message:"Unexpected constrained ranges"
  | _ -> Error "Unexpected constraint state"

let custom_incompat = fun terms ->
  Pubgrub.Incompatibility.create_external
    terms
    (Pubgrub.Incompatibility.Custom ("custom", Pubgrub.full, "test"))

let rec equal_string_lists = fun left right ->
  match (left, right) with
  | [], [] -> true
  | left :: left_rest, right :: right_rest ->
      String.equal left right && equal_string_lists left_rest right_rest
  | _ -> false

let assert_solution_packages = fun expected result ->
  match result with
  | Ok (Pubgrub.Solver.Success solution) ->
      let actual = List.map solution ~fn:(fun (pkg, _ver) -> pkg) in
      if equal_string_lists expected actual then
        Ok ()
      else
        Error
          (
            "Wrong solution order: got ["
            ^ String.concat ", " actual
            ^ "], expected ["
            ^ String.concat ", " expected
            ^ "]"
          )
  | Ok (Pubgrub.Solver.Failure incompat) ->
      Error ("Unexpected conflict: " ^ Pubgrub.explain_conflict incompat)
  | Error err -> Error ("Error: " ^ err)

let assert_raises = fun message fn ->
  try
    let _ = fn () in
    Error message
  with
  | _ -> Ok ()

let assert_inline_text = fun ~ctx ~expected ~actual ->
  Test.Snapshot.assert_inline_text ~ctx ~expected ~actual

let assert_int_equal = fun ~expected ~actual ~message ->
  if Int.equal expected actual then
    Ok ()
  else
    Error
      (message
      ^ ": got "
      ^ Int.to_string actual
      ^ ", expected "
      ^ Int.to_string expected)

let prop_ranges_complement_matches_membership =
  Propane.property
    "Property: range complement matches membership negation"
    Arbitrary.(pair range_arb version_arb)
    (fun (ranges, version) ->
      let contains = Pubgrub.Ranges.contains ~compare_v:Pubgrub.version_compare ranges version in
      let complement_contains = Pubgrub.Ranges.contains
        ~compare_v:Pubgrub.version_compare
        (Pubgrub.Ranges.complement ~compare_v:Pubgrub.version_compare ranges)
        version in
      Bool.equal complement_contains (not contains))

let prop_ranges_double_complement_is_identity =
  Propane.property
    "Property: range double complement is semantic identity"
    range_arb
    (fun ranges ->
      ranges_equal
        ranges
        (Pubgrub.Ranges.complement
           ~compare_v:Pubgrub.version_compare
           (Pubgrub.Ranges.complement ~compare_v:Pubgrub.version_compare ranges)))

let prop_ranges_union_matches_boolean_or =
  Propane.property
    "Property: range union membership matches boolean or"
    Arbitrary.(triple range_arb range_arb version_arb)
    (fun (left, right, version) ->
      let union_contains = Pubgrub.Ranges.contains
        ~compare_v:Pubgrub.version_compare
        (Pubgrub.Ranges.union ~compare_v:Pubgrub.version_compare left right)
        version in
      Bool.equal
        union_contains
        (Pubgrub.Ranges.contains ~compare_v:Pubgrub.version_compare left version
        || Pubgrub.Ranges.contains ~compare_v:Pubgrub.version_compare right version))

let prop_ranges_intersection_matches_boolean_and =
  Propane.property
    "Property: range intersection membership matches boolean and"
    Arbitrary.(triple range_arb range_arb version_arb)
    (fun (left, right, version) ->
      let intersection_contains = Pubgrub.Ranges.contains
        ~compare_v:Pubgrub.version_compare
        (Pubgrub.Ranges.intersection ~compare_v:Pubgrub.version_compare left right)
        version in
      Bool.equal
        intersection_contains
        (Pubgrub.Ranges.contains ~compare_v:Pubgrub.version_compare left version
        && Pubgrub.Ranges.contains ~compare_v:Pubgrub.version_compare right version))

let prop_term_negation_is_involutive =
  Propane.property
    "Property: term negation is involutive"
    term_arb
    (fun term ->
      let twice = Pubgrub.Term.negate (Pubgrub.Term.negate term) in
      String.equal (Pubgrub.Term.package term) (Pubgrub.Term.package twice)
      && Bool.equal (Pubgrub.Term.is_positive term) (Pubgrub.Term.is_positive twice)
      && ranges_equal (Pubgrub.Term.ranges term) (Pubgrub.Term.ranges twice))

let test_ranges_empty_contains_nothing =
  Test.case "Ranges: empty contains nothing"
    (fun _ctx ->
      assert_range_membership
        ~present:[]
        ~absent:[ v 0 0 0; v 1 0 0; v 2 0 0 ]
        Pubgrub.empty)

let test_ranges_full_contains_everything =
  Test.case "Ranges: full contains sampled versions"
    (fun _ctx ->
      assert_range_membership
        ~present:[ v 0 0 0; v 1 0 0; v 2 0 0; v 9 0 0 ]
        ~absent:[]
        Pubgrub.full)

let test_ranges_singleton_exact =
  Test.case "Ranges: singleton contains only one version"
    (fun _ctx ->
      assert_range_membership
        ~present:[ v 1 0 0 ]
        ~absent:[ v 0 9 9; v 1 0 1; v 2 0 0 ]
        (Pubgrub.singleton (v 1 0 0)))

let test_ranges_higher_than_is_inclusive =
  Test.case "Ranges: higher_than is inclusive"
    (fun _ctx ->
      assert_range_membership
        ~present:[ v 2 0 0; v 3 0 0 ]
        ~absent:[ v 1 9 9 ]
        (Pubgrub.higher_than (v 2 0 0)))

let test_ranges_strictly_higher_than_is_exclusive =
  Test.case "Ranges: strictly_higher_than is exclusive"
    (fun _ctx ->
      assert_range_membership
        ~present:[ v 2 0 1; v 3 0 0 ]
        ~absent:[ v 2 0 0; v 1 9 9 ]
        (Pubgrub.strictly_higher_than (v 2 0 0)))

let test_ranges_lower_than_is_inclusive =
  Test.case "Ranges: lower_than is inclusive"
    (fun _ctx ->
      assert_range_membership
        ~present:[ v 0 9 9; v 2 0 0 ]
        ~absent:[ v 2 0 1; v 3 0 0 ]
        (Pubgrub.lower_than (v 2 0 0)))

let test_ranges_strictly_lower_than_is_exclusive =
  Test.case "Ranges: strictly_lower_than is exclusive"
    (fun _ctx ->
      assert_range_membership
        ~present:[ v 0 9 9; v 1 9 9 ]
        ~absent:[ v 2 0 0; v 2 0 1 ]
        (Pubgrub.strictly_lower_than (v 2 0 0)))

let test_ranges_between_is_half_open =
  Test.case "Ranges: between is half-open"
    (fun _ctx ->
      assert_range_membership
        ~present:[ v 1 0 0; v 1 9 9 ]
        ~absent:[ v 0 9 9; v 2 0 0; v 2 0 1 ]
        (Pubgrub.between (v 1 0 0) (v 2 0 0)))

let test_ranges_intersection_with_empty =
  Test.case "Ranges: intersection with empty is empty"
    (fun _ctx ->
      assert_ranges_equal
        ~expected:Pubgrub.empty
        ~actual:(Pubgrub.Ranges.intersection
          ~compare_v:Pubgrub.version_compare
          (Pubgrub.higher_than (v 1 0 0))
          Pubgrub.empty)
        ~message:"Expected intersection with empty to be empty")

let test_ranges_intersection_with_full =
  Test.case "Ranges: intersection with full preserves the range"
    (fun _ctx ->
      let ranges = Pubgrub.between (v 1 0 0) (v 3 0 0) in
      assert_ranges_equal
        ~expected:ranges
        ~actual:(Pubgrub.Ranges.intersection
          ~compare_v:Pubgrub.version_compare
          ranges
          Pubgrub.full)
        ~message:"Expected intersection with full to preserve the range")

let test_ranges_union_with_empty =
  Test.case "Ranges: union with empty preserves the range"
    (fun _ctx ->
      let ranges = Pubgrub.between (v 1 0 0) (v 3 0 0) in
      assert_ranges_equal
        ~expected:ranges
        ~actual:(Pubgrub.Ranges.union
          ~compare_v:Pubgrub.version_compare
          ranges
          Pubgrub.empty)
        ~message:"Expected union with empty to preserve the range")

let test_ranges_complement_of_singleton =
  Test.case "Ranges: complement of singleton excludes only that version"
    (fun _ctx ->
      assert_range_membership
        ~present:[ v 0 9 9; v 1 0 1; v 2 0 0 ]
        ~absent:[ v 1 0 0 ]
        (Pubgrub.Ranges.complement
          ~compare_v:Pubgrub.version_compare
          (Pubgrub.singleton (v 1 0 0))))

let test_ranges_touching_exclusive_intersection_is_empty =
  Test.case "Ranges: touching exclusive bounds intersect to empty"
    (fun _ctx ->
      assert_ranges_equal
        ~expected:Pubgrub.empty
        ~actual:(Pubgrub.Ranges.intersection
          ~compare_v:Pubgrub.version_compare
          (Pubgrub.between (v 1 0 0) (v 2 0 0))
          (Pubgrub.between (v 2 0 0) (v 3 0 0)))
        ~message:"Expected touching half-open ranges to be disjoint")

let test_ranges_union_of_overlapping_intervals =
  Test.case "Ranges: union of overlapping intervals merges semantically"
    (fun _ctx ->
      assert_ranges_equal
        ~expected:(Pubgrub.between (v 1 0 0) (v 4 0 0))
        ~actual:(Pubgrub.Ranges.union
          ~compare_v:Pubgrub.version_compare
          (Pubgrub.between (v 1 0 0) (v 3 0 0))
          (Pubgrub.between (v 2 0 0) (v 4 0 0)))
        ~message:"Expected overlapping union to cover the merged interval")

let test_ranges_complement_of_multiple_segments =
  Test.case "Ranges: complement of multiple segments yields the gaps"
    (fun _ctx ->
      let ranges = Pubgrub.Ranges.union
        ~compare_v:Pubgrub.version_compare
        (Pubgrub.between (v 1 0 0) (v 2 0 0))
        (Pubgrub.between (v 3 0 0) (v 4 0 0)) in
      let expected = Pubgrub.Ranges.union
        ~compare_v:Pubgrub.version_compare
        (Pubgrub.strictly_lower_than (v 1 0 0))
        (Pubgrub.Ranges.union
          ~compare_v:Pubgrub.version_compare
          (Pubgrub.between (v 2 0 0) (v 3 0 0))
          (Pubgrub.higher_than (v 4 0 0))) in
      assert_ranges_equal
        ~expected
        ~actual:(Pubgrub.Ranges.complement ~compare_v:Pubgrub.version_compare ranges)
        ~message:"Expected complement to preserve the gaps between segments")

let test_ranges_is_disjoint_matches_empty_intersection =
  Test.case "Ranges: is_disjoint matches intersection emptiness"
    (fun _ctx ->
      let left = Pubgrub.between (v 1 0 0) (v 2 0 0) in
      let right = Pubgrub.between (v 2 0 0) (v 3 0 0) in
      let intersection = Pubgrub.Ranges.intersection
        ~compare_v:Pubgrub.version_compare
        left
        right in
      if Bool.equal
        (Pubgrub.Ranges.is_disjoint ~compare_v:Pubgrub.version_compare left right)
        (ranges_equal intersection Pubgrub.empty)
      then
        Ok ()
      else
        Error "Expected is_disjoint to agree with intersection emptiness")

let test_ranges_subset_and_double_complement =
  Test.case "Ranges: subset_of and double complement are semantic"
    (fun _ctx ->
      let base = Pubgrub.between (v 1 0 0) (v 4 0 0) in
      let subset = Pubgrub.between (v 2 0 0) (v 3 0 0) in
      let roundtrip = Pubgrub.Ranges.complement
        ~compare_v:Pubgrub.version_compare
        (Pubgrub.Ranges.complement ~compare_v:Pubgrub.version_compare base) in
      if
        Pubgrub.Ranges.subset_of ~compare_v:Pubgrub.version_compare subset base
        && not (Pubgrub.Ranges.subset_of ~compare_v:Pubgrub.version_compare base subset)
        && ranges_equal base roundtrip
      then
        Ok ()
      else
        Error "Expected subset_of and double complement semantics to hold")

let test_ranges_normalize_collapses_semantic_duplicates =
  Test.case "Ranges: normalize collapses overlapping semantic duplicates"
    (fun _ctx ->
      let ranges = Pubgrub.Ranges.union
        ~compare_v:Pubgrub.version_compare
        (Pubgrub.between (v 1 0 0) (v 3 0 0))
        (Pubgrub.between (v 2 0 0) (v 4 0 0)) in
      let normalized = Pubgrub.Ranges.normalize
        ~compare_v:Pubgrub.version_compare
        ranges in
      assert_ranges_equal
        ~expected:(Pubgrub.between (v 1 0 0) (v 4 0 0))
        ~actual:normalized
        ~message:"Expected normalize to collapse overlapping segments")

let test_ranges_compare_is_semantic =
  Test.case "Ranges: compare is semantic after normalization"
    (fun _ctx ->
      let left = Pubgrub.Ranges.union
        ~compare_v:Pubgrub.version_compare
        (Pubgrub.between (v 1 0 0) (v 3 0 0))
        (Pubgrub.between (v 2 0 0) (v 4 0 0)) in
      let right = Pubgrub.between (v 1 0 0) (v 4 0 0) in
      if Int.equal (Pubgrub.Ranges.compare ~compare_v:Pubgrub.version_compare left right) 0 then
        Ok ()
      else
        Error "Expected compare to treat semantically equal ranges as equal")

let test_ranges_to_string_is_stable =
  Test.case "Ranges: to_string prints canonical segments"
    (fun _ctx ->
      let ranges = Pubgrub.Ranges.union
        ~compare_v:Pubgrub.version_compare
        (Pubgrub.strictly_lower_than (v 1 0 0))
        (Pubgrub.higher_than (v 2 0 0)) in
      let actual = Pubgrub.Ranges.to_string
        ~to_string_v:Pubgrub.version_to_string
        ranges in
      if String.equal actual "(-inf, 1.0.0) | [2.0.0, +inf)" then
        Ok ()
      else
        Error ("Unexpected range string: " ^ actual))

let test_term_positive_full_is_any =
  Test.case "Term: positive full is tautological"
    (fun _ctx ->
      if Pubgrub.Term.is_any (Pubgrub.Term.positive "pkg" Pubgrub.full) then
        Ok ()
      else
        Error "Expected positive full term to be tautological")

let test_term_negative_empty_is_any =
  Test.case "Term: negative empty is tautological"
    (fun _ctx ->
      if Pubgrub.Term.is_any (Pubgrub.Term.negative "pkg" Pubgrub.empty) then
        Ok ()
      else
        Error "Expected negative empty term to be tautological")

let test_term_positive_empty_is_not_any =
  Test.case "Term: positive empty is not tautological"
    (fun _ctx ->
      if Pubgrub.Term.is_any (Pubgrub.Term.positive "pkg" Pubgrub.empty) then
        Error "Expected positive empty term to remain a contradiction"
      else
        Ok ())

let test_term_negative_full_is_not_any =
  Test.case "Term: negative full is not tautological"
    (fun _ctx ->
      if Pubgrub.Term.is_any (Pubgrub.Term.negative "pkg" Pubgrub.full) then
        Error "Expected negative full term to remain a contradiction"
      else
        Ok ())

let test_term_structural_package_equality =
  Test.case "Term: equal package strings compare structurally"
    (fun _ctx ->
      let dynamic_pkg = String.concat "" [ "sha"; "red" ] in
      try
        let term = Pubgrub.Term.intersection
          (Pubgrub.Term.positive dynamic_pkg Pubgrub.full)
          (Pubgrub.Term.positive "shared" (Pubgrub.singleton (v 1 0 0)))
        in
        if
          String.equal (Pubgrub.Term.package term) "shared"
          && Pubgrub.Term.is_positive term
          && Pubgrub.Ranges.contains
            ~compare_v:Pubgrub.version_compare
            (Pubgrub.Term.ranges term)
            (v 1 0 0)
        then
          Ok ()
        else
          Error "Expected structural package equality to preserve term intersection"
      with
      | _ -> Error "Term.intersection raised for equal package names with distinct allocations")

let test_term_mixed_union_can_be_tautological =
  Test.case "Term: mixed union preserves tautologies"
    (fun _ctx ->
      let term = Pubgrub.Term.union
        (Pubgrub.Term.positive "pkg" Pubgrub.full)
        (Pubgrub.Term.negative "pkg" Pubgrub.full) in
      if Pubgrub.Term.is_any term then
        Ok ()
      else
        Error "Expected positive full union negative full to be tautological")

let test_term_negation_is_involutive =
  Test.case "Term: negation is involutive"
    (fun _ctx ->
      let term = Pubgrub.Term.positive "pkg" (Pubgrub.between (v 1 0 0) (v 3 0 0)) in
      assert_term
        ~package:"pkg"
        ~positive:true
        ~ranges:(Pubgrub.between (v 1 0 0) (v 3 0 0))
        (Pubgrub.Term.negate (Pubgrub.Term.negate term)))

let test_term_positive_positive_intersection =
  Test.case "Term: positive intersection uses range intersection"
    (fun _ctx ->
      assert_term
        ~package:"pkg"
        ~positive:true
        ~ranges:(Pubgrub.between (v 2 0 0) (v 3 0 0))
        (Pubgrub.Term.intersection
          (Pubgrub.Term.positive "pkg" (Pubgrub.between (v 1 0 0) (v 3 0 0)))
          (Pubgrub.Term.positive "pkg" (Pubgrub.between (v 2 0 0) (v 4 0 0)))))

let test_term_positive_positive_union =
  Test.case "Term: positive union uses range union"
    (fun _ctx ->
      assert_term
        ~package:"pkg"
        ~positive:true
        ~ranges:(Pubgrub.between (v 1 0 0) (v 4 0 0))
        (Pubgrub.Term.union
          (Pubgrub.Term.positive "pkg" (Pubgrub.between (v 1 0 0) (v 3 0 0)))
          (Pubgrub.Term.positive "pkg" (Pubgrub.between (v 2 0 0) (v 4 0 0)))))

let test_term_negative_negative_intersection =
  Test.case "Term: negative intersection uses range union semantics"
    (fun _ctx ->
      assert_term
        ~package:"pkg"
        ~positive:false
        ~ranges:(Pubgrub.between (v 1 0 0) (v 5 0 0))
        (Pubgrub.Term.intersection
          (Pubgrub.Term.negative "pkg" (Pubgrub.between (v 1 0 0) (v 4 0 0)))
          (Pubgrub.Term.negative "pkg" (Pubgrub.between (v 2 0 0) (v 5 0 0)))))

let test_term_negative_negative_union =
  Test.case "Term: negative union uses range intersection semantics"
    (fun _ctx ->
      assert_term
        ~package:"pkg"
        ~positive:false
        ~ranges:(Pubgrub.between (v 2 0 0) (v 4 0 0))
        (Pubgrub.Term.union
          (Pubgrub.Term.negative "pkg" (Pubgrub.between (v 1 0 0) (v 4 0 0)))
          (Pubgrub.Term.negative "pkg" (Pubgrub.between (v 2 0 0) (v 5 0 0)))))

let test_term_positive_negative_intersection =
  Test.case "Term: positive-negative intersection excludes the forbidden hole"
    (fun _ctx ->
      let term = Pubgrub.Term.intersection
        (Pubgrub.Term.positive "pkg" (Pubgrub.between (v 1 0 0) (v 4 0 0)))
        (Pubgrub.Term.negative "pkg" (Pubgrub.between (v 2 0 0) (v 3 0 0))) in
      if
        Pubgrub.Term.is_positive term
        && assert_range_membership
          ~present:[ v 1 0 0; v 3 0 0 ]
          ~absent:[ v 2 0 0; v 2 5 0; v 4 0 0 ]
          (Pubgrub.Term.ranges term) = Ok ()
      then
        Ok ()
      else
        Error "Expected positive-negative intersection to preserve only the allowed parts")

let test_term_positive_negative_union =
  Test.case "Term: positive-negative union keeps complement semantics"
    (fun _ctx ->
      assert_term
        ~package:"pkg"
        ~positive:false
        ~ranges:(Pubgrub.between (v 1 0 0) (v 2 0 0))
        (Pubgrub.Term.union
          (Pubgrub.Term.positive "pkg" (Pubgrub.between (v 2 0 0) (v 4 0 0)))
          (Pubgrub.Term.negative "pkg" (Pubgrub.between (v 1 0 0) (v 3 0 0)))))

let test_term_union_on_different_packages_raises =
  Test.case "Term: union on different packages raises"
    (fun _ctx ->
      assert_raises
        "Expected union on different packages to raise"
        (fun () ->
          Pubgrub.Term.union
            (Pubgrub.Term.positive "left" Pubgrub.full)
            (Pubgrub.Term.positive "right" Pubgrub.full)))

let test_term_intersection_on_different_packages_raises =
  Test.case "Term: intersection on different packages raises"
    (fun _ctx ->
      assert_raises
        "Expected intersection on different packages to raise"
        (fun () ->
          Pubgrub.Term.intersection
            (Pubgrub.Term.positive "left" Pubgrub.full)
            (Pubgrub.Term.positive "right" Pubgrub.full)))

let test_provider_choose_version_missing_package =
  Test.case "Provider: missing package choose_version returns none"
    (fun _ctx ->
      let provider = Pubgrub.to_provider (Pubgrub.create_offline ()) in
      assert_choose_none (provider.choose_version "missing" Pubgrub.full))

let test_provider_count_versions_missing_package =
  Test.case "Provider: missing package count_versions returns zero"
    (fun _ctx ->
      let provider = Pubgrub.to_provider (Pubgrub.create_offline ()) in
      assert_count_versions ~expected:0 (provider.count_versions "missing" Pubgrub.full))

let test_provider_get_dependencies_missing_package =
  Test.case "Provider: missing package get_dependencies returns unavailable"
    (fun _ctx ->
      let provider = Pubgrub.to_provider (Pubgrub.create_offline ()) in
      assert_unavailable
        ~expected:"Package 'missing' not found"
        (provider.get_dependencies "missing" (v 1 0 0)))

let test_provider_choose_version_picks_highest_match =
  Test.case "Provider: choose_version picks the highest matching version"
    (fun _ctx ->
      let offline = Pubgrub.create_offline () in
      Pubgrub.add_package offline "foo" (v 1 0 0) [];
      Pubgrub.add_package offline "foo" (v 2 0 0) [];
      Pubgrub.add_package offline "foo" (v 3 0 0) [];
      let provider = Pubgrub.to_provider offline in
      assert_choose_version
        ~expected:(v 3 0 0)
        (provider.choose_version "foo" (Pubgrub.higher_than (v 2 0 0))))

let test_provider_choose_version_skips_nonmatching_higher =
  Test.case "Provider: choose_version skips a nonmatching higher version"
    (fun _ctx ->
      let offline = Pubgrub.create_offline () in
      Pubgrub.add_package offline "foo" (v 1 0 0) [];
      Pubgrub.add_package offline "foo" (v 2 0 0) [];
      Pubgrub.add_package offline "foo" (v 3 0 0) [];
      let provider = Pubgrub.to_provider offline in
      assert_choose_version
        ~expected:(v 2 0 0)
        (provider.choose_version "foo" (Pubgrub.strictly_lower_than (v 3 0 0))))

let test_provider_choose_version_exact_match =
  Test.case "Provider: choose_version respects an exact singleton range"
    (fun _ctx ->
      let offline = Pubgrub.create_offline () in
      Pubgrub.add_package offline "foo" (v 1 0 0) [];
      Pubgrub.add_package offline "foo" (v 2 0 0) [];
      Pubgrub.add_package offline "foo" (v 3 0 0) [];
      let provider = Pubgrub.to_provider offline in
      assert_choose_version
        ~expected:(v 2 0 0)
        (provider.choose_version "foo" (Pubgrub.singleton (v 2 0 0))))

let test_provider_count_versions_across_disjoint_ranges =
  Test.case "Provider: count_versions handles disjoint semantic ranges"
    (fun _ctx ->
      let offline = Pubgrub.create_offline () in
      Pubgrub.add_package offline "foo" (v 1 0 0) [];
      Pubgrub.add_package offline "foo" (v 2 0 0) [];
      Pubgrub.add_package offline "foo" (v 3 0 0) [];
      let provider = Pubgrub.to_provider offline in
      let ranges = Pubgrub.Ranges.union
        ~compare_v:Pubgrub.version_compare
        (Pubgrub.singleton (v 1 0 0))
        (Pubgrub.singleton (v 3 0 0)) in
      assert_count_versions ~expected:2 (provider.count_versions "foo" ranges))

let test_provider_duplicate_version_latest_deps_win =
  Test.case "Provider: duplicate package versions use deterministic latest deps"
    (fun _ctx ->
      let offline = Pubgrub.create_offline () in
      Pubgrub.add_package offline "foo" (v 1 0 0) [ ("left", Pubgrub.full) ];
      Pubgrub.add_package offline "foo" (v 1 0 0) [ ("right", Pubgrub.singleton (v 2 0 0)) ];
      let provider = Pubgrub.to_provider offline in
      assert_dependencies
        ~expected:[ ("right", Pubgrub.singleton (v 2 0 0)) ]
        (provider.get_dependencies "foo" (v 1 0 0)))

let test_provider_duplicate_versions_count_once =
  Test.case "Provider: duplicate package versions count once"
    (fun _ctx ->
      let offline = Pubgrub.create_offline () in
      Pubgrub.add_package offline "foo" (v 1 0 0) [];
      Pubgrub.add_package offline "foo" (v 1 0 0) [ ("right", Pubgrub.singleton (v 2 0 0)) ];
      let provider = Pubgrub.to_provider offline in
      assert_count_versions ~expected:1 (provider.count_versions "foo" Pubgrub.full))

let test_provider_insertion_order_is_semantic =
  Test.case "Provider: insertion order does not affect highest-match selection"
    (fun _ctx ->
      let forward = Pubgrub.create_offline () in
      Pubgrub.add_package forward "foo" (v 1 0 0) [];
      Pubgrub.add_package forward "foo" (v 3 0 0) [ ("dep", Pubgrub.singleton (v 3 0 0)) ];
      Pubgrub.add_package forward "foo" (v 2 0 0) [];
      let reverse = Pubgrub.create_offline () in
      Pubgrub.add_package reverse "foo" (v 2 0 0) [];
      Pubgrub.add_package reverse "foo" (v 1 0 0) [];
      Pubgrub.add_package reverse "foo" (v 3 0 0) [ ("dep", Pubgrub.singleton (v 3 0 0)) ];
      let forward_provider = Pubgrub.to_provider forward in
      let reverse_provider = Pubgrub.to_provider reverse in
      match (
        forward_provider.choose_version "foo" Pubgrub.full,
        reverse_provider.choose_version "foo" Pubgrub.full
      ) with
      | Ok (Some left), Ok (Some right) ->
          assert_version_equal
            ~expected:left
            ~actual:right
            ~message:"Expected insertion order to preserve selected version"
      | _ -> Error "Expected both providers to choose a version")

let test_incompatibility_from_dependency_nonempty =
  Test.case "Incompatibility: from_dependency encodes parent and dependency terms"
    (fun _ctx ->
      let dep_ranges = Pubgrub.between (v 1 0 0) (v 3 0 0) in
      let incompat = Pubgrub.Incompatibility.from_dependency "root" (v 1 0 0) ("dep", dep_ranges) in
      let terms = Pubgrub.Incompatibility.terms incompat in
      if not (Int.equal (List.length terms) 2) then
        Error "Expected two terms for nonempty dependency incompatibility"
      else
        match Pubgrub.Incompatibility.as_dependency incompat with
        | Some ("root", "dep") -> (
            match assert_term_incompat
              ~package:"root"
              ~positive:true
              ~ranges:(Pubgrub.singleton (v 1 0 0))
              incompat with
            | Error _ as err -> err
            | Ok () ->
                assert_term_incompat
                  ~package:"dep"
                  ~positive:false
                  ~ranges:dep_ranges
                  incompat
          )
        | _ -> Error "Expected dependency incompatibility to round-trip through as_dependency")

let test_incompatibility_from_dependency_empty =
  Test.case "Incompatibility: from_dependency drops empty dependency terms"
    (fun _ctx ->
      let incompat = Pubgrub.Incompatibility.from_dependency "root" (v 1 0 0) ("dep", Pubgrub.empty) in
      if Int.equal (List.length (Pubgrub.Incompatibility.terms incompat)) 1 then
        assert_term_incompat
          ~package:"root"
          ~positive:true
          ~ranges:(Pubgrub.singleton (v 1 0 0))
          incompat
      else
        Error "Expected empty dependency range to produce only the parent term")

let test_incompatibility_no_versions_preserves_requested_range =
  Test.case "Incompatibility: no_versions preserves the requested range"
    (fun _ctx ->
      let requested = Pubgrub.between (v 2 0 0) (v 4 0 0) in
      let incompat = Pubgrub.Incompatibility.no_versions "foo" requested in
      if not (Int.equal (List.length (Pubgrub.Incompatibility.terms incompat)) 1) then
        Error "Expected no_versions incompatibility to keep a single requested term"
      else
        match Pubgrub.Incompatibility.as_dependency incompat with
        | None ->
            assert_term_incompat
              ~package:"foo"
              ~positive:true
              ~ranges:requested
              incompat
        | Some _ -> Error "Expected no_versions incompatibility not to look like a dependency")

let test_incompatibility_as_dependency_only_for_dependencies =
  Test.case "Incompatibility: as_dependency only recognizes dependency causes"
    (fun _ctx ->
      let dependency = Pubgrub.Incompatibility.from_dependency "root" (v 1 0 0) ("dep", Pubgrub.full) in
      let no_versions = Pubgrub.Incompatibility.no_versions "dep" Pubgrub.full in
      match (Pubgrub.Incompatibility.as_dependency dependency, Pubgrub.Incompatibility.as_dependency no_versions) with
      | Some ("root", "dep"), None -> Ok ()
      | _ -> Error "Expected only dependency incompatibilities to round-trip via as_dependency")

let test_incompatibility_merge_dependents_matching =
  Test.case "Incompatibility: merge_dependents merges matching dependency causes"
    (fun _ctx ->
      let dep_ranges = Pubgrub.between (v 1 0 0) (v 3 0 0) in
      let left = Pubgrub.Incompatibility.from_dependency "root" (v 1 0 0) ("dep", dep_ranges) in
      let right = Pubgrub.Incompatibility.from_dependency "root" (v 2 0 0) ("dep", dep_ranges) in
      match Pubgrub.Incompatibility.merge_dependents left right with
      | Some merged -> (
          match assert_term_incompat
            ~package:"root"
            ~positive:false
            ~ranges:(Pubgrub.Ranges.union
              ~compare_v:Pubgrub.version_compare
              (Pubgrub.singleton (v 1 0 0))
              (Pubgrub.singleton (v 2 0 0)))
            merged with
          | Error _ as err -> err
          | Ok () ->
              assert_term_incompat
                ~package:"dep"
                ~positive:true
                ~ranges:dep_ranges
                merged
        )
      | None -> Error "Expected matching dependency causes to merge")

let test_incompatibility_merge_dependents_different_ranges =
  Test.case "Incompatibility: merge_dependents keeps distinct dependency ranges separate"
    (fun _ctx ->
      let left = Pubgrub.Incompatibility.from_dependency
        "root"
        (v 1 0 0)
        ("dep", Pubgrub.between (v 1 0 0) (v 2 0 0)) in
      let right = Pubgrub.Incompatibility.from_dependency
        "root"
        (v 2 0 0)
        ("dep", Pubgrub.between (v 2 0 0) (v 3 0 0)) in
      match Pubgrub.Incompatibility.merge_dependents left right with
      | None -> Ok ()
      | Some _ -> Error "Expected different dependency ranges not to merge")

let test_incompatibility_prior_cause_merges_and_canonicalizes =
  Test.case "Incompatibility: prior_cause merges target terms and canonicalizes duplicates"
    (fun _ctx ->
      let left = custom_incompat
        [
          Pubgrub.Term.positive "shared" (Pubgrub.between (v 1 0 0) (v 3 0 0));
          Pubgrub.Term.positive "left" (Pubgrub.singleton (v 1 0 0));
          Pubgrub.Term.positive "side" (Pubgrub.between (v 1 0 0) (v 4 0 0));
        ] in
      let right = custom_incompat
        [
          Pubgrub.Term.positive "shared" (Pubgrub.between (v 2 0 0) (v 5 0 0));
          Pubgrub.Term.positive "right" (Pubgrub.singleton (v 2 0 0));
          Pubgrub.Term.positive "side" (Pubgrub.between (v 2 0 0) (v 3 0 0));
        ] in
      let prior = Pubgrub.Incompatibility.prior_cause
        ~extra_term:(Pubgrub.Term.negative "extra" (Pubgrub.singleton (v 9 0 0)))
        left
        right
        "shared" in
      let terms = Pubgrub.Incompatibility.terms prior in
      if not (Int.equal (List.length terms) 5) then
        Error "Expected prior_cause to leave one canonical term per package"
      else
        match assert_term_incompat
          ~package:"shared"
          ~positive:true
          ~ranges:(Pubgrub.between (v 1 0 0) (v 5 0 0))
          prior with
        | Error _ as err -> err
        | Ok () -> (
            match assert_term_incompat
              ~package:"side"
              ~positive:true
              ~ranges:(Pubgrub.between (v 2 0 0) (v 3 0 0))
              prior with
            | Error _ as err -> err
            | Ok () -> (
                match assert_term_incompat
                  ~package:"left"
                  ~positive:true
                  ~ranges:(Pubgrub.singleton (v 1 0 0))
                  prior with
                | Error _ as err -> err
                | Ok () -> (
                    match assert_term_incompat
                      ~package:"right"
                      ~positive:true
                      ~ranges:(Pubgrub.singleton (v 2 0 0))
                      prior with
                    | Error _ as err -> err
                    | Ok () ->
                        assert_term_incompat
                          ~package:"extra"
                          ~positive:false
                          ~ranges:(Pubgrub.singleton (v 9 0 0))
                          prior
                  )
              )
          )
    )

let test_incompatibility_not_root_is_terminal_for_root =
  Test.case "Incompatibility: not_root is terminal for the selected root"
    (fun _ctx ->
      let incompat = Pubgrub.Incompatibility.not_root "root" (v 1 0 0) in
      if Pubgrub.Incompatibility.is_terminal incompat "root" (v 1 0 0) then
        Ok ()
      else
        Error "Expected not_root incompatibility to be terminal for the root decision")

let test_relation_satisfied_when_all_terms_met =
  Test.case "Partial_solution: relation is satisfied when all terms are met"
    (fun _ctx ->
      let solution = Pubgrub.Partial_solution.empty ()
        |> fun solution -> Pubgrub.Partial_solution.add_decision solution "foo" (v 1 0 0)
        |> fun solution -> Pubgrub.Partial_solution.add_decision solution "bar" (v 2 0 0) in
      let incompat = custom_incompat
        [
          Pubgrub.Term.positive "foo" (Pubgrub.singleton (v 1 0 0));
          Pubgrub.Term.negative "bar" (Pubgrub.singleton (v 1 0 0));
        ] in
      assert_relation `Satisfied (Pubgrub.Partial_solution.relation solution incompat))

let test_relation_almost_satisfied_with_one_undecided =
  Test.case "Partial_solution: relation is almost satisfied with one undecided package"
    (fun _ctx ->
      let solution = Pubgrub.Partial_solution.empty ()
        |> fun solution -> Pubgrub.Partial_solution.add_decision solution "foo" (v 1 0 0) in
      let incompat = custom_incompat
        [
          Pubgrub.Term.positive "foo" (Pubgrub.singleton (v 1 0 0));
          Pubgrub.Term.positive "bar" (Pubgrub.singleton (v 2 0 0));
        ] in
      assert_relation
        (`AlmostSatisfied "bar")
        (Pubgrub.Partial_solution.relation solution incompat))

let test_relation_contradicted_by_decision =
  Test.case "Partial_solution: relation is contradicted by a decided version"
    (fun _ctx ->
      let solution = Pubgrub.Partial_solution.empty ()
        |> fun solution -> Pubgrub.Partial_solution.add_decision solution "foo" (v 1 0 0) in
      let incompat = custom_incompat
        [ Pubgrub.Term.positive "foo" (Pubgrub.singleton (v 2 0 0)) ] in
      assert_relation
        (`Contradicted "foo")
        (Pubgrub.Partial_solution.relation solution incompat))

let test_relation_constrained_positive_subset_is_satisfied =
  Test.case "Partial_solution: relation treats constrained positive subsets as satisfied"
    (fun _ctx ->
      let constraint_incompat = custom_incompat
        [ Pubgrub.Term.negative "foo" (Pubgrub.between (v 2 0 0) (v 3 0 0)) ] in
      let solution = Pubgrub.Partial_solution.empty ()
        |> fun solution -> Pubgrub.Partial_solution.add_derivation solution "foo" constraint_incompat in
      let incompat = custom_incompat
        [ Pubgrub.Term.positive "foo" (Pubgrub.between (v 1 0 0) (v 4 0 0)) ] in
      assert_relation `Satisfied (Pubgrub.Partial_solution.relation solution incompat))

let test_relation_constrained_positive_overlap_is_almost_satisfied =
  Test.case "Partial_solution: relation narrows overlapping constrained positives"
    (fun _ctx ->
      let constraint_incompat = custom_incompat
        [ Pubgrub.Term.negative "foo" (Pubgrub.between (v 2 0 0) (v 4 0 0)) ] in
      let solution = Pubgrub.Partial_solution.empty ()
        |> fun solution -> Pubgrub.Partial_solution.add_derivation solution "foo" constraint_incompat in
      let incompat = custom_incompat
        [ Pubgrub.Term.positive "foo" (Pubgrub.between (v 3 0 0) (v 5 0 0)) ] in
      assert_relation
        (`AlmostSatisfied "foo")
        (Pubgrub.Partial_solution.relation solution incompat))

let test_relation_constrained_negative_disjoint_is_satisfied =
  Test.case "Partial_solution: relation treats constrained negative disjointness as satisfied"
    (fun _ctx ->
      let constraint_incompat = custom_incompat
        [ Pubgrub.Term.negative "foo" (Pubgrub.between (v 1 0 0) (v 2 0 0)) ] in
      let solution = Pubgrub.Partial_solution.empty ()
        |> fun solution -> Pubgrub.Partial_solution.add_derivation solution "foo" constraint_incompat in
      let incompat = custom_incompat
        [ Pubgrub.Term.negative "foo" (Pubgrub.between (v 2 0 0) (v 3 0 0)) ] in
      assert_relation `Satisfied (Pubgrub.Partial_solution.relation solution incompat))

let test_relation_constrained_negative_subset_is_contradicted =
  Test.case "Partial_solution: relation treats constrained negative subsets as contradicted"
    (fun _ctx ->
      let constraint_incompat = custom_incompat
        [ Pubgrub.Term.negative "foo" (Pubgrub.between (v 2 0 0) (v 3 0 0)) ] in
      let solution = Pubgrub.Partial_solution.empty ()
        |> fun solution -> Pubgrub.Partial_solution.add_derivation solution "foo" constraint_incompat in
      let incompat = custom_incompat
        [ Pubgrub.Term.negative "foo" (Pubgrub.between (v 2 0 0) (v 4 0 0)) ] in
      assert_relation
        (`Contradicted "foo")
        (Pubgrub.Partial_solution.relation solution incompat))

let test_partial_solution_cached_constraints_intersect_derivations =
  Test.case "Partial_solution: cached constraints intersect derivations"
    (fun _ctx ->
      let left = custom_incompat
        [ Pubgrub.Term.negative "foo" (Pubgrub.between (v 1 0 0) (v 4 0 0)) ] in
      let right = custom_incompat
        [ Pubgrub.Term.negative "foo" (Pubgrub.between (v 2 0 0) (v 5 0 0)) ] in
      let solution = Pubgrub.Partial_solution.empty ()
        |> fun solution -> Pubgrub.Partial_solution.add_derivation solution "foo" left
        |> fun solution -> Pubgrub.Partial_solution.add_derivation solution "foo" right in
      assert_constraint
        ~expected:(`Constrained (Pubgrub.between (v 2 0 0) (v 4 0 0)))
        (Pubgrub.Partial_solution.get_constraint solution "foo"))

let test_partial_solution_backtrack_restores_cached_derivation =
  Test.case "Partial_solution: backtrack restores cached derivations"
    (fun _ctx ->
      let constraint_incompat = custom_incompat
        [ Pubgrub.Term.negative "foo" (Pubgrub.between (v 2 0 0) (v 4 0 0)) ] in
      let solution = Pubgrub.Partial_solution.empty ()
        |> fun solution -> Pubgrub.Partial_solution.add_derivation solution "foo" constraint_incompat
        |> fun solution -> Pubgrub.Partial_solution.add_decision solution "foo" (v 2 1 0) in
      let backtracked = Pubgrub.Partial_solution.backtrack solution 0 in
      assert_constraint
        ~expected:(`Constrained (Pubgrub.between (v 2 0 0) (v 4 0 0)))
        (Pubgrub.Partial_solution.get_constraint backtracked "foo"))

let test_partial_solution_missing_derivation_package_raises =
  Test.case "Partial_solution: add_derivation requires matching package term"
    (fun _ctx ->
      let solution = Pubgrub.Partial_solution.empty () in
      let incompat = Pubgrub.Incompatibility.no_versions "foo" Pubgrub.full in
      assert_raises
        "Expected add_derivation to raise when the package is absent from the incompatibility"
        (fun () -> Pubgrub.Partial_solution.add_derivation solution "bar" incompat))

let test_solution_order_is_deterministic =
  Test.case "Solve: returned solution order is deterministic"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("zeta", Pubgrub.full); ("alpha", Pubgrub.full) ];
      Pubgrub.add_package provider "zeta" (v 1 0 0) [];
      Pubgrub.add_package provider "alpha" (v 1 0 0) [];
      assert_solution_packages
        [ "alpha"; "root"; "zeta" ]
        (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_report_no_versions_includes_requested_range =
  Test.case "Report: no_versions explanation includes requested range"
    (fun ctx ->
      let incompat = Pubgrub.Incompatibility.no_versions
        "foo"
        (Pubgrub.between (v 1 0 0) (v 2 0 0)) in
      assert_inline_text
        ~ctx
        ~expected:
          "Conflict:\nno versions of foo match [1.0.0, 2.0.0).\n\nTherefore, version solving failed."
        ~actual:(Pubgrub.Report.explain_conflict incompat))

let test_report_from_dependency_includes_dependency_range =
  Test.case "Report: dependency explanation includes dependency range"
    (fun ctx ->
      let incompat = Pubgrub.Incompatibility.from_dependency
        "root"
        (v 1 0 0)
        ("foo", Pubgrub.between (v 1 0 0) (v 2 0 0)) in
      assert_inline_text
        ~ctx
        ~expected:
          "Conflict:\nroot@1.0.0 depends on foo in [1.0.0, 2.0.0).\n\nTherefore, version solving failed."
        ~actual:(Pubgrub.Report.explain_conflict incompat))

let test_report_derived_explanations_are_readable =
  Test.case "Report: derived explanations stay readable"
    (fun ctx ->
      let dep = Pubgrub.Incompatibility.from_dependency
        "root"
        (v 1 0 0)
        ("foo", Pubgrub.between (v 1 0 0) (v 2 0 0)) in
      let no_versions = Pubgrub.Incompatibility.no_versions
        "foo"
        (Pubgrub.between (v 1 0 0) (v 2 0 0)) in
      let incompat = Pubgrub.Incompatibility.create_derived
        [ Pubgrub.Term.positive "root" (Pubgrub.singleton (v 1 0 0)) ]
        dep
        no_versions
        None in
      assert_inline_text
        ~ctx
        ~expected:
          "Conflict:\nBecause:\n  root@1.0.0 depends on foo in [1.0.0, 2.0.0).\nAnd because:\n  no versions of foo match [1.0.0, 2.0.0).\nSo root in [1.0.0, 1.0.0].\n\nTherefore, version solving failed."
        ~actual:(Pubgrub.Report.explain_conflict incompat))

let test_solver_stats_expose_structured_counters =
  Test.case "Solver: solve_with_stats exposes structured counters"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full) ];
      Pubgrub.add_package provider "foo" (v 1 0 0) [];
      let outcome = Pubgrub.solve_with_stats (Pubgrub.to_provider provider) "root" (v 1 0 0) in
      match outcome.result with
      | Ok (Pubgrub.Solver.Success solution) -> (
          match assert_int_equal ~expected:2 ~actual:(List.length solution) ~message:"Unexpected package count" with
          | Error _ as err -> err
          | Ok () -> (
              match assert_int_equal ~expected:2 ~actual:outcome.stats.iterations ~message:"Unexpected iteration count" with
              | Error _ as err -> err
              | Ok () -> (
                  match assert_int_equal ~expected:2 ~actual:outcome.stats.decisions ~message:"Unexpected decision count" with
                  | Error _ as err -> err
                  | Ok () -> (
                      match assert_int_equal ~expected:1 ~actual:outcome.stats.derivations ~message:"Unexpected derivation count" with
                      | Error _ as err -> err
                      | Ok () -> (
                          match
                            assert_int_equal
                              ~expected:0
                              ~actual:outcome.stats.conflicts
                              ~message:"Unexpected conflict count"
                          with
                          | Error _ as err -> err
                          | Ok () -> (
                              match
                                assert_int_equal
                                  ~expected:1
                                  ~actual:outcome.stats.provider_choose_version_calls
                                  ~message:"Unexpected choose_version call count"
                              with
                              | Error _ as err -> err
                              | Ok () -> (
                                  match
                                    assert_int_equal
                                      ~expected:1
                                      ~actual:outcome.stats.provider_count_versions_calls
                                      ~message:"Unexpected count_versions call count"
                                  with
                                  | Error _ as err -> err
                                  | Ok () -> (
                                      match
                                        assert_int_equal
                                          ~expected:2
                                          ~actual:outcome.stats.provider_get_dependencies_calls
                                          ~message:"Unexpected get_dependencies call count"
                                      with
                                      | Error _ as err -> err
                                      | Ok () -> (
                                          match
                                            assert_int_equal
                                              ~expected:4
                                              ~actual:outcome.stats.provider_calls
                                              ~message:"Unexpected aggregate provider call count"
                                          with
                                          | Error _ as err -> err
                                          | Ok () ->
                                              assert_int_equal
                                                ~expected:2
                                                ~actual:outcome.stats.max_decision_depth
                                                ~message:"Unexpected max decision depth"
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
      | Ok (Pubgrub.Solver.Failure incompat) ->
          Error ("Expected success but got failure: " ^ Pubgrub.Report.explain_conflict incompat)
      | Error err ->
          Error ("Unexpected error: " ^ err))

let test_solver_options_control_iteration_limit =
  Test.case "Solver: max_iterations option is configurable"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full) ];
      Pubgrub.add_package provider "foo" (v 1 0 0) [];
      let options = { Pubgrub.default_options with max_iterations = 0 } in
      match Pubgrub.solve ~options (Pubgrub.to_provider provider) "root" (v 1 0 0) with
      | Error msg when String.equal msg "Too many iterations - likely infinite loop" -> Ok ()
      | Error msg -> Error ("Unexpected solver error: " ^ msg)
      | Ok (Pubgrub.Solver.Success _) ->
          Error "Expected iteration-limited solve to fail"
      | Ok (Pubgrub.Solver.Failure incompat) ->
          Error ("Expected iteration limit error but got conflict: " ^ Pubgrub.Report.explain_conflict incompat))

let test_conflicting_root_constraints_fail =
  Test.case "Solve: conflicting root constraints fail"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package
        provider
        "root"
        (v 1 0 0)
        [
          ("foo", Pubgrub.singleton (v 1 0 0));
          ("foo", Pubgrub.singleton (v 2 0 0));
        ];
      Pubgrub.add_package provider "foo" (v 1 0 0) [];
      Pubgrub.add_package provider "foo" (v 2 0 0) [];
      assert_conflict (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_derivations_are_created =
  Test.case "Derivations are created"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full) ];
      Pubgrub.add_package provider "foo" (v 1 0 0) [];
      assert_solution 2 (Pubgrub.Solver.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_empty_root =
  Test.case "Empty root package"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [];
      assert_solution 1 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_single_dependency =
  Test.case "Single direct dependency"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full) ];
      Pubgrub.add_package provider "foo" (v 1 0 0) [];
      assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_two_dependencies =
  Test.case "Two independent dependencies"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full); ("bar", Pubgrub.full) ];
      Pubgrub.add_package provider "foo" (v 1 0 0) [];
      Pubgrub.add_package provider "bar" (v 1 0 0) [];
      assert_solution 3 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_transitive_chain =
  Test.case "Transitive dependency chain (3 levels)"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("a", Pubgrub.full) ];
      Pubgrub.add_package provider "a" (v 1 0 0) [ ("b", Pubgrub.full) ];
      Pubgrub.add_package provider "b" (v 1 0 0) [ ("c", Pubgrub.full) ];
      Pubgrub.add_package provider "c" (v 1 0 0) [];
      assert_solution 4 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_deep_chain =
  Test.case "Deep dependency chain (5 levels)"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("a", Pubgrub.full) ];
      Pubgrub.add_package provider "a" (v 1 0 0) [ ("b", Pubgrub.full) ];
      Pubgrub.add_package provider "b" (v 1 0 0) [ ("c", Pubgrub.full) ];
      Pubgrub.add_package provider "c" (v 1 0 0) [ ("d", Pubgrub.full) ];
      Pubgrub.add_package provider "d" (v 1 0 0) [ ("e", Pubgrub.full) ];
      Pubgrub.add_package provider "e" (v 1 0 0) [];
      assert_solution 6 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_diamond_dependency =
  Test.case "Diamond dependency"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("a", Pubgrub.full); ("b", Pubgrub.full) ];
      Pubgrub.add_package provider "a" (v 1 0 0) [ ("c", Pubgrub.full) ];
      Pubgrub.add_package provider "b" (v 1 0 0) [ ("c", Pubgrub.full) ];
      Pubgrub.add_package provider "c" (v 1 0 0) [];
      assert_solution 4 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_multiple_versions_picks_latest =
  Test.case "Multiple versions available - picks latest"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full) ];
      Pubgrub.add_package provider "foo" (v 1 0 0) [];
      Pubgrub.add_package provider "foo" (v 1 5 0) [];
      Pubgrub.add_package provider "foo" (v 2 0 0) [];
      Pubgrub.add_package provider "foo" (v 2 1 0) [];
      assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_version_constraint_lower =
  Test.case "Version constraint: >= 2.0.0"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.higher_than (v 2 0 0)) ];
      Pubgrub.add_package provider "foo" (v 1 0 0) [];
      Pubgrub.add_package provider "foo" (v 2 0 0) [];
      Pubgrub.add_package provider "foo" (v 3 0 0) [];
      assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_version_constraint_upper =
  Test.case "Version constraint: < 2.0.0"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package
        provider
        "root"
        (v 1 0 0)
        [ ("foo", Pubgrub.strictly_lower_than (v 2 0 0)) ];
      Pubgrub.add_package provider "foo" (v 1 0 0) [];
      Pubgrub.add_package provider "foo" (v 1 5 0) [];
      Pubgrub.add_package provider "foo" (v 2 0 0) [];
      assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_version_range =
  Test.case "Version range: >= 1.0.0 and < 2.0.0"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.between (v 1 0 0) (v 2 0 0)) ];
      Pubgrub.add_package provider "foo" (v 0 9 0) [];
      Pubgrub.add_package provider "foo" (v 1 0 0) [];
      Pubgrub.add_package provider "foo" (v 1 5 0) [];
      Pubgrub.add_package provider "foo" (v 2 0 0) [];
      assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_wide_tree =
  Test.case "Wide dependency tree (5 deps)"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package
        provider
        "root"
        (v 1 0 0)
        [
          ("a", Pubgrub.full);
          ("b", Pubgrub.full);
          ("c", Pubgrub.full);
          ("d", Pubgrub.full);
          ("e", Pubgrub.full);
        ];
      Pubgrub.add_package provider "a" (v 1 0 0) [];
      Pubgrub.add_package provider "b" (v 1 0 0) [];
      Pubgrub.add_package provider "c" (v 1 0 0) [];
      Pubgrub.add_package provider "d" (v 1 0 0) [];
      Pubgrub.add_package provider "e" (v 1 0 0) [];
      assert_solution 6 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_nested_diamonds =
  Test.case "Nested diamond dependencies"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("a", Pubgrub.full); ("b", Pubgrub.full) ];
      Pubgrub.add_package provider "a" (v 1 0 0) [ ("c", Pubgrub.full); ("d", Pubgrub.full) ];
      Pubgrub.add_package provider "b" (v 1 0 0) [ ("c", Pubgrub.full); ("d", Pubgrub.full) ];
      Pubgrub.add_package provider "c" (v 1 0 0) [ ("e", Pubgrub.full) ];
      Pubgrub.add_package provider "d" (v 1 0 0) [ ("e", Pubgrub.full) ];
      Pubgrub.add_package provider "e" (v 1 0 0) [];
      assert_solution 6 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_complex_graph =
  Test.case "Complex dependency graph"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("a", Pubgrub.full); ("b", Pubgrub.full) ];
      Pubgrub.add_package provider "a" (v 1 0 0) [ ("c", Pubgrub.full); ("d", Pubgrub.full) ];
      Pubgrub.add_package provider "b" (v 1 0 0) [ ("d", Pubgrub.full); ("e", Pubgrub.full) ];
      Pubgrub.add_package provider "c" (v 1 0 0) [ ("f", Pubgrub.full) ];
      Pubgrub.add_package provider "d" (v 1 0 0) [ ("f", Pubgrub.full); ("g", Pubgrub.full) ];
      Pubgrub.add_package provider "e" (v 1 0 0) [ ("g", Pubgrub.full) ];
      Pubgrub.add_package provider "f" (v 1 0 0) [];
      Pubgrub.add_package provider "g" (v 1 0 0) [];
      assert_solution 8 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_many_versions =
  Test.case "Package with many versions (10)"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full) ];
      for i = 0 to 9 do
        Pubgrub.add_package provider "foo" (v 1 i 0) []
      done;
      assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_semantic_versions =
  Test.case "Semantic version ordering"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full) ];
      Pubgrub.add_package provider "foo" (v 0 1 0) [];
      Pubgrub.add_package provider "foo" (v 0 9 0) [];
      Pubgrub.add_package provider "foo" (v 1 0 0) [];
      Pubgrub.add_package provider "foo" (v 1 0 1) [];
      Pubgrub.add_package provider "foo" (v 1 1 0) [];
      Pubgrub.add_package provider "foo" (v 2 0 0) [];
      assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_transitive_constraints =
  Test.case "Transitive version constraints"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("a", Pubgrub.full) ];
      Pubgrub.add_package provider "a" (v 1 0 0) [ ("b", Pubgrub.higher_than (v 2 0 0)) ];
      Pubgrub.add_package provider "b" (v 1 0 0) [];
      Pubgrub.add_package provider "b" (v 2 0 0) [];
      Pubgrub.add_package provider "b" (v 3 0 0) [];
      assert_solution 3 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_larger_graph =
  Test.case "Larger dependency graph (15 packages)"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package
        provider
        "root"
        (v 1 0 0)
        [ ("a", Pubgrub.full); ("b", Pubgrub.full); ("c", Pubgrub.full) ];
      Pubgrub.add_package provider "a" (v 1 0 0) [ ("d", Pubgrub.full); ("e", Pubgrub.full) ];
      Pubgrub.add_package provider "b" (v 1 0 0) [ ("f", Pubgrub.full); ("g", Pubgrub.full) ];
      Pubgrub.add_package provider "c" (v 1 0 0) [ ("h", Pubgrub.full); ("i", Pubgrub.full) ];
      Pubgrub.add_package provider "d" (v 1 0 0) [ ("j", Pubgrub.full) ];
      Pubgrub.add_package provider "e" (v 1 0 0) [ ("k", Pubgrub.full) ];
      Pubgrub.add_package provider "f" (v 1 0 0) [ ("l", Pubgrub.full) ];
      Pubgrub.add_package provider "g" (v 1 0 0) [ ("m", Pubgrub.full) ];
      Pubgrub.add_package provider "h" (v 1 0 0) [ ("n", Pubgrub.full) ];
      Pubgrub.add_package provider "i" (v 1 0 0) [];
      Pubgrub.add_package provider "j" (v 1 0 0) [];
      Pubgrub.add_package provider "k" (v 1 0 0) [];
      Pubgrub.add_package provider "l" (v 1 0 0) [];
      Pubgrub.add_package provider "m" (v 1 0 0) [];
      Pubgrub.add_package provider "n" (v 1 0 0) [];
      assert_solution 15 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_version_selection_strategy =
  Test.case "Version selection with many options"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full) ];
      for major = 0 to 2 do
        for minor = 0 to 3 do
          for patch = 0 to 2 do
            Pubgrub.add_package provider "foo" (v major minor patch) []
          done
        done
      done;
      assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_very_deep_chain =
  Test.case "Very deep dependency chain (10 levels)"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("a", Pubgrub.full) ];
      Pubgrub.add_package provider "a" (v 1 0 0) [ ("b", Pubgrub.full) ];
      Pubgrub.add_package provider "b" (v 1 0 0) [ ("c", Pubgrub.full) ];
      Pubgrub.add_package provider "c" (v 1 0 0) [ ("d", Pubgrub.full) ];
      Pubgrub.add_package provider "d" (v 1 0 0) [ ("e", Pubgrub.full) ];
      Pubgrub.add_package provider "e" (v 1 0 0) [ ("f", Pubgrub.full) ];
      Pubgrub.add_package provider "f" (v 1 0 0) [ ("g", Pubgrub.full) ];
      Pubgrub.add_package provider "g" (v 1 0 0) [ ("h", Pubgrub.full) ];
      Pubgrub.add_package provider "h" (v 1 0 0) [ ("i", Pubgrub.full) ];
      Pubgrub.add_package provider "i" (v 1 0 0) [];
      assert_solution 10 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_very_wide_tree =
  Test.case "Very wide dependency tree (10 deps)"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0)
        [
          ("a", Pubgrub.full);
          ("b", Pubgrub.full);
          ("c", Pubgrub.full);
          ("d", Pubgrub.full);
          ("e", Pubgrub.full);
          ("f", Pubgrub.full);
          ("g", Pubgrub.full);
          ("h", Pubgrub.full);
          ("i", Pubgrub.full);
          ("j", Pubgrub.full);
        ];
      Pubgrub.add_package provider "a" (v 1 0 0) [];
      Pubgrub.add_package provider "b" (v 1 0 0) [];
      Pubgrub.add_package provider "c" (v 1 0 0) [];
      Pubgrub.add_package provider "d" (v 1 0 0) [];
      Pubgrub.add_package provider "e" (v 1 0 0) [];
      Pubgrub.add_package provider "f" (v 1 0 0) [];
      Pubgrub.add_package provider "g" (v 1 0 0) [];
      Pubgrub.add_package provider "h" (v 1 0 0) [];
      Pubgrub.add_package provider "i" (v 1 0 0) [];
      Pubgrub.add_package provider "j" (v 1 0 0) [];
      assert_solution 11 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_triple_diamond =
  Test.case "Triple diamond pattern"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("a", Pubgrub.full); ("b", Pubgrub.full) ];
      Pubgrub.add_package provider "a" (v 1 0 0) [ ("c", Pubgrub.full); ("d", Pubgrub.full) ];
      Pubgrub.add_package provider "b" (v 1 0 0) [ ("c", Pubgrub.full); ("d", Pubgrub.full) ];
      Pubgrub.add_package provider "c" (v 1 0 0) [ ("e", Pubgrub.full); ("f", Pubgrub.full) ];
      Pubgrub.add_package provider "d" (v 1 0 0) [ ("e", Pubgrub.full); ("f", Pubgrub.full) ];
      Pubgrub.add_package provider "e" (v 1 0 0) [ ("g", Pubgrub.full) ];
      Pubgrub.add_package provider "f" (v 1 0 0) [ ("g", Pubgrub.full) ];
      Pubgrub.add_package provider "g" (v 1 0 0) [];
      assert_solution 8 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_many_versions_20 =
  Test.case "Package with 20 versions"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full) ];
      for i = 0 to 19 do
        Pubgrub.add_package provider "foo" (v 1 i 0) []
      done;
      assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_many_versions_50 =
  Test.case "Package with 50 versions"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full) ];
      for i = 0 to 49 do
        Pubgrub.add_package provider "foo" (v 1 i 0) []
      done;
      assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_constraint_range_narrow =
  Test.case "Narrow version range constraint"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.between (v 1 5 0) (v 1 7 0)) ];
      for i = 0 to 10 do
        Pubgrub.add_package provider "foo" (v 1 i 0) []
      done;
      assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_patch_versions =
  Test.case "Patch version selection"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full) ];
      for i = 0 to 10 do
        Pubgrub.add_package provider "foo" (v 1 0 i) []
      done;
      assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_branching_graph =
  Test.case "Branching dependency graph"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("a", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "a"
        (v 1 0 0)
        [ ("b", Pubgrub.full); ("c", Pubgrub.full); ("d", Pubgrub.full) ];
      Pubgrub.add_package provider "b" (v 1 0 0) [ ("e", Pubgrub.full) ];
      Pubgrub.add_package provider "c" (v 1 0 0) [ ("f", Pubgrub.full) ];
      Pubgrub.add_package provider "d" (v 1 0 0) [ ("g", Pubgrub.full) ];
      Pubgrub.add_package provider "e" (v 1 0 0) [];
      Pubgrub.add_package provider "f" (v 1 0 0) [];
      Pubgrub.add_package provider "g" (v 1 0 0) [];
      assert_solution 8 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_deep_and_wide =
  Test.case "Deep and wide combined (20 packages)"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package
        provider
        "root"
        (v 1 0 0)
        [ ("a", Pubgrub.full); ("b", Pubgrub.full); ("c", Pubgrub.full); ("d", Pubgrub.full); ];
      Pubgrub.add_package provider "a" (v 1 0 0) [ ("e", Pubgrub.full); ("f", Pubgrub.full) ];
      Pubgrub.add_package provider "b" (v 1 0 0) [ ("g", Pubgrub.full); ("h", Pubgrub.full) ];
      Pubgrub.add_package provider "c" (v 1 0 0) [ ("i", Pubgrub.full); ("j", Pubgrub.full) ];
      Pubgrub.add_package provider "d" (v 1 0 0) [ ("k", Pubgrub.full); ("l", Pubgrub.full) ];
      Pubgrub.add_package provider "e" (v 1 0 0) [ ("m", Pubgrub.full) ];
      Pubgrub.add_package provider "f" (v 1 0 0) [ ("n", Pubgrub.full) ];
      Pubgrub.add_package provider "g" (v 1 0 0) [ ("o", Pubgrub.full) ];
      Pubgrub.add_package provider "h" (v 1 0 0) [ ("p", Pubgrub.full) ];
      Pubgrub.add_package provider "i" (v 1 0 0) [ ("q", Pubgrub.full) ];
      Pubgrub.add_package provider "j" (v 1 0 0) [ ("r", Pubgrub.full) ];
      Pubgrub.add_package provider "k" (v 1 0 0) [ ("s", Pubgrub.full) ];
      Pubgrub.add_package provider "l" (v 1 0 0) [ ("t", Pubgrub.full) ];
      Pubgrub.add_package provider "m" (v 1 0 0) [];
      Pubgrub.add_package provider "n" (v 1 0 0) [];
      Pubgrub.add_package provider "o" (v 1 0 0) [];
      Pubgrub.add_package provider "p" (v 1 0 0) [];
      Pubgrub.add_package provider "q" (v 1 0 0) [];
      Pubgrub.add_package provider "r" (v 1 0 0) [];
      Pubgrub.add_package provider "s" (v 1 0 0) [];
      Pubgrub.add_package provider "t" (v 1 0 0) [];
      assert_solution 21 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let generate_web_framework_tests = fun () ->
  let tests = [] in
  let tests = ref tests in
  for i = 1 to 25 do
    let test =
      Test.case ("Web framework scenario " ^ (Int.to_string i))
        (fun _ctx ->
          let provider = Pubgrub.create_offline () in
          Pubgrub.add_package
            provider
            "root"
            (v 1 0 0)
            [ ("http", Pubgrub.full); ("router", Pubgrub.full); ("middleware", Pubgrub.full); ];
          Pubgrub.add_package provider "http" (v (1 + (i mod 3)) 0 0) [ ("sockets", Pubgrub.full) ];
          Pubgrub.add_package provider "router" (v 1 (i mod 5) 0) [ ("path-parser", Pubgrub.full) ];
          Pubgrub.add_package provider "middleware" (v 2 0 0) [ ("logger", Pubgrub.full) ];
          Pubgrub.add_package provider "sockets" (v 1 0 0) [];
          Pubgrub.add_package provider "path-parser" (v 1 0 0) [];
          Pubgrub.add_package provider "logger" (v 1 0 0) [];
          assert_solution 7 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))
    in
    tests := test :: !tests
  done;
  List.rev !tests

let generate_database_tests = fun () ->
  let tests = [] in
  let tests = ref tests in
  for i = 1 to 25 do
    let test =
      Test.case ("Database scenario " ^ (Int.to_string i))
        (fun _ctx ->
          let provider = Pubgrub.create_offline () in
          Pubgrub.add_package
            provider
            "root"
            (v 1 0 0)
            [ ("db-driver", Pubgrub.full); ("migrations", Pubgrub.full); ("orm", Pubgrub.full); ];
          Pubgrub.add_package
            provider
            "db-driver"
            (v (1 + (i mod 4)) 0 0)
            [ ("connection-pool", Pubgrub.full) ];
          Pubgrub.add_package
            provider
            "migrations"
            (v 1 (i mod 6) 0)
            [ ("sql-parser", Pubgrub.full) ];
          Pubgrub.add_package provider "orm" (v 2 0 0) [ ("query-builder", Pubgrub.full) ];
          Pubgrub.add_package provider "connection-pool" (v 1 0 0) [];
          Pubgrub.add_package provider "sql-parser" (v 1 0 0) [];
          Pubgrub.add_package provider "query-builder" (v 1 0 0) [];
          assert_solution 7 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))
    in
    tests := test :: !tests
  done;
  List.rev !tests

let generate_compiler_tests = fun () ->
  let tests = [] in
  let tests = ref tests in
  for i = 1 to 22 do
    let test =
      Test.case ("Compiler toolchain scenario " ^ (Int.to_string i))
        (fun _ctx ->
          let provider = Pubgrub.create_offline () in
          Pubgrub.add_package
            provider
            "root"
            (v 1 0 0)
            [
              ("lexer", Pubgrub.full);
              ("parser", Pubgrub.full);
              ("codegen", Pubgrub.full);
              ("optimizer", Pubgrub.full);
            ];
          Pubgrub.add_package provider "lexer" (v (1 + (i mod 3)) 0 0) [ ("regex", Pubgrub.full) ];
          Pubgrub.add_package provider "parser" (v 1 (i mod 4) 0) [ ("ast", Pubgrub.full) ];
          Pubgrub.add_package provider "codegen" (v 2 0 0) [ ("llvm-bindings", Pubgrub.full) ];
          Pubgrub.add_package provider "optimizer" (v 1 0 0) [ ("analysis", Pubgrub.full) ];
          Pubgrub.add_package provider "regex" (v 1 0 0) [];
          Pubgrub.add_package provider "ast" (v 1 0 0) [];
          Pubgrub.add_package provider "llvm-bindings" (v 1 0 0) [];
          Pubgrub.add_package provider "analysis" (v 1 0 0) [];
          assert_solution 9 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))
    in
    tests := test :: !tests
  done;
  List.rev !tests

let test_large_graph_30_packages =
  Test.case "Large graph: 30 packages with mixed dependencies"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package
        provider
        "root"
        (v 1 0 0)
        [ ("a", Pubgrub.full); ("b", Pubgrub.full); ("c", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "a"
        (v 1 0 0)
        [ ("d", Pubgrub.full); ("e", Pubgrub.full); ("f", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "b"
        (v 1 0 0)
        [ ("g", Pubgrub.full); ("h", Pubgrub.full); ("i", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "c"
        (v 1 0 0)
        [ ("j", Pubgrub.full); ("k", Pubgrub.full); ("l", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "d"
        (v 1 0 0)
        [ ("m0", Pubgrub.full); ("n0", Pubgrub.full); ("o0", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "e"
        (v 1 0 0)
        [ ("m1", Pubgrub.full); ("n1", Pubgrub.full); ("o1", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "f"
        (v 1 0 0)
        [ ("m2", Pubgrub.full); ("n2", Pubgrub.full); ("o2", Pubgrub.full) ];
      List.iter
        (fun pkg ->
          Pubgrub.add_package provider pkg (v 1 0 0) [])
        [
          "g";
          "h";
          "i";
          "j";
          "k";
          "l";
          "m0";
          "n0";
          "o0";
          "m1";
          "n1";
          "o1";
          "m2";
          "n2";
          "o2";
        ];
      assert_solution 22 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_conflict_missing_dependency =
  Test.case "Conflict: missing dependency"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("nonexistent", Pubgrub.full) ];
      assert_conflict (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_balanced_tree =
  Test.case "Balanced binary tree of dependencies"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package
        provider
        "root"
        (v 1 0 0)
        [ ("l1-a", Pubgrub.full); ("l1-b", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "l1-a"
        (v 1 0 0)
        [ ("l2-a", Pubgrub.full); ("l2-b", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "l1-b"
        (v 1 0 0)
        [ ("l2-c", Pubgrub.full); ("l2-d", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "l2-a"
        (v 1 0 0)
        [ ("l3-a", Pubgrub.full); ("l3-b", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "l2-b"
        (v 1 0 0)
        [ ("l3-c", Pubgrub.full); ("l3-d", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "l2-c"
        (v 1 0 0)
        [ ("l3-e", Pubgrub.full); ("l3-f", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "l2-d"
        (v 1 0 0)
        [ ("l3-g", Pubgrub.full); ("l3-h", Pubgrub.full) ];
      List.iter
        (fun pkg ->
          Pubgrub.add_package provider pkg (v 1 0 0) [])
        [ "l3-a"; "l3-b"; "l3-c"; "l3-d"; "l3-e"; "l3-f"; "l3-g"; "l3-h" ];
      assert_solution 15 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_monorepo_structure =
  Test.case "Monorepo: multiple packages with shared deps"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package
        provider
        "root"
        (v 1 0 0)
        [ ("pkg-a", Pubgrub.full); ("pkg-b", Pubgrub.full); ("pkg-c", Pubgrub.full); ];
      Pubgrub.add_package
        provider
        "pkg-a"
        (v 1 0 0)
        [ ("shared-utils", Pubgrub.full); ("dep-a", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "pkg-b"
        (v 1 0 0)
        [ ("shared-utils", Pubgrub.full); ("dep-b", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "pkg-c"
        (v 1 0 0)
        [ ("shared-utils", Pubgrub.full); ("dep-c", Pubgrub.full) ];
      Pubgrub.add_package provider "shared-utils" (v 1 0 0) [ ("common", Pubgrub.full) ];
      Pubgrub.add_package provider "dep-a" (v 1 0 0) [];
      Pubgrub.add_package provider "dep-b" (v 1 0 0) [];
      Pubgrub.add_package provider "dep-c" (v 1 0 0) [];
      Pubgrub.add_package provider "common" (v 1 0 0) [];
      assert_solution 9 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_massive_graph_100_packages =
  Test.case "Massive graph: 100+ packages with complex dependencies"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      let deps = ref [] in
      for i = 0 to 9 do
        deps := (("dep-" ^ (Int.to_string i)), Pubgrub.full) :: !deps
      done;
      Pubgrub.add_package provider "root" (v 1 0 0) !deps;
      for i = 0 to 9 do
        let sub_deps = ref [] in
        for j = 0 to 9 do
          sub_deps := (("sub-" ^ (Int.to_string i) ^ "-" ^ (Int.to_string j)), Pubgrub.full) :: !sub_deps
        done;
        Pubgrub.add_package provider ("dep-" ^ (Int.to_string i)) (v 1 0 0) !sub_deps;
        for j = 0 to 9 do
          Pubgrub.add_package
            provider
            ("sub-" ^ (Int.to_string i) ^ "-" ^ (Int.to_string j))
            (v 1 0 0)
            []
        done
      done;
      assert_solution 111 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_massive_versions =
  Test.case "Massive versions: package with 100 versions"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("lib", Pubgrub.full) ];
      for i = 0 to 99 do
        Pubgrub.add_package provider "lib" (v 1 i 0) []
      done;
      assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_complex_constraint_web =
  Test.case "Complex constraints: realistic web stack"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package
        provider
        "root"
        (v 1 0 0)
        [
          ("http-server", Pubgrub.between (v 2 0 0) (v 3 0 0));
          ("database", Pubgrub.higher_than (v 1 5 0));
          ("cache", Pubgrub.full);
        ];
      for i = 0 to 5 do
        Pubgrub.add_package
          provider
          "http-server"
          (v 2 i 0)
          [ ("router", Pubgrub.between (v 1 0 0) (v 2 0 0)); ("middleware", Pubgrub.full); ]
      done;
      for i = 0 to 10 do
        Pubgrub.add_package provider "database" (v 1 i 0) [ ("connection-pool", Pubgrub.full) ]
      done;
      Pubgrub.add_package provider "cache" (v 1 0 0) [ ("redis-client", Pubgrub.full) ];
      for i = 0 to 3 do
        Pubgrub.add_package provider "router" (v 1 i 0) [ ("path-parser", Pubgrub.full) ]
      done;
      Pubgrub.add_package provider "middleware" (v 1 0 0) [ ("logger", Pubgrub.full) ];
      Pubgrub.add_package provider "connection-pool" (v 1 0 0) [];
      Pubgrub.add_package provider "redis-client" (v 1 0 0) [];
      Pubgrub.add_package provider "path-parser" (v 1 0 0) [];
      Pubgrub.add_package provider "logger" (v 1 0 0) [];
      assert_solution 10 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_deep_shared_dependency =
  Test.case "Deep graph with shared transitive dependencies"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package
        provider
        "root"
        (v 1 0 0)
        [ ("frontend", Pubgrub.full); ("backend", Pubgrub.full); ("shared", Pubgrub.full); ];
      Pubgrub.add_package
        provider
        "frontend"
        (v 1 0 0)
        [ ("ui-lib", Pubgrub.full); ("state", Pubgrub.full); ("shared-utils", Pubgrub.full); ];
      Pubgrub.add_package
        provider
        "backend"
        (v 1 0 0)
        [ ("api", Pubgrub.full); ("auth", Pubgrub.full); ("shared-utils", Pubgrub.full); ];
      Pubgrub.add_package
        provider
        "shared"
        (v 1 0 0)
        [ ("shared-utils", Pubgrub.full); ("types", Pubgrub.full) ];
      Pubgrub.add_package provider "ui-lib" (v 1 0 0) [ ("renderer", Pubgrub.full) ];
      Pubgrub.add_package provider "state" (v 1 0 0) [ ("store", Pubgrub.full) ];
      Pubgrub.add_package provider "api" (v 1 0 0) [ ("router", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "auth"
        (v 1 0 0)
        [ ("jwt", Pubgrub.full); ("crypto", Pubgrub.full) ];
      Pubgrub.add_package provider "shared-utils" (v 1 0 0) [ ("validation", Pubgrub.full) ];
      Pubgrub.add_package provider "types" (v 1 0 0) [];
      Pubgrub.add_package provider "renderer" (v 1 0 0) [];
      Pubgrub.add_package provider "store" (v 1 0 0) [];
      Pubgrub.add_package provider "router" (v 1 0 0) [];
      Pubgrub.add_package provider "jwt" (v 1 0 0) [];
      Pubgrub.add_package provider "crypto" (v 1 0 0) [];
      Pubgrub.add_package provider "validation" (v 1 0 0) [];
      assert_solution 16 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_plugin_system =
  Test.case "Plugin system with core and extensions"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package
        provider
        "root"
        (v 1 0 0)
        [
          ("core", Pubgrub.full);
          ("plugin-a", Pubgrub.full);
          ("plugin-b", Pubgrub.full);
          ("plugin-c", Pubgrub.full);
        ];
      Pubgrub.add_package
        provider
        "core"
        (v 1 0 0)
        [ ("api", Pubgrub.full); ("runtime", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "plugin-a"
        (v 1 0 0)
        [ ("core", Pubgrub.full); ("helper-a", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "plugin-b"
        (v 1 0 0)
        [ ("core", Pubgrub.full); ("helper-b", Pubgrub.full) ];
      Pubgrub.add_package
        provider
        "plugin-c"
        (v 1 0 0)
        [ ("core", Pubgrub.full); ("helper-c", Pubgrub.full) ];
      Pubgrub.add_package provider "api" (v 1 0 0) [];
      Pubgrub.add_package provider "runtime" (v 1 0 0) [];
      Pubgrub.add_package provider "helper-a" (v 1 0 0) [];
      Pubgrub.add_package provider "helper-b" (v 1 0 0) [];
      Pubgrub.add_package provider "helper-c" (v 1 0 0) [];
      assert_solution 10 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_multi_level_constraints =
  Test.case "Multi-level version constraints (4 levels)"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("a", Pubgrub.full) ];
      Pubgrub.add_package provider "a" (v 1 0 0) [ ("b", Pubgrub.higher_than (v 2 0 0)) ];
      Pubgrub.add_package provider "b" (v 3 0 0) [ ("c", Pubgrub.between (v 1 0 0) (v 3 0 0)) ];
      Pubgrub.add_package provider "c" (v 2 0 0) [ ("d", Pubgrub.strictly_lower_than (v 5 0 0)) ];
      for i = 0 to 10 do
        Pubgrub.add_package provider "b" (v i 0 0) [ ("c", Pubgrub.between (v 1 0 0) (v 3 0 0)) ];
        Pubgrub.add_package provider "c" (v i 0 0) [ ("d", Pubgrub.strictly_lower_than (v 5 0 0)) ];
        Pubgrub.add_package provider "d" (v i 0 0) []
      done;
      assert_solution 5 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0)))

let test_ref_same_result_on_repeated_runs =
  Test.case "REF: Same result on repeated runs"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "c" (v 0 0 0) [];
      Pubgrub.add_package provider "c" (v 2 0 0) [];
      Pubgrub.add_package provider "b" (v 0 0 0) [];
      Pubgrub.add_package provider "b" (v 1 0 0) [ ("c", Pubgrub.between (v 0 0 0) (v 1 0 0)) ];
      Pubgrub.add_package provider "a" (v 0 0 0) [ ("b", Pubgrub.full); ("c", Pubgrub.full) ];
      let result1 = Pubgrub.solve (Pubgrub.to_provider provider) "a" (v 0 0 0) in
      let result2 = Pubgrub.solve (Pubgrub.to_provider provider) "a" (v 0 0 0) in
      match (result1, result2) with
      | Ok (Pubgrub.Solver.Success s1), Ok (Pubgrub.Solver.Success s2) ->
          if List.length s1 = List.length s2 then
            Ok ()
          else
            Error "Results have different lengths"
      | _ -> Error "Expected both to succeed")

let test_ref_no_solution_empty_dep =
  Test.case "REF: No solution with empty dependency"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "a" (v 0 0 0) [ ("b", Pubgrub.empty) ];
      match Pubgrub.solve (Pubgrub.to_provider provider) "a" (v 0 0 0) with
      | Ok (Pubgrub.Solver.Failure _) -> Ok ()
      | Ok (Pubgrub.Solver.Success _) -> Error "Expected failure but got success"
      | Error _ -> Error "Unexpected error")

let test_ref_no_solution_transitive =
  Test.case "REF: No solution transitively"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "a" (v 0 0 0) [ ("b", Pubgrub.empty) ];
      Pubgrub.add_package provider "c" (v 0 0 0) [ ("a", Pubgrub.full) ];
      match Pubgrub.solve (Pubgrub.to_provider provider) "c" (v 0 0 0) with
      | Ok (Pubgrub.Solver.Failure _) -> Ok ()
      | Ok (Pubgrub.Solver.Success _) -> Error "Expected failure but got success"
      | Error _ -> Error "Unexpected error")

let test_ref_depend_on_self_ok =
  Test.case "REF: Depend on self (should work)"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "a" (v 0 0 0) [ ("a", Pubgrub.full) ];
      match Pubgrub.solve (Pubgrub.to_provider provider) "a" (v 0 0 0) with
      | Ok (Pubgrub.Solver.Success _) -> Ok ()
      | Ok (Pubgrub.Solver.Failure _) -> Error "Expected success but got failure"
      | Error _ -> Error "Unexpected error")

let test_ref_depend_on_self_impossible =
  Test.case "REF: Depend on self impossible version"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "a" (v 66 0 0) [ ("a", Pubgrub.singleton (v 111 0 0)) ];
      match Pubgrub.solve (Pubgrub.to_provider provider) "a" (v 66 0 0) with
      | Ok (Pubgrub.Solver.Failure _) ->
          Ok ()
      | Ok (Pubgrub.Solver.Success sol) ->
          let packages =
            List.map sol ~fn:(fun (name, _) -> name)
          in
          Error ("Expected failure but got success: " ^ (String.concat ", " packages))
      | Error err ->
          Error ("Unexpected error: " ^ err))

let test_ref_no_conflict =
  Test.case "REF: No conflict (from Dart docs)"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.between (v 1 0 0) (v 2 0 0)) ];
      Pubgrub.add_package provider "foo" (v 1 0 0) [ ("bar", Pubgrub.between (v 1 0 0) (v 2 0 0)) ];
      Pubgrub.add_package provider "bar" (v 1 0 0) [];
      Pubgrub.add_package provider "bar" (v 2 0 0) [];
      match Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
      | Ok (Pubgrub.Solver.Success solution) ->
          if List.length solution = 3 then
            Ok ()
          else
            Error ("Expected 3 packages, got " ^ (Int.to_string (List.length solution)))
      | Ok (Pubgrub.Solver.Failure _) -> Error "Expected success but got failure"
      | Error err -> Error ("Unexpected error: " ^ err))

let test_ref_avoiding_conflict =
  Test.case "REF: Avoiding conflict during decision"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package
        provider
        "root"
        (v 1 0 0)
        [
          ("foo", Pubgrub.between (v 1 0 0) (v 2 0 0));
          ("bar", Pubgrub.between (v 1 0 0) (v 2 0 0));
        ];
      Pubgrub.add_package provider "foo" (v 1 1 0) [ ("bar", Pubgrub.between (v 2 0 0) (v 3 0 0)) ];
      Pubgrub.add_package provider "foo" (v 1 0 0) [];
      Pubgrub.add_package provider "bar" (v 1 0 0) [];
      Pubgrub.add_package provider "bar" (v 1 1 0) [];
      Pubgrub.add_package provider "bar" (v 2 0 0) [];
      match Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
      | Ok (Pubgrub.Solver.Success solution) ->
          if List.length solution = 3 then
            Ok ()
          else
            Error ("Expected 3 packages, got " ^ (Int.to_string (List.length solution)))
      | Ok (Pubgrub.Solver.Failure _) -> Error "Expected success but got failure"
      | Error err -> Error ("Unexpected error: " ^ err))

let test_ref_conflict_resolution =
  Test.case "REF: Conflict resolution (from Dart docs)"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.higher_than (v 1 0 0)) ];
      Pubgrub.add_package provider "foo" (v 2 0 0) [ ("bar", Pubgrub.between (v 1 0 0) (v 2 0 0)) ];
      Pubgrub.add_package provider "bar" (v 1 0 0) [];
      match Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
      | Ok (Pubgrub.Solver.Success solution) ->
          if List.length solution = 3 then
            Ok ()
          else
            Error ("Expected 3 packages, got " ^ (Int.to_string (List.length solution)))
      | Ok (Pubgrub.Solver.Failure _) -> Error "Expected success but got failure"
      | Error err -> Error ("Unexpected error: " ^ err))

let make_partial_satisfier_provider = fun () ->
  let provider = Pubgrub.create_offline () in
  Pubgrub.add_package
    provider
    "root"
    (v 1 0 0)
    [
      ("foo", Pubgrub.between (v 1 0 0) (v 2 0 0));
      ("target", Pubgrub.between (v 2 0 0) (v 3 0 0));
    ];
  Pubgrub.add_package
    provider
    "foo"
    (v 1 1 0)
    [
      ("left", Pubgrub.between (v 1 0 0) (v 2 0 0));
      ("right", Pubgrub.between (v 1 0 0) (v 2 0 0));
    ];
  Pubgrub.add_package provider "foo" (v 1 0 0) [];
  Pubgrub.add_package provider "left" (v 1 0 0) [ ("shared", Pubgrub.higher_than (v 1 0 0)) ];
  Pubgrub.add_package
    provider
    "right"
    (v 1 0 0)
    [ ("shared", Pubgrub.strictly_lower_than (v 2 0 0)) ];
  Pubgrub.add_package provider "shared" (v 1 0 0) [ ("target", Pubgrub.between (v 1 0 0) (v 2 0 0)) ];
  Pubgrub.add_package provider "shared" (v 2 0 0) [];
  Pubgrub.add_package provider "target" (v 2 0 0) [];
  Pubgrub.add_package provider "target" (v 1 0 0) [];
  provider

let test_conflict_partial_satisfier_variant =
  Test.case "Conflict with partial satisfier variant"
    (fun _ctx ->
      let provider = make_partial_satisfier_provider () in
      match Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
      | Ok (Pubgrub.Solver.Success solution) ->
          let packages =
            List.map solution ~fn:(fun (name, ver) -> name ^ "@" ^ (Pubgrub.version_to_string ver))
          in
          if List.length solution = 3 then
            Ok ()
          else
            Error ("Expected 3 packages, got "
            ^ (Int.to_string (List.length solution))
            ^ ": "
            ^ (String.concat ", " packages))
      | Ok (Pubgrub.Solver.Failure incompat) ->
          Error ("Expected success but got failure: " ^ (Pubgrub.Report.explain_conflict incompat))
      | Error err ->
          Error ("Unexpected error: " ^ err))

let test_ref_conflict_partial_satisfier =
  Test.case "REF: Conflict with partial satisfier"
    (fun _ctx ->
      let provider = make_partial_satisfier_provider () in
      match Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
      | Ok (Pubgrub.Solver.Success solution) ->
          let packages =
            List.map solution ~fn:(fun (name, ver) -> name ^ "@" ^ (Pubgrub.version_to_string ver))
          in
          if List.length solution = 3 then
            Ok ()
          else
            Error ("Expected 3 packages, got "
            ^ (Int.to_string (List.length solution))
            ^ ": "
            ^ (String.concat ", " packages))
      | Ok (Pubgrub.Solver.Failure incompat) ->
          Error ("Expected success but got failure: " ^ (Pubgrub.Report.explain_conflict incompat))
      | Error err ->
          Error ("Unexpected error: " ^ err))

let test_trace_conflict_partial_satisfier =
  Test.case "TRACE: Conflict with partial satisfier"
    (fun ctx ->
      let provider = make_partial_satisfier_provider () in
      let trace_ctx = Pubgrub.Trace.create () in
      match Pubgrub.solve ~trace_ctx (Pubgrub.to_provider provider) "root" (v 1 0 0) with
      | Ok (Pubgrub.Solver.Success _) -> Test.Snapshot.assert_json
        ~ctx
        ~actual:(Pubgrub.Trace.to_json trace_ctx)
      | Ok (Pubgrub.Solver.Failure incompat) -> Error ("Expected success but got failure: "
      ^ (Pubgrub.Report.explain_conflict incompat))
      | Error err -> Error ("Unexpected error: " ^ err))

let test_ref_double_choices =
  Test.case "REF: Double choices"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "a" (v 0 0 0) [ ("b", Pubgrub.full); ("c", Pubgrub.full) ];
      Pubgrub.add_package provider "b" (v 0 0 0) [ ("d", Pubgrub.singleton (v 0 0 0)) ];
      Pubgrub.add_package provider "b" (v 1 0 0) [ ("d", Pubgrub.singleton (v 1 0 0)) ];
      Pubgrub.add_package provider "c" (v 0 0 0) [];
      Pubgrub.add_package provider "c" (v 1 0 0) [ ("d", Pubgrub.singleton (v 2 0 0)) ];
      Pubgrub.add_package provider "d" (v 0 0 0) [];
      match Pubgrub.solve (Pubgrub.to_provider provider) "a" (v 0 0 0) with
      | Ok (Pubgrub.Solver.Success solution) ->
          let packages =
            List.map solution ~fn:(fun (name, ver) -> name ^ "@" ^ (Pubgrub.version_to_string ver))
          in
          if List.length solution = 4 then
            Ok ()
          else
            Error ("Expected 4 packages, got "
            ^ (Int.to_string (List.length solution))
            ^ ": "
            ^ (String.concat ", " packages))
      | Ok (Pubgrub.Solver.Failure err) ->
          Error ("Expected success but got failure: " ^ (Pubgrub.Report.explain_conflict err))
      | Error err ->
          Error ("Unexpected error: " ^ err))

let test_ref_confusing_with_holes =
  Test.case "REF: Confusing with lots of holes"
    (fun _ctx ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full); ("baz", Pubgrub.full) ];
      for i = 1 to 5 do
        Pubgrub.add_package provider "foo" (v i 0 0) [ ("bar", Pubgrub.full) ]
      done;
      Pubgrub.add_package provider "baz" (v 1 0 0) [];
      match Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
      | Ok (Pubgrub.Solver.Failure _) -> Ok ()
      | Ok (Pubgrub.Solver.Success _) -> Error "Expected failure but got success"
      | Error err -> Error ("Unexpected error: " ^ err))

let all_tests =
  let base_tests = [
    test_ranges_empty_contains_nothing;
    test_ranges_full_contains_everything;
    test_ranges_singleton_exact;
    test_ranges_higher_than_is_inclusive;
    test_ranges_strictly_higher_than_is_exclusive;
    test_ranges_lower_than_is_inclusive;
    test_ranges_strictly_lower_than_is_exclusive;
    test_ranges_between_is_half_open;
    test_ranges_intersection_with_empty;
    test_ranges_intersection_with_full;
    test_ranges_union_with_empty;
    test_ranges_complement_of_singleton;
    test_ranges_touching_exclusive_intersection_is_empty;
    test_ranges_union_of_overlapping_intervals;
    test_ranges_complement_of_multiple_segments;
    test_ranges_is_disjoint_matches_empty_intersection;
    test_ranges_subset_and_double_complement;
    test_ranges_normalize_collapses_semantic_duplicates;
    test_ranges_compare_is_semantic;
    test_ranges_to_string_is_stable;
    test_term_positive_full_is_any;
    test_term_negative_empty_is_any;
    test_term_positive_empty_is_not_any;
    test_term_negative_full_is_not_any;
    test_term_structural_package_equality;
    test_term_mixed_union_can_be_tautological;
    test_term_negation_is_involutive;
    test_term_positive_positive_intersection;
    test_term_positive_positive_union;
    test_term_negative_negative_intersection;
    test_term_negative_negative_union;
    test_term_positive_negative_intersection;
    test_term_positive_negative_union;
    test_term_union_on_different_packages_raises;
    test_term_intersection_on_different_packages_raises;
    test_provider_choose_version_missing_package;
    test_provider_count_versions_missing_package;
    test_provider_get_dependencies_missing_package;
    test_provider_choose_version_picks_highest_match;
    test_provider_choose_version_skips_nonmatching_higher;
    test_provider_choose_version_exact_match;
    test_provider_count_versions_across_disjoint_ranges;
    test_provider_duplicate_version_latest_deps_win;
    test_provider_duplicate_versions_count_once;
    test_provider_insertion_order_is_semantic;
    test_incompatibility_from_dependency_nonempty;
    test_incompatibility_from_dependency_empty;
    test_incompatibility_no_versions_preserves_requested_range;
    test_incompatibility_as_dependency_only_for_dependencies;
    test_incompatibility_merge_dependents_matching;
    test_incompatibility_merge_dependents_different_ranges;
    test_incompatibility_prior_cause_merges_and_canonicalizes;
    test_incompatibility_not_root_is_terminal_for_root;
    test_relation_satisfied_when_all_terms_met;
    test_relation_almost_satisfied_with_one_undecided;
    test_relation_contradicted_by_decision;
    test_relation_constrained_positive_subset_is_satisfied;
    test_relation_constrained_positive_overlap_is_almost_satisfied;
    test_relation_constrained_negative_disjoint_is_satisfied;
    test_relation_constrained_negative_subset_is_contradicted;
    test_partial_solution_cached_constraints_intersect_derivations;
    test_partial_solution_backtrack_restores_cached_derivation;
    test_partial_solution_missing_derivation_package_raises;
    test_solution_order_is_deterministic;
    test_report_no_versions_includes_requested_range;
    test_report_from_dependency_includes_dependency_range;
    test_report_derived_explanations_are_readable;
    test_solver_stats_expose_structured_counters;
    test_solver_options_control_iteration_limit;
    test_conflicting_root_constraints_fail;
    test_derivations_are_created;
    test_empty_root;
    test_single_dependency;
    test_two_dependencies;
    test_transitive_chain;
    test_deep_chain;
    test_diamond_dependency;
    test_nested_diamonds;
    test_complex_graph;
    test_multiple_versions_picks_latest;
    test_many_versions;
    test_semantic_versions;
    test_version_constraint_lower;
    test_version_constraint_upper;
    test_version_range;
    test_transitive_constraints;
    test_wide_tree;
    test_larger_graph;
    test_version_selection_strategy;
    test_very_deep_chain;
    test_very_wide_tree;
    test_triple_diamond;
    test_many_versions_20;
    test_many_versions_50;
    test_constraint_range_narrow;
    test_patch_versions;
    test_branching_graph;
    test_deep_and_wide;
    test_large_graph_30_packages;
    test_conflict_missing_dependency;
    test_balanced_tree;
    test_monorepo_structure;
    test_massive_graph_100_packages;
    test_massive_versions;
    test_complex_constraint_web;
    test_deep_shared_dependency;
    test_plugin_system;
    test_multi_level_constraints;
  ]
  in
  let web_tests = generate_web_framework_tests () in
  let db_tests = generate_database_tests () in
  let compiler_tests = generate_compiler_tests () in
  let reference_tests = [
    test_ref_same_result_on_repeated_runs;
    test_ref_no_solution_empty_dep;
    test_ref_no_solution_transitive;
    test_ref_depend_on_self_ok;
    test_ref_depend_on_self_impossible;
    test_ref_no_conflict;
    test_ref_avoiding_conflict;
    test_ref_conflict_resolution;
    test_conflict_partial_satisfier_variant;
    test_ref_conflict_partial_satisfier;
    test_trace_conflict_partial_satisfier;
    test_ref_double_choices;
    test_ref_confusing_with_holes;
    prop_ranges_complement_matches_membership;
    prop_ranges_double_complement_is_identity;
    prop_ranges_union_matches_boolean_or;
    prop_ranges_intersection_matches_boolean_and;
    prop_term_negation_is_involutive;
  ]
  in
  base_tests @ web_tests @ db_tests @ compiler_tests @ reference_tests

(*
(* TEMPORARILY COMMENTED OUT - these test New_solver internals *)
let test_new_solver_compute_pending () =
  Log.info "=== Testing NEW solver compute_pending ===";

  (* Create a simple state with root decided and one dependency *)
  let solution = Partial_solution.empty () in
  let solution = Partial_solution.add_decision solution "root" (v 1 0 0) in

  let incompats = Collections.HashMap.create () in
  (* Add root to incompatibilities so it gets found during iteration *)
  ignore (Collections.HashMap.insert incompats "root" []);

  let dep_graph = New_solver.DependencyGraph.empty () in
  let dep_graph =
    New_solver.DependencyGraph.add_dependencies dep_graph "root" (v 1 0 0)
      [ ("foo", Ranges.full) ]
  in

  let state =
    {
      New_solver.solution;
      incompatibilities = incompats;
      dependency_graph = dep_graph;
    }
  in

  let pending = New_solver.compute_pending state in
  Log.info "Pending list has %d packages" (List.length pending);
  List.iter (fun (pkg, _) -> Log.info "  - %s" pkg) pending;

  if List.length pending = 1 then Log.info "✓ compute_pending test passed"
  else Log.error "✗ compute_pending test FAILED"

let test_new_solver_basic () =
  Log.info "=== Testing NEW solver basic solve ===";

  let provider = Pubgrub.create_offline () in
  Pubgrub.add_package provider "root" (v 1 0 0) [];

  match New_solver.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
  | Ok (New_solver.Success solution) ->
      Log.info "✓ NEW solver basic test passed";
      Log.info "  Solution has %d packages" (List.length solution);
      List.iter
        (fun (pkg, ver) -> Log.info "    %s@%s" pkg (Version.to_string ver))
        solution
  | Ok (New_solver.Failure _) -> Log.error "✗ NEW solver: unexpected failure"
  | Error err -> Log.error "✗ NEW solver error: %s" err

let test_new_solver_with_dependency () =
  Log.info "=== Testing NEW solver with dependency ===";

  let provider = Pubgrub.create_offline () in
  Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full) ];
  Pubgrub.add_package provider "foo" (v 1 0 0) [];

  match New_solver.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
  | Ok (New_solver.Success solution) ->
      if List.length solution = 2 then
        Log.info "✓ NEW solver dependency test passed"
      else
        Log.error "✗ NEW solver: expected 2 packages, got %d"
          (List.length solution)
  | Ok (New_solver.Failure _) -> Log.error "✗ NEW solver: unexpected failure"
  | Error err -> Log.error "✗ NEW solver error: %s" err

let test_new_solver_on_test_suite () =
  Log.info "=== Running first 10 tests with NEW solver ===";

  (* Test 1: Empty root *)
  let provider = Pubgrub.create_offline () in
  Pubgrub.add_package provider "root" (v 1 0 0) [];
  (match New_solver.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
  | Ok (New_solver.Success solution) ->
      if List.length solution = 1 then Log.info "✓ Test 1 (Empty root) passed"
      else Log.error "✗ Test 1 failed: expected 1 package"
  | _ -> Log.error "✗ Test 1 failed");

  (* Test 2: Single dependency *)
  let provider = Pubgrub.create_offline () in
  Pubgrub.add_package provider "root" (v 1 0 0) [ ("foo", Pubgrub.full) ];
  Pubgrub.add_package provider "foo" (v 1 0 0) [];
  (match New_solver.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
  | Ok (New_solver.Success solution) ->
      if List.length solution = 2 then
        Log.info "✓ Test 2 (Single dependency) passed"
      else Log.error "✗ Test 2 failed: expected 2 packages"
  | _ -> Log.error "✗ Test 2 failed");

  (* Test 3: Two independent dependencies *)
  let provider = Pubgrub.create_offline () in
  Pubgrub.add_package provider "root" (v 1 0 0)
    [ ("foo", Pubgrub.full); ("bar", Pubgrub.full) ];
  Pubgrub.add_package provider "foo" (v 1 0 0) [];
  Pubgrub.add_package provider "bar" (v 1 0 0) [];
  (match New_solver.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
  | Ok (New_solver.Success solution) ->
      if List.length solution = 3 then
        Log.info "✓ Test 3 (Two independent deps) passed"
      else
        Log.error "✗ Test 3 failed: expected 3 packages, got %d"
          (List.length solution)
  | _ -> Log.error "✗ Test 3 failed");

  Log.info "NEW solver preliminary tests complete";

  (* Test 4: Conflict - missing dependency *)
  let provider = Pubgrub.create_offline () in
  Pubgrub.add_package provider "root" (v 1 0 0)
    [ ("nonexistent", Pubgrub.full) ];
  (match New_solver.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
  | Ok (New_solver.Failure _) ->
      Log.info "✓ Test 4 (Missing dependency conflict) passed"
  | Ok (New_solver.Success _) -> Log.error "✗ Test 4 failed: expected failure"
  | Error err -> Log.error "✗ Test 4 error: %s" err);

  Log.info "NEW solver conflict tests complete"

let test_new_solver_on_failing_tests () =
  Log.info "=== Testing NEW solver on previously failing tests ===";

  (* First, a simpler conflicting deps test *)
  let provider = Pubgrub.create_offline () in
  Pubgrub.add_package provider "root" (v 1 0 0)
    [ ("a", Pubgrub.full); ("b", Pubgrub.full) ];
  Pubgrub.add_package provider "a" (v 1 0 0)
    [ ("c", Pubgrub.singleton (v 1 0 0)) ];
  Pubgrub.add_package provider "b" (v 1 0 0)
    [ ("c", Pubgrub.singleton (v 2 0 0)) ];
  Pubgrub.add_package provider "c" (v 1 0 0) [];
  Pubgrub.add_package provider "c" (v 2 0 0) [];
  (match New_solver.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
  | Ok (New_solver.Success solution) ->
      Log.info "✓ Test: Simple conflicting deps passed (%d packages)"
        (List.length solution);
      List.iter
        (fun (pkg, ver) -> Log.info "    %s@%s" pkg (Version.to_string ver))
        solution
  | Ok (New_solver.Failure incompat) ->
      Log.error "✗ Test: Simple conflicting deps failed (got failure)";
      Log.error "  Reason: %s" (Pubgrub.Report.explain_conflict incompat)
  | Error err -> Log.error "✗ Test: Simple conflicting deps error: %s" err);

  (* Test: No solution transitively *)
  let provider = Pubgrub.create_offline () in
  Pubgrub.add_package provider "a" (v 0 0 0) [ ("b", Pubgrub.empty) ];
  Pubgrub.add_package provider "c" (v 0 0 0) [ ("a", Pubgrub.full) ];
  (match New_solver.solve (Pubgrub.to_provider provider) "c" (v 0 0 0) with
  | Ok (New_solver.Failure _) ->
      Log.info "✓ Test: No solution transitively passed"
  | Ok (New_solver.Success _) ->
      Log.error "✗ Test: No solution transitively failed (got success)"
  | Error err -> Log.error "✗ Test: No solution transitively error: %s" err);

  (* Test: Avoiding conflict during decision *)
  let provider = Pubgrub.create_offline () in
  Pubgrub.add_package provider "root" (v 1 0 0)
    [
      ("foo", Pubgrub.between (v 1 0 0) (v 2 0 0));
      ("bar", Pubgrub.between (v 1 0 0) (v 2 0 0));
    ];
  Pubgrub.add_package provider "foo" (v 1 1 0)
    [ ("bar", Pubgrub.between (v 2 0 0) (v 3 0 0)) ];
  Pubgrub.add_package provider "foo" (v 1 0 0) [];
  Pubgrub.add_package provider "bar" (v 1 0 0) [];
  Pubgrub.add_package provider "bar" (v 1 1 0) [];
  Pubgrub.add_package provider "bar" (v 2 0 0) [];
  (match New_solver.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
  | Ok (New_solver.Success solution) ->
      if List.length solution = 3 then
        Log.info "✓ Test: Avoiding conflict during decision passed"
      else
        Log.error
          "✗ Test: Avoiding conflict during decision failed (expected 3, got \
           %d)"
          (List.length solution)
  | Ok (New_solver.Failure _) ->
      Log.error "✗ Test: Avoiding conflict during decision failed (got failure)"
  | Error err ->
      Log.error "✗ Test: Avoiding conflict during decision error: %s" err);

  (* Test: Double choices *)
  Log.info ">>> Starting Double choices test";
  let provider = Pubgrub.create_offline () in
  Pubgrub.add_package provider "a" (v 0 0 0)
    [ ("b", Pubgrub.full); ("c", Pubgrub.full) ];
  Pubgrub.add_package provider "b" (v 0 0 0)
    [ ("d", Pubgrub.singleton (v 0 0 0)) ];
  Pubgrub.add_package provider "b" (v 1 0 0)
    [ ("d", Pubgrub.singleton (v 1 0 0)) ];
  Pubgrub.add_package provider "c" (v 0 0 0) [];
  Pubgrub.add_package provider "c" (v 1 0 0)
    [ ("d", Pubgrub.singleton (v 2 0 0)) ];
  Pubgrub.add_package provider "d" (v 0 0 0) [];
  (match New_solver.solve (Pubgrub.to_provider provider) "a" (v 0 0 0) with
  | Ok (New_solver.Success solution) ->
      if List.length solution = 4 then Log.info "✓ Test: Double choices passed"
      else (
        Log.error "✗ Test: Double choices failed (expected 4, got %d)"
          (List.length solution);
        List.iter
          (fun (pkg, ver) ->
            Log.info "    Solution: %s@%s" pkg (Version.to_string ver))
          solution)
  | Ok (New_solver.Failure incompat) ->
      Log.error "✗ Test: Double choices failed (got failure)";
      Log.error "  Reason: %s" (Pubgrub.Report.explain_conflict incompat)
  | Error err -> Log.error "✗ Test: Double choices error: %s" err);

  (* Test: Confusing with lots of holes *)
  let provider = Pubgrub.create_offline () in
  Pubgrub.add_package provider "root" (v 1 0 0)
    [ ("foo", Pubgrub.full); ("baz", Pubgrub.full) ];
  for i = 1 to 5 do
    Pubgrub.add_package provider "foo" (v i 0 0) [ ("bar", Pubgrub.full) ]
  done;
  Pubgrub.add_package provider "baz" (v 1 0 0) [];
  (match New_solver.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
  | Ok (New_solver.Failure _) ->
      Log.info "✓ Test: Confusing with lots of holes passed"
  | Ok (New_solver.Success _) ->
      Log.error "✗ Test: Confusing with lots of holes failed (got success)"
  | Error err -> Log.error "✗ Test: Confusing with lots of holes error: %s" err);

  Log.info "NEW solver failing tests complete"

let run_all_tests_with_new_solver () =
  Log.info "=== Running ALL package tests with NEW solver ===";
  let passed = ref 0 in
  let failed = ref 0 in

  List.iter
    (fun test ->
      match Test.run test with Ok () -> incr passed | Error _ -> incr failed)
    all_tests;

  Log.info "NEW SOLVER RESULTS: %d/%d tests passed (%.1f%%)" !passed
    (!passed + !failed)
    (float_of_int !passed /. float_of_int (!passed + !failed) *. 100.0)
*)

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"pubgrub" ~tests:all_tests ~args)
    ~args:Env.args
    ()
