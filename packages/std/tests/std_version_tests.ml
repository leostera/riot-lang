open Std

let parse_version = fun source ->
  Version.parse source
  |> Result.unwrap

let parse_requirement = fun source ->
  Version.parse_requirement source
  |> Result.unwrap

let test_parse_plain_version = fun _ctx ->
  match Version.parse "1.2.3" with
  | Ok {
      major = 1;
      minor = 2;
      patch = 3;
      pre = [];
      build = None;
    } ->
      Ok ()
  | Ok _ -> Error "expected Version.parse 1.2.3 to return a plain semver"
  | Error _ -> Error "expected Version.parse 1.2.3 to succeed"

let test_parse_zero_version = fun _ctx ->
  match Version.parse "0.0.0" with
  | Ok {
      major = 0;
      minor = 0;
      patch = 0;
      pre = [];
      build = None;
    } ->
      Ok ()
  | Ok _ -> Error "expected Version.parse 0.0.0 to return the zero version"
  | Error _ -> Error "expected Version.parse 0.0.0 to succeed"

let test_parse_pre_release_alpha = fun _ctx ->
  match Version.parse "1.2.3-alpha" with
  | Ok { pre = [ Version.Alphanumeric "alpha" ]; _ } -> Ok ()
  | Ok _ -> Error "expected Version.parse to capture the alpha pre-release segment"
  | Error _ -> Error "expected Version.parse 1.2.3-alpha to succeed"

let test_parse_pre_release_mixed_segments = fun _ctx ->
  match Version.parse "1.2.3-alpha.1" with
  | Ok { pre = [ Version.Alphanumeric "alpha"; Numeric 1 ]; _ } -> Ok ()
  | Ok _ -> Error "expected Version.parse to preserve mixed pre-release segments"
  | Error _ -> Error "expected Version.parse 1.2.3-alpha.1 to succeed"

let test_parse_build_metadata = fun _ctx ->
  match Version.parse "1.2.3+build.5" with
  | Ok { build = Some "build.5"; _ } -> Ok ()
  | Ok _ -> Error "expected Version.parse to capture build metadata"
  | Error _ -> Error "expected Version.parse 1.2.3+build.5 to succeed"

let test_parse_prerelease_and_build = fun _ctx ->
  match Version.parse "1.2.3-alpha+build.5" with
  | Ok {
      pre = [ Version.Alphanumeric "alpha" ];
      build = Some "build.5";
      _;
    } ->
      Ok ()
  | Ok _ -> Error "expected Version.parse to capture both pre-release and build metadata"
  | Error _ -> Error "expected Version.parse 1.2.3-alpha+build.5 to succeed"

let test_parse_rejects_missing_patch = fun _ctx ->
  match Version.parse "1.2" with
  | Error _ -> Ok ()
  | Ok _ -> Error "expected Version.parse to reject versions missing a patch segment"

let test_parse_rejects_too_many_segments = fun _ctx ->
  match Version.parse "1.2.3.4" with
  | Error _ -> Ok ()
  | Ok _ -> Error "expected Version.parse to reject versions with too many core segments"

let test_parse_rejects_invalid_pre_release_characters = fun _ctx ->
  match Version.parse "1.2.3-alpha!" with
  | Error (Version.Invalid_pre_release_segment "alpha!") -> Ok ()
  | _ -> Error "expected Version.parse to reject invalid pre-release characters"

let test_to_string_roundtrips_parsed_versions = fun _ctx ->
  let source = "1.2.3-alpha.1+build.5" in
  let version = parse_version source in
  if String.equal (Version.to_string version) source then
    Ok ()
  else
    Error "expected Version.to_string to roundtrip canonical semver strings"

let test_compare_ignores_build_metadata = fun _ctx ->
  let left = parse_version "1.2.3+build.1" in
  let right = parse_version "1.2.3+build.2" in
  if Version.compare left right = Order.EQ then
    Ok ()
  else
    Error "expected Version.compare to ignore build metadata"

let test_compare_stable_release_is_greater_than_prerelease = fun _ctx ->
  let stable = parse_version "1.2.3" in
  let prerelease = parse_version "1.2.3-alpha" in
  if Version.compare stable prerelease = Order.GT then
    Ok ()
  else
    Error "expected stable releases to compare greater than their prereleases"

let test_compare_numeric_pre_release_segments = fun _ctx ->
  let left = parse_version "1.0.0-alpha.1" in
  let right = parse_version "1.0.0-alpha.2" in
  if Version.compare left right = Order.LT then
    Ok ()
  else
    Error "expected numeric pre-release segments to compare numerically"

let test_compare_numeric_segments_lower_than_alphanumeric = fun _ctx ->
  let left = parse_version "1.0.0-1" in
  let right = parse_version "1.0.0-alpha" in
  if Version.compare left right = Order.LT then
    Ok ()
  else
    Error "expected numeric pre-release segments to compare lower than alphanumeric ones"

let test_compare_shorter_prerelease_lists_lower = fun _ctx ->
  let left = parse_version "1.0.0-alpha" in
  let right = parse_version "1.0.0-alpha.1" in
  if Version.compare left right = Order.LT then
    Ok ()
  else
    Error "expected shorter equal-prefix pre-release lists to compare lower"

let test_ordering_helpers_agree_with_compare = fun _ctx ->
  let left = parse_version "1.2.3-alpha" in
  let right = parse_version "1.2.3" in
  if
    Version.lt left right
    && Version.lte left right
    && Version.gt right left
    && Version.gte right left
  then
    Ok ()
  else
    Error "expected Version.lt/lte/gt/gte to agree with Version.compare"

let test_make_defaults_optional_fields = fun _ctx ->
  let version = Version.make ~major:1 ~minor:2 ~patch:3 () in
  if Version.equal version (parse_version "1.2.3") then
    Ok ()
  else
    Error "expected Version.make to default to no pre-release and no build metadata"

let test_parse_requirement_any = fun _ctx ->
  match Version.view_requirement (parse_requirement "*") with
  | Version.AnyRequirement -> Ok ()
  | _ -> Error "expected Version.parse_requirement * to return AnyRequirement"

let test_parse_requirement_exact = fun _ctx ->
  match Version.view_requirement (parse_requirement "== 1.2.3") with
  | Version.ExactRequirement version when Version.equal version (parse_version "1.2.3") -> Ok ()
  | _ -> Error "expected Version.parse_requirement == 1.2.3 to return an exact requirement"

let test_parse_requirement_tilde = fun _ctx ->
  match Version.view_requirement (parse_requirement "~> 1.2.3") with
  | Version.TildeRequirement version when Version.equal version (parse_version "1.2.3") -> Ok ()
  | _ -> Error "expected Version.parse_requirement ~> 1.2.3 to return a tilde requirement"

let test_parse_requirement_major_prefix = fun _ctx ->
  match Version.view_requirement (parse_requirement "1") with
  | Version.PrefixMajorRequirement 1 -> Ok ()
  | _ -> Error "expected Version.parse_requirement 1 to return a major-prefix requirement"

let test_parse_requirement_minor_prefix = fun _ctx ->
  match Version.view_requirement (parse_requirement "1.2") with
  | Version.PrefixMinorRequirement (1, 2) -> Ok ()
  | _ -> Error "expected Version.parse_requirement 1.2 to return a minor-prefix requirement"

let test_parse_requirement_rejects_empty = fun _ctx ->
  match Version.parse_requirement "" with
  | Error (Version.Invalid_format "") -> Ok ()
  | Ok requirement ->
      Error ("expected empty requirement to be rejected but parsed "
      ^ Version.requirement_to_string requirement)
  | Error _ -> Error "expected empty requirement to return Invalid_format \"\""

let test_requirement_to_string_roundtrips = fun _ctx ->
  let requirement = parse_requirement ">= 1.2.3" in
  if String.equal (Version.requirement_to_string requirement) ">= 1.2.3" then
    Ok ()
  else
    Error "expected Version.requirement_to_string to preserve the canonical representation"

let test_matches_exact_requirement = fun _ctx ->
  let requirement = parse_requirement "== 1.2.3" in
  if
    Version.matches requirement (parse_version "1.2.3")
    && not (Version.matches requirement (parse_version "1.2.4"))
  then
    Ok ()
  else
    Error "expected exact requirements to match only the same semantic version"

let test_matches_major_prefix_requirement = fun _ctx ->
  let requirement = parse_requirement "1" in
  if
    Version.matches requirement (parse_version "1.0.0")
    && Version.matches requirement (parse_version "1.9.9")
    && not (Version.matches requirement (parse_version "2.0.0"))
  then
    Ok ()
  else
    Error "expected major-prefix requirements to match only that major line"

let test_matches_minor_prefix_requirement = fun _ctx ->
  let requirement = parse_requirement "1.2" in
  if
    Version.matches requirement (parse_version "1.2.0")
    && Version.matches requirement (parse_version "1.2.99")
    && not (Version.matches requirement (parse_version "1.3.0"))
  then
    Ok ()
  else
    Error "expected minor-prefix requirements to match only that minor line"

let test_matches_tilde_requirement = fun _ctx ->
  let requirement = parse_requirement "~> 1.2.3" in
  if
    Version.matches requirement (parse_version "1.2.3")
    && Version.matches requirement (parse_version "1.2.9")
    && not (Version.matches requirement (parse_version "1.3.0"))
  then
    Ok ()
  else
    Error "expected tilde requirements to accept >= anchor and < next minor"

let tests =
  Test.[
    case "Version.parse parses a plain semantic version" test_parse_plain_version;
    case "Version.parse parses the zero version" test_parse_zero_version;
    case "Version.parse captures alpha prerelease segments" test_parse_pre_release_alpha;
    case "Version.parse captures mixed prerelease segments" test_parse_pre_release_mixed_segments;
    case "Version.parse captures build metadata" test_parse_build_metadata;
    case
      "Version.parse captures prerelease and build metadata together"
      test_parse_prerelease_and_build;
    case "Version.parse rejects versions missing the patch segment" test_parse_rejects_missing_patch;
    case
      "Version.parse rejects versions with too many segments"
      test_parse_rejects_too_many_segments;
    case
      "Version.parse rejects invalid prerelease characters"
      test_parse_rejects_invalid_pre_release_characters;
    case
      "Version.to_string roundtrips parsed canonical versions"
      test_to_string_roundtrips_parsed_versions;
    case "Version.compare ignores build metadata" test_compare_ignores_build_metadata;
    case
      "Version.compare ranks stable releases above prereleases"
      test_compare_stable_release_is_greater_than_prerelease;
    case
      "Version.compare orders numeric prerelease segments numerically"
      test_compare_numeric_pre_release_segments;
    case
      "Version.compare ranks numeric prerelease segments below alphanumeric ones"
      test_compare_numeric_segments_lower_than_alphanumeric;
    case
      "Version.compare ranks shorter equal-prefix prerelease lists lower"
      test_compare_shorter_prerelease_lists_lower;
    case
      "Version ordering helpers agree with Version.compare"
      test_ordering_helpers_agree_with_compare;
    case "Version.make defaults optional fields" test_make_defaults_optional_fields;
    case "Version.parse_requirement parses * as AnyRequirement" test_parse_requirement_any;
    case "Version.parse_requirement parses exact requirements" test_parse_requirement_exact;
    case "Version.parse_requirement parses tilde requirements" test_parse_requirement_tilde;
    case "Version.parse_requirement parses bare major prefixes" test_parse_requirement_major_prefix;
    case "Version.parse_requirement parses bare minor prefixes" test_parse_requirement_minor_prefix;
    case "Version.parse_requirement rejects empty strings" test_parse_requirement_rejects_empty;
    case
      "Version.requirement_to_string renders canonical requirements"
      test_requirement_to_string_roundtrips;
    case "Version.matches exact requirements precisely" test_matches_exact_requirement;
    case
      "Version.matches major prefixes across the whole major line"
      test_matches_major_prefix_requirement;
    case
      "Version.matches minor prefixes across the whole minor line"
      test_matches_minor_prefix_requirement;
    case "Version.matches tilde requirements within the minor line" test_matches_tilde_requirement;
  ]

let main ~args = Test.Cli.main ~name:"Version" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
