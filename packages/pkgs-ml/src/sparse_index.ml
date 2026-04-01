open Std

let normalized_name = fun package_name ->
  String.lowercase_ascii package_name

let package_prefix = fun package_name ->
  let name = normalized_name package_name in
  match String.length name with
  | 0 -> Path.v ""
  | 1 -> Path.v "1"
  | 2 -> Path.v "2"
  | 3 -> Path.(Path.v "3" / Path.v (String.sub name 0 1))
  | _ -> Path.(Path.v (String.sub name 0 2) / Path.v (String.sub name 2 2))

let package_relpath = fun package_name ->
  let name = normalized_name package_name in
  Path.(package_prefix name / Path.v name)

let package_cache_path = fun cache ~package_name ->
  Path.(Registry_cache.index_dir cache / package_relpath package_name)

module Tests = struct
  let expect_relpath = fun ~package_name ~expected ->
    let actual = package_relpath package_name |> Path.to_string in
    if String.equal actual expected then
      Ok ()
    else
      Error ("expected sparse index path '" ^ expected ^ "', got '" ^ actual ^ "'")

  let test_single_character_name () =
    expect_relpath ~package_name:"a" ~expected:"1/a" [@test]

  let test_two_character_name () =
    expect_relpath ~package_name:"ab" ~expected:"2/ab" [@test]

  let test_three_character_name () =
    expect_relpath ~package_name:"abc" ~expected:"3/a/abc" [@test]

  let test_longer_name () =
    expect_relpath ~package_name:"cargo" ~expected:"ca/rg/cargo" [@test]

  let test_names_are_normalized_to_lowercase () =
    expect_relpath ~package_name:"AbCd" ~expected:"ab/cd/abcd" [@test]
end [@test]
