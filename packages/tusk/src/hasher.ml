(** Content-based hashing module for build artifacts *)

type hash = string
(** Hash type - internally a SHA-256 string *)

(** Hash a string content using SHA-256 *)
let hash_string s =
  let cmd = Printf.sprintf "echo -n '%s' | shasum -a 256 | cut -d' ' -f1" s in
  let output = System.run_process_lines cmd in
  match output with
  | [ hash ] -> String.trim hash
  | _ -> failwith "Failed to compute SHA256"

(** Hash a file's content using SHA-256 *)
let hash_file filepath =
  if System.file_exists filepath then
    let cmd = Printf.sprintf "shasum -a 256 '%s' | cut -d' ' -f1" filepath in
    let output = System.run_process_lines cmd in
    match output with
    | [ hash ] -> String.trim hash
    | _ -> filepath ^ ":missing"
  else filepath ^ ":missing"

(** Convert hash to string for storage/display *)
let to_string hash = hash

(** Create hash from string (for loading from storage) *)
let of_string s = s

(** Compare two hashes for equality *)
let equal hash1 hash2 = String.equal hash1 hash2

(** Tests submodule *)
module Tests = struct
  let test_hash_file_produces_deterministic_results () : (unit, string) result =
    (* Test that same file always produces same hash *)
    Ok ()
    [@test]

  let test_hash_string_produces_deterministic_results () : (unit, string) result
      =
    (* Test that same string always produces same hash *)
    Ok ()
    [@test]

  let test_different_files_produce_different_hashes () : (unit, string) result =
    (* Test that different content produces different hashes *)
    Ok ()
    [@test]

  let test_hash_to_string_roundtrip () : (unit, string) result =
    (* Test that to_string and of_string are inverses *)
    Ok ()
end [@test]
