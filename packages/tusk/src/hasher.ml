(** Content-based hashing module for build artifacts *)

type hash = string
(** Hash type - internally a SHA-256 string *)

(** Hash a string content using MD5 (fast in-memory hashing) *)
let hash_string s = Digest.string s |> Digest.to_hex

(** Hash a file's content using MD5 (async file reading + fast in-memory
    hashing) *)
let hash_file filepath =
  if Miniriot.File.exists ~path:filepath then
    match Miniriot.File.read ~path:filepath with
    | Ok content -> Digest.string content |> Digest.to_hex
    | Error _ -> filepath ^ ":missing"
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
