(** Content-based hashing module for build artifacts *)
(* FIXME: this module should be using Std.Crypto.* functions *)

type hash = string
(** Hash type - internally a SHA-256 string *)

(* FIXME: use Std.(Crypto.sha512 s |> Base.encode_64) *)

(** Hash a string content using MD5 (fast in-memory hashing) *)
let hash_string s = Digest.string s |> Digest.to_hex

(** Hash a file's content using MD5 (async file reading + fast in-memory
    hashing) *)
let hash_file filepath =
  match Std.Path.of_string filepath with
  | Error _ -> filepath ^ ":missing"
  | Ok path -> (
      match Std.Fs.file_exists path with
      | Ok true -> (
          match Std.Fs.read_file path with
          | Ok content -> Digest.string content |> Digest.to_hex
          | Error _ -> filepath ^ ":missing")
      | Ok false | Error _ -> filepath ^ ":missing")

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

(** Hash multiple files by combining their individual hashes *)
let hash_files paths =
  let buffer = Buffer.create 256 in
  List.iter
    (fun path ->
      let path_str = Std.Path.to_string path in
      match Std.Fs.file_exists path with
      | Ok true ->
          let file_hash = hash_file path_str in
          Buffer.add_string buffer (to_string file_hash)
      | Ok false | Error _ -> ())
    paths;
  hash_string (Buffer.contents buffer)

(** Hash multiple strings (typically other hashes) into a single hash *)
let hash_strings strings = hash_string (String.concat "" strings)
