open Std

module Bytes = Kernel.IO.Bytes

(** URI Normalization for stable entity IDs
    
    WARNING: These rules are FROZEN FOREVER. Changing them after data exists
    causes corruption (same logical URI → different entity IDs).
    
    See: packages/poneglyph/docs/URI_NORMALIZATION.md
*)
module Normalize = struct
  (** Lowercase scheme and host (RFC 3986 §6.2.2.1) *)
  let lowercase_scheme_host uri_str =
    (* Simple implementation for poneglyph:// URIs *)
    (* For full HTTP URI support, would use proper URI parser *)
    match String.index_opt uri_str ':' with
    | None -> uri_str
    | Some idx ->
        let scheme = String.sub uri_str 0 idx in
        let rest = String.sub uri_str idx (String.length uri_str - idx) in
        String.lowercase_ascii scheme ^ rest
  
  (** Remove trailing slash except for root *)
  let normalize_trailing_slash uri_str =
    if String.length uri_str > 1 && String.ends_with ~suffix:"/" uri_str then
      String.sub uri_str 0 (String.length uri_str - 1)
    else
      uri_str
  
  (** Apply all normalization rules *)
  let normalize uri_str =
    uri_str
    |> lowercase_scheme_host
    |> normalize_trailing_slash
end

(** URI is a record containing both the string and its SHA-256 hash *)
type t = {
  uri : string;      (** Original normalized URI string *)
  sha256 : bytes;    (** Full SHA-256 hash (32 bytes) for storage/comparison *)
}

type part = Ns of string | Kind of string | Id of string | Field of string

(** Compute SHA-256 hash from normalized URI string *)
let compute_sha256 normalized =
  let hash = Crypto.Sha256.hash_string normalized in
  Crypto.Digest.bytes hash

(** Expand shorthand @ prefix to poneglyph: *)
let expand_shorthand str =
  if String.starts_with ~prefix:"@" str then
    "poneglyph:" ^ String.sub str 1 (String.length str - 1)
  else str

(** Create a URI from a string *)
let of_string str =
  let expanded = expand_shorthand str in
  let normalized = Normalize.normalize expanded in
  let sha256 = compute_sha256 normalized in
  { uri = normalized; sha256 }

(** Convert part to string *)
let part_to_string = function 
  | Ns s | Kind s | Id s | Field s -> s

(** Construct a URI from parts *)
let make parts =
  let str = String.concat ":" (List.map part_to_string parts) in
  of_string str

(** Convert URI to string *)
let to_string t = t.uri

(** Fast equality check (compares SHA-256 hashes) *)
let equal a b = Bytes.equal a.sha256 b.sha256

(** Fast comparison (compares SHA-256 hashes) *)
let compare a b = Bytes.compare a.sha256 b.sha256

(** Part builders *)
let ns s = Ns s
let kind s = Kind s
let id s = Id s
let field s = Field s
