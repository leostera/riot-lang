open Std

module Test = Std.Test
module Bytes = Kernel.Bytes

let raw_hash = fun s -> Crypto.Hash.from_bytes (Bytes.from_string s)

let test_hash_of_bytes_copies_the_input_buffer = fun _ctx ->
  let buffer = Bytes.from_string "abc" in
  let hash = Crypto.Hash.from_bytes buffer in
  Bytes.set_unchecked buffer ~at:0 ~char:'z';
  if String.equal (Bytes.to_string (Crypto.Hash.to_bytes hash)) "abc" then
    Ok ()
  else
    Error "Hash.from_bytes should copy the source buffer"

let test_hash_to_bytes_returns_a_copy = fun _ctx ->
  let hash = raw_hash "abc" in
  let copy = Crypto.Hash.to_bytes hash in
  Bytes.set_unchecked copy ~at:0 ~char:'z';
  if String.equal (Bytes.to_string (Crypto.Hash.to_bytes hash)) "abc" then
    Ok ()
  else
    Error "Hash.to_bytes should return a defensive copy"

let test_hash_length_reports_the_byte_length = fun _ctx ->
  if Int.equal (Crypto.Hash.length (raw_hash "abcdef")) 6 then
    Ok ()
  else
    Error "Hash.length should report the number of bytes"

let test_hash_equal_reports_identical_hashes = fun _ctx ->
  if Crypto.Hash.equal (raw_hash "abc") (raw_hash "abc") then
    Ok ()
  else
    Error "Hash.equal should report identical hashes as equal"

let test_hash_compare_uses_bytewise_order = fun _ctx ->
  if Crypto.Hash.compare (raw_hash "abc") (raw_hash "abd") = Order.LT then
    Ok ()
  else
    Error "Hash.compare should use bytewise ordering"

let test_digest_hex_encodes_raw_bytes = fun _ctx ->
  let hash = raw_hash "\x00\x0f\x10\xff" in
  if String.equal (Crypto.Digest.hex hash) "000f10ff" then
    Ok ()
  else
    Error "Digest.hex should encode bytes as lowercase hexadecimal"

let test_digest_base64_encodes_raw_bytes = fun _ctx ->
  if String.equal (Crypto.Digest.base64 (raw_hash "abc")) "YWJj" then
    Ok ()
  else
    Error "Digest.base64 should encode raw bytes as base64"

let test_digest_base64_url_rewrites_unsafe_characters = fun _ctx ->
  let hash = raw_hash "\xfb\xff\xff" in
  if
    String.equal (Crypto.Digest.base64 hash) "+///"
    && String.equal (Crypto.Digest.base64_url hash) "-___"
  then
    Ok ()
  else
    Error "Digest.base64_url should rewrite + and / into URL-safe characters"

let test_digest_to_int64_interprets_bytes_little_endian = fun _ctx ->
  let hash = raw_hash "\x01\x02\x03" in
  if Int64.equal (Crypto.Digest.to_int64 hash) 197_121L then
    Ok ()
  else
    Error "Digest.to_int64 should interpret bytes in little-endian order"

let test_digest_to_int_matches_to_int64_truncation = fun _ctx ->
  let hash = raw_hash "\x01\x02\x03" in
  if Int.equal (Crypto.Digest.to_int hash) (Int64.to_int (Crypto.Digest.to_int64 hash)) then
    Ok ()
  else
    Error "Digest.to_int should match Digest.to_int64 truncation"

let test_crypto_hash_string_matches_default_hasher = fun _ctx ->
  if Crypto.Hash.equal (Crypto.hash_string "abc") (Crypto.DefaultHasher.hash_string "abc") then
    Ok ()
  else
    Error "Crypto.hash_string should delegate to the default hasher"

let test_crypto_hash_bytes_matches_hash_string_for_same_content = fun _ctx ->
  if
    Crypto.Hash.equal (Crypto.hash_bytes (Bytes.from_string "abc")) (Crypto.hash_string "abc")
  then
    Ok ()
  else
    Error "Crypto.hash_bytes should match Crypto.hash_string for the same payload"

let test_sha256_incremental_matches_one_shot = fun _ctx ->
  let state = Crypto.Sha256.create () in
  Crypto.Sha256.write state "ab";
  Crypto.Sha256.write state "c";
  if Crypto.Hash.equal (Crypto.Sha256.finish state) (Crypto.Sha256.hash_string "abc") then
    Ok ()
  else
    Error "Sha256 incremental hashing should match one-shot hashing"

let test_sha1_incremental_matches_one_shot = fun _ctx ->
  let state = Crypto.Sha1.create () in
  Crypto.Sha1.write state "ab";
  Crypto.Sha1.write state "c";
  if Crypto.Hash.equal (Crypto.Sha1.finish state) (Crypto.Sha1.hash_string "abc") then
    Ok ()
  else
    Error "Sha1 incremental hashing should match one-shot hashing"

let test_sha512_incremental_matches_one_shot = fun _ctx ->
  let state = Crypto.Sha512.create () in
  Crypto.Sha512.write state "ab";
  Crypto.Sha512.write state "c";
  if Crypto.Hash.equal (Crypto.Sha512.finish state) (Crypto.Sha512.hash_string "abc") then
    Ok ()
  else
    Error "Sha512 incremental hashing should match one-shot hashing"

let test_md5_incremental_matches_one_shot = fun _ctx ->
  let state = Crypto.Md5.create () in
  Crypto.Md5.write state "ab";
  Crypto.Md5.write state "c";
  if Crypto.Hash.equal (Crypto.Md5.finish state) (Crypto.Md5.hash_string "abc") then
    Ok ()
  else
    Error "Md5 incremental hashing should match one-shot hashing"

let test_sha256_known_abc_digest = fun _ctx ->
  if
    String.equal
      (Crypto.Digest.hex (Crypto.Sha256.hash_string "abc"))
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  then
    Ok ()
  else
    Error "Sha256 should match the known digest for abc"

let test_sha1_known_abc_digest = fun _ctx ->
  if
    String.equal
      (Crypto.Digest.hex (Crypto.Sha1.hash_string "abc"))
      "a9993e364706816aba3e25717850c26c9cd0d89d"
  then
    Ok ()
  else
    Error "Sha1 should match the known digest for abc"

let test_md5_known_abc_digest = fun _ctx ->
  if
    String.equal
      (Crypto.Digest.hex (Crypto.Md5.hash_string "abc"))
      "900150983cd24fb0d6963f7d28e17f72"
  then
    Ok ()
  else
    Error "Md5 should match the known digest for abc"

let test_hmac_sha256_matches_known_vector = fun _ctx ->
  let digest =
    Crypto.hmac_sha256 ~key:"key" ~data:"The quick brown fox jumps over the lazy dog"
    |> raw_hash
    |> Crypto.Digest.hex
  in
  if String.equal digest "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8" then
    Ok ()
  else
    Error "HMAC-SHA256 should match the known RFC 4231-style vector"

let test_hash_bool_distinguishes_true_and_false = fun _ctx ->
  if not (Crypto.Hash.equal (Crypto.hash_bool true) (Crypto.hash_bool false)) then
    Ok ()
  else
    Error "hash_bool should distinguish true from false"

let test_hash_list_is_order_sensitive = fun _ctx ->
  let left = Crypto.hash_list Crypto.hash_int [ 1; 2; 3 ] in
  let right = Crypto.hash_list Crypto.hash_int [ 3; 2; 1 ] in
  if not (Crypto.Hash.equal left right) then
    Ok ()
  else
    Error "hash_list should depend on element order"

let test_hash_array_is_order_sensitive = fun _ctx ->
  let left = Crypto.hash_array Crypto.hash_int [|1; 2; 3|] in
  let right = Crypto.hash_array Crypto.hash_int [|3; 2; 1|] in
  if not (Crypto.Hash.equal left right) then
    Ok ()
  else
    Error "hash_array should depend on element order"

let tests =
  Test.[
    case "Hash.from_bytes copies the input buffer" test_hash_of_bytes_copies_the_input_buffer;
    case "Hash.to_bytes returns a copy" test_hash_to_bytes_returns_a_copy;
    case "Hash.length reports byte length" test_hash_length_reports_the_byte_length;
    case "Hash.equal reports identical hashes" test_hash_equal_reports_identical_hashes;
    case "Hash.compare uses bytewise order" test_hash_compare_uses_bytewise_order;
    case "Digest.hex encodes raw bytes" test_digest_hex_encodes_raw_bytes;
    case "Digest.base64 encodes raw bytes" test_digest_base64_encodes_raw_bytes;
    case
      "Digest.base64_url rewrites unsafe characters"
      test_digest_base64_url_rewrites_unsafe_characters;
    case
      "Digest.to_int64 interprets bytes little-endian"
      test_digest_to_int64_interprets_bytes_little_endian;
    case "Digest.to_int matches to_int64 truncation" test_digest_to_int_matches_to_int64_truncation;
    case
      "Crypto.hash_string matches the default hasher"
      test_crypto_hash_string_matches_default_hasher;
    case
      "Crypto.hash_bytes matches hash_string for the same content"
      test_crypto_hash_bytes_matches_hash_string_for_same_content;
    case "Sha256 incremental hashing matches one-shot" test_sha256_incremental_matches_one_shot;
    case "Sha1 incremental hashing matches one-shot" test_sha1_incremental_matches_one_shot;
    case "Sha512 incremental hashing matches one-shot" test_sha512_incremental_matches_one_shot;
    case "Md5 incremental hashing matches one-shot" test_md5_incremental_matches_one_shot;
    case "Sha256 matches the known abc digest" test_sha256_known_abc_digest;
    case "Sha1 matches the known abc digest" test_sha1_known_abc_digest;
    case "Md5 matches the known abc digest" test_md5_known_abc_digest;
    case "HMAC-SHA256 matches a known vector" test_hmac_sha256_matches_known_vector;
    case "hash_bool distinguishes true and false" test_hash_bool_distinguishes_true_and_false;
    case "hash_list is order sensitive" test_hash_list_is_order_sensitive;
    case "hash_array is order sensitive" test_hash_array_is_order_sensitive;
  ]

let main ~args = Test.Cli.main ~name:"crypto" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
