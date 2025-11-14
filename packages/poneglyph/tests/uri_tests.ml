(** Tests for Uri module - interning and construction *)

open Std
open Poneglyph

let test_uri_interning () =
  let uri1 = Uri.of_string "tusk:file:main.ml" in
  let uri2 = Uri.of_string "tusk:file:main.ml" in
  let uri3 = Uri.of_string "tusk:file:other.ml" in

  (* Same string should intern to same ID *)
  if not (Uri.equal uri1 uri2) then
    Error "URI interning failed: same strings should produce equal URIs"
  else if Uri.to_string uri1 != "tusk:file:main.ml" then
    Error "URI to_string failed"
  else if Uri.equal uri1 uri3 then
    Error "URI interning failed: different strings should produce different URIs"
  else
    Ok ()

let test_uri_construction () =
  let uri = Uri.make Uri.[ ns "tusk"; kind "file"; id "src/main.ml" ] in
  if Uri.to_string uri != "tusk:file:src/main.ml" then
    Error "URI construction with path failed"
  else
    let uri_with_id = Uri.make Uri.[ ns "test"; kind "user"; id "alice-42" ] in
    if Uri.to_string uri_with_id != "test:user:alice-42" then
      Error "URI construction with id failed"
    else
      Ok ()

let test_uri_shorthand () =
  let uri1 = Uri.of_string "@field:doc" in
  let uri2 = Uri.of_string "poneglyph:field:doc" in
  
  (* Shorthand @ should expand to poneglyph: *)
  if not (Uri.equal uri1 uri2) then
    Error "URI shorthand @ expansion failed"
  else
    Ok ()

let tests =
  Test.[
    case "URI interning" test_uri_interning;
    case "URI construction" test_uri_construction;
    case "URI shorthand expansion" test_uri_shorthand;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"poneglyph/uri" ~tests ~args)
    ~args:Env.args ()
