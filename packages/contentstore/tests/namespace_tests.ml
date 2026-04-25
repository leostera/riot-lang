open Std

module Test = Std.Test

let test_namespace_rejects_invalid_parts = fun _ctx ->
  match Contentstore.Namespace.from_parts [ "typ/module" ] with
  | Error (Contentstore.Namespace.Invalid_part "typ/module") -> Ok ()
  | Error err -> Error ("unexpected namespace error: " ^ Contentstore.Namespace.error_message err)
  | Ok _ -> Error "expected invalid namespace part to be rejected"

let test_namespace_rejects_empty_part = fun _ctx ->
  match Contentstore.Namespace.from_parts [ "typ"; "" ] with
  | Error Contentstore.Namespace.Empty_part -> Ok ()
  | Error err -> Error ("unexpected namespace error: " ^ Contentstore.Namespace.error_message err)
  | Ok _ -> Error "expected empty namespace part to be rejected"

let test_namespace_rejects_dot_part = fun _ctx ->
  match Contentstore.Namespace.from_parts [ "." ] with
  | Error (Contentstore.Namespace.Invalid_part ".") -> Ok ()
  | Error err -> Error ("unexpected namespace error: " ^ Contentstore.Namespace.error_message err)
  | Ok _ -> Error "expected dot namespace part to be rejected"

let test_namespace_rejects_dotdot_part = fun _ctx ->
  match Contentstore.Namespace.from_parts [ ".." ] with
  | Error (Contentstore.Namespace.Invalid_part "..") -> Ok ()
  | Error err -> Error ("unexpected namespace error: " ^ Contentstore.Namespace.error_message err)
  | Ok _ -> Error "expected dotdot namespace part to be rejected"

let test_namespace_roundtrips_parts = fun _ctx ->
  let ns = Contentstore.Namespace.from_parts [ "typ"; "modules" ] |> Result.expect ~msg:"namespace should be valid" in
  match Contentstore.Namespace.parts ns with
  | [ "typ"; "modules" ] -> Ok ()
  | _ -> Error "expected namespace to preserve its validated parts"

let test_namespace_accepts_unicode_parts = fun _ctx ->
  let ns = Contentstore.Namespace.from_parts [ "módulos"; "東京" ] |> Result.expect ~msg:"unicode namespace should be valid" in
  match Contentstore.Namespace.parts ns with
  | [ "módulos"; "東京" ] -> Ok ()
  | _ -> Error "expected namespace to preserve unicode parts"

let tests =
  [
    Test.case "namespace rejects invalid parts" test_namespace_rejects_invalid_parts;
    Test.case "namespace rejects empty parts" test_namespace_rejects_empty_part;
    Test.case "namespace rejects dot part" test_namespace_rejects_dot_part;
    Test.case "namespace rejects dotdot part" test_namespace_rejects_dotdot_part;
    Test.case "namespace preserves validated parts" test_namespace_roundtrips_parts;
    Test.case "namespace accepts unicode parts" test_namespace_accepts_unicode_parts;
  ]

let main ~args = Test.Cli.main ~name:"contentstore_namespace_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
