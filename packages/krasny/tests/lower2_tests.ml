open Std
open Std.Collections

let sample_ml = Path.v "sample.ml"

let sample_mli = Path.v "sample.mli"

let source_slice = fun source ->
  match IO.IoVec.IoSlice.from_string source with
  | Ok slice -> slice
  | Error error -> panic ("failed to create source slice: " ^ Kernel.IO.Error.message error)

let parse2_ml = fun source -> Syn.parse2 ~filename:sample_ml (source_slice source)

let parse2_mli = fun source -> Syn.parse2 ~filename:sample_mli (source_slice source)

let format2_ml = fun source -> parse2_ml source |> Krasny.format2

let format2_mli = fun source -> parse2_mli source |> Krasny.format2

let parse2_source = fun ~filename source -> Syn.parse2 ~filename (source_slice source)

let format2_source = fun ~filename source -> parse2_source ~filename source |> Krasny.format2

let top_level = fun items -> String.concat "\n\n" items ^ "\n"

let assert_format2_ml = fun ~expected source ->
  let actual = format2_ml source |> Result.expect ~msg:"implementation should format through lower2" in
  if expected = actual then
    Ok ()
  else
    Error ("lower2 implementation output mismatch\nexpected:\n" ^ expected ^ "\nactual:\n" ^ actual)

let assert_format2_mli = fun ~expected source ->
  let actual = format2_mli source |> Result.expect ~msg:"interface should format through lower2" in
  if expected = actual then
    Ok ()
  else
    Error ("lower2 interface output mismatch\nexpected:\n" ^ expected ^ "\nactual:\n" ^ actual)

let assert_format2_ml_fails = fun source ->
  match format2_ml source with
  | Ok formatted -> Error ("lower2 unexpectedly formatted unsupported source as:\n" ^ formatted)
  | Error _ -> Ok ()

let approved_snapshot_path = fun path ->
  match Path.extension path with
  | Some ext -> Path.add_extension path ~ext:(ext ^ ".expected")
  | None -> Path.add_extension path ~ext:"expected"

let assert_lower2_fixture_matches_approved = fun path ->
  let source = Fs.read path |> Result.expect ~msg:"fixture file should exist" in
  match format2_source ~filename:path source with
  | Error err -> Error (Path.to_string path
  ^ " failed lower2 formatting: "
  ^ Krasny.format_error_to_string err)
  | Ok formatted -> (
      let expected_path = approved_snapshot_path path in
      let expected = Fs.read expected_path |> Result.expect ~msg:"approved fixture snapshot should exist" in
      if not (String.equal expected formatted) then
        Error (Path.to_string path
        ^ " lower2 output did not match approved formatter snapshot\nexpected:\n"
        ^ expected
        ^ "\nactual:\n"
        ^ formatted)
      else
        match format2_source ~filename:path formatted with
        | Error err -> Error (Path.to_string path
        ^ " formatted once but failed to format again: "
        ^ Krasny.format_error_to_string err)
        | Ok reformatted ->
            if formatted = reformatted then
              Ok ()
            else
              Error (Path.to_string path
              ^ " is not lower2-idempotent after one format\nfirst:\n"
              ^ formatted
              ^ "\nsecond:\n"
              ^ reformatted)
    )

let assert_lower2_existing_fixture_subset = fun () ->
  let fixtures = [
    Path.v "packages/krasny/tests/fixtures/0100_atoms_and_basic_expressions.ml";
    Path.v "packages/krasny/tests/fixtures/0200_operators_and_parens.ml";
    Path.v "packages/krasny/tests/fixtures/0300_bindings_and_control_flow.ml";
    Path.v "packages/krasny/tests/fixtures/0400_functions_match_and_patterns.ml";
    Path.v "packages/krasny/tests/fixtures/0500_labeled_and_optional_arguments.ml";
    Path.v "packages/krasny/tests/fixtures/0700_types_and_type_declarations.ml";
    Path.v "packages/krasny/tests/fixtures/0710_first_class_module_types.ml";
    Path.v "packages/krasny/tests/fixtures/0720_signature_external_declaration.mli";
    Path.v "packages/krasny/tests/fixtures/0721_poly_variant_inherit_type_alias.mli";
    Path.v "packages/krasny/tests/fixtures/0722_poly_variant_union_type_alias.ml";
    Path.v "packages/krasny/tests/fixtures/0723_variant_constructor_poly_variant_payload.ml";
    Path.v "packages/krasny/tests/fixtures/0724_type_extension_poly_variant_payload.ml";
    Path.v "packages/krasny/tests/fixtures/0725_signature_type_alias_docstring.mli";
    Path.v "packages/krasny/tests/fixtures/0726_signature_consecutive_type_aliases.mli";
    Path.v "packages/krasny/tests/fixtures/0727_signature_abstract_type_then_value_docstrings.mli";
    Path.v "packages/krasny/tests/fixtures/0728_external_declaration_attribute.ml";
    Path.v "packages/krasny/tests/fixtures/0729_shortcut_extension_declaration_items.ml";
    Path.v "packages/krasny/tests/fixtures/0730_signature_operator_value_declarations.mli";
    Path.v "packages/krasny/tests/fixtures/0731_signature_docstring_after_open.mli";
    Path.v "packages/krasny/tests/fixtures/0732_signature_section_between_value_docstrings.mli";
    Path.v "packages/krasny/tests/fixtures/0733_signature_value_docstring_then_section_then_type_docstrings.mli";
    Path.v "packages/krasny/tests/fixtures/0734_signature_type_trailing_doc_then_heading.mli";
  ]
  in
  let rec loop = function
    | [] -> Ok ()
    | path :: rest -> (
        match assert_lower2_fixture_matches_approved path with
        | Ok () -> loop rest
        | Error _ as err -> err
      )
  in
  loop fixtures

let tests = [
  Test.case
    "lower2 keeps empty implementations empty"
    (fun _ctx -> assert_format2_ml ~expected:"" "");
  Test.case
    "lower2 formats simple let bindings"
    (fun _ctx -> assert_format2_ml ~expected:"let x = 1 + 2\n" "let x = 1 + 2\n");
  Test.case
    "lower2 formats parameterized let bindings"
    (fun _ctx -> assert_format2_ml ~expected:"let id x = x\n" "let id x = x\n");
  Test.case
    "lower2 formats typed let binding heads"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [ "let value: int = 1"; "let id x: int = x"; "let keep_pattern (x: int) = x" ])
        "let value : int = 1\nlet id x : int = x\nlet keep_pattern (x : int) = x\n");
  Test.case
    "lower2 formats mutual recursive let bindings"
    (fun _ctx -> assert_format2_ml ~expected:"let rec f = g and g = f\n" "let rec f = g\nand g = f\n");
  Test.case
    "lower2 formats local let expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let x =\n  let y = 1 in\n  y\n" "let x = let y = 1 in y\n");
  Test.case
    "lower2 formats function expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let id = fun x -> x\n" "let id = fun x -> x\n");
  Test.case
    "lower2 formats match expressions"
    (fun _ctx ->
      assert_format2_ml ~expected:"let value =\n  match x with\n  | 0 -> 1\n  | _ -> 2\n" "let value = match x with | 0 -> 1 | _ -> 2\n");
  Test.case
    "lower2 formats sequence expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let run =\n  first;\n  second\n" "let run = first; second\n");
  Test.case
    "lower2 formats list and array expressions"
    (fun _ctx ->
      assert_format2_ml ~expected:(top_level [ "let values = [1; 2]"; "let array = [|1; 2|]" ]) "let values = [1; 2]\nlet array = [|1; 2|]\n");
  Test.case
    "lower2 preserves parens around function application arguments"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let folded = List.fold_left (fun acc doc -> (indent, doc) :: acc) rest\n"
        "let folded = List.fold_left (fun acc doc -> (indent, doc) :: acc) rest\n");
  Test.case
    "lower2 formats labels and optional labels"
    (fun _ctx -> assert_format2_ml ~expected:"let f ~x ?y = g ~x ?y\n" "let f ~x ?y = g ~x ?y\n");
  Test.case
    "lower2 formats polymorphic variants"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [ "let ok = `Ok 1"; "let classify = function\n  | `Ok value -> value\n  | `Error -> 0" ])
        "let ok = `Ok 1\nlet classify = function | `Ok value -> value | `Error -> 0\n");
  Test.case
    "lower2 formats expression and pattern attributes"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level [ "let value = target [@inline always]"; "let (x [@foo]) = value" ])
        "let value = target [@inline always]\nlet (x [@foo]) = value\n");
  Test.case
    "lower2 formats expression pattern and item extensions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let value = [%expr payload]";
            "let [%pat payload] = value";
            "[%%item payload]";
            "[@@@warning \"-32\"]";
          ])
        "let value = [%expr payload]\nlet [%pat payload] = value\n[%%item payload]\n[@@@warning \"-32\"]\n");
  Test.case
    "lower2 formats signature extension and attribute items"
    (fun _ctx ->
      assert_format2_mli
        ~expected:(top_level [ "[%%foo payload]"; "[@@@warning \"-32\"]"; "val id: int" ])
        "[%%foo payload]\n[@@@warning \"-32\"]\nval id : int\n");
  Test.case
    "lower2 formats selectors and index expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [ "let field = value.name"; "let item = values.(index)"; "let char = text.[index]" ])
        "let field = value.name\nlet item = values.(index)\nlet char = text.[index]\n");
  Test.case
    "lower2 formats record expressions and patterns"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let record = { x = 1; y }";
            "let updated = { base with x = 2; y }";
            "let { x; y = z; _ } = record";
          ])
        "let record = { x = 1; y }\nlet updated = { base with x = 2; y }\nlet { x; y = z; _ } = record\n");
  Test.case
    "lower2 formats binding operator expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let value = let* x = fetch in let+ y = decode in pair x y";
            "let both = let+ x = a and+ y = b in pair x y";
          ])
        "let value = let* x = fetch in let+ y = decode in pair x y\nlet both = let+ x = a and+ y = b in pair x y\n");
  Test.case
    "lower2 formats local open expressions and patterns"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level [ "let value = let open Foo.Bar in result"; "let Foo.Bar.(x) = value" ])
        "let value = let open Foo.Bar in result\nlet Foo.Bar.(x) = value\n");
  Test.case
    "lower2 formats first-class module expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level [ "let packed = (module Foo.Bar)"; "let typed = (module Foo : S.T)" ])
        "let packed = (module Foo.Bar)\nlet typed = (module Foo : S.T)\n");
  Test.case
    "lower2 formats locally abstract and first-class module patterns"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [ "let f (type a b) (module M : S.T) = value"; "let g (module _) = value" ])
        "let f (type a b) (module M : S.T) = value\nlet g (module _) = value\n");
  Test.case
    "lower2 formats let module expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let value = let module M = Foo.Bar in result";
            "let empty = let module Empty = struct end in done_";
          ])
        "let value = let module M = Foo.Bar in result\nlet empty = let module Empty = struct end in done_\n");
  Test.case
    "lower2 formats let exception expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let value = let exception Local of int * Foo.t in result";
            "let bare = let exception Done in done_";
          ])
        "let value = let exception Local of int * Foo.t in result\nlet bare = let exception Done in done_\n");
  Test.case
    "lower2 formats unreachable expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:"let value =\n  match maybe with\n  | Some value -> value\n  | None -> .\n"
        "let value = match maybe with | Some value -> value | None -> .\n");
  Test.case
    "lower2 formats assertion and lazy expressions"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level [ "let _ = assert ready"; "let later = lazy compute" ])
        "let _ = assert ready\nlet later = lazy compute\n");
  Test.case
    "lower2 formats try expressions"
    (fun _ctx -> assert_format2_ml ~expected:"let value =\n  try read () with\n  | Failure -> 0\n" "let value = try read () with | Failure -> 0\n");
  Test.case
    "lower2 formats while and for loops"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let poll = while ready do step () done";
            "let up = for i = 0 to n do step i done";
            "let down = for i = n downto 0 do step i done";
          ])
        "let poll = while ready do step () done\nlet up = for i = 0 to n do step i done\nlet down = for i = n downto 0 do step i done\n");
  Test.case
    "lower2 formats lazy exception and interval patterns"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "let force = function\n  | lazy value -> value";
            "let recovered =\n  match read () with\n  | exception Failure -> 0\n  | value -> value";
            "let classify = function\n  | 'a' .. 'z' -> 1\n  | _ -> 0";
          ])
        "let force = function | lazy value -> value\nlet recovered = match read () with | exception Failure -> 0 | value -> value\nlet classify = function | 'a' .. 'z' -> 1 | _ -> 0\n");
  Test.case
    "lower2 adds a final newline"
    (fun _ctx -> assert_format2_ml ~expected:"let x = 1\n" "let x = 1");
  Test.case
    "lower2 formats open declarations"
    (fun _ctx -> assert_format2_ml ~expected:"open Foo.Bar\n" "open Foo.Bar\n");
  Test.case
    "lower2 formats simple include external and exception declarations"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "include Foo.Bar";
            "external id: 'a -> 'a = \"%identity\" \"caml_id\"";
            "exception Boom"
          ])
        "include Foo.Bar\nexternal id : 'a -> 'a = \"%identity\" \"caml_id\"\nexception Boom\n");
  Test.case
    "lower2 formats simple module and module type declarations"
    (fun _ctx ->
      assert_format2_ml
        ~expected:(top_level
          [
            "module Alias = Foo.Bar";
            "module Empty = struct end";
            "module type S = Foo.S";
            "module type Empty = sig end";
          ])
        "module Alias = Foo.Bar\nmodule Empty = struct end\nmodule type S = Foo.S\nmodule type Empty = sig end\n");
  Test.case
    "lower2 formats simple signature module declarations"
    (fun _ctx ->
      assert_format2_mli
        ~expected:(top_level
          [
            "module Alias: Foo.S";
            "module Empty: sig end";
            "module type S = Foo.S";
            "module type Abstract"
          ])
        "module Alias : Foo.S\nmodule Empty : sig end\nmodule type S = Foo.S\nmodule type Abstract\n");
  Test.case
    "lower2 formats type aliases with parameters"
    (fun _ctx -> assert_format2_mli ~expected:"type 'a t = 'a list\n" "type 'a t = 'a list\n");
  Test.case
    "lower2 formats tuple type separators structurally"
    (fun _ctx ->
      assert_format2_mli
        ~expected:"type ('a, 'e) result_like = ('a, 'e) result\ntype pair = int * string\n"
        "type ('a, 'e) result_like = ('a, 'e) result\ntype pair = int * string\n");
  Test.case
    "lower2 formats simple value declarations"
    (fun _ctx -> assert_format2_mli ~expected:"val id: 'a -> 'a\n" "val id : 'a -> 'a\n");
  Test.case
    "lower2 rejects unsupported shapes instead of replaying source"
    (fun _ctx -> assert_format2_ml_fails "let object_value = object end\n");
  Test.case
    "lower2 formats a selected existing fixture subset idempotently"
    (fun _ctx -> assert_lower2_existing_fixture_subset ());
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"krasny:lower2" ~tests ~args ())
    ~args:Env.args
    ()
