open Std
open Typ
open Typ.Analysis
open Typ.Diagnostics
open Typ.Model
open Typ.Session

let expect_cst = fun ~filename parse_result ->
  match Syn.build_cst parse_result with
  | Ok cst -> cst
  | Error (Syn.Parse_diagnostics diagnostics) ->
      panic
        ("expected successful CST for "
        ^ filename
        ^ " but parser reported diagnostics: "
        ^ String.concat "; " (List.map Syn.Diagnostic.to_string diagnostics))
  | Error (Syn.Cst_builder_error error) ->
      panic ("expected successful CST for " ^ filename ^ " but CST build failed: " ^ error.message)

let create_source = fun session ~kind ~origin ~text ->
  let filename =
    match origin with
    | Source.Path path -> path
    | Source.Label label -> Path.v label
  in
  let parse_result = Syn.parse ~filename text in
  let cst = expect_cst ~filename:(Path.to_string filename) parse_result in
  let implicit_opens = [] in
  Session.create_source
    session
    ~kind
    ~module_name:(Source.infer_module_name origin)
    ~implicit_opens
    ~origin
    ~source_hash:(Source.hash ~implicit_opens ~cst)
    ~parse_result
    ~cst

let summary_pair_for = fun source ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) =
    create_source
      session
      ~kind:Source.File
      ~origin:(Source.Label "persisted_summary.ml")
      ~text:source
  in
  let snapshot = Session.snapshot session in
  match (Query.file_summary_of snapshot source_id, Query.module_typings_of snapshot source_id) with
  | (Some summary, Some typings) -> Ok (summary, typings)
  | (None, _) -> Error "expected file summary for analyzed source"
  | (_, None) -> Error "expected module typings for analyzed source"

let assert_roundtrip = fun ~ctx source ->
  match summary_pair_for source with
  | Error _ as err -> err
  | Ok (summary, typings) ->
      let actual_json = ModuleTypings.Json.to_json typings in
      begin
        match ModuleTypings.Json.of_json actual_json with
        | Error _ as err -> err
        | Ok decoded ->
            let roundtripped_summary =
              ModuleTypings.to_file_summary ~source_id:summary.source_id decoded
            in
            let roundtripped_persisted_json = ModuleTypings.Json.to_json decoded in
            let roundtripped_json = FileSummary.to_json roundtripped_summary in
            let original_json = FileSummary.to_json summary in
            if not (actual_json = roundtripped_persisted_json) then
              Error ("module typings roundtrip changed module typings json\noriginal:\n"
              ^ Data.Json.to_string_pretty actual_json
              ^ "\nroundtripped:\n"
              ^ Data.Json.to_string_pretty roundtripped_persisted_json)
            else if not (original_json = roundtripped_json) then
              Error ("module typings roundtrip changed file summary\noriginal:\n"
              ^ Data.Json.to_string_pretty original_json
              ^ "\nroundtripped:\n"
              ^ Data.Json.to_string_pretty roundtripped_json)
            else
              Test.Snapshot.assert_json
                ~ctx
                ~actual:(Data.Json.Object [
                  ("module_typings", actual_json);
                  ("file_summary", original_json);
                ])
      end

let test_trusted_summary_roundtrip = fun ctx ->
  assert_roundtrip
    ~ctx
    "let id x = x\nlet const x _ = x\n"

let test_errored_summary_roundtrip = fun ctx -> assert_roundtrip ~ctx "let broken = missing\n"

let test_type_decl_summary_roundtrip = fun ctx ->
  assert_roundtrip
    ~ctx
    "type point = { x: int; y: int }\nlet origin = { x = 0; y = 0 }\n"

let test_recursive_type_decl_summary_roundtrip = fun ctx ->
  assert_roundtrip
    ~ctx
    "type inline_node =\n\
    \  | Text of string\n\
    \  | Emphasis of inline_node list\n"

let main ~args =
  let tests = [
    Test.case "trusted summary roundtrips through persisted json" test_trusted_summary_roundtrip;
    Test.case "errored summary roundtrips through persisted json" test_errored_summary_roundtrip;
    Test.case "type declarations roundtrip through persisted json" test_type_decl_summary_roundtrip;
    Test.case
      "recursive type declarations roundtrip through persisted json"
      test_recursive_type_decl_summary_roundtrip;
  ]
  in
  Test.Cli.main ~name:"typ:module_typings" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
