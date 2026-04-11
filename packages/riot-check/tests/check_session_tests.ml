open Std
open Riot_check
module Test = Std.Test
module Check_session = Riot_check__Check__Session

let expect_cst = fun ~filename parse_result ->
  match Syn.build_cst parse_result with
  | Ok cst -> cst
  | Error (Syn.Parse_diagnostics diagnostics) -> panic
    (format
      Format.[
        str "expected successful CST for ";
        str filename;
        str " but parser reported diagnostics: ";
        str (String.concat "; " (List.map Syn.Diagnostic.to_string diagnostics));
      ])
  | Error (Syn.Cst_builder_error error) -> panic
    (format
      Format.[
        str "expected successful CST for ";
        str filename;
        str " but CST build failed: ";
        str error.message;
      ])

let make_package_typ_source = fun ~filename ~module_name ~text ->
  let path = Path.v filename in
  let parse_result = Syn.parse ~filename:path text in
  let source_id = Typ.Model.SourceId.of_int 0 in
  let origin = Typ.Model.Source.Label filename in
  let cst = expect_cst ~filename parse_result in
  let source = Typ.Model.Source.make_prepared
    ~source_id
    ~kind:Typ.Model.Source.File
    ~module_name
    ~implicit_opens:[]
    ~origin
    ~revision:0
    ~source_hash:(Typ.Model.Source.hash ~implicit_opens:[] ~cst)
    ~parse_result
    ~cst in
  Ok Check_session.{
    internal_module_name = module_name;
    local_module_name = module_name;
    public_module_name = None;
    display_path = path;
    source_id;
    source;
  }

let test_rooted_reconstruction_emits_rooted_engine_event = fun _ctx ->
  match make_package_typ_source ~filename:"single.ml" ~module_name:"Single" ~text:"let value = 42\n" with
  | Error _ as err -> err
  | Ok source ->
      let events = ref [] in
      let _checked_group = Check_session.checked_group_for_ordered_sources_via_rooted_sessions
        ~on_event:(fun event -> events := event :: !events)
        ~package_name:"demo"
        ~group_targets:true
        Typ.Config.default
        [ source ]
        [ Path.v "single.ml" ] in
      let engine_event = !events
      |> List.rev
      |> List.find_opt
        (
          function
          | Check.Event.PackageEngineSelected _ -> true
          | _ -> false
        )
      |> Option.expect ~msg:"missing package engine event" in
      match engine_event with
      | Check.Event.PackageEngineSelected { package_name; engine } ->
          Test.assert_equal ~expected:"demo" ~actual:package_name;
          Test.assert_equal ~expected:Check.Event.RootedSnapshotReconstruction ~actual:engine;
          Ok ()
      | _ -> Error "unexpected non-engine event"

let tests = [
  Test.case
    "rooted reconstruction emits rooted package engine event"
    test_rooted_reconstruction_emits_rooted_engine_event;
]

let name = "Riot Check Session Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
