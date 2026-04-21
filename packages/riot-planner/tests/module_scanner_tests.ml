open Std
module Test = Std.Test

let rec flatten_entries = fun entries ->
  List.flat_map entries
    ~fn:(
      function
      | Riot_planner.Module_scanner.ML _ as entry -> [ entry ]
      | MLI _ as entry -> [ entry ]
      | C _ as entry -> [ entry ]
      | H _ as entry -> [ entry ]
      | Other _ as entry -> [ entry ]
      | Dir (_, _, children) as entry -> entry :: flatten_entries children
    )

let test_scan_tags_c_and_h_files = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"module_scanner_test"
      (fun tmpdir ->
        let src_dir = Path.(tmpdir / Path.v "src") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"create src failed" in
        let _ = Fs.write "int stub(void) { return 0; }" Path.(src_dir / Path.v "stubs.c")
        |> Result.expect ~msg:"write c source failed" in
        let _ = Fs.write "#define STUBS_H" Path.(src_dir / Path.v "stubs.h") |> Result.expect ~msg:"write header failed" in
        let _ = Fs.write "let value = 1" Path.(src_dir / Path.v "lib.ml") |> Result.expect ~msg:"write ml source failed" in
        let entries = Riot_planner.Module_scanner.scan ~root:tmpdir ~source_dir:(Path.v "src") in
        let flat = flatten_entries entries in
        let has_c =
          List.any flat
            ~fn:(
              function
              | Riot_planner.Module_scanner.C (name, path) -> String.equal name "stubs.c"
              && Path.equal path Path.(Path.v "src/stubs.c")
              | _ -> false
            )
        in
        let has_h =
          List.any flat
            ~fn:(
              function
              | Riot_planner.Module_scanner.H (name, path) -> String.equal name "stubs.h"
              && Path.equal path Path.(Path.v "src/stubs.h")
              | _ -> false
            )
        in
        if has_c && has_h then
          Ok ()
        else
          Error "scanner did not tag .c and .h files")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let tests = Test.[ case "scan tags C and H files" test_scan_tags_c_and_h_files; ]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"module_scanner_tests" ~tests ~args ())
    ~args:Env.args
    ()
