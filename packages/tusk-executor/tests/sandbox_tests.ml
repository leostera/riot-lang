open Std
module Test = Std.Test

let test_with_sandbox_copies_inputs () =
  match
    Fs.with_tempdir ~prefix:"sandbox_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let input1 = Path.(tmpdir / Path.v "input1.txt") in
        let input2 = Path.(tmpdir / Path.v "input2.txt") in
        let _ =
          Fs.write "content1" input1 |> Result.expect ~msg:"Write failed"
        in
        let _ =
          Fs.write "content2" input2 |> Result.expect ~msg:"Write failed"
        in

        Tusk_executor.Sandbox.with_sandbox ~workspace ~inputs:[ input1; input2 ]
          ~expected_outputs:[] (fun sandbox ->
            let sandbox_dir = Tusk_executor.Sandbox.get_dir sandbox in
            let copied1 = Path.(sandbox_dir / Path.v "input1.txt") in
            let copied2 = Path.(sandbox_dir / Path.v "input2.txt") in

            let exists1 =
              Fs.exists copied1 |> Result.unwrap_or ~default:false
            in
            let exists2 =
              Fs.exists copied2 |> Result.unwrap_or ~default:false
            in

            if exists1 && exists2 then
              match (Fs.read copied1, Fs.read copied2) with
              | Ok c1, Ok c2
                when String.equal c1 "content1" && String.equal c2 "content2" ->
                  Ok ()
              | _ -> Error "Content mismatch"
            else Error "Inputs not copied to sandbox"))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_with_sandbox_verifies_outputs () =
  match
    Fs.with_tempdir ~prefix:"sandbox_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let output1 = Path.v "output1.txt" in
        let output2 = Path.v "output2.txt" in

        Tusk_executor.Sandbox.with_sandbox ~workspace ~inputs:[]
          ~expected_outputs:[ output1; output2 ] (fun sandbox ->
            let sandbox_dir = Tusk_executor.Sandbox.get_dir sandbox in
            let out1_path = Path.(sandbox_dir / output1) in
            let out2_path = Path.(sandbox_dir / output2) in

            let _ =
              Fs.write "out1" out1_path |> Result.expect ~msg:"Write failed"
            in
            let _ =
              Fs.write "out2" out2_path |> Result.expect ~msg:"Write failed"
            in
            Ok ()))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_with_sandbox_fails_on_missing_output () =
  match
    Fs.with_tempdir ~prefix:"sandbox_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let output = Path.v "missing.txt" in

        try
          let _ =
            Tusk_executor.Sandbox.with_sandbox ~workspace ~inputs:[]
              ~expected_outputs:[ output ] (fun _sandbox -> ())
          in
          Error "Expected panic for missing output"
        with _ -> Ok ())
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_with_sandbox_cleans_up () =
  match
    Fs.with_tempdir ~prefix:"sandbox_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let sandbox_ref = Cell.create None in

        let _ =
          Tusk_executor.Sandbox.with_sandbox ~workspace ~inputs:[]
            ~expected_outputs:[] (fun sandbox ->
              let dir = Tusk_executor.Sandbox.get_dir sandbox in
              Cell.set sandbox_ref (Some dir);
              ())
        in

        match Cell.get sandbox_ref with
        | Some dir ->
            let exists = Fs.exists dir |> Result.unwrap_or ~default:true in
            if not exists then Ok ()
            else Error "Sandbox directory not cleaned up"
        | None -> Error "Sandbox directory not captured")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_with_sandbox_empty_inputs_outputs () =
  match
    Fs.with_tempdir ~prefix:"sandbox_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        Tusk_executor.Sandbox.with_sandbox ~workspace ~inputs:[]
          ~expected_outputs:[] (fun sandbox ->
            let dir = Tusk_executor.Sandbox.get_dir sandbox in
            let exists = Fs.exists dir |> Result.unwrap_or ~default:false in
            if exists then Ok () else Error "Sandbox directory not created"))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_with_sandbox_nested_input_paths () =
  match
    Fs.with_tempdir ~prefix:"sandbox_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        let nested_dir = Path.(tmpdir / Path.v "nested" / Path.v "deep") in
        let _ = Fs.create_dir_all nested_dir in
        let input = Path.(nested_dir / Path.v "file.txt") in
        let _ = Fs.write "nested" input |> Result.expect ~msg:"Write failed" in

        Tusk_executor.Sandbox.with_sandbox ~workspace ~inputs:[ input ]
          ~expected_outputs:[] (fun sandbox ->
            let sandbox_dir = Tusk_executor.Sandbox.get_dir sandbox in
            let copied =
              Path.(
                sandbox_dir / Path.v "nested" / Path.v "deep"
                / Path.v "file.txt")
            in

            let file_exists =
              Fs.exists copied |> Result.unwrap_or ~default:false
            in

            if file_exists then Ok ()
            else Error "Nested input not copied correctly"))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_sandbox_get_dir_returns_valid_path () =
  match
    Fs.with_tempdir ~prefix:"sandbox_test" (fun tmpdir ->
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [];
            }
        in
        Tusk_executor.Sandbox.with_sandbox ~workspace ~inputs:[]
          ~expected_outputs:[] (fun sandbox ->
            let dir = Tusk_executor.Sandbox.get_dir sandbox in
            let dir_str = Path.to_string dir in
            if String.length dir_str > 0 then Ok ()
            else Error "get_dir returned empty path"))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let tests =
  Test.
    [
      case "with_sandbox: copies inputs" test_with_sandbox_copies_inputs;
      case "with_sandbox: verifies outputs" test_with_sandbox_verifies_outputs;
      case "with_sandbox: fails on missing output"
        test_with_sandbox_fails_on_missing_output;
      case "with_sandbox: cleans up" test_with_sandbox_cleans_up;
      case "with_sandbox: empty inputs/outputs"
        test_with_sandbox_empty_inputs_outputs;
      case "with_sandbox: nested input paths"
        test_with_sandbox_nested_input_paths;
      case "get_dir: returns valid path" test_sandbox_get_dir_returns_valid_path;
    ]

let name = "Sandbox Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
