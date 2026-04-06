open Std
module Test = Std.Test

let ( let* ) = Result.and_then

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let test_fs_write_roundtrips_large_binary_payload = fun _ctx ->
  with_tempdir "std_fs_write"
    (fun tempdir ->
      let path = Path.(tempdir / Path.v "payload.bin") in
      let payload =
        String.init (1024 * 1024)
          (fun idx -> Char.unsafe_chr (idx mod 256))
      in
      let* () = Fs.write payload path |> Result.map_error IO.error_message in
      let* actual = Fs.read_to_string path |> Result.map_error IO.error_message in
      if String.equal actual payload then
        Ok ()
      else
        Error "expected Fs.write to persist the full payload")

let tests = [
  Test.case "Fs.write persists complete payloads" test_fs_write_roundtrips_large_binary_payload;
]

let main = fun ~args -> Test.Cli.main ~name:"std_fs_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
