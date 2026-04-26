open Std

module Test = Std.Test
module Kernel = Kernel

let test_iovec_roundtrips_string_payload = fun _ctx ->
  let iovec =
    Kernel.IO.IoVec.from_string_array [|"hello"; " "; "riot"|]
    |> Result.unwrap
  in
  if Kernel.String.equal (Kernel.IO.IoVec.to_string iovec) "hello riot" then
    Ok ()
  else
    Error "expected iovec string roundtrip to preserve segment order"

let test_iovec_sub_slices_segments = fun _ctx ->
  let iovec =
    Kernel.IO.IoVec.from_string_array [|"hello"; " "; "riot"|]
    |> Result.unwrap
  in
  let actual =
    Kernel.IO.IoVec.sub ~pos:3 ~len:5 iovec
    |> Result.unwrap
    |> Kernel.IO.IoVec.to_string
  in
  if Kernel.String.equal actual "lo ri" then
    Ok ()
  else
    Error "expected iovec slicing to preserve partial segment boundaries"

let tests = [
  Test.case "IoVec string roundtrips preserve payload" test_iovec_roundtrips_string_payload;
  Test.case "IoVec sub slices across segment boundaries" test_iovec_sub_slices_segments;
]

let main ~args = Test.Cli.main ~name:"kernel_new_iovec_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
