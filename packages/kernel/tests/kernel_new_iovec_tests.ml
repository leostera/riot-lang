open Std
module Test = Std.Test
module Kernel = Kernel

let test_iovec_roundtrips_string_payload = fun _ctx ->
  let iovec = Kernel.IO.Iovec.of_string_array [|"hello"; " "; "riot"|] in
  if Kernel.String.equal (Kernel.IO.Iovec.into_string iovec) "hello riot" then
    Ok ()
  else
    Error "expected iovec string roundtrip to preserve segment order"

let test_iovec_sub_slices_segments = fun _ctx ->
  let iovec = Kernel.IO.Iovec.of_string_array [|"hello"; " "; "riot"|] in
  let actual = Kernel.IO.Iovec.sub ~pos:3 ~len:5 iovec |> Kernel.IO.Iovec.into_string in
  if Kernel.String.equal actual "lo ri" then
    Ok ()
  else
    Error "expected iovec slicing to preserve partial segment boundaries"

let tests = [
  Test.case "Iovec string roundtrips preserve payload" test_iovec_roundtrips_string_payload;
  Test.case "Iovec sub slices across segment boundaries" test_iovec_sub_slices_segments;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_iovec_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
