open Std
module Test = Std.Test

let test_kernel_format_concatenates_primitives = fun _ctx ->
  let rendered = Kernel.format
    Kernel.Format.[str "hello ";
    int 2_112;
    str " ";
    int32 7l;
    str " ";
    int64 42L;
    str " ";
    float 3.5;
    str " ";
    bool false;
    str " ";
    bytes (Stdlib.Bytes.of_string "ok");
    char '!';]
  in
  Test.assert_equal ~expected:"hello 2112 7 42 3.5 false ok!" ~actual:rendered;
  Ok ()

let test_kernel_format_supports_uchar = fun _ctx ->
  let rendered = Kernel.format Kernel.Format.[uchar (Stdlib.Uchar.of_int 0x41)] in
  Test.assert_equal ~expected:"A" ~actual:rendered;
  Ok ()

let test_std_global_reexports_kernel_format = fun _ctx ->
  let rendered = format Format.[ str "TODO: "; int 7 ] in
  Test.assert_equal ~expected:"TODO: 7" ~actual:rendered;
  Ok ()

let tests = [
  Test.case "kernel format concatenates primitive values" test_kernel_format_concatenates_primitives;
  Test.case "kernel format supports uchar fragments" test_kernel_format_supports_uchar;
  Test.case "std global reexports kernel format" test_std_global_reexports_kernel_format;
]

let main = fun ~args -> Test.Cli.main ~name:"format_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
