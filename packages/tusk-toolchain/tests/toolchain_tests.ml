open Std
module Test = Std.Test

let test_compile_impl_disables_no_cmi_file_by_default = fun () ->
  let ocamlc = Tusk_toolchain.Ocamlc.make (Path.v "/tmp/ocamlopt.opt") in
  let invocation =
    Tusk_toolchain.Ocamlc.compile_impl
      ocamlc
      ~cwd:(Path.v "/tmp/work")
      ~includes:[ Path.v "src" ]
      ~flags:[]
      ~output:(Path.v "foo.cmx")
      (Path.v "src/foo.ml")
  in
  let command = Tusk_toolchain.Ocamlc.to_string invocation in
  if String.contains command "-w -49" then
    Ok ()
  else
    Error ("expected default warning baseline to disable warning 49, got: " ^ command)

let tests =
  Test.[
    case
      "compile impl disables no-cmi-file by default"
      test_compile_impl_disables_no_cmi_file_by_default;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"toolchain_tests" ~tests ~args)
    ~args:Env.args
    ()
