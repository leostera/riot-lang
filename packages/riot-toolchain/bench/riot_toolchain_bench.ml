open Std

let sample_ocaml_warning =
  String.concat
    "\n"
    [
      "File \"/tmp/sandbox/pkg/src/install.ml\", line 25, characters 8-26:";
      "25 |     let displayed_packages = HashMap.create () in";
      "             ^^^^^^^^^^^^^^^^^^";
      "Warning 26 [unused-var]: unused variable displayed_packages.";
    ]

let sample_colored_ocaml_warning =
  String.concat
    "\n"
    [
      "\027[1mFile \"/tmp/sandbox/pkg/src/install.ml\", line 25, characters 8-26\027[0m:";
      "25 |     let displayed_packages = HashMap.create () in";
      "             \027[1;35m^^^^^^^^^^^^^^^^^^\027[0m";
      "\027[1;35mWarning\027[0m 26 [unused-var]: unused variable \027[1mdisplayed_packages\027[0m.";
    ]

let sample_c_error =
  "/tmp/sandbox/pkg/native/kernel_uuid.c:14:10: error: uuid/uuid.h: No such file or directory"

let bench_compile_impl_to_string = fun () ->
  let ocamlc = Riot_toolchain.Ocamlc.make (Path.v "/tmp/ocamlopt.opt") in
  let invocation =
    Riot_toolchain.Ocamlc.compile_impl
      ocamlc
      ~cwd:(Path.v "/tmp/work")
      ~includes:[ Path.v "src"; Path.v "_build/generated"; Path.v "vendor/lib" ]
      ~flags:[
        Riot_toolchain.Ocamlc.NoPervasives;
        Riot_toolchain.Ocamlc.NoStdlib;
        Riot_toolchain.Ocamlc.Inline 0;
        Riot_toolchain.Ocamlc.WarnError [
          Riot_toolchain.Ocamlc.PartialMatch;
          Riot_toolchain.Ocamlc.UnusedVariable;
        ];
        Riot_toolchain.Ocamlc.Raw "-O2";
      ]
      ~output:(Path.v "foo.cmx")
      (Path.v "src/foo.ml")
  in
  let _ = Riot_toolchain.Ocamlc.to_string invocation in
  ()

let bench_parse_ocaml_warning = fun () ->
  let _ = Riot_toolchain.Ocamlc.Diagnostic.parse sample_ocaml_warning in
  ()

let bench_parse_colored_ocaml_warning = fun () ->
  let _ = Riot_toolchain.Ocamlc.Diagnostic.parse sample_colored_ocaml_warning in
  ()

let bench_parse_c_error = fun () ->
  let _ = Riot_toolchain.Ocamlc.Diagnostic.parse sample_c_error in
  ()

let medium: Bench.bench_config = { iterations = 300; warmup = 30 }

let benchmarks =
  Bench.[
    with_config ~config:medium "riot-toolchain compile_impl to_string" bench_compile_impl_to_string;
    with_config ~config:medium "riot-toolchain parse ocaml warning" bench_parse_ocaml_warning;
    with_config
      ~config:medium
      "riot-toolchain parse colored ocaml warning"
      bench_parse_colored_ocaml_warning;
    with_config ~config:medium "riot-toolchain parse c error" bench_parse_c_error;
  ]

let main ~args = Bench.Cli.main ~name:"riot-toolchain benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
