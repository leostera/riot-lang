open Std
open Std.Result.Syntax

let starter_package_manifest = fun ~package_name ~module_file_stem ~is_library ->
  let lib_or_bin_section =
    if is_library then
      "[lib]\npath = \"src/" ^ module_file_stem ^ ".ml\"\n"
    else
      "[[bin]]\nname = \"" ^ package_name ^ "\"\npath = \"src/main.ml\"\n"
  in
  {|[package]
name = "|}
  ^ package_name
  ^ {|"
version = "0.1.0"

|}
  ^ lib_or_bin_section
  ^ {|
[dependencies]
std = "*"
|}

let library_ml = fun ~workspace_name ->
  "open Std\n\n(** Return the starter greeting for "
  ^ workspace_name
  ^ ". *)\nlet hello = fun () -> \"Hello from "
  ^ workspace_name
  ^ "\"\n"

let library_mli = fun ~workspace_name ->
  "(** Return the starter greeting for " ^ workspace_name ^ ". *)\nval hello: unit -> string\n"

let binary_main_ml = fun ~module_name ->
  "open Std\n\nlet main ~args:_ =\n  Std.Config.load ();\n  Std.Log.set_level Info;\n  let _ = Std.Log.start_link () in\n  println ("
  ^ module_name
  ^ ".hello ());\n  Ok ()\n\nlet () = Runtime.run ~main ~args:Env.args ()\n"

let test_ml = fun ~workspace_name ~module_name ~test_file_stem ->
  "open Std\n\nlet test_starter_greeting = fun _ctx ->\n  Test.assert_equal ~expected:\"Hello from "
  ^ workspace_name
  ^ "\" ~actual:("
  ^ module_name
  ^ ".hello ());\n  Ok ()\n\nlet tests =\n  Test.[ case \"starter greeting\" test_starter_greeting ]\n\nlet main ~args =\n  Test.Cli.main ~name:\""
  ^ test_file_stem
  ^ "\" ~tests ~args ()\n\nlet () = Runtime.run ~main ~args:Env.args ()\n"

let write = fun config ~relative_path ~content ->
  Writer.write_file
    config
    ~emit:false
    ~relative_path
    ~content
    ~executable:false

let materialize = fun (config: Context.t) ->
  let module_name = Names.package_name_to_module_name config.package_name in
  let module_file_stem = Names.module_name_to_file_stem module_name in
  let test_file_stem = Names.module_name_to_test_file_stem module_name in
  let package_root = "packages/" ^ config.package_name in
  let src_root = package_root ^ "/src" in
  let tests_root = package_root ^ "/tests" in
  let* () =
    write
      config
      ~relative_path:(package_root ^ "/riot.toml")
      ~content:(starter_package_manifest
        ~package_name:config.package_name
        ~module_file_stem
        ~is_library:config.is_library)
  in
  let* () =
    if config.is_library then
      let* () =
        write
          config
          ~relative_path:(src_root ^ "/" ^ module_file_stem ^ ".ml")
          ~content:(library_ml ~workspace_name:config.workspace_name)
      in
      write
        config
        ~relative_path:(src_root ^ "/" ^ module_file_stem ^ ".mli")
        ~content:(library_mli ~workspace_name:config.workspace_name)
    else
      let* () =
        write config ~relative_path:(src_root ^ "/main.ml") ~content:(binary_main_ml ~module_name)
      in
      let* () =
        write
          config
          ~relative_path:(src_root ^ "/" ^ module_file_stem ^ ".ml")
          ~content:(library_ml ~workspace_name:config.workspace_name)
      in
      write
        config
        ~relative_path:(src_root ^ "/" ^ module_file_stem ^ ".mli")
        ~content:(library_mli ~workspace_name:config.workspace_name)
  in
  let* () =
    write
      config
      ~relative_path:(tests_root ^ "/" ^ test_file_stem ^ ".ml")
      ~content:(test_ml ~workspace_name:config.workspace_name ~module_name ~test_file_stem)
  in
  Writer.emit_created config (package_root ^ "/");
  Ok ()
