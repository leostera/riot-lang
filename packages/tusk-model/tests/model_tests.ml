open Std
open Tusk_model
module Test = Std.Test

let test_namespace_empty () =
  let ns = Namespace.empty in
  if Namespace.is_empty ns then Ok ()
  else Error "Expected empty namespace to be empty"

let test_namespace_append () =
  let ns = Namespace.append (Namespace.append Namespace.empty "Foo") "Bar" in
  let result = Namespace.to_string ns in
  if result = "Foo__Bar" then Ok ()
  else Error (format "Expected 'Foo__Bar', got '%s'" result)

let test_namespace_of_string () =
  let ns = Namespace.of_string "Foo__Bar__Baz" in
  let result = Namespace.to_list ns in
  if result = [ "Foo"; "Bar"; "Baz" ] then Ok ()
  else
    Error
      (format "Expected [Foo; Bar; Baz], got [%s]" (String.concat "; " result))

let test_namespace_to_string_empty () =
  let ns = Namespace.empty in
  let result = Namespace.to_string ns in
  if result = "" then Ok ()
  else Error (format "Expected empty string, got '%s'" result)

let test_module_name_of_string () =
  let mod_name = Module_name.of_string "hello" in
  let result = Module_name.to_string mod_name in
  if result = "Hello" then Ok ()
  else Error (format "Expected 'Hello', got '%s'" result)

let test_module_name_sanitize () =
  let mod_name = Module_name.of_string "foo-bar" in
  let result = Module_name.to_string mod_name in
  if result = "Foo_bar" then Ok ()
  else Error (format "Expected 'Foo_bar', got '%s'" result)

let test_module_name_qualified () =
  let ns = Namespace.of_string "MyLib" in
  let mod_name =
    Module_name.make ~filename:(Path.v "foo.ml") ~namespace:ns ~name:"Foo"
  in
  let result = Module_name.qualified_name mod_name in
  if result = "MyLib__Foo" then Ok ()
  else Error (format "Expected 'MyLib__Foo', got '%s'" result)

let test_module_name_cmi () =
  let mod_name = Module_name.of_string "foo" in
  let result = Module_name.cmi mod_name |> Path.to_string in
  if result = "Foo.cmi" then Ok ()
  else Error (format "Expected 'Foo.cmi', got '%s'" result)

let test_module_name_cmx () =
  let mod_name = Module_name.of_string "bar" in
  let result = Module_name.cmx mod_name |> Path.to_string in
  if result = "Bar.cmx" then Ok ()
  else Error (format "Expected 'Bar.cmx', got '%s'" result)

let test_module_name_cmxa () =
  let mod_name = Module_name.of_string "mylib" in
  let result = Module_name.cmxa mod_name |> Path.to_string in
  if result = "Mylib.cmxa" then Ok ()
  else Error (format "Expected 'Mylib.cmxa', got '%s'" result)

let test_module_creation () =
  let mod_ =
    Module.make ~namespace:Namespace.empty ~filename:(Path.v "src/foo.ml")
  in
  let name = Module.module_name mod_ |> Module_name.to_string in
  if name = "Foo" then Ok () else Error (format "Expected 'Foo', got '%s'" name)

let test_module_kind_ml () =
  let mod_ =
    Module.make ~namespace:Namespace.empty ~filename:(Path.v "foo.ml")
  in
  match Module.kind mod_ with
  | `implementation -> Ok ()
  | `interface -> Error "Expected implementation, got interface"

let test_module_kind_mli () =
  let mod_ =
    Module.make ~namespace:Namespace.empty ~filename:(Path.v "foo.mli")
  in
  match Module.kind mod_ with
  | `interface -> Ok ()
  | `implementation -> Error "Expected interface, got implementation"

let test_module_outputs () =
  let mod_ =
    Module.make ~namespace:Namespace.empty ~filename:(Path.v "foo.ml")
  in
  let cmi = Module.cmi mod_ |> Path.to_string in
  let cmx = Module.cmx mod_ |> Path.to_string in
  if cmi = "Foo.cmi" && cmx = "Foo.cmx" then Ok ()
  else Error (format "Expected Foo.cmi and Foo.cmx, got %s and %s" cmi cmx)

let tests =
  Test.
    [
      case "Namespace: empty namespace" test_namespace_empty;
      case "Namespace: append to namespace" test_namespace_append;
      case "Namespace: from string" test_namespace_of_string;
      case "Namespace: empty to string" test_namespace_to_string_empty;
      case "Module_name: from string" test_module_name_of_string;
      case "Module_name: sanitize dashes" test_module_name_sanitize;
      case "Module_name: qualified name" test_module_name_qualified;
      case "Module_name: cmi output" test_module_name_cmi;
      case "Module_name: cmx output" test_module_name_cmx;
      case "Module_name: cmxa output" test_module_name_cmxa;
      case "Module: creation" test_module_creation;
      case "Module: kind .ml" test_module_kind_ml;
      case "Module: kind .mli" test_module_kind_mli;
      case "Module: outputs" test_module_outputs;
    ]

let name = "Tusk Model Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
