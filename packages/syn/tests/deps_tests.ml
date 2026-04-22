open Std
open Syn

let parse_modules = fun ~env ~filename source ->
  let parse_result = Syn.parse ~filename:(Path.v filename) source in
  match Syn.Deps.of_parse_result ~env parse_result with
  | Ok deps -> Ok (Syn.Deps.modules deps)
  | Error (Syn.Deps.Parse_diagnostics diagnostics) -> Error ("parse diagnostics: "
  ^ String.concat "; " (List.map diagnostics ~fn:Syn.Diagnostic.to_string))
  | Error (Syn.Deps.Cst_builder_error err) -> Error ("cst builder error: " ^ err.message)

let alias_exports = fun names ->
  List.fold_left names ~init:Syn.Deps.Env.empty
    ~fn:(fun exports name -> Syn.Deps.Env.add_path exports ~path:[ name ] ~free_names:[ name ])

let open_paths = fun paths env ->
  List.fold_left paths ~init:env
    ~fn:(fun env path -> Syn.Deps.Env.open_path env ~path)

let test_deps_collect_value_declaration_modules_from_implicit_alias_opens = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Kernel"; "Aliases" ]
      ~free_names:[ "Kernel__Aliases" ]
      ~exports:(alias_exports [ "Result" ])
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Kernel"; "Net"; "Aliases" ]
      ~free_names:[ "Kernel__Net__Aliases" ]
      ~exports:(alias_exports [ "Socket_addr" ])
    |> open_paths [ [ "Kernel"; "Aliases" ]; [ "Kernel"; "Net"; "Aliases" ] ]
  in
  match
    parse_modules
      ~env
      ~filename:"unix.mli"
      "val resolve_stream: host:string -> port:int -> (Socket_addr.t array, error) Result.t\n"
  with
  | Ok modules when modules = [ "Result"; "Socket_addr" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Result, Socket_addr], got ["
  ^ String.concat ", " modules
  ^ "]")
  | Error err -> Error err

let test_deps_collect_opened_public_root_module_instead_of_child_module = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_binding
      ~path:[ "Syn" ]
      ~free_names:[ "Syn" ]
      ~exports:(alias_exports [ "Token" ])
  in
  match
    parse_modules
      ~env
      ~filename:"main.ml"
      "open Syn\n\nlet token_kind = fun (token: Token.t) -> token.kind\n"
  with
  | Ok modules when modules = [ "Syn" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Syn], got ["
  ^ String.concat ", " modules
  ^ "]")
  | Error err -> Error err

let test_deps_collect_qualified_public_root_module_instead_of_child_module = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_binding
      ~path:[ "Syn" ]
      ~free_names:[ "Syn" ]
      ~exports:(alias_exports [ "Token" ])
  in
  match
    parse_modules
      ~env
      ~filename:"main.ml"
      "let token_kind = fun (token: Syn.Token.t) -> token.kind\n"
  with
  | Ok modules when modules = [ "Syn" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Syn], got ["
  ^ String.concat ", " modules
  ^ "]")
  | Error err -> Error err

let test_deps_collect_manifest_type_modules_from_implicit_alias_opens = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Kernel"; "Fs"; "Aliases" ]
      ~free_names:[ "Kernel__Fs__Aliases" ]
      ~exports:(alias_exports [ "File" ])
    |> open_paths [ [ "Kernel"; "Fs"; "Aliases" ] ]
  in
  match
    parse_modules
      ~env
      ~filename:"read_dir.mli"
      "type kind = File.kind =\n  | RegularFile\n"
  with
  | Ok modules when modules = [ "File" ] -> Ok ()
  | Ok modules -> Error ("expected deps [File], got ["
  ^ String.concat ", " modules
  ^ "]")
  | Error err -> Error err

let test_deps_collect_qualified_public_root_from_implicit_root_alias_open = fun _ctx ->
  let fs_exports =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_binding
      ~path:[ "Fs" ]
      ~free_names:[ "Fs" ]
      ~exports:(alias_exports [ "File" ])
  in
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Kernel"; "Aliases" ]
      ~free_names:[ "Kernel__Aliases" ]
      ~exports:fs_exports
    |> open_paths [ [ "Kernel"; "Aliases" ] ]
  in
  match
    parse_modules
      ~env
      ~filename:"process.mli"
      "type error =\n  | File of Fs.File.error\n"
  with
  | Ok modules when modules = [ "Fs" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Fs], got ["
  ^ String.concat ", " modules
  ^ "]")
  | Error err -> Error err

let test_deps_collect_variant_payload_modules_from_implicit_alias_opens = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Kernel"; "Aliases" ]
      ~free_names:[ "Kernel__Aliases" ]
      ~exports:(alias_exports [ "System_error" ])
    |> open_paths [ [ "Kernel"; "Aliases" ] ]
  in
  match
    parse_modules
      ~env
      ~filename:"unix.mli"
      "type error =\n  | System of System_error.t\n"
  with
  | Ok modules when modules = [ "System_error" ] -> Ok ()
  | Ok modules -> Error ("expected deps [System_error], got ["
  ^ String.concat ", " modules
  ^ "]")
  | Error err -> Error err

let test_deps_collect_field_access_modules_from_implicit_alias_opens = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Kernel"; "Async"; "Aliases" ]
      ~free_names:[ "Kernel__Async__Aliases" ]
      ~exports:(alias_exports [ "Libc" ])
    |> open_paths [ [ "Kernel"; "Async"; "Aliases" ] ]
  in
  match
    parse_modules
      ~env
      ~filename:"unix.ml"
      "let is_error = fun event -> event.flags land Libc.ev_error != 0\n"
  with
  | Ok modules when modules = [ "Libc" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Libc], got ["
  ^ String.concat ", " modules
  ^ "]")
  | Error err -> Error err

let name = "syn-deps"

let tests = Test.[
  case
    "deps collect value declaration modules from implicit alias opens"
    test_deps_collect_value_declaration_modules_from_implicit_alias_opens;
  case
    "deps collect manifest type modules from implicit alias opens"
    test_deps_collect_manifest_type_modules_from_implicit_alias_opens;
  case
    "deps collect qualified public root from implicit root alias open"
    test_deps_collect_qualified_public_root_from_implicit_root_alias_open;
  case
    "deps collect opened public root module instead of child module"
    test_deps_collect_opened_public_root_module_instead_of_child_module;
  case
    "deps collect qualified public root module instead of child module"
    test_deps_collect_qualified_public_root_module_instead_of_child_module;
  case
    "deps collect variant payload modules from implicit alias opens"
    test_deps_collect_variant_payload_modules_from_implicit_alias_opens;
  case
    "deps collect field access modules from implicit alias opens"
    test_deps_collect_field_access_modules_from_implicit_alias_opens;
]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name ~tests ~args ()) ~args:Env.args ()
