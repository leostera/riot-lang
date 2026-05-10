open Std
open Std.Result.Syntax

module Test = Std.Test
module Dep_analyzer = Riot_planner.Dep_analyzer
module Item = Dep_analyzer.Item

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"expected test source slice"

let parse = fun ?(filename = Path.v "test.ml") source -> Syn.parse ~filename (source_slice source)

let analyze_source = fun ?implicit_opens ?module_path ~path source ->
  Dep_analyzer.analyze
    ?implicit_opens
    ?module_path
    ~source:path
    ~source_hash:(Crypto.hash_string source)
    (parse ~filename:path source)
  |> Result.map_err
    ~fn:(fun (Dep_analyzer.Parse_diagnostics diagnostics) ->
      "analyzer parse diagnostics: "
      ^ String.concat "; " (List.map diagnostics ~fn:Syn.Diagnostic.to_string))

let sorted = fun values ->
  List.unique
    (List.sort values ~compare:String.compare)
    ~compare:String.compare

let std_provider =
  Dep_analyzer.{
    path = [ "Std" ];
    free_names = [ "Std" ];
    exports = [ [ "Config" ]; [ "Env" ]; [ "IO" ]; [ "Utils" ]; ];
  }

let env = Dep_analyzer.Env.make [ std_provider ]

let analyzer_modules = fun source ->
  let* summary = analyze_source ~path:(Path.v "test.ml") source in
  match Dep_analyzer.resolve env [ summary ] with
  | Ok [ resolved ] ->
      Ok (
        Dep_analyzer.ResolvedSource.modules resolved
        @ Dep_analyzer.ResolvedSource.unresolved resolved
        |> sorted
      )
  | Ok _ -> Error "expected one resolved source"
  | Error _ -> Error "expected analyzer resolution"

let assert_modules = fun ~expected source ->
  let* actual = analyzer_modules source in
  let expected = sorted expected in
  if expected = actual then
    Ok ()
  else
    Error ("expected modules ["
    ^ String.concat ", " expected
    ^ "] but got ["
    ^ String.concat ", " actual
    ^ "]")

let assert_modules_contain = fun ~required source ->
  let* actual = analyzer_modules source in
  let missing =
    List.filter required ~fn:(fun expected -> not (List.any actual ~fn:(String.equal expected)))
  in
  match missing with
  | [] -> Ok ()
  | _ ->
      Error ("expected modules to contain ["
      ^ String.concat ", " missing
      ^ "], got ["
      ^ String.concat ", " actual
      ^ "]")

let parse_modules = fun
  ?(env = Dep_analyzer.Env.empty) ?implicit_opens ?module_path ~filename source ->
  let* summary = analyze_source ?implicit_opens ?module_path ~path:(Path.v filename) source in
  match Dep_analyzer.resolve env [ summary ] with
  | Ok [ resolved ] ->
      Ok (
        Dep_analyzer.ResolvedSource.modules resolved
        |> sorted
      )
  | Ok _ -> Error "expected one resolved source"
  | Error _ -> Error "expected dependency analyzer resolution"

let assert_modules_with_env = fun ?env ?implicit_opens ?module_path ~filename ~expected source ->
  let* actual = parse_modules ?env ?implicit_opens ?module_path ~filename source in
  let expected = sorted expected in
  if expected = actual then
    Ok ()
  else
    Error ("expected deps ["
    ^ String.concat ", " expected
    ^ "], got ["
    ^ String.concat ", " actual
    ^ "]")

let provider = fun ~path ~free_names ~exports -> Dep_analyzer.{ path; free_names; exports }

let env_of_providers = Dep_analyzer.Env.make

let alias_items = fun names ->
  List.map
    names
    ~fn:(fun name -> Item.ModuleAlias { name; target = Item.Use (Item.Ident.of_strings [ name ]) })

let generated_alias_summary = fun ~module_path ~items ->
  Dep_analyzer.{
    source = Path.v (String.concat "__" module_path ^ ".ml-gen");
    source_hash = Crypto.hash_string (String.concat "." module_path);
    module_path = Some module_path;
    kind = Implementation;
    items;
  }

let generated_alias_env = fun summaries ->
  Dep_analyzer.Env.add_external_summaries
    Dep_analyzer.Env.empty
    summaries

let generated_alias = fun ~module_path names ->
  generated_alias_summary
    ~module_path
    ~items:(alias_items names)

let generated_alias_with_super = fun ~module_path names ->
  generated_alias_summary
    ~module_path
    ~items:(alias_items names
    @ [ Item.Module { name = "Super"; signature = []; body = alias_items names }; ])

let open_std_resolves_exported_module ctx =
  let _ = ctx in
  assert_modules ~expected:[ "Std" ] {ocaml|
open Std

let x = IO.read
|ocaml}

let unresolved_open_preserves_root_dependency ctx =
  let _ = ctx in
  assert_modules
    ~expected:[ "Missing"; "Std" ]
    {ocaml|
open Std
open Missing

let x = IO.read
|ocaml}

let local_module_binding_does_not_escape_as_dependency ctx =
  let _ = ctx in
  assert_modules
    ~expected:[ "Std" ]
    {ocaml|
open Std

module A = struct
  let b = ()
end

let x = IO.read A.b
|ocaml}

let local_module_binding_covers_qualified_record_and_type_uses ctx =
  let _ = ctx in
  let source =
    {ocaml|
open Prelude

module Raw = struct
  type id = private int

  type 'value state =
    | Running
    | Finished of 'value

  type 'value term_sync = {
    mutable state: 'value state;
    mut: Sync.Mutex.t;
    cond: Sync.Condition.t;
  }

  external get_recommended_domain_count: unit -> int = "caml_recommended_domain_count"
  external spawn: (unit -> 'value) -> 'value term_sync -> id = "caml_domain_spawn"
end

let available_parallelism =
  Raw.get_recommended_domain_count ()

type 'value t = {
  domain: Raw.id;
  term_sync: 'value Raw.term_sync;
}

let spawn = fun () ->
  let term_sync =
    Raw.{ state = Running; mut = Sync.Mutex.create (); cond = Sync.Condition.create () }
  in
  Raw.spawn (fun () -> ()) term_sync

let join = fun term_sync ->
  match term_sync.state with
  | Raw.Running -> ()
  | Raw.Finished _ -> ()
|ocaml}
  in
  let* () = assert_modules_with_env ~filename:"thread.ml" ~expected:[ "Prelude"; "Sync" ] source in
  assert_modules_with_env
    ~module_path:[ "Kernel"; "Thread"; "Thread" ]
    ~filename:"thread.ml"
    ~expected:[ "Prelude"; "Sync" ]
    source

let local_module_binding_survives_generated_alias_implicit_opens ctx =
  let _ = ctx in
  let source =
    {ocaml|
open Prelude

module Raw = struct
  type id = private int

  type 'value state =
    | Running
    | Finished of 'value

  type 'value term_sync = {
    mutable state: 'value state;
    mut: Sync.Mutex.t;
    cond: Sync.Condition.t;
  }

  external get_recommended_domain_count: unit -> int = "caml_recommended_domain_count"
  external spawn: (unit -> 'value) -> 'value term_sync -> id = "caml_domain_spawn"
end

let available_parallelism =
  Raw.get_recommended_domain_count ()

type 'value t = {
  domain: Raw.id;
  term_sync: 'value Raw.term_sync;
}

let spawn = fun () ->
  let term_sync =
    Raw.{ state = Running; mut = Sync.Mutex.create (); cond = Sync.Condition.create () }
  in
  Raw.spawn (fun () -> ()) term_sync

let join = fun term_sync ->
  match term_sync.state with
  | Raw.Running -> ()
  | Raw.Finished _ -> ()
|ocaml}
  in
  let root_alias =
    generated_alias
      ~module_path:[ "Kernel"; "Aliases" ]
      [ "Exception"; "Prelude"; "Sync"; "Thread" ]
  in
  let thread_alias =
    generated_alias ~module_path:[ "Kernel"; "Thread"; "Aliases" ] [ "Thread"; "Unix" ]
  in
  let* summary =
    analyze_source
      ~implicit_opens:[ [ "Kernel"; "Aliases" ]; [ "Kernel"; "Thread"; "Aliases" ] ]
      ~module_path:[ "Kernel"; "Thread"; "Thread" ]
      ~path:(Path.v "thread.ml")
      source
  in
  match Dep_analyzer.resolve Dep_analyzer.Env.empty [ root_alias; thread_alias; summary ] with
  | Ok [ _; _; resolved ] ->
      let actual =
        Dep_analyzer.ResolvedSource.modules resolved
        @ Dep_analyzer.ResolvedSource.unresolved resolved
        |> sorted
      in
      if List.any actual ~fn:(String.equal "Raw") then
        Error ("expected generated alias implicit opens not to leak Raw, got ["
        ^ String.concat ", " actual
        ^ "]")
      else
        Ok ()
  | Ok _ -> Error "expected three resolved summaries"
  | Error _ -> Error "expected dependency analyzer resolution"

let qualified_dependency_resolves_to_provider_root ctx =
  let _ = ctx in
  assert_modules ~expected:[ "Std" ] {ocaml|
let x = Std.IO.read
|ocaml}

let module_alias_keeps_provider_dependency ctx =
  let _ = ctx in
  assert_modules ~expected:[ "Std" ] {ocaml|
module Config = Std.Config

let x = Config.load
|ocaml}

let dependency_ir_preserves_open_order ctx =
  let _ = ctx in
  let source = {ocaml|
open Std
open Missing

let x = IO.read
|ocaml}
  in
  let parsed = parse source in
  let use_path = fun __tmp1 ->
    match __tmp1 with
    | Dep_analyzer.Item.Use path -> Dep_analyzer.Item.Ident.to_strings path
    | _ -> []
  in
  match Dep_analyzer.analyze
    ~source:(Path.v "test.ml")
    ~source_hash:(Crypto.hash_string source)
    parsed with
  | Error _ -> Error "expected analyzer summary"
  | Ok summary ->
      match summary.Dep_analyzer.items with
      | [ Dep_analyzer.Item.Open use_std; Dep_analyzer.Item.Open use_missing; use_io ] when use_path
        use_std
      = [ "Std" ]
      && use_path use_missing = [ "Missing" ]
      && use_path use_io = [ "IO" ] -> Ok ()
      | _ -> Error "expected dependency IR to preserve open/use order"

let resolves_local_summaries_as_providers ctx =
  let _ = ctx in
  let source_a = "module IO = struct let read = () end" in
  let source_b = "open Utils\nlet x = IO.read" in
  let parsed_a = parse source_a in
  let parsed_b = parse source_b in
  let* summary_a =
    Dep_analyzer.analyze
      ~module_path:[ "Utils" ]
      ~source:(Path.v "utils.ml")
      ~source_hash:(Crypto.hash_string source_a)
      parsed_a
    |> Result.map_err ~fn:(fun _ -> "expected utils summary")
  in
  let* summary_b =
    Dep_analyzer.analyze
      ~module_path:[ "Main" ]
      ~source:(Path.v "main.ml")
      ~source_hash:(Crypto.hash_string source_b)
      parsed_b
    |> Result.map_err ~fn:(fun _ -> "expected main summary")
  in
  match Dep_analyzer.resolve env [ summary_a; summary_b ] with
  | Ok [ _; resolved_b ] ->
      let actual =
        Dep_analyzer.ResolvedSource.modules resolved_b
        |> sorted
      in
      if actual = [ "Utils" ] then
        Ok ()
      else
        Error ("expected local summary provider Utils but got [" ^ String.concat ", " actual ^ "]")
  | Ok _ -> Error "expected two resolved summaries"
  | Error _ -> Error "expected analyzer resolution"

let local_module_alias_resolves_inside_source ctx =
  let _ = ctx in
  assert_modules ~expected:[ "Std" ] {ocaml|
module S = Std

open S

let x = IO.read
|ocaml}

let nested_summary_provider_resolves_opened_child ctx =
  let _ = ctx in
  let source_a =
    {ocaml|
module A = struct
  module IO = struct
    let read = ()
  end
end
|ocaml}
  in
  let source_b = {ocaml|
open Utils.A

let x = IO.read
|ocaml}
  in
  let parsed_a = parse source_a in
  let parsed_b = parse source_b in
  let* summary_a =
    Dep_analyzer.analyze
      ~module_path:[ "Utils" ]
      ~source:(Path.v "utils.ml")
      ~source_hash:(Crypto.hash_string source_a)
      parsed_a
    |> Result.map_err ~fn:(fun _ -> "expected utils summary")
  in
  let* summary_b =
    Dep_analyzer.analyze
      ~module_path:[ "Main" ]
      ~source:(Path.v "main.ml")
      ~source_hash:(Crypto.hash_string source_b)
      parsed_b
    |> Result.map_err ~fn:(fun _ -> "expected main summary")
  in
  match Dep_analyzer.resolve env [ summary_a; summary_b ] with
  | Ok [ _; resolved_b ] ->
      let actual =
        Dep_analyzer.ResolvedSource.modules resolved_b
        |> sorted
      in
      if actual = [ "Utils" ] then
        Ok ()
      else
        Error ("expected nested summary provider Utils but got [" ^ String.concat ", " actual ^ "]")
  | Ok _ -> Error "expected two resolved summaries"
  | Error _ -> Error "expected analyzer resolution"

let local_nested_module_open_uses_binding_tree ctx =
  let _ = ctx in
  assert_modules
    ~expected:[]
    {ocaml|
module A = struct
  module B = struct
    module IO = struct
      let read = ()
    end
  end
end

open A.B

let x = IO.read
|ocaml}

let type_alias_records_module_dependency ctx =
  let _ = ctx in
  assert_modules
    ~expected:[ "Module_planner" ]
    {ocaml|
type module_plan_result = Module_planner.plan_result
|ocaml}

let polymorphic_variant_payload_records_module_dependency ctx =
  let _ = ctx in
  assert_modules_with_env
    ~filename:"test.mli"
    ~expected:[ "Style" ]
    {ocaml|
val make:
  ?color:[ | `Plain of Style.color | `Gradient of Style.color * Style.color] ->
  unit ->
  t
|ocaml}

let polymorphic_variant_inherited_row_records_module_dependency ctx =
  let _ = ctx in
  assert_modules_with_env
    ~filename:"test.mli"
    ~expected:[ "Style" ]
    "type color = [ | Style.color | `Plain ]\n"

let qualified_record_expr_records_module_dependency ctx =
  let _ = ctx in
  assert_modules
    ~expected:[ "Acceptor" ]
    {ocaml|
let state =
  Acceptor.{
    listener;
    buffer_size;
  }
|ocaml}

let qualified_record_expr_inside_for_records_module_dependency ctx =
  let _ = ctx in
  assert_modules
    ~expected:[ "Acceptor" ]
    {ocaml|
let start = fun listener ->
  for i = 1 to 10 do
    let start () =
      let state =
        Acceptor.{
          listener;
        }
      in
      Acceptor.spawn state
    in
    start ()
  done
|ocaml}

let qualified_record_expr_after_sibling_aliases_records_module_dependency ctx =
  let _ = ctx in
  assert_modules
    ~expected:[ "Acceptor"; "Connection"; "Handler"; "Std"; "Transport" ]
    {ocaml|
open Std
open Std.Collections

module Connection = Connection
module Handler = Handler
module Transport = Transport

let start = fun listener ->
  for i = 1 to 10 do
    let start () =
      let state =
        Acceptor.{
          listener;
        }
      in
      Acceptor.spawn state
    in
    start ()
  done
|ocaml}

let qualified_record_expr_in_start_link_records_module_dependency ctx =
  let _ = ctx in
  assert_modules_contain
    ~required:[ "Acceptor" ]
    {ocaml|
open Std
open Std.Collections

module Connection = Connection
module Handler = Handler
module Transport = Transport

let validate_start_options = fun ~acceptors ~buffer_size -> Ok ()

let start_link = fun
  ~host
  ~port
  ?(acceptors = 100)
  ?(buffer_size = 4_096)
  ?(transport = Transport.tcp ())
  (type s e)
  (handler: (s, e) Handler.handler)
  (initial_ctx: s) ->
  match validate_start_options ~acceptors ~buffer_size with
  | Error error -> Error error
  | Ok () ->
      match Net.Addr.from_host_and_port ~host ~port with
      | Error error -> Error error
      | Ok addr -> (
          match Net.TcpListener.bind ~reuse_addr:true ~reuse_port:false addr with
          | Error error -> Error error
          | Ok listener ->
              for i = 1 to acceptors do
                let start () =
                  let state =
                    Acceptor.{
                      listener;
                      buffer_size;
                      handler;
                      initial_ctx;
                      transport;
                    }
                  in
                  Acceptor.spawn state
                in
                start ()
              done;
              Ok ()
        )
|ocaml}

let qualified_record_pattern_records_module_dependency ctx =
  let _ = ctx in
  assert_modules
    ~expected:[ "Run" ]
    {ocaml|
let target =
  fun Run.{ package_name; binary_name } ->
    package_name ^ binary_name
|ocaml}

let qualified_record_pattern_in_labeled_callback_records_module_dependency ctx =
  let _ = ctx in
  assert_modules
    ~expected:[ "Run"; "Result" ]
    {ocaml|
let target =
  Result.map value ~fn:(fun Run.{ package_name; binary_name } ->
    package_name ^ binary_name)
|ocaml}

let nested_syntax_open_does_not_shadow_sibling_record_pattern ctx =
  let _ = ctx in
  let env =
    env_of_providers
      [
        provider ~path:[ "Std" ] ~free_names:[ "Std" ] ~exports:[ [ "Result"; "Syntax" ] ];
        provider ~path:[ "Run" ] ~free_names:[ "Run" ] ~exports:[];
      ]
  in
  assert_modules_with_env
    ~env
    ~filename:"install.ml"
    ~expected:[ "Run"; "Std" ]
    {ocaml|open Std
open Std.Result.Syntax

let target =
  Result.map value ~fn:(fun Run.{ package_name; binary_name } ->
    package_name ^ binary_name)
|ocaml}

let first_class_module_pattern_binds_local_module ctx =
  let _ = ctx in
  assert_modules
    ~expected:[]
    {ocaml|
let token = fun (E ((module Event), state)) -> Event.token state
|ocaml}

let generated_alias_open_resolves_to_child_module ctx =
  let _ = ctx in
  let alias_source =
    {ocaml|
module Module_planner = Riot_planner__Module_planner
module Package_planner = Riot_planner__Package_planner

module Super = struct
  module Module_planner = Riot_planner__Module_planner
  module Package_planner = Riot_planner__Package_planner
end
|ocaml}
  in
  let root_source =
    {ocaml|
type module_plan_result = Module_planner.plan_result
type package_plan_result = Package_planner.plan_result
|ocaml}
  in
  let child_source = "type plan_result = unit\n" in
  let* alias_summary =
    analyze_source
      ~module_path:[ "Riot_planner"; "Aliases" ]
      ~path:(Path.v "Riot_planner__Aliases.ml-gen")
      alias_source
  in
  let* root_summary =
    analyze_source
      ~implicit_opens:[ [ "Riot_planner"; "Aliases" ] ]
      ~module_path:[ "Riot_planner" ]
      ~path:(Path.v "Riot_planner.mli")
      root_source
  in
  let* module_summary =
    analyze_source
      ~module_path:[ "Riot_planner"; "Module_planner" ]
      ~path:(Path.v "module_planner.mli")
      child_source
  in
  let* package_summary =
    analyze_source
      ~module_path:[ "Riot_planner"; "Package_planner" ]
      ~path:(Path.v "package_planner.mli")
      child_source
  in
  match Dep_analyzer.resolve
    Dep_analyzer.Env.empty
    [ alias_summary; root_summary; module_summary; package_summary ] with
  | Ok [ _; resolved_root; _; _ ] ->
      let actual =
        Dep_analyzer.ResolvedSource.modules resolved_root
        |> sorted
      in
      if actual = [ "Module_planner"; "Package_planner" ] then
        Ok ()
      else
        Error ("expected generated alias to resolve child modules but got ["
        ^ String.concat ", " actual
        ^ "]")
  | Ok _ -> Error "expected generated alias fixture summaries"
  | Error _ -> Error "expected generated alias resolution"

let generated_alias_child_open_exposes_child_exports ctx =
  let _ = ctx in
  let alias_source = {ocaml|
module Iter = Std__Iter
module String = Std__String
|ocaml}
  in
  let iter_source = {ocaml|
module Iterator: sig type 'value t end
|ocaml}
  in
  let string_source = {ocaml|
open Iter

val into_iter: string -> char Iterator.t
|ocaml}
  in
  let* alias_summary =
    analyze_source
      ~module_path:[ "Std"; "Aliases" ]
      ~path:(Path.v "Std__Aliases.ml-gen")
      alias_source
  in
  let* iter_summary =
    analyze_source ~module_path:[ "Std"; "Iter" ] ~path:(Path.v "iter.mli") iter_source
  in
  let* string_summary =
    analyze_source
      ~implicit_opens:[ [ "Std"; "Aliases" ] ]
      ~module_path:[ "Std"; "String" ]
      ~path:(Path.v "string.mli")
      string_source
  in
  match Dep_analyzer.resolve Dep_analyzer.Env.empty [ alias_summary; iter_summary; string_summary ] with
  | Ok [ _; _; resolved_string ] ->
      let actual =
        Dep_analyzer.ResolvedSource.modules resolved_string
        |> sorted
      in
      if actual = [ "Iter" ] then
        Ok ()
      else
        Error ("expected generated alias child open to resolve Iter but got ["
        ^ String.concat ", " actual
        ^ "]")
  | Ok _ -> Error "expected generated alias child open summaries"
  | Error _ -> Error "expected generated alias child open resolution"

let local_namespace_open_exposes_child_exports ctx =
  let _ = ctx in
  let* alias_summary =
    analyze_source
      ~module_path:[ "Std"; "Aliases" ]
      ~path:(Path.v "Std__Aliases.ml-gen")
      "module Iter = Std__Iter\nmodule String = Std__String\n"
  in
  let* root_summary =
    analyze_source
      ~module_path:[ "Std" ]
      ~path:(Path.v "std.ml")
      "module Iter = Iter\nmodule String = String\n"
  in
  let* iter_summary =
    analyze_source
      ~module_path:[ "Std"; "Iter" ]
      ~path:(Path.v "iter.mli")
      "module Iterator: sig type 'value t end\n"
  in
  let* iterator_summary =
    analyze_source
      ~module_path:[ "Std"; "Iter"; "Iterator" ]
      ~path:(Path.v "iterator.ml")
      "type 'value t = unit\n"
  in
  let* string_summary =
    analyze_source
      ~implicit_opens:[ [ "Std"; "Aliases" ] ]
      ~module_path:[ "Std"; "String" ]
      ~path:(Path.v "string.mli")
      "open Iter\nval into_iter: string -> char Iterator.t\n"
  in
  match Dep_analyzer.resolve
    Dep_analyzer.Env.empty
    [ alias_summary; root_summary; iter_summary; iterator_summary; string_summary ] with
  | Ok [ _; _; _; _; resolved_string ] ->
      let actual =
        Dep_analyzer.ResolvedSource.modules resolved_string
        |> sorted
      in
      if actual = [ "Iter" ] then
        Ok ()
      else
        Error ("expected local namespace open to resolve Iter but got ["
        ^ String.concat ", " actual
        ^ "]")
  | Ok _ -> Error "expected local namespace summaries"
  | Error _ -> Error "expected local namespace resolution"

let open_fallback_shadows_sibling_summary_for_local_open ctx =
  let _ = ctx in
  let* alias_summary =
    analyze_source
      ~module_path:[ "Std"; "Aliases" ]
      ~path:(Path.v "Std__Aliases.ml-gen")
      "module IO = Std__IO\n"
  in
  let* io_summary =
    analyze_source
      ~module_path:[ "Std"; "IO" ]
      ~path:(Path.v "IO.mli")
      "module Bytes: module type of Bytes\n"
  in
  let bytes_summary =
    Dep_analyzer.{
      source = Path.v "IO/bytes.ml";
      source_hash = Crypto.hash_string "Bytes";
      module_path = Some [ "Std"; "IO"; "Bytes" ];
      kind = Implementation;
      items = [];
    }
  in
  let* uuid_summary =
    analyze_source
      ~implicit_opens:[ [ "Std"; "Aliases" ] ]
      ~module_path:[ "Std"; "Uuid" ]
      ~path:(Path.v "uuid.ml")
      {ocaml|open IO

let direct bytes = Bytes.length bytes

let local bytes =
  let open Bytes in
  get bytes ~at:0
|ocaml}
  in
  match Dep_analyzer.resolve
    Dep_analyzer.Env.empty
    [ alias_summary; io_summary; bytes_summary; uuid_summary ] with
  | Ok [ _; _; _; resolved_uuid ] ->
      let actual =
        Dep_analyzer.ResolvedSource.modules resolved_uuid
        |> sorted
      in
      if actual = [ "IO" ] then
        Ok ()
      else
        Error ("expected Bytes references opened through IO to resolve to IO but got ["
        ^ String.concat ", " actual
        ^ "]")
  | Ok _ -> Error "expected generated alias, IO, Bytes, and uuid summaries"
  | Error _ -> Error "expected open fallback shadowing resolution"

let local_child_summary_does_not_pollute_public_root_exports ctx =
  let _ = ctx in
  let root_source = "module Token = Token\n" in
  let child_source = "type t\n" in
  let main_source = "let token_kind = fun (token: Syn.Token.t) -> token\n" in
  let* root_summary = analyze_source ~module_path:[ "Syn" ] ~path:(Path.v "syn.ml") root_source in
  let* child_summary =
    analyze_source ~module_path:[ "Syn"; "Token" ] ~path:(Path.v "token.mli") child_source
  in
  let* main_summary = analyze_source ~module_path:[ "Main" ] ~path:(Path.v "main.ml") main_source in
  match Dep_analyzer.resolve Dep_analyzer.Env.empty [ root_summary; child_summary; main_summary ] with
  | Ok [ _; _; resolved_main ] ->
      let actual =
        Dep_analyzer.ResolvedSource.modules resolved_main
        |> sorted
      in
      if actual = [ "Syn" ] then
        Ok ()
      else
        Error ("expected Syn.Token through public root to resolve to Syn but got ["
        ^ String.concat ", " actual
        ^ "]")
  | Ok _ -> Error "expected public root summary resolution"
  | Error _ -> Error "expected public root summary resolution"

let external_nested_open_exposes_children_as_dependency_root ctx =
  let _ = ctx in
  let io_source = "module Buffer: module type of Buffer\n" in
  let user_source =
    {ocaml|open Std
open Std.IO

let render = fun () ->
  let open Buffer in
  create ~size:128
|ocaml}
  in
  let* io_summary = analyze_source ~module_path:[ "Std"; "IO" ] ~path:(Path.v "IO.mli") io_source in
  let* user_summary = analyze_source ~module_path:[ "User" ] ~path:(Path.v "user.ml") user_source in
  let env = Dep_analyzer.Env.add_external_summaries Dep_analyzer.Env.empty [ io_summary ] in
  match Dep_analyzer.resolve env [ user_summary ] with
  | Ok [ resolved_user ] ->
      let actual =
        Dep_analyzer.ResolvedSource.modules resolved_user
        |> sorted
      in
      if actual = [ "Std" ] then
        Ok ()
      else
        Error ("expected nested dependency open to resolve to Std but got ["
        ^ String.concat ", " actual
        ^ "]")
  | Ok _ -> Error "expected nested dependency open summary"
  | Error _ -> Error "expected nested dependency open resolution"

let deps_collect_value_declaration_modules_from_implicit_alias_opens ctx =
  let _ = ctx in
  let env =
    generated_alias_env
      [
        generated_alias ~module_path:[ "Kernel"; "Aliases" ] [ "Result" ];
        generated_alias ~module_path:[ "Kernel"; "Net"; "Aliases" ] [ "Socket_addr" ];
      ]
  in
  assert_modules_with_env
    ~env
    ~implicit_opens:[ [ "Kernel"; "Aliases" ]; [ "Kernel"; "Net"; "Aliases" ] ]
    ~filename:"unix.mli"
    ~expected:[ "Result"; "Socket_addr" ]
    "val resolve_stream: host:string -> port:int -> (Socket_addr.t array, error) Result.t\n"

let deps_collect_manifest_type_modules_from_implicit_alias_opens ctx =
  let _ = ctx in
  let env =
    generated_alias_env [ generated_alias ~module_path:[ "Kernel"; "Fs"; "Aliases" ] [ "File" ]; ]
  in
  assert_modules_with_env
    ~env
    ~implicit_opens:[ [ "Kernel"; "Fs"; "Aliases" ] ]
    ~filename:"read_dir.mli"
    ~expected:[ "File" ]
    "type kind = File.kind =\n  | RegularFile\n"

let deps_collect_qualified_public_root_from_implicit_root_alias_open ctx =
  let _ = ctx in
  let env =
    Dep_analyzer.Env.add_external_summaries
      Dep_analyzer.Env.empty
      [
        generated_alias ~module_path:[ "Kernel"; "Aliases" ] [ "Fs" ];
        Dep_analyzer.{
          source = Path.v "Fs.ml";
          source_hash = Crypto.hash_string "Fs";
          module_path = Some [ "Fs" ];
          kind = Implementation;
          items = [ Item.Module { name = "File"; signature = []; body = [] } ];
        };
      ]
  in
  assert_modules_with_env
    ~env
    ~implicit_opens:[ [ "Kernel"; "Aliases" ] ]
    ~filename:"process.mli"
    ~expected:[ "Fs" ]
    "type error =\n  | File of Fs.File.error\n"

let deps_collect_opened_public_root_module_instead_of_child_module ctx =
  let _ = ctx in
  let env =
    env_of_providers [ provider ~path:[ "Syn" ] ~free_names:[ "Syn" ] ~exports:[ [ "Token" ] ]; ]
  in
  assert_modules_with_env
    ~env
    ~filename:"main.ml"
    ~expected:[ "Syn" ]
    "open Syn\n\nlet token_kind = fun (token: Token.t) -> token.kind\n"

let deps_collect_opaque_opened_public_root_for_child_module ctx =
  let _ = ctx in
  let env = env_of_providers [ provider ~path:[ "Std" ] ~free_names:[ "Std" ] ~exports:[] ] in
  assert_modules_with_env
    ~env
    ~filename:"environment.ml"
    ~expected:[ "Std" ]
    {ocaml|open Std

let cwd = Env.current_dir ()

let value = Env.get Env.String ~var:"RIOT_ENV"
|ocaml}

let deps_collect_opaque_opened_public_root_for_qualified_child_module ctx =
  let _ = ctx in
  let env = env_of_providers [ provider ~path:[ "Std" ] ~free_names:[ "Std" ] ~exports:[] ] in
  assert_modules_with_env
    ~env
    ~filename:"pretext.ml"
    ~expected:[ "Std" ]
    {ocaml|open Std

let render = fun () ->
  let buffer = IO.Buffer.create ~size:256 in
  IO.Buffer.contents buffer
|ocaml}

let deps_collect_qualified_public_root_module_instead_of_child_module ctx =
  let _ = ctx in
  let env =
    env_of_providers [ provider ~path:[ "Syn" ] ~free_names:[ "Syn" ] ~exports:[ [ "Token" ] ]; ]
  in
  assert_modules_with_env
    ~env
    ~filename:"main.ml"
    ~expected:[ "Syn" ]
    "let token_kind = fun (token: Syn.Token.t) -> token.kind\n"

let deps_do_not_collect_exported_children_from_module_alias ctx =
  let _ = ctx in
  let env =
    env_of_providers
      [
        provider ~path:[ "Kernel" ] ~free_names:[ "Kernel" ] ~exports:[ [ "Array" ]; [ "Result" ] ];
      ]
  in
  assert_modules_with_env
    ~env
    ~filename:"kernel_new_addr_tests.ml"
    ~expected:[ "Kernel" ]
    "module Kernel = Kernel\nlet len = Kernel.Array.length values\n"

let deps_do_not_collect_exported_children_from_facade_alias ctx =
  let _ = ctx in
  let env =
    env_of_providers
      [ provider ~path:[ "Syntax_tree" ] ~free_names:[ "Syntax_tree" ] ~exports:[ [ "Builder" ] ]; ]
  in
  assert_modules_with_env
    ~env
    ~filename:"syn.ml"
    ~expected:[ "Syntax_tree" ]
    "module SyntaxTree = Syntax_tree\n"

let deps_open_scoped_root_keeps_child_modules_on_opened_root ctx =
  let _ = ctx in
  let env =
    env_of_providers [ provider ~path:[ "Std" ] ~free_names:[ "Std" ] ~exports:[ [ "Array" ] ]; ]
  in
  assert_modules_with_env
    ~env
    ~filename:"stdin_bench.ml"
    ~expected:[ "Std" ]
    {ocaml|open Std

let length = fun values -> Array.length values
|ocaml}

let deps_opened_child_named_like_current_file_stays_on_opened_root ctx =
  let _ = ctx in
  let env =
    env_of_providers [ provider ~path:[ "Std" ] ~free_names:[ "Std" ] ~exports:[ [ "Config" ] ]; ]
  in
  assert_modules_with_env
    ~env
    ~filename:"config.ml"
    ~expected:[ "Std" ]
    {ocaml|open Std

let load = fun path -> Config.load_file path
|ocaml}

let deps_keep_unknown_qualified_roots_after_open ctx =
  let _ = ctx in
  assert_modules_with_env
    ~filename:"regex.ml"
    ~expected:[ "Prelude"; "Regex_stubs" ]
    {ocaml|open Prelude

type t = Regex_stubs.compiled

let compile = Regex_stubs.compile
|ocaml}

let deps_collect_opened_module_root_for_exported_child ctx =
  let _ = ctx in
  let env =
    env_of_providers
      [
        provider
          ~path:[ "Iter" ]
          ~free_names:[ "Iter" ]
          ~exports:[ [ "Iterator" ]; [ "MutIterator" ] ];
      ]
  in
  assert_modules_with_env
    ~env
    ~filename:"string.mli"
    ~expected:[ "Iter" ]
    "open Iter\nval into_iter: string -> char Iterator.t\nval into_mut_iter: string -> char MutIterator.t\n"

let deps_collect_super_alias_child_module ctx =
  let _ = ctx in
  let env =
    generated_alias_env
      [ generated_alias_with_super ~module_path:[ "Gooey"; "Aliases" ] [ "Config"; "Viewport" ]; ]
  in
  assert_modules_with_env
    ~env
    ~implicit_opens:[ [ "Gooey"; "Aliases" ] ]
    ~filename:"element.mli"
    ~expected:[ "Config"; "Viewport" ]
    {ocaml|type t = {
  measure: (constraints:Super.Config.constraints -> Viewport.t);
}
|ocaml}

let deps_ignore_lowercase_field_access_roots ctx =
  let _ = ctx in
  assert_modules_with_env
    ~filename:"walker.ml"
    ~expected:[ "List" ]
    {ocaml|type opts = { follow_symlinks: bool }
type iterator_state = { roots: int list; opts: opts }
let update = fun state -> { state.opts with follow_symlinks = false }
let size = fun state -> List.length state.roots
let make =
let module Base = struct
  type state = iterator_state
  let size = fun state -> List.length state.roots
end in
()
|ocaml}

let deps_collect_variant_payload_modules_from_implicit_alias_opens ctx =
  let _ = ctx in
  let env =
    generated_alias_env [ generated_alias ~module_path:[ "Kernel"; "Aliases" ] [ "System_error" ]; ]
  in
  assert_modules_with_env
    ~env
    ~implicit_opens:[ [ "Kernel"; "Aliases" ] ]
    ~filename:"unix.mli"
    ~expected:[ "System_error" ]
    "type error =\n  | System of System_error.t\n"

let deps_collect_field_access_modules_from_implicit_alias_opens ctx =
  let _ = ctx in
  let env =
    generated_alias_env
      [ generated_alias ~module_path:[ "Kernel"; "Async"; "Aliases" ] [ "Libc" ]; ]
  in
  assert_modules_with_env
    ~env
    ~implicit_opens:[ [ "Kernel"; "Async"; "Aliases" ] ]
    ~filename:"unix.ml"
    ~expected:[ "Libc" ]
    "let is_error = fun event -> event.flags land Libc.ev_error != 0\n"

let deps_ignore_polymorphic_variant_tags ctx =
  let _ = ctx in
  assert_modules_with_env
    ~filename:"tags.ml"
    ~expected:[]
    {ocaml|type t = {
  mutable active: [
    `Data
    | `Control
  ];
}
let set_data = fun t -> t.active <- `Data
|ocaml}

let deps_ignore_local_module_type_in_first_class_module ctx =
  let _ = ctx in
  assert_modules_with_env
    ~filename:"iter.ml"
    ~expected:[]
    {ocaml|module type Intf = sig
  type state
  type item
end

type ('item, 'state) iter = (module Intf with type item = 'item and type state = External.state)
|ocaml}

let deps_bind_first_class_module_function_parameter ctx =
  let _ = ctx in
  assert_modules_with_env
    ~filename:"config.ml"
    ~expected:[]
    {ocaml|module type ConfigSpec = sig
  type t
  val spec: t
  val get: t -> string
end

let get (type a) ((module M : ConfigSpec with type t = a)) =
  M.get M.spec
|ocaml}

let deps_bind_first_class_module_match_pattern ctx =
  let _ = ctx in
  assert_modules_with_env
    ~filename:"packed.ml"
    ~expected:[]
    {ocaml|module type Intf = sig
  val render: int -> string
end

type t =
  | Pack: (module Intf) * int -> t

let render = function
  | Pack ((module I), state) -> I.render state
|ocaml}

let deps_local_open_keeps_child_modules_on_opened_root ctx =
  let _ = ctx in
  assert_modules_with_env
    ~filename:"action.ml"
    ~expected:[ "Dep_graph"; "Module" ]
    {ocaml|let output = fun mod_ ->
  let open Dep_graph in
  Module.cmi mod_
|ocaml}

let deps_open_keeps_child_modules_on_opened_root ctx =
  let _ = ctx in
  assert_modules_with_env
    ~filename:"stdin_bench.ml"
    ~expected:[ "Array"; "Std" ]
    {ocaml|open Std

let length = fun values -> Array.length values
|ocaml}

let tests =
  Test.[
    case "dep analyzer open Std resolves exported module" open_std_resolves_exported_module;
    case
      "dep analyzer unresolved open preserves root dependency"
      unresolved_open_preserves_root_dependency;
    case
      "dep analyzer local module binding does not escape as dependency"
      local_module_binding_does_not_escape_as_dependency;
    case
      "dep analyzer local module binding covers qualified record and type uses"
      local_module_binding_covers_qualified_record_and_type_uses;
    case
      "dep analyzer local module binding survives generated alias implicit opens"
      local_module_binding_survives_generated_alias_implicit_opens;
    case
      "dep analyzer qualified dependency resolves to provider root"
      qualified_dependency_resolves_to_provider_root;
    case
      "dep analyzer module alias keeps provider dependency"
      module_alias_keeps_provider_dependency;
    case "dep analyzer dependency IR preserves open order" dependency_ir_preserves_open_order;
    case "dep analyzer resolves local summaries as providers" resolves_local_summaries_as_providers;
    case
      "dep analyzer local module alias resolves inside source"
      local_module_alias_resolves_inside_source;
    case
      "dep analyzer nested summary provider resolves opened child"
      nested_summary_provider_resolves_opened_child;
    case
      "dep analyzer local nested module open uses binding tree"
      local_nested_module_open_uses_binding_tree;
    case "dep analyzer type alias records module dependency" type_alias_records_module_dependency;
    case
      "dep analyzer polymorphic variant payload records module dependency"
      polymorphic_variant_payload_records_module_dependency;
    case
      "dep analyzer polymorphic variant inherited row records module dependency"
      polymorphic_variant_inherited_row_records_module_dependency;
    case
      "dep analyzer qualified record expr records module dependency"
      qualified_record_expr_records_module_dependency;
    case
      "dep analyzer qualified record expr inside for records module dependency"
      qualified_record_expr_inside_for_records_module_dependency;
    case
      "dep analyzer qualified record expr after sibling aliases records module dependency"
      qualified_record_expr_after_sibling_aliases_records_module_dependency;
    case
      "dep analyzer qualified record expr in start_link records module dependency"
      qualified_record_expr_in_start_link_records_module_dependency;
    case
      "dep analyzer qualified record pattern records module dependency"
      qualified_record_pattern_records_module_dependency;
    case
      "dep analyzer qualified record pattern in labeled callback records module dependency"
      qualified_record_pattern_in_labeled_callback_records_module_dependency;
    case
      "dep analyzer nested syntax open does not shadow sibling record pattern"
      nested_syntax_open_does_not_shadow_sibling_record_pattern;
    case
      "dep analyzer first-class module pattern binds local module"
      first_class_module_pattern_binds_local_module;
    case
      "dep analyzer generated alias open resolves to child module"
      generated_alias_open_resolves_to_child_module;
    case
      "dep analyzer generated alias child open exposes child exports"
      generated_alias_child_open_exposes_child_exports;
    case
      "dep analyzer local namespace open exposes child exports"
      local_namespace_open_exposes_child_exports;
    case
      "dep analyzer open fallback shadows sibling summary for local open"
      open_fallback_shadows_sibling_summary_for_local_open;
    case
      "dep analyzer local child summary does not pollute public root exports"
      local_child_summary_does_not_pollute_public_root_exports;
    case
      "dep analyzer external nested open exposes children as dependency root"
      external_nested_open_exposes_children_as_dependency_root;
    case
      "dep analyzer deps collect value declaration modules from implicit alias opens"
      deps_collect_value_declaration_modules_from_implicit_alias_opens;
    case
      "dep analyzer deps collect manifest type modules from implicit alias opens"
      deps_collect_manifest_type_modules_from_implicit_alias_opens;
    case
      "dep analyzer deps collect qualified public root from implicit root alias open"
      deps_collect_qualified_public_root_from_implicit_root_alias_open;
    case
      "dep analyzer deps collect opened public root module instead of child module"
      deps_collect_opened_public_root_module_instead_of_child_module;
    case
      "dep analyzer deps collect opaque opened public root for child module"
      deps_collect_opaque_opened_public_root_for_child_module;
    case
      "dep analyzer deps collect opaque opened public root for qualified child module"
      deps_collect_opaque_opened_public_root_for_qualified_child_module;
    case
      "dep analyzer deps collect qualified public root module instead of child module"
      deps_collect_qualified_public_root_module_instead_of_child_module;
    case
      "dep analyzer deps do not collect exported children from module alias"
      deps_do_not_collect_exported_children_from_module_alias;
    case
      "dep analyzer deps do not collect exported children from facade alias"
      deps_do_not_collect_exported_children_from_facade_alias;
    case
      "dep analyzer deps open scoped root keeps child modules on opened root"
      deps_open_scoped_root_keeps_child_modules_on_opened_root;
    case
      "dep analyzer deps opened child named like current file stays on opened root"
      deps_opened_child_named_like_current_file_stays_on_opened_root;
    case
      "dep analyzer deps keep unknown qualified roots after open"
      deps_keep_unknown_qualified_roots_after_open;
    case
      "dep analyzer deps collect opened module root for exported child"
      deps_collect_opened_module_root_for_exported_child;
    case "dep analyzer deps collect Super alias child module" deps_collect_super_alias_child_module;
    case
      "dep analyzer deps ignore lowercase field access roots"
      deps_ignore_lowercase_field_access_roots;
    case
      "dep analyzer deps collect variant payload modules from implicit alias opens"
      deps_collect_variant_payload_modules_from_implicit_alias_opens;
    case
      "dep analyzer deps collect field access modules from implicit alias opens"
      deps_collect_field_access_modules_from_implicit_alias_opens;
    case "dep analyzer deps ignore polymorphic variant tags" deps_ignore_polymorphic_variant_tags;
    case
      "dep analyzer deps ignore local module type in first class module"
      deps_ignore_local_module_type_in_first_class_module;
    case
      "dep analyzer deps bind first class module function parameter"
      deps_bind_first_class_module_function_parameter;
    case
      "dep analyzer deps bind first class module match pattern"
      deps_bind_first_class_module_match_pattern;
    case
      "dep analyzer deps local open keeps child modules on opened root"
      deps_local_open_keeps_child_modules_on_opened_root;
    case
      "dep analyzer deps open keeps child modules on opened root"
      deps_open_keeps_child_modules_on_opened_root;
  ]

let main ~args = Test.Cli.main ~name:"dep_analyzer_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
