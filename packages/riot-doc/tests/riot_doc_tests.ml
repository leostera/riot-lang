open Std
open Std.Result.Syntax
open Riot_model

module Test = Std.Test

let package_name = fun name ->
  Package_name.from_string name
  |> Result.expect ~msg:("expected valid package name: " ^ name)

let source = fun ?(workspace = false) () ->
  Package.{
    workspace;
    builtin = false;
    path = None;
    source_locator = None;
    ref_ = None;
    version = None;
  }

let dependency = fun name ->
  Package.{ name = package_name name; source = source ~workspace:true () }

let sources = fun ?(examples = []) src ->
  Package.{
    src;
    native = [];
    tests = [];
    examples;
    bench = [];
  }

let write = fun path content ->
  Fs.write content path
  |> Result.expect ~msg:("expected file write to succeed: " ^ Path.to_string path)

let create_dir = fun path ->
  Fs.create_dir_all path
  |> Result.expect ~msg:("expected dir creation to succeed: " ^ Path.to_string path)

let read_file = fun path ->
  Fs.read path
  |> Result.map_err ~fn:IO.error_message

let assert_contains = fun ~label text expected ->
  if String.contains text expected then
    Ok ()
  else
    Error ("expected " ^ label ^ " to contain: " ^ expected)

let assert_not_contains = fun ~label text unexpected ->
  if String.contains text unexpected then
    Error ("expected " ^ label ^ " not to contain: " ^ unexpected)
  else
    Ok ()

let make_doc_request = fun ~workspace ~output_dir package_name ->
  Riot_doc.{
    workspace;
    package_name = Some package_name;
    all = false;
    release = false;
    output_root = Some output_dir;
    force = true;
    no_cache = true;
  }

let test_doc_resolves_qualified_child_modules_from_opened_dependency = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_doc_opened_dependency"
    (fun tmpdir ->
      let pkg_root = Path.(tmpdir / Path.v "pkg") in
      let pkg_src = Path.(pkg_root / Path.v "src") in
      let pkg_a_src = Path.(pkg_src / Path.v "A") in
      let app_root = Path.(tmpdir / Path.v "app") in
      let app_src = Path.(app_root / Path.v "src") in
      create_dir pkg_a_src;
      create_dir app_src;
      write Path.(pkg_src / Path.v "pkg.ml") {ocaml|module A = A
|ocaml};
      write
        Path.(pkg_src / Path.v "pkg.mli")
        {ocaml|module A: sig
  module B: sig
    val make: unit -> int
  end
end
|ocaml};
      write Path.(pkg_a_src / Path.v "A.ml") {ocaml|module B = B
|ocaml};
      write Path.(pkg_a_src / Path.v "B.ml") {ocaml|let make = fun () -> 1
|ocaml};
      write Path.(app_src / Path.v "app.ml") {ocaml|open Pkg

let value = A.B.make ()
|ocaml};
      write Path.(app_src / Path.v "app.mli") {ocaml|val value: int
|ocaml};
      let pkg =
        Package.make
          ~name:(package_name "pkg")
          ~path:pkg_root
          ~relative_path:(Path.v "pkg")
          ~library:{ path = Path.v "src/pkg.ml" }
          ~sources:(sources
            [ Path.v "src/pkg.ml"; Path.v "src/pkg.mli"; Path.v "src/A/A.ml"; Path.v "src/A/B.ml"; ])
          ()
      in
      let app =
        Package.make
          ~name:(package_name "app")
          ~path:app_root
          ~relative_path:(Path.v "app")
          ~dependencies:[ dependency "pkg" ]
          ~library:{ path = Path.v "src/app.ml" }
          ~sources:(sources [ Path.v "src/app.ml"; Path.v "src/app.mli" ])
          ()
      in
      let workspace =
        Workspace.make_realized ~root:tmpdir ~packages:[ pkg; app ] ~target_dir:"target" ()
      in
      let request = make_doc_request ~workspace ~output_dir:Path.(tmpdir / Path.v "docs") "app" in
      let* summaries = Riot_doc.run request in
      match summaries with
      | [ summary ] when Package_name.equal summary.Riot_doc.package app.name -> Ok ()
      | [ summary ] ->
          Error ("expected app docs, got " ^ Package_name.to_string summary.Riot_doc.package)
      | _ -> Error "expected exactly one generated docs summary") with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let test_doc_generates_root_module_detail_page = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_doc_root_module_page"
    (fun tmpdir ->
      let pkg_root = Path.(tmpdir / Path.v "pretext") in
      let pkg_src = Path.(pkg_root / Path.v "src") in
      let pkg_examples = Path.(pkg_root / Path.v "examples") in
      let output_root = Path.(tmpdir / Path.v "docs") in
      create_dir pkg_src;
      create_dir pkg_examples;
      write
        Path.(pkg_src / Path.v "pretext.ml")
        {ocaml|let max = 80

let format = fun value -> value
|ocaml};
      write
        Path.(pkg_src / Path.v "pretext.mli")
        {ocaml|(** Tiny text documents. *)

(** Maximum default width. *)
val max: int

(** Format a document to text. *)
val format: string -> string
|ocaml};
      write Path.(pkg_src / Path.v "main.ml") {ocaml|let main ~args:_ = ()
|ocaml};
      write Path.(pkg_src / Path.v "serve_cmd.ml") {ocaml|let main ~args:_ = ()
|ocaml};
      write Path.(pkg_src / Path.v "pretext_rules.ml") {ocaml|let rules = []
|ocaml};
      write
        Path.(pkg_examples / Path.v "basic.ml")
        {ocaml|let value = Pretext.format "hello"
|ocaml};
      let pkg_name = package_name "pretext" in
      let pkg =
        Package.make
          ~name:pkg_name
          ~path:pkg_root
          ~relative_path:(Path.v "pretext")
          ~binaries:[ { name = "pretext"; path = Path.v "src/main.ml" } ]
          ~commands:[
            Package_command.{
              name = "serve";
              description = "Serve Pretext documents.";
              package_name = pkg_name;
              package_path = pkg_root;
              command_module = "Serve_cmd";
              command_source = Path.v "src/serve_cmd.ml";
              command_binary = Path.v "_build/out/pretext/Serve_cmd";
            };
          ]
          ~fix_providers:[
            Fix_provider.{
              name = "pretext-rules";
              package_name = pkg_name;
              package_path = pkg_root;
              source_path = Path.v "src/pretext_rules.ml";
              rules = [ "pretext/no-empty"; ];
            };
          ]
          ~library:{ path = Path.v "src/pretext.ml" }
          ~sources:(sources
            ~examples:[ Path.v "examples/basic.ml" ]
            [
              Path.v "src/pretext.ml";
              Path.v "src/pretext.mli";
              Path.v "src/main.ml";
              Path.v "src/serve_cmd.ml";
              Path.v "src/pretext_rules.ml";
            ])
          ()
      in
      let workspace =
        Workspace.make_realized ~root:tmpdir ~packages:[ pkg ] ~target_dir:"target" ()
      in
      let request = make_doc_request ~workspace ~output_dir:output_root "pretext" in
      let* summaries = Riot_doc.run request in
      let* summary =
        match summaries with
        | [ summary ] -> Ok summary
        | _ -> Error "expected one generated docs summary"
      in
      let* index = read_file Path.(summary.Riot_doc.output_dir / Path.v "index.html") in
      let* manifest = read_file Path.(summary.Riot_doc.output_dir / Path.v "manifest.json") in
      let* root_page =
        read_file Path.(summary.Riot_doc.output_dir / Path.v "Pretext" / Path.v "index.html")
      in
      let* () = assert_contains ~label:"package index" index "href=\"Pretext/index.html\"" in
      let* () =
        assert_not_contains
          ~label:"package index"
          index
          "href=\"Pretext/index.html#function_format\""
      in
      let* () = assert_contains ~label:"package index" index "id=\"commands\"" in
      let* () = assert_contains ~label:"package index" index "Serve Pretext documents." in
      let* () = assert_contains ~label:"package index" index "id=\"executables\"" in
      let* () = assert_contains ~label:"package index" index "src/main.ml" in
      let* () = assert_contains ~label:"package index" index "id=\"lint-rules\"" in
      let* () = assert_contains ~label:"package index" index "pretext/no-empty" in
      let* () = assert_contains ~label:"package index" index "id=\"examples\"" in
      let* () = assert_contains ~label:"package index" index "examples/basic.ml" in
      let* () =
        assert_contains ~label:"docs manifest" manifest "\"schema\": \"riot-doc.manifest.v1\""
      in
      let* () = assert_contains ~label:"docs manifest" manifest "\"generator\": \"riot-doc:v26\"" in
      let* () = assert_contains ~label:"docs manifest" manifest "\"manifest.json\"" in
      let* () = assert_not_contains ~label:"root module page" root_page "Redirecting..." in
      let* () =
        assert_contains
          ~label:"root module page"
          root_page
          "<article class=\"item-detail\" data-kind=\"val\" id=\"value_max\">"
      in
      let* () =
        assert_contains
          ~label:"root module page"
          root_page
          "<span class=\"item-detail-signature\">: int</span>"
      in
      let* () =
        assert_contains
          ~label:"root module page"
          root_page
          "<article class=\"item-detail\" data-kind=\"fn\" id=\"function_format\">"
      in
      let* () =
        assert_contains ~label:"root module page" root_page "<h1 class=\"page-title\">Pretext</h1>"
      in
      let* () = assert_contains ~label:"root module page" root_page "data-filterable" in
      let* () =
        assert_contains
          ~label:"root module page"
          root_page
          "<span class=\"item-detail-signature\">: string -&gt; string</span>"
      in
      let* () = assert_not_contains ~label:"root module page" root_page "val format:" in
      let* () = assert_not_contains ~label:"root module page" root_page "(** Format" in
      assert_contains ~label:"root module page" root_page "Format a document to text.") with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let test_doc_uses_nested_module_source_for_item_snippets = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_doc_split_module_snippets"
    (fun tmpdir ->
      let pkg_root = Path.(tmpdir / Path.v "parquet") in
      let pkg_src = Path.(pkg_root / Path.v "src") in
      let output_root = Path.(tmpdir / Path.v "docs") in
      create_dir pkg_src;
      write Path.(pkg_src / Path.v "parquet.ml") {ocaml|module Reader = Reader
|ocaml};
      write
        Path.(pkg_src / Path.v "reader.ml")
        {ocaml|type t = string

let from_string = fun value -> value
|ocaml};
      write
        Path.(pkg_src / Path.v "parquet.mli")
        {ocaml|(** Parquet facade. *)
module Reader: module type of Reader
|ocaml};
      write
        Path.(pkg_src / Path.v "reader.mli")
        {ocaml|(** Reader API. *)
type t

(** Decode a reader from a string. *)
val from_string: string -> t
|ocaml};
      let pkg =
        Package.make
          ~name:(package_name "parquet")
          ~path:pkg_root
          ~relative_path:(Path.v "parquet")
          ~library:{ path = Path.v "src/parquet.ml" }
          ~sources:(sources
            [
              Path.v "src/parquet.ml";
              Path.v "src/parquet.mli";
              Path.v "src/reader.ml";
              Path.v "src/reader.mli";
            ])
          ()
      in
      let workspace =
        Workspace.make_realized ~root:tmpdir ~packages:[ pkg ] ~target_dir:"target" ()
      in
      let request = make_doc_request ~workspace ~output_dir:output_root "parquet" in
      let* summaries = Riot_doc.run request in
      let* summary =
        match summaries with
        | [ summary ] -> Ok summary
        | _ -> Error "expected one generated docs summary"
      in
      let reader_dir = Path.(summary.Riot_doc.output_dir / Path.v "Parquet" / Path.v "Reader") in
      let* reader_page = read_file Path.(reader_dir / Path.v "index.html") in
      let* reader_source = read_file Path.(reader_dir / Path.v "source.html") in
      let* () =
        assert_contains
          ~label:"nested module page"
          reader_page
          "<article class=\"item-detail\" data-kind=\"fn\" id=\"function_from_string\">"
      in
      let* () =
        assert_contains
          ~label:"nested module page"
          reader_page
          "<span class=\"item-detail-signature\">: string -&gt; t</span>"
      in
      let* () =
        assert_contains
          ~label:"nested module source"
          reader_source
          "val from_string: string -&gt; t"
      in
      assert_not_contains ~label:"nested module source" reader_source "module Reader") with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let test_doc_extracts_record_field_docstrings_from_signatures = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_doc_record_field_docstrings"
    (fun tmpdir ->
      let pkg_root = Path.(tmpdir / Path.v "dotenv") in
      let pkg_src = Path.(pkg_root / Path.v "src") in
      let output_root = Path.(tmpdir / Path.v "docs") in
      create_dir pkg_src;
      write
        Path.(pkg_src / Path.v "dotenv.ml")
        {ocaml|type binding = {
  key: string;
  value: string;
  line: int;
}
|ocaml};
      write
        Path.(pkg_src / Path.v "dotenv.mli")
        {ocaml|(** Dotenv parser. *)

(** A parsed dotenv assignment. *)
type binding = {
  (** Environment variable name, such as `DATABASE_URL`. *)
  key: string;

  (** Parsed value after unescaping and substitution. *)
  value: string;

  (** 1-based source line where the assignment started. *)
  line: int;
}
|ocaml};
      let pkg =
        Package.make
          ~name:(package_name "dotenv")
          ~path:pkg_root
          ~relative_path:(Path.v "dotenv")
          ~library:{ path = Path.v "src/dotenv.ml" }
          ~sources:(sources [ Path.v "src/dotenv.ml"; Path.v "src/dotenv.mli" ])
          ()
      in
      let workspace =
        Workspace.make_realized ~root:tmpdir ~packages:[ pkg ] ~target_dir:"target" ()
      in
      let request = make_doc_request ~workspace ~output_dir:output_root "dotenv" in
      let* summaries = Riot_doc.run request in
      let* summary =
        match summaries with
        | [ summary ] -> Ok summary
        | _ -> Error "expected one generated docs summary"
      in
      let* root_page =
        read_file Path.(summary.Riot_doc.output_dir / Path.v "Dotenv" / Path.v "index.html")
      in
      let* () = assert_contains ~label:"root module page" root_page "key: string" in
      let* () = assert_contains ~label:"root module page" root_page "Environment variable name" in
      let* () =
        assert_contains
          ~label:"root module page"
          root_page
          "<article class=\"item-detail\" data-kind=\"record\" id=\"type_binding\">"
      in
      assert_not_contains ~label:"root module page" root_page "(** Environment variable name") with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let test_doc_preserves_fenced_code_block_relative_indentation = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_doc_code_fence_indentation"
    (fun tmpdir ->
      let pkg_root = Path.(tmpdir / Path.v "dotenv") in
      let pkg_src = Path.(pkg_root / Path.v "src") in
      let output_root = Path.(tmpdir / Path.v "docs") in
      create_dir pkg_src;
      write
        Path.(pkg_src / Path.v "dotenv.ml")
        {ocaml|type binding = string

let error_to_string = fun value -> value

let parse = fun _source -> Ok []
|ocaml};
      write
        Path.(pkg_src / Path.v "dotenv.mli")
        {ocaml|(** Dotenv parser. *)

type binding

val error_to_string: string -> string

(** Parse dotenv text.

    ```ocaml
    let bindings =
      match Dotenv.parse "HOST=localhost\nURL=http://$HOST:8080" with
      | Ok bindings -> bindings
      | Error error ->
          Std.eprintln (Dotenv.error_to_string error);
          []
    ```
*)
val parse: string -> (binding list, string) result
|ocaml};
      let pkg =
        Package.make
          ~name:(package_name "dotenv")
          ~path:pkg_root
          ~relative_path:(Path.v "dotenv")
          ~library:{ path = Path.v "src/dotenv.ml" }
          ~sources:(sources [ Path.v "src/dotenv.ml"; Path.v "src/dotenv.mli" ])
          ()
      in
      let workspace =
        Workspace.make_realized ~root:tmpdir ~packages:[ pkg ] ~target_dir:"target" ()
      in
      let request = make_doc_request ~workspace ~output_dir:output_root "dotenv" in
      let* summaries = Riot_doc.run request in
      let* summary =
        match summaries with
        | [ summary ] -> Ok summary
        | _ -> Error "expected one generated docs summary"
      in
      let* root_page =
        read_file Path.(summary.Riot_doc.output_dir / Path.v "Dotenv" / Path.v "index.html")
      in
      let* () =
        assert_contains
          ~label:"root module page"
          root_page
          "<pre><code class=\"language-ocaml\">let bindings =\n  match Dotenv.parse"
      in
      let* () =
        assert_contains
          ~label:"root module page"
          root_page
          "| Error error -&gt;\n      Std.eprintln"
      in
      assert_contains ~label:"root module page" root_page "      []") with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let test_doc_ignores_binary_sources_when_collecting_interfaces = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_doc_binary_sources"
    (fun tmpdir ->
      let pkg_root = Path.(tmpdir / Path.v "synlike") in
      let pkg_src = Path.(pkg_root / Path.v "src") in
      let output_root = Path.(tmpdir / Path.v "docs") in
      create_dir pkg_src;
      write Path.(pkg_src / Path.v "synlike.ml") {ocaml|let parse = fun value -> value
|ocaml};
      write
        Path.(pkg_src / Path.v "synlike.mli")
        {ocaml|(** Synlike facade. *)

(** Parse a value. *)
val parse: string -> string
|ocaml};
      write
        Path.(pkg_src / Path.v "main.ml")
        {ocaml|let main ~args:_ =
  let _ = Synlike.parse "ok" in
  ()
|ocaml};
      let pkg =
        Package.make
          ~name:(package_name "synlike")
          ~path:pkg_root
          ~relative_path:(Path.v "synlike")
          ~binaries:[ { name = "synlike"; path = Path.v "src/main.ml" } ]
          ~library:{ path = Path.v "src/synlike.ml" }
          ~sources:(sources
            [ Path.v "src/synlike.ml"; Path.v "src/synlike.mli"; Path.v "src/main.ml"; ])
          ()
      in
      let workspace =
        Workspace.make_realized ~root:tmpdir ~packages:[ pkg ] ~target_dir:"target" ()
      in
      let request = make_doc_request ~workspace ~output_dir:output_root "synlike" in
      let* summaries = Riot_doc.run request in
      let* summary =
        match summaries with
        | [ summary ] -> Ok summary
        | _ -> Error "expected one generated docs summary"
      in
      let* root_page =
        read_file Path.(summary.Riot_doc.output_dir / Path.v "Synlike" / Path.v "index.html")
      in
      let* () =
        assert_contains
          ~label:"root module page"
          root_page
          "<span class=\"item-detail-signature\">: string -&gt; string</span>"
      in
      assert_not_contains ~label:"root module page" root_page "Synlike/Main/index.html") with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let name = "riot-doc"

let tests =
  Test.[
    case
      "doc resolves qualified child modules from opened dependency"
      test_doc_resolves_qualified_child_modules_from_opened_dependency;
    case "doc generates root module detail page" test_doc_generates_root_module_detail_page;
    case
      "doc uses nested module source for item snippets"
      test_doc_uses_nested_module_source_for_item_snippets;
    case
      "doc extracts record field docstrings from signatures"
      test_doc_extracts_record_field_docstrings_from_signatures;
    case
      "doc preserves fenced code block relative indentation"
      test_doc_preserves_fenced_code_block_relative_indentation;
    case
      "doc ignores binary sources when collecting interfaces"
      test_doc_ignores_binary_sources_when_collecting_interfaces;
  ]

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
