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

let sources = fun src ->
  Package.{
    src;
    native = [];
    tests = [];
    examples = [];
    bench = [];
  }

let write = fun path content ->
  Fs.write content path
  |> Result.expect ~msg:("expected file write to succeed: " ^ Path.to_string path)

let create_dir = fun path ->
  Fs.create_dir_all path
  |> Result.expect ~msg:("expected dir creation to succeed: " ^ Path.to_string path)

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
      let request =
        Riot_doc.{
          workspace;
          package_name = Some "app";
          all = false;
          release = false;
          output_root = Some Path.(tmpdir / Path.v "docs");
          force = true;
          no_cache = true;
        }
      in
      let* summaries = Riot_doc.run request in
      match summaries with
      | [ summary ] when Package_name.equal summary.Riot_doc.package app.name -> Ok ()
      | [ summary ] ->
          Error ("expected app docs, got " ^ Package_name.to_string summary.Riot_doc.package)
      | _ -> Error "expected exactly one generated docs summary") with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let name = "riot-doc"

let tests =
  Test.[
    case
      "doc resolves qualified child modules from opened dependency"
      test_doc_resolves_qualified_child_modules_from_opened_dependency;
  ]

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
