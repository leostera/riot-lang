open Std
open Std.Bench

module Package_manifest = Riot_model.Package_manifest

let bench_config: Bench.bench_config = { iterations = 20; warmup = 5 }

type fixture = {
  label: string;
  package_path: Path.t;
  manifest: Package_manifest.t;
}

let sink = ref 0

let consume_sources = fun (sources: Riot_model.Package.sources) ->
  let total =
    List.length sources.src
    + List.length sources.native
    + List.length sources.tests
    + List.length sources.examples
    + List.length sources.bench
  in
  sink := !sink + total;
  if total <= 0 then
    panic "expected source scan benchmark to discover at least one source file"

let load_manifest = fun ~workspace_root ~relative_path ->
  let package_path = Path.(workspace_root / relative_path) in
  let manifest_path = Path.(package_path / Path.v "riot.toml") in
  let manifest_source =
    Fs.read manifest_path
    |> Result.expect ~msg:"expected benchmark manifest to load"
  in
  let manifest_toml =
    Data.Toml.parse manifest_source
    |> Result.expect ~msg:"expected benchmark manifest to parse"
  in
  let manifest =
    Package_manifest.from_toml
      manifest_toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:package_path
      ~relative_path
    |> Result.expect ~msg:"expected benchmark package manifest to decode"
  in
  { label = Path.to_string relative_path; package_path; manifest }

let should_skip_source_entry = fun filename ->
  String.starts_with
    ~prefix:"."
    (Path.basename filename)

let should_skip_old_test_support_path = fun rel_path ->
  let path_str = Path.to_string rel_path in
  String.starts_with ~prefix:"tests/fixtures/" path_str
  || String.starts_with ~prefix:"tests/generated/" path_str
  || String.starts_with ~prefix:"tests/diagnostics/" path_str

let old_collect_relative_files = fun ~package_path ~root ?(excluded_relpaths = []) () ->
  let excluded_relpath_strings = List.map excluded_relpaths ~fn:Path.to_string in
  let walker =
    match Fs.Walker.create ~roots:[ root ] ~sort:true ~follow_symlinks:true () with
    | Ok walker -> walker
    | Error _ -> panic "old-style benchmark walker configuration should be valid"
  in
  let walker =
    Fs.Walker.filter_entry
      walker
      ~f:(fun (entry: Fs.Walker.FileItem.t) ->
        let path = Fs.Walker.FileItem.path entry in
        if Int.equal (Fs.Walker.FileItem.depth entry) 0 then
          true
        else
          match Path.strip_prefix path ~prefix:package_path with
          | Error _ -> false
          | Ok rel_path ->
              match Fs.Walker.FileItem.kind entry with
              | Directory ->
                  not
                    (should_skip_source_entry rel_path || should_skip_old_test_support_path rel_path)
              | File ->
                  not
                    (should_skip_source_entry rel_path
                    || should_skip_old_test_support_path rel_path
                    || List.contains excluded_relpath_strings ~value:(Path.to_string rel_path))
              | Symlink
              | Other -> false)
  in
  let iter = Fs.Walker.into_iter walker in
  let rec loop acc iter =
    match Iter.Iterator.next iter with
    | (None, _) -> List.reverse acc
    | (Some (Error _), iter') -> loop acc iter'
    | (Some (Ok (entry: Fs.Walker.FileItem.t)), iter') ->
        let path = Fs.Walker.FileItem.path entry in
        match Fs.Walker.FileItem.kind entry with
        | File ->
            (match Path.strip_prefix path ~prefix:package_path with
            | Ok rel_path -> loop (rel_path :: acc) iter'
            | Error _ -> loop acc iter')
        | Directory
        | Symlink
        | Other -> loop acc iter'
  in
  loop [] iter

let old_scan_sources = fun ~package_path ~roots ->
  let collect root = old_collect_relative_files ~package_path ~root () in
  let sources_for_root root =
    let root_dir = Path.(package_path / Path.v root) in
    collect root_dir
  in
  let find_bucket name =
    if List.contains roots ~value:name then
      sources_for_root name
    else
      []
  in
  Riot_model.Package.{
    src = find_bucket "src";
    native = find_bucket "native";
    tests = find_bucket "tests";
    examples = find_bucket "examples";
    bench = find_bucket "bench";
  }

let bench_old_style_runtime = fun (fixture: fixture) () ->
  old_scan_sources ~package_path:fixture.package_path ~roots:[ "src"; "native" ]
  |> consume_sources

let bench_old_style_all_buckets = fun (fixture: fixture) () ->
  old_scan_sources
    ~package_path:fixture.package_path
    ~roots:[ "src"; "tests"; "native"; "examples"; "bench"; ]
  |> consume_sources

let bench_current_runtime = fun (fixture: fixture) () ->
  Package_manifest.realize ~intent:Package_manifest.Runtime fixture.manifest
  |> fun pkg -> consume_sources pkg.sources

let bench_current_dev = fun (fixture: fixture) () ->
  Package_manifest.realize ~intent:Package_manifest.Dev fixture.manifest
  |> fun pkg -> consume_sources pkg.sources

let benchmark_group = fun (fixture: fixture) ->
  Bench.[
    with_config
      ~config:bench_config
      ("source scan old-style runtime buckets: " ^ fixture.label)
      (bench_old_style_runtime fixture);
    with_config
      ~config:bench_config
      ("source scan current runtime realize: " ^ fixture.label)
      (bench_current_runtime fixture);
    with_config
      ~config:bench_config
      ("source scan old-style all buckets: " ^ fixture.label)
      (bench_old_style_all_buckets fixture);
    with_config
      ~config:bench_config
      ("source scan current dev realize: " ^ fixture.label)
      (bench_current_dev fixture);
  ]

let benchmarks = fun () ->
  let workspace_root =
    Env.current_dir ()
    |> Result.expect ~msg:"expected benchmark cwd"
  in
  let fixtures = [
    load_manifest ~workspace_root ~relative_path:(Path.v "packages/std");
    load_manifest ~workspace_root ~relative_path:(Path.v "packages/kernel");
    load_manifest ~workspace_root ~relative_path:(Path.v "packages/syn");
  ]
  in
  fixtures
  |> List.map ~fn:benchmark_group
  |> List.concat

let main ~args =
  Bench.Cli.main ~name:"Riot Model Source Scan Benchmarks" ~benchmarks:(benchmarks ()) ~args

let () = Runtime.run ~main ~args:Env.args ()
