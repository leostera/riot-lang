module Conf = Config
open Std
open Tusk_model

(* Direct references to avoid cyclic dependency *)
module Module_graph = Analyzer.Module_graph
module Module_node = Analyzer.Module_node

module G = Std.Graph.SimpleGraph

type module_fact = {
  uri : Poneglyph.Uri.t;
  name : string;
  qualified_name : string;
  package_name : string;
  package_uri : Poneglyph.Uri.t;
  file_path : Path.t;
  file_uri : Poneglyph.Uri.t;
  kind : [ `ML | `MLI ];
}

(** Extract module facts from a graph node *)
let extract_module_fact ~package_name ~sha256 (node : Module_node.t G.node) =
  let data = node.value in
  match data.kind with
  | Module_node.ML m | Module_node.MLI m ->
      (* Only extract facts for concrete source files, skip generated ones *)
      (match data.file with
      | Module_node.Concrete fp ->
          let module_name = Module.module_name m in
          let simple_name = Module_name.simple_name module_name in
          let qualified_name = Module_name.qualified_name module_name in
          let package_uri = Schema.Tusk.Package.uri package_name in
          let file_uri = Schema.Codedb.File.uri ~path:(Path.to_string fp) ~sha256 in
          let kind =
            match data.kind with
            | Module_node.ML _ -> `ML
            | Module_node.MLI _ -> `MLI
            | _ -> panic "unreachable: extract_module_fact matched non-ML/MLI node"
          in
          Some {
            uri = Schema.OCaml.Module.uri qualified_name;
            name = simple_name;
            qualified_name;
            package_name;
            package_uri;
            file_path = fp;
            file_uri;
            kind;
          }
      | Module_node.Generated _ ->
          (* Skip generated files - they don't exist in source tree *)
          None)
  | _ -> None

(** Index a single package and store facts in Poneglyph *)
let index_package ~config ~db (pkg : Package.t) =
  let start_time = Time.Instant.now () in
  try
    let file_count = ref 0 in
    let all_facts = ref [] in
    let skipped_count = ref 0 in
    
    (* Index src/ directory using module graph *)
    let graph_config : Module_graph.config =
      {
        root = pkg.path;
        source_dir = Path.v "src";
        namespace = String.capitalize_ascii pkg.name;
        package = pkg;
        toolchain = Conf.toolchain config;
        workspace = Conf.workspace config;
      }
    in

    let graph = Module_graph.create graph_config in
    Module_graph.wire_dependencies graph (Path.v "src");

    (* Extract module facts from src/ directory *)
    let g = Module_graph.graph graph in
    let facts = ref [] in
    
    G.iter g ~fn:(fun _id node ->
        (* First, get the file path from the node to read it *)
        let data = node.value in
        let file_path_opt = match data.kind with
          | Module_node.ML _ | Module_node.MLI _ ->
              (match data.file with
              | Module_node.Concrete fp -> Some fp
              | Module_node.Generated _ -> None)
          | _ -> None
        in
        
        match file_path_opt with
        | None -> ()
        | Some relative_path ->
            (* Construct absolute path and read file *)
            let absolute_path = Path.(pkg.path / relative_path) in
            (match Fs.read_to_string absolute_path with
            | Error _err -> ()
            | Ok content ->
                (* Compute SHA256 *)
                let hash = Crypto.Sha256.hash_string content in
                let sha256 = Crypto.Digest.hex hash in
                let analysis_uri = Schema.Codedb.Analysis.uri ~sha256 in
                
                (* Check if already analyzed *)
                if Poneglyph.exists db analysis_uri then
                  skipped_count := !skipped_count + 1
                else
                  (* Now extract module fact with SHA256 *)
                  (match extract_module_fact ~package_name:pkg.name ~sha256 node with
                  | None -> ()
                  | Some fact ->
                      file_count := !file_count + 1;
                      (* Store fact with absolute path *)
                      let fact_with_absolute_path = { fact with file_path = absolute_path } in
                       facts := (fact_with_absolute_path, content, sha256) :: !facts)));
    
    (* Add facts from src/ *)
    all_facts := !facts @ !all_facts;
    
    (* Index test/example/bench files directly (they're binaries, not in module graph) *)
    (* Binaries don't have a namespace - they're standalone modules *)
    let empty_namespace = Tusk_model.Namespace.of_string "" in
    
    let index_standalone_files file_paths =
      List.iter (fun file_path ->
        let absolute_path = Path.(pkg.path / file_path) in
        match Fs.read_to_string absolute_path with
        | Error _ -> ()
        | Ok content ->
            let hash = Crypto.Sha256.hash_string content in
            let sha256 = Crypto.Digest.hex hash in
            let analysis_uri = Schema.Codedb.Analysis.uri ~sha256 in
            
            if Poneglyph.exists db analysis_uri then
              skipped_count := !skipped_count + 1
            else (
              (* Only index .ml and .mli files *)
              match Path.extension file_path with
              | Some ".ml" | Some ".mli" ->
                  (* Binaries use empty namespace - module name is just the filename *)
                  let module_obj = Tusk_model.Module.make ~namespace:empty_namespace ~filename:absolute_path in
                  let module_name = Tusk_model.Module.module_name module_obj in
                  let simple_name = Tusk_model.Module_name.simple_name module_name in
                  let qualified_name = Tusk_model.Module_name.qualified_name module_name in
                  let package_uri = Schema.Tusk.Package.uri pkg.name in
                  let file_uri = Schema.Codedb.File.uri ~path:(Path.to_string absolute_path) ~sha256 in
                  let kind = if Path.extension file_path = Some ".mli" then `MLI else `ML in
                  let fact = {
                    uri = Schema.OCaml.Module.uri qualified_name;
                    name = simple_name;
                    qualified_name;
                    package_name = pkg.name;
                    package_uri;
                    file_path = absolute_path;
                    file_uri;
                    kind;
                  } in
                  file_count := !file_count + 1;
                  all_facts := (fact, content, sha256) :: !all_facts
              | _ -> ()
            )
      ) file_paths
    in
    
    index_standalone_files pkg.sources.tests;
    index_standalone_files pkg.sources.examples;
    index_standalone_files pkg.sources.bench;
    
    let facts = !all_facts in

  (* Convert to Poneglyph facts *)
  let tx_id = UUID.v7_monotonic () in
  let source = Schema.Codedb.source in
  let stated_at = Datetime.now () in
  
  let poneglyph_facts =
    List.concat_map
      (fun (fact, content, sha256) ->
        let entity = fact.uri in
        let file_uri = Schema.Codedb.File.uri ~path:(Path.to_string fact.file_path) ~sha256 in
        let analysis_uri = Schema.Codedb.Analysis.uri ~sha256 in
        let file_size = String.length content in

        let package_uri = Schema.Tusk.Package.uri fact.package_name in
        
        List.concat [
          (* Module facts - using both Codedb and OCaml schemas *)
          [
            (* Codedb attributes *)
            Poneglyph.fact ~source ~entity ~attribute:Schema.Codedb.kind
              ~value:(Poneglyph.Fact.String (match fact.kind with `ML -> "ml" | `MLI -> "mli"))
              ~tx_id ~stated_at;
            Poneglyph.fact ~source ~entity ~attribute:Schema.Codedb.package
              ~value:(Poneglyph.Fact.Uri package_uri) ~tx_id ~stated_at;
            Poneglyph.fact ~source ~entity ~attribute:Schema.Codedb.package_name
              ~value:(Poneglyph.Fact.String fact.package_name) ~tx_id ~stated_at;
            Poneglyph.fact ~source ~entity ~attribute:Schema.Codedb.path
              ~value:(Poneglyph.Fact.String (Path.to_string fact.file_path)) ~tx_id ~stated_at;
            
            (* OCaml-specific name attributes *)
            Poneglyph.fact ~source ~entity ~attribute:Schema.OCaml.simple_name
              ~value:(Poneglyph.Fact.String fact.name) ~tx_id ~stated_at;
            Poneglyph.fact ~source ~entity ~attribute:Schema.OCaml.qualified_name
              ~value:(Poneglyph.Fact.String fact.qualified_name) ~tx_id ~stated_at;
            
            (* Link module to file entity via URI *)
            Poneglyph.fact ~source ~entity ~attribute:(
              match fact.kind with
              | `ML -> Schema.OCaml.implementation_file
              | `MLI -> Schema.OCaml.interface_file
            )
              ~value:(Poneglyph.Fact.Uri file_uri) ~tx_id ~stated_at;
          ];
          (* File facts *)
          [
            Poneglyph.fact ~source ~entity:file_uri ~attribute:Schema.Codedb.path
              ~value:(Poneglyph.Fact.String (Path.to_string fact.file_path)) ~tx_id ~stated_at;
            Poneglyph.fact ~source ~entity:file_uri ~attribute:Schema.Codedb.sha256
              ~value:(Poneglyph.Fact.String sha256) ~tx_id ~stated_at;
            Poneglyph.fact ~source ~entity:file_uri ~attribute:Schema.Codedb.size
              ~value:(Poneglyph.Fact.Int file_size) ~tx_id ~stated_at;
            Poneglyph.fact ~source ~entity:file_uri ~attribute:Schema.Codedb.modified_at
              ~value:(Poneglyph.Fact.DateTime stated_at) ~tx_id ~stated_at;
          ];
          (* Analysis facts *)
          [
            Poneglyph.fact ~source ~entity:analysis_uri ~attribute:Schema.Codedb.analysis_of
              ~value:(Poneglyph.Fact.Uri file_uri) ~tx_id ~stated_at;
            Poneglyph.fact ~source ~entity:analysis_uri ~attribute:Schema.Codedb.analyzed_at
              ~value:(Poneglyph.Fact.DateTime stated_at) ~tx_id ~stated_at;
          ];
        ])
      facts
  in

    (* Store facts in Poneglyph *)
    let fact_count = Poneglyph.state db poneglyph_facts in
    
    (* Calculate elapsed time *)
    let elapsed = Time.Instant.elapsed start_time in
    let elapsed_ms = Time.Duration.to_millis elapsed in
    
    (* Print summary *)
    let summary = 
      if !skipped_count > 0 then
        "  📦 " ^ pkg.name ^ "...  " ^ Int.to_string !file_count ^ " files / " ^
        Int.to_string (List.length facts) ^ " modules / " ^
        Int.to_string fact_count ^ " facts  (" ^
        Int.to_string elapsed_ms ^ "ms) [" ^ Int.to_string !skipped_count ^ " cached]"
      else
        "  📦 " ^ pkg.name ^ "...  " ^ Int.to_string !file_count ^ " files / " ^
        Int.to_string (List.length facts) ^ " modules / " ^
        Int.to_string fact_count ^ " facts  (" ^
        Int.to_string elapsed_ms ^ "ms)"
    in
    println summary;
    ()
  with
  | exn ->
      println ("  📦 " ^ pkg.name ^ "...  ✗ Error: " ^ Exception.to_string exn);
      ()

(** Index all packages in workspace *)
let index_workspace ~config ~db =
  let start_time = Time.Instant.now () in
  let workspace = Conf.workspace config in
  let packages = workspace.packages in

  List.iter (fun pkg -> index_package ~config ~db pkg) packages;

  let elapsed = Time.Instant.elapsed start_time in
  let elapsed_ms = Time.Duration.to_millis elapsed in
  
  println
    ("\n✅ Indexed " ^ Int.to_string (List.length packages) ^ " packages in "
   ^ Int.to_string elapsed_ms ^ "ms\n")

(** Find which package a file belongs to *)
let find_package_for_file ~config ~file_path =
  let workspace = Conf.workspace config in
  List.find_opt
    (fun (pkg : Package.t) ->
      (* Check if file_path starts with pkg.path *)
      let pkg_path_str = Path.to_string pkg.path in
      let file_path_str = Path.to_string file_path in
      String.starts_with ~prefix:pkg_path_str file_path_str)
    workspace.packages

(** Extract module info from a single file without building the graph *)
let extract_file_module_info ~package_name ~namespace ~file_path ~sha256 =
  let kind =
    match Path.extension file_path with
    | Some ".ml" -> Some `ML
    | Some ".mli" -> Some `MLI
    | _ -> None
  in
  Option.map
    (fun k ->
      let module_obj = Tusk_model.Module.make ~namespace ~filename:file_path in
      let module_name = Tusk_model.Module.module_name module_obj in
      let simple_name = Tusk_model.Module_name.simple_name module_name in
      let qualified_name = Tusk_model.Module_name.qualified_name module_name in
      let package_uri = Schema.Tusk.Package.uri package_name in
      let file_uri = Schema.Codedb.File.uri ~path:(Path.to_string file_path) ~sha256 in
      {
        uri = Schema.OCaml.Module.uri qualified_name;
        name = simple_name;
        qualified_name;
        package_name;
        package_uri;
        file_path;
        file_uri;
        kind = k;
      })
    kind

(** Convert module_fact to Poneglyph facts *)
let module_fact_to_poneglyph_facts (fact : module_fact) =
  let entity = fact.uri in
  let source = Schema.Codedb.source in
  let tx_id = UUID.v7_monotonic () in
  let stated_at = Datetime.now () in
  [
    (* Codedb attributes *)
    Poneglyph.fact ~source ~entity ~attribute:Schema.Codedb.kind
      ~value:(Poneglyph.Fact.String (match fact.kind with `ML -> "ml" | `MLI -> "mli"))
      ~tx_id ~stated_at;
    Poneglyph.fact ~source ~entity ~attribute:Schema.Codedb.package
      ~value:(Poneglyph.Fact.Uri fact.package_uri) ~tx_id ~stated_at;
    Poneglyph.fact ~source ~entity ~attribute:Schema.Codedb.package_name
      ~value:(Poneglyph.Fact.String fact.package_name) ~tx_id ~stated_at;
    Poneglyph.fact ~source ~entity ~attribute:Schema.Codedb.path
      ~value:(Poneglyph.Fact.String (Path.to_string fact.file_path)) ~tx_id ~stated_at;
    
    (* OCaml-specific name attributes *)
    Poneglyph.fact ~source ~entity ~attribute:Schema.OCaml.simple_name
      ~value:(Poneglyph.Fact.String fact.name) ~tx_id ~stated_at;
    Poneglyph.fact ~source ~entity ~attribute:Schema.OCaml.qualified_name
      ~value:(Poneglyph.Fact.String fact.qualified_name) ~tx_id ~stated_at;
    
    (* Link module to file entity via URI *)
    Poneglyph.fact ~source ~entity ~attribute:(
      match fact.kind with
      | `ML -> Schema.OCaml.implementation_file
      | `MLI -> Schema.OCaml.interface_file
    )
      ~value:(Poneglyph.Fact.Uri fact.file_uri) ~tx_id ~stated_at;
  ]

(** Index a single file that was created or modified *)
let index_file ~config ~db ~file_path =
  let start_time = Time.Instant.now () in
  
  match find_package_for_file ~config ~file_path with
  | None ->
      Log.debug ("File not in any package: " ^ Path.to_string file_path)
  | Some pkg ->
      (* Read file content and compute SHA256 *)
      let content_result = Fs.read_to_string file_path in
      (match content_result with
      | Error _ ->
          Log.warn ("Could not read file: " ^ Path.to_string file_path)
      | Ok content ->
          let hash = Crypto.Sha256.hash_string content in
          let current_sha256 = Crypto.Digest.hex hash in
          
          (* Create file URI based on path and SHA256 *)
          let file_uri = Schema.Codedb.File.uri ~path:(Path.to_string file_path) ~sha256:current_sha256 in
          
          (* Create analysis URI based on SHA256 *)
          let analysis_uri = Schema.Codedb.Analysis.uri ~sha256:current_sha256 in
          
          (* Check if this SHA256 has already been analyzed *)
          let already_analyzed = Poneglyph.exists db analysis_uri in
          let needs_indexing = not already_analyzed in
          
          if not needs_indexing then (
            (* Compute relative path for display *)
            let workspace_root = Conf.workspace_root config in
            let workspace_root_str = Path.to_string workspace_root in
            let file_path_str = Path.to_string file_path in
            let relative_path = 
              if String.starts_with ~prefix:workspace_root_str file_path_str then
                let prefix_len = String.length workspace_root_str in
                let relative = String.sub file_path_str prefix_len (String.length file_path_str - prefix_len) in
                if String.length relative > 0 && String.get relative 0 = '/' then
                  String.sub relative 1 (String.length relative - 1)
                else relative
              else file_path_str
            in
            println (String.concat "" [ "⏭️  Skipped (unchanged): "; relative_path ])
          ) else
            let namespace = Tusk_model.Namespace.of_string (String.capitalize_ascii pkg.name) in
            (match extract_file_module_info ~package_name:pkg.name ~namespace ~file_path ~sha256:current_sha256 with
            | None ->
                Log.debug ("Skipping non-ML/MLI file: " ^ Path.to_string file_path)
            | Some fact ->
                (* Store module facts *)
                let module_facts = module_fact_to_poneglyph_facts fact in
                
                (* Store file facts with SHA256 in the URI *)
                (* file_uri: codedb:file:<path>#<sha256> *)
                let source = Schema.Codedb.source in
                let tx_id = UUID.v7_monotonic () in
                let stated_at = Datetime.now () in
                let file_size = String.length content in
                
                let file_facts = [
                  Poneglyph.fact ~source ~entity:file_uri ~attribute:Schema.Codedb.path
                    ~value:(Poneglyph.Fact.String (Path.to_string file_path)) ~tx_id ~stated_at;
                  Poneglyph.fact ~source ~entity:file_uri ~attribute:Schema.Codedb.sha256
                    ~value:(Poneglyph.Fact.String current_sha256) ~tx_id ~stated_at;
                  Poneglyph.fact ~source ~entity:file_uri ~attribute:Schema.Codedb.size
                    ~value:(Poneglyph.Fact.Int file_size) ~tx_id ~stated_at;
                  Poneglyph.fact ~source ~entity:file_uri ~attribute:Schema.Codedb.modified_at
                    ~value:(Poneglyph.Fact.DateTime stated_at) ~tx_id ~stated_at;
                ] in
                
                (* Store analysis facts - this marks the SHA256 as analyzed *)
                (* analysis_uri: codedb:analysis:<sha256> *)
                let analysis_facts = [
                  Poneglyph.fact ~source ~entity:analysis_uri ~attribute:Schema.Codedb.analysis_of
                    ~value:(Poneglyph.Fact.Uri file_uri) ~tx_id ~stated_at;
                  Poneglyph.fact ~source ~entity:analysis_uri ~attribute:Schema.Codedb.analyzed_at
                    ~value:(Poneglyph.Fact.DateTime stated_at) ~tx_id ~stated_at;
                ] in
                
                let all_facts = List.concat [module_facts; file_facts; analysis_facts] in
                let fact_count = Poneglyph.state db all_facts in
                let elapsed = Time.Instant.elapsed start_time in
                let elapsed_ms = Time.Duration.to_millis elapsed in
                
                (* Compute relative path *)
                let workspace_root = Conf.workspace_root config in
                let workspace_root_str = Path.to_string workspace_root in
                let file_path_str = Path.to_string file_path in
                let relative_path = 
                  if String.starts_with ~prefix:workspace_root_str file_path_str then
                    let prefix_len = String.length workspace_root_str in
                    let relative = String.sub file_path_str prefix_len (String.length file_path_str - prefix_len) in
                    if String.length relative > 0 && String.get relative 0 = '/' then
                      String.sub relative 1 (String.length relative - 1)
                    else relative
                  else file_path_str
                in
                
                println (String.concat "" [
                  "📄 Indexed: "; fact.qualified_name; " ("; relative_path; ") ";
                  "— "; Int.to_string fact_count; " facts in "; Int.to_string elapsed_ms; "ms"
                ])))

(** Mark a file as deleted by stating a deleted_at fact *)
let mark_file_deleted ~config ~db ~file_path =
  match find_package_for_file ~config ~file_path with
  | None ->
      Log.debug ("File not in any package: " ^ Path.to_string file_path)
  | Some pkg ->
      (* Check if this is an ML/MLI file *)
      let kind =
        match Path.extension file_path with
        | Some ".ml" -> Some `ML
        | Some ".mli" -> Some `MLI
        | _ -> None
      in
      (match kind with
      | None ->
          Log.debug ("Skipping non-ML/MLI file: " ^ Path.to_string file_path)
      | Some _ ->
          (* Construct module URI directly without needing SHA256 *)
          let namespace = Tusk_model.Namespace.of_string (String.capitalize_ascii pkg.name) in
          let module_obj = Tusk_model.Module.make ~namespace ~filename:file_path in
          let module_name = Tusk_model.Module.module_name module_obj in
          let qualified_name = Tusk_model.Module_name.qualified_name module_name in
          let module_uri = Schema.OCaml.Module.uri qualified_name in
          
          (* State a deleted_at fact *)
          let tx_id = UUID.v7_monotonic () in
          let stated_at = Datetime.now () in
          let source = Schema.Codedb.source in
          let deletion_fact =
            Poneglyph.fact ~source ~entity:module_uri
              ~attribute:Schema.Codedb.deleted_at
              ~value:(Poneglyph.Fact.DateTime stated_at) ~tx_id ~stated_at
          in
          let _count = Poneglyph.state db [ deletion_fact ] in
          println ("🗑️  Deleted: " ^ qualified_name ^ " (" ^ Path.to_string file_path ^ ")"))
