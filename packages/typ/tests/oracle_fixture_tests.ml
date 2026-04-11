open Std
open Typ
open Typ.Analysis
open Typ.Diagnostics
open Typ.Model
open Typ.Session
module Cst = Syn.Cst
module CstBuilder = Syn.CstBuilder

let fixtures_dir = Path.v "packages/typ/tests/fixtures/oracle"

let append_snapshot_suffix = fun path suffix ->
  Path.to_string path ^ suffix |> Path.of_string |> Result.expect ~msg:"snapshot path should stay valid UTF-8"

let approved_snapshot_path = fun path -> append_snapshot_suffix path ".expected"

let oracle_fixture_basename = fun path -> Path.basename path

let contains_substring = fun ~needle haystack ->
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec loop index =
    if needle_length = 0 then
      true
    else if index + needle_length > haystack_length then
      false
    else if String.sub haystack index needle_length = needle then
      true
    else
      loop (index + 1)
  in
  loop 0

let parse_fixture_number = fun basename ->
  match String.split_on_char '_' basename with
  | prefix :: _rest -> int_of_string_opt prefix
  | [] -> None

let env_int_opt = fun name ->
  match Env.var Env.String ~name with
  | Some value -> int_of_string_opt value
  | None -> None

let oracle_range_filter = fun basename ->
  let fixture_number = parse_fixture_number basename in
  let start_inclusive = env_int_opt "TYP_ORACLE_START" in
  let end_inclusive = env_int_opt "TYP_ORACLE_END" in
  let satisfies_start =
    match (start_inclusive, fixture_number) with
    | Some start, Some number -> number >= start
    | Some _, None -> false
    | None, _ -> true
  in
  let satisfies_end =
    match (end_inclusive, fixture_number) with
    | Some end_, Some number -> number <= end_
    | Some _, None -> false
    | None, _ -> true
  in
  satisfies_start && satisfies_end

let oracle_name_filter = fun basename ->
  match Env.var Env.String ~name:"TYP_ORACLE_FILTER" with
  | Some needle when not (String.equal needle "") -> contains_substring ~needle basename
  | _ -> true

let skipped_fixture_basename = fun basename ->
  contains_substring ~needle:"array_" basename || contains_substring ~needle:"recursive_modules" basename

let fixture_filter = fun path ->
  let basename = oracle_fixture_basename path in
  match Path.extension path with
  | Some ".ml" when oracle_range_filter basename
  && oracle_name_filter basename
  && not (skipped_fixture_basename basename) -> `keep
  | _ -> `skip

let skip_snapshot_assertion = fun () ->
  match Env.var Env.String ~name:"TYP_ORACLE_SKIP_SNAPSHOT" with
  | Some "1"
  | Some "true"
  | Some "yes" -> true
  | _ -> false

let stable_fixture_filename = fun (ctx: Test.FixtureRunner.ctx) ->
  Path.join fixtures_dir ctx.fixture_relpath

let parse_failure_report = fun ~filename ->
  fun parse_result ->
    fun error ->
      let source_id = SourceId.of_int 0 in
      let (parse_diagnostics, lowering_diagnostics) =
        match error with
        | Syn.Parse_diagnostics diagnostics -> (diagnostics, [])
        | Syn.Cst_builder_error builder_error -> (
          parse_result.Syn.Parser.diagnostics,
          [ Diagnostic.CstBuilderError { builder_error } ]
        )
      in
      {
        Check_result.source_id;
        filename;
        parse_diagnostics;
        item_tree = None;
        body_arena = None;
        origin_map = None;
        semantic_tree = None;
        lowering_diagnostics;
        typing_diagnostics = [];
        file_summary = FileSummary.missing ~source_id ();
        type_index = TypeIndex.empty;
        exports = [];
        item_traces = [];
        expr_traces = [];
      }

let with_check_source_stage = fun ~filename ~stage thunk ->
  try thunk () with
  | Stack_overflow -> panic
    (format
      Format.[
        str "stack overflow during check_source stage ";
        str stage;
        str " for ";
        str (Path.to_string filename);
      ])

let check_source_text = fun ~filename text ->
  let parse_result = Syn.parse ~filename text in
  match Syn.build_cst parse_result with
  | Ok cst ->
      let config = Config.default in
      let session =
        with_check_source_stage ~filename ~stage:"session_empty" (fun () -> Session.empty ~config)
      in
      let origin = Source.Path filename in
      let module_name =
        with_check_source_stage
          ~filename
          ~stage:"infer_module_name"
          (fun () -> Source.infer_module_name origin)
      in
      let implicit_opens = [] in
      let source_hash =
        with_check_source_stage
          ~filename
          ~stage:"source_hash"
          (fun () -> Source.hash ~implicit_opens ~cst)
      in
      let (session, source_id) =
        with_check_source_stage
          ~filename
          ~stage:"create_source"
          (fun () ->
            Session.create_source
              session
              ~kind:Source.File
              ~module_name
              ~implicit_opens
              ~origin
              ~source_hash
              ~parse_result
              ~cst)
      in
      let source =
        with_check_source_stage
          ~filename
          ~stage:"make_prepared_source"
          (fun () ->
            Source.make_prepared
              ~source_id
              ~kind:Source.File
              ~module_name
              ~implicit_opens
              ~origin
              ~revision:0
              ~source_hash
              ~parse_result
              ~cst)
      in
      let fallback_analysis =
        with_check_source_stage
          ~filename
          ~stage:"fallback_source_analysis"
          (fun () -> SourceAnalysis.analyze ~config source)
      in
      let _ =
        with_check_source_stage
          ~filename
          ~stage:"direct_module_pairing_from_fallback_analysis"
          (fun () ->
            ModulePairing.of_sources
              ~internal_name:(LocalModules.InternalName.of_string module_name)
              [
                {
                  ModulePairing.source;
                  analysis = fallback_analysis;
                  ambient_type_decls = Typ.Config.ambient_type_decls config;
                }
              ])
      in
      let prepared_snapshot =
        with_check_source_stage
          ~filename
          ~stage:"prepare_snapshot"
          (fun () -> Session.prepare_snapshot session ~roots:[ source_id ])
      in
      let analysis =
        with_check_source_stage ~filename ~stage:"query_analysis_of_source"
          (fun () ->
            match prepared_snapshot with
            | Ok snapshot -> (
                match Query.analysis_of_source snapshot source_id with
                | Some analysis -> analysis
                | None -> fallback_analysis
              )
            | Error _ -> fallback_analysis)
      in
      let (item_tree, body_arena, origin_map) =
        with_check_source_stage ~filename ~stage:"extract_semantic_tree"
          (fun () ->
            match analysis.semantic_tree with
            | Some semantic_tree -> (
              Some semantic_tree.item_tree,
              Some semantic_tree.body_arena,
              Some semantic_tree.origin_map
            )
            | None -> (None, None, None))
      in
      with_check_source_stage ~filename ~stage:"assemble_report"
        (fun () ->
          {
            Check_result.source_id;
            filename;
            parse_diagnostics = analysis.parse_diagnostics;
            item_tree;
            body_arena;
            origin_map;
            semantic_tree = analysis.semantic_tree;
            lowering_diagnostics = analysis.lowering_diagnostics;
            typing_diagnostics = analysis.typing_diagnostics;
            file_summary = analysis.file_summary;
            type_index = analysis.type_index;
            exports = SourceAnalysis.exports analysis
            |> List.map (fun (name, scheme) -> (SurfacePath.to_string name, scheme));
            item_traces = analysis.item_traces;
            expr_traces = analysis.expr_traces;
          })
  | Error error -> parse_failure_report ~filename parse_result error

let path_exists = fun path -> Fs.exists path |> Result.unwrap_or ~default:false

let split_nonempty_lines = fun text ->
  text
  |> String.split_on_char '\n'
  |> List.filter (fun line -> not (String.equal (String.trim line) ""))

let replace_all = fun ~needle ~replacement haystack ->
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec find_from index =
    if needle_length = 0 then
      Some index
    else if index + needle_length > haystack_length then
      None
    else if String.sub haystack index needle_length = needle then
      Some index
    else
      find_from (index + 1)
  in
  let rec loop start acc =
    match find_from start with
    | Some index ->
        let prefix = String.sub haystack start (index - start) in
        loop (index + needle_length) (replacement :: prefix :: acc)
    | None ->
        let suffix = String.sub haystack start (haystack_length - start) in
        List.rev (suffix :: acc) |> String.concat ""
  in
  if needle_length = 0 then
    haystack
  else
    loop 0 []

let oracle_host_tokens = fun () ->
  let host = System.host_triplet in
  let base = [ host.architecture; host.vendor; host.os ] in
  match host.abi with
  | Some abi -> base @ [ abi ]
  | None -> base

let toolchain_ocamlc_candidates = fun () ->
  let home_dir = Env.home_dir () |> Option.expect ~msg:"HOME should exist to locate riot toolchains" in
  let toolchains_root = Path.join (Path.join home_dir (Path.v ".riot")) (Path.v "toolchains") in
  if not (path_exists toolchains_root) then
    []
  else
    let output = Command.make
      "find"
      ~args:[ Path.to_string toolchains_root; "-name"; "ocamlc"; "-print" ]
    |> Command.output
    |> Result.expect ~msg:"expected toolchain oracle find command to spawn" in
    if output.status != 0 then
      []
    else
      output.stdout
      |> split_nonempty_lines
      |> List.filter_map (fun path -> Path.of_string path |> Result.to_option)

let preferred_toolchain_ocamlc = fun ocamlc_paths ->
  let host_tokens = oracle_host_tokens ()
  |> List.filter (fun token -> not (String.equal token "") && not (String.equal token "unknown")) in
  let score_path path =
    let rendered = Path.to_string path in
    host_tokens |> List.filter (fun token -> contains_substring ~needle:token rendered) |> List.length
  in
  let best_score = ocamlc_paths |> List.map score_path |> List.fold_left max 0 in
  match
    ocamlc_paths |> List.filter
      (fun path ->
        Int.equal (score_path path) best_score)
  with
  | [ ocamlc_path ] -> Some ocamlc_path
  | _ -> None

let oracle_ocamlc_path = fun () ->
  match Env.var Env.String ~name:"TYP_OCAMLC_ORACLE" with
  | Some path -> Path.of_string path |> Result.expect ~msg:"TYP_OCAMLC_ORACLE must be a valid UTF-8 path"
  | None ->
      let ocamlc_paths = toolchain_ocamlc_candidates () in
      if List.is_empty ocamlc_paths then
        panic "expected exactly one riot-managed ocamlc oracle, found none";
      match preferred_toolchain_ocamlc ocamlc_paths with
      | Some ocamlc_path -> ocamlc_path
      | None -> panic
        (format
          Format.[
            str "expected exactly one riot-managed ocamlc oracle, found ";
            int (List.length ocamlc_paths);
          ])

let oracle_stdlib_path =
  let cached = ref None in
  fun () ->
    match !cached with
    | Some path -> path
    | None ->
        let ocamlc_path = oracle_ocamlc_path () in
        let output = Command.make (Path.to_string ocamlc_path) ~args:[ "-where" ]
        |> Command.output
        |> Result.expect ~msg:"expected ocamlc -where oracle invocation to spawn" in
        if output.status != 0 then
          panic
            (format
              Format.[
                str "ocamlc -where failed\nstderr:\n";
                str output.stderr;
                str "\nstdout:\n";
                str output.stdout;
              ]);
        let path = output.stdout |> String.trim |> Path.of_string |> Result.expect ~msg:"ocamlc -where should return a valid UTF-8 path" in
        let () =
          cached := Some path
        in
        path

type oracle_command_result = {
  output: Command.output;
  source_path: Path.t;
}

let run_oracle_command = fun ~fixture_filename:_ ~source_text ~args ->
  let ocamlc_path = oracle_ocamlc_path () in
  Fs.with_tempdir ~prefix:"typ_oracle"
    (fun tmpdir ->
      let oracle_filename = "Oracle_fixture.ml" in
      let source_path = Path.join tmpdir (Path.v oracle_filename) in
      Fs.write source_text source_path |> Result.expect ~msg:"oracle fixture temp source should be writable";
      let output = Command.make
        (Path.to_string ocamlc_path)
        ~args:(args @ [ Path.to_string source_path ])
      |> Command.output
      |> Result.expect ~msg:"expected ocamlc oracle invocation to spawn" in
      { output; source_path }) |> Result.expect ~msg:"oracle tempdir should be creatable"

let strip_identifier_stamps = fun text -> text

type oracle_value_export = {
  name: string;
  scheme: string;
}

type oracle_module_alias = {
  alias_name: string;
  target_name: string;
}

type oracle_signature_parts = {
  value_exports: oracle_value_export list;
  value_export_types: (string * Cst.core_type) list;
  type_names: string list;
  type_aliases: (string * string) list;
  module_aliases: oracle_module_alias list;
  module_type_templates: oracle_module_type_template list;
}

and oracle_module_type_template = {
  name: string;
  parts: oracle_signature_parts;
}

type oracle_interface = {
  text: string;
  value_exports: oracle_value_export list;
  value_export_types: (string * Cst.core_type) list;
  type_names: string list;
  type_aliases: (string * string) list;
  module_aliases: oracle_module_alias list;
  module_type_templates: oracle_module_type_template list;
}

let split_once = fun line ch ->
  match String.index_opt line ch with
  | Some index ->
      let left = String.sub line 0 index in
      let right = String.sub line (index + 1) (String.length line - index - 1) in
      Some (left, right)
  | None -> None

let looks_like_type_abbreviation = fun manifest ->
  not (contains_substring ~needle:"|" manifest)
  && not (contains_substring ~needle:"{" manifest)
  && not (contains_substring ~needle:"}" manifest)
  && not (contains_substring ~needle:" of " manifest)
  && not (contains_substring ~needle:"private " manifest)

let compare_value_export = fun (left: oracle_value_export) (right: oracle_value_export) ->
  match String.compare left.name right.name with
  | 0 -> String.compare left.scheme right.scheme
  | order -> order

let compare_type_alias = fun (left_name, _left_manifest) (right_name, _right_manifest) ->
  String.compare left_name right_name

let empty_oracle_signature_parts: oracle_signature_parts = {
  value_exports = [];
  value_export_types = [];
  type_names = [];
  type_aliases = [];
  module_aliases = [];
  module_type_templates = [];
}

let merge_oracle_signature_parts = fun (left: oracle_signature_parts) (right: oracle_signature_parts) ->
  {
    value_exports = left.value_exports @ right.value_exports;
    value_export_types = left.value_export_types @ right.value_export_types;
    type_names = left.type_names @ right.type_names;
    type_aliases = left.type_aliases @ right.type_aliases;
    module_aliases = left.module_aliases @ right.module_aliases;
    module_type_templates = left.module_type_templates @ right.module_type_templates;
  }

let qualify_name = fun prefix name ->
  match prefix with
  | Some prefix -> prefix ^ "." ^ name
  | None -> name

let prefix_export = fun prefix ({ name; scheme }: oracle_value_export) ->
  { name = qualify_name (Some prefix) name; scheme }

let prefix_type_name = fun prefix type_name -> qualify_name (Some prefix) type_name

let prefix_type_alias = fun prefix (type_name, manifest) ->
  (prefix_type_name prefix type_name, manifest)

let token_list_text = fun tokens ->
  tokens
  |> List.map Cst.Token.text
  |> String.concat " "
  |> String.split_on_char ' '
  |> List.filter (fun token -> not (String.equal token ""))
  |> String.concat " "

let ident_text = fun ident -> Cst.Ident.segments ident |> List.map Cst.Token.text |> String.concat "."

let syntax_text = fun source_text syntax_node ->
  let span = Ceibo.Red.SyntaxNode.span syntax_node in
  String.sub source_text span.start (span.end_ - span.start) |> String.trim

let core_type_text = fun source_text type_ ->
  syntax_text source_text (Cst.CoreType.syntax_node type_)

let oracle_interface_filename = fun fixture_filename -> append_snapshot_suffix fixture_filename ".oracle.mli"

let trim_prefix_opt = fun ~prefix text ->
  if String.starts_with ~prefix text then
    Some (String.sub text (String.length prefix) (String.length text - String.length prefix))
  else
    None

let rec type_declaration_parts = fun ~prefix ~source_text declaration ->
  let type_name = qualify_name prefix (Cst.Token.text (Cst.TypeDeclaration.name_token declaration)) in
  let type_aliases =
    let manifest =
      match Cst.TypeDeclaration.manifest_alias declaration with
      | Some manifest_alias -> Some (core_type_text source_text manifest_alias)
      | None -> (
          match Cst.TypeDeclaration.type_definition declaration with
          | Cst.TypeDefinition.Alias { manifest; _ } -> Some (core_type_text source_text manifest)
          | _ -> None
        )
    in
    match manifest with
    | Some manifest ->
        if looks_like_type_abbreviation manifest then
          [ (type_name, manifest) ]
        else
          []
    | None -> []
  in
  let current = { empty_oracle_signature_parts with type_names = [ type_name ]; type_aliases } in
  match Cst.TypeDeclaration.next_and_declaration declaration with
  | Some next -> merge_oracle_signature_parts
    current
    (type_declaration_parts ~prefix ~source_text next)
  | None -> current

let local_alias_target_name = fun ~prefix target_name ->
  if contains_substring ~needle:"." target_name then
    target_name
  else
    match prefix with
    | Some prefix -> prefix ^ "." ^ target_name
    | None -> target_name

let signature_items_of_module_type_oracle = fun module_type ->
  Syn.CstBuilder.signature_items_of_module_type module_type |> Result.to_option

let expand_module_aliases = fun (parts: oracle_signature_parts) ->
  parts.module_aliases |> List.fold_left
    (fun (current: oracle_signature_parts) ({ alias_name; target_name }: oracle_module_alias) ->
      let aliased_exports =
        current.value_exports
        |> List.filter_map
          (fun (export: oracle_value_export) ->
            if String.starts_with ~prefix:(target_name ^ ".") export.name then
              Some {
                export
                with name = alias_name
                ^ "."
                ^ String.sub
                  export.name
                  (String.length target_name + 1)
                  (String.length export.name - String.length target_name - 1)
              }
            else
              None)
      in
      let aliased_export_types =
        current.value_export_types
        |> List.filter_map
          (fun (export_name, core_type) ->
            if String.starts_with ~prefix:(target_name ^ ".") export_name then
              Some (
                alias_name
                ^ "."
                ^ String.sub
                  export_name
                  (String.length target_name + 1)
                  (String.length export_name - String.length target_name - 1),
                core_type
              )
            else
              None)
      in
      let aliased_type_names =
        current.type_names
        |> List.filter_map
          (fun type_name ->
            if String.starts_with ~prefix:(target_name ^ ".") type_name then
              Some (alias_name
              ^ "."
              ^ String.sub
                type_name
                (String.length target_name + 1)
                (String.length type_name - String.length target_name - 1))
            else
              None)
      in
      let aliased_type_aliases =
        current.type_aliases
        |> List.filter_map
          (fun (type_name, manifest) ->
            if String.starts_with ~prefix:(target_name ^ ".") type_name then
              Some (
                alias_name
                ^ "."
                ^ String.sub
                  type_name
                  (String.length target_name + 1)
                  (String.length type_name - String.length target_name - 1),
                manifest
              )
            else
              None)
      in
      {
        current
        with value_exports = current.value_exports @ aliased_exports;
        value_export_types = current.value_export_types @ aliased_export_types;
        type_names = current.type_names @ aliased_type_names;
        type_aliases = current.type_aliases @ aliased_type_aliases
      })
    parts

let rec module_type_declaration_parts = fun ~fixture_filename ~source_text declaration ->
  match Cst.ModuleTypeDeclaration.module_type declaration with
  | Some module_type -> (
      match signature_items_of_module_type_oracle module_type with
      | Some items ->
          let template_parts = signature_items_parts ~fixture_filename ~source_text ~prefix:None items
          |> expand_module_aliases in
          {
            empty_oracle_signature_parts
            with module_type_templates = [
              { name = Cst.ModuleTypeDeclaration.name declaration; parts = template_parts }
            ]
          }
      | None -> empty_oracle_signature_parts
    )
  | None -> empty_oracle_signature_parts

and signature_item_parts = fun ~fixture_filename ~source_text ~prefix item ->
  match item with
  | Cst.SignatureItem.ValueDeclaration declaration ->
      let name = qualify_name prefix (token_list_text declaration.name_tokens) in
      {
        empty_oracle_signature_parts
        with value_exports = [ { name; scheme = core_type_text source_text declaration.type_ } ];
        value_export_types = [ (name, declaration.type_) ]
      }
  | Cst.SignatureItem.ExternalDeclaration declaration ->
      let name = qualify_name prefix (token_list_text declaration.name_tokens) in
      {
        empty_oracle_signature_parts
        with value_exports = [ { name; scheme = core_type_text source_text declaration.type_ } ];
        value_export_types = [ (name, declaration.type_) ]
      }
  | Cst.SignatureItem.TypeDeclaration declaration ->
      type_declaration_parts ~prefix ~source_text declaration
  | Cst.SignatureItem.ModuleDeclaration declaration ->
      module_declaration_parts ~fixture_filename ~source_text ~prefix declaration
  | Cst.SignatureItem.ModuleTypeDeclaration declaration ->
      module_type_declaration_parts ~fixture_filename ~source_text declaration
  | Cst.SignatureItem.IncludeStatement include_statement -> (
      match include_statement.target with
      | Cst.ModuleType module_type -> (
          match signature_items_of_module_type_oracle module_type with
          | Some items -> signature_items_parts ~fixture_filename ~source_text ~prefix items
          | None -> empty_oracle_signature_parts
        )
      | Cst.ModuleExpression _ -> empty_oracle_signature_parts
    )
  | _ ->
      empty_oracle_signature_parts

and module_declaration_parts = fun ~fixture_filename ~source_text ~prefix declaration ->
  let module_name = qualify_name prefix (Cst.ModuleSignature.name declaration) in
  match Cst.ModuleSignature.definition declaration with
  | Cst.ModuleSignature.Signature module_type -> begin
      match signature_items_of_module_type_oracle module_type with
      | Some items -> signature_items_parts
        ~fixture_filename
        ~source_text
        ~prefix:(Some module_name)
        items
      | None -> empty_oracle_signature_parts
    end
  | Cst.ModuleSignature.Alias module_expression -> (
      match module_expression with
      | Cst.ModuleExpression.Path ident -> {
        empty_oracle_signature_parts
        with module_aliases = [
          {
            alias_name = module_name;
            target_name = local_alias_target_name ~prefix (ident_text ident)
          }
        ]
      }
      | _ -> empty_oracle_signature_parts
    )

and signature_items_parts = fun ~fixture_filename ~source_text ~prefix items ->
  items
  |> List.map (signature_item_parts ~fixture_filename ~source_text ~prefix)
  |> List.fold_left merge_oracle_signature_parts empty_oracle_signature_parts

let run_interface_oracle = fun ~fixture_filename ~source_text ->
  let result = run_oracle_command
    ~fixture_filename
    ~source_text
    ~args:[ "-nopervasives"; "-nostdlib"; "-i" ] in
  let output = result.output in
  if output.status != 0 then
    panic
      (format
        Format.[
          str "ocamlc -i failed for ";
          str (Path.to_string fixture_filename);
          str "\nstderr:\n";
          str output.stderr;
          str "\nstdout:\n";
          str output.stdout;
        ]);
  let interface_text = output.stdout in
  let interface_parse_result = Syn.parse ~filename:(oracle_interface_filename fixture_filename) interface_text in
  let parts =
    match Syn.build_cst interface_parse_result with
    | Ok (Cst.Interface interface) -> signature_items_parts
      ~fixture_filename
      ~source_text:interface_text
      ~prefix:None interface.items
    |> expand_module_aliases
    | Ok (Cst.Implementation _) -> panic
      (format
        Format.[
          str "ocamlc -i produced an implementation for ";
          str (Path.to_string fixture_filename);
          str "\ninterface text:\n";
          str interface_text;
        ])
    | Error (Syn.Parse_diagnostics diagnostics) -> panic
      (format
        Format.[
          str "failed to parse ocamlc -i output as interface for ";
          str (Path.to_string fixture_filename);
          str "\ndiagnostics:\n";
          str (String.concat "\n" (List.map Syn.Diagnostic.to_string diagnostics));
          str "\ninterface text:\n";
          str interface_text;
        ])
    | Error (Syn.Cst_builder_error error) ->
        panic
          (
            format
              Format.[
                str "failed to build CST from ocamlc -i output for ";
                str (Path.to_string fixture_filename);
                str "\nerror:\n";
                str error.Syn.CstBuilder.message;
                str "\nsyntax_kind: ";
                str (Syn.SyntaxKind.to_string error.syntax_kind);
                str "\ncontext: ";
                str (String.concat " > " error.context);
                str "\ninterface text:\n";
                str interface_text;
              ]
          )
  in
  let value_exports = parts.value_exports |> List.sort compare_value_export |> List.sort_uniq compare_value_export in
  let value_export_types = parts.value_export_types in
  let type_names = parts.type_names |> List.sort String.compare |> List.sort_uniq String.compare in
  let type_aliases = parts.type_aliases |> List.sort compare_type_alias |> List.sort_uniq compare_type_alias in
  let module_type_templates =
    parts.module_type_templates
    |> List.sort_uniq
      (fun left right ->
        String.compare left.name right.name)
  in
  {
    text = interface_text;
    value_exports;
    value_export_types;
    type_names;
    type_aliases;
    module_aliases = parts.module_aliases;
    module_type_templates;
  }

let run_typedtree_oracle = fun ~fixture_filename ~source_text ->
  let stdlib_path = oracle_stdlib_path () in
  let result = run_oracle_command
    ~fixture_filename
    ~source_text
    ~args:[ "-nopervasives"; "-nostdlib"; "-I"; Path.to_string stdlib_path; "-dtypedtree"; "-c"; ] in
  let output = result.output in
  if output.status != 0 then
    panic
      (format
        Format.[
          str "ocamlc -dtypedtree failed for ";
          str (Path.to_string fixture_filename);
          str "\nstderr:\n";
          str output.stderr;
          str "\nstdout:\n";
          str output.stdout;
        ]);
  output.stderr
  |> replace_all
    ~needle:(Path.to_string result.source_path)
    ~replacement:(Path.to_string fixture_filename)
  |> strip_identifier_stamps
  |> split_nonempty_lines
  |> String.concat "\n"

let split_on_needle = fun ~needle text ->
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec find_from index =
    if needle_length = 0 then
      Some index
    else if index + needle_length > text_length then
      None
    else if String.sub text index needle_length = needle then
      Some index
    else
      find_from (index + 1)
  in
  let rec loop start acc =
    match find_from start with
    | Some index ->
        let part = String.sub text start (index - start) in
        loop (index + needle_length) (part :: acc)
    | None ->
        let suffix = String.sub text start (text_length - start) in
        List.rev (suffix :: acc)
  in
  loop 0 []

let oracle_module_type_templates = fun (interface: oracle_interface) -> interface.module_type_templates

let oracle_module_type_template = fun (interface: oracle_interface) module_type_name ->
  oracle_module_type_templates interface |> List.find_opt
    (fun ({ name; _ }: oracle_module_type_template) ->
      String.equal name module_type_name)

let package_constraint_entries = fun text ->
  let trimmed = String.trim text in
  if String.equal trimmed "" then
    []
  else
    let without_with =
      match trim_prefix_opt ~prefix:"with " trimmed with
      | Some rest -> rest
      | None -> trimmed
    in
    split_on_needle ~needle:" and type " without_with |> List.filter_map
      (fun entry ->
        let entry =
          match trim_prefix_opt ~prefix:"type " (String.trim entry) with
          | Some rest -> rest
          | None -> String.trim entry
        in
        split_once entry '='
        |> Option.map (fun (name, replacement) -> (String.trim name, String.trim replacement)))

let replace_standalone_package_occurrences = fun ~needle ~replacement haystack ->
  let is_type_path_char = function
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_'
    | '\''
    | '.' -> true
    | _ -> false
  in
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec find_from index =
    if needle_length = 0 then
      Some index
    else if index + needle_length > haystack_length then
      None
    else if String.sub haystack index needle_length = needle then
      let before_ok =
        if index = 0 then
          true
        else
          not (is_type_path_char haystack.[index - 1])
      in
      let after_index = index + needle_length in
      let after_ok =
        if after_index >= haystack_length then
          true
        else
          not (is_type_path_char haystack.[after_index])
      in
      if before_ok && after_ok then
        Some index
      else
        find_from (index + 1)
    else
      find_from (index + 1)
  in
  let rec loop start acc =
    match find_from start with
    | Some index ->
        let prefix = String.sub haystack start (index - start) in
        loop (index + needle_length) (replacement :: prefix :: acc)
    | None ->
        let suffix = String.sub haystack start (haystack_length - start) in
        List.rev (suffix :: acc) |> String.concat ""
  in
  if needle_length = 0 then
    haystack
  else
    loop 0 []

let apply_package_constraints = fun constraints scheme ->
  constraints
  |> List.fold_left
    (fun current (type_name, replacement) ->
      replace_standalone_package_occurrences ~needle:type_name ~replacement current)
    scheme

let oracle_value_export_type = fun (interface: oracle_interface) export_name ->
  interface.value_export_types |> List.find_map
    (fun (name, core_type) ->
      if String.equal name export_name then
        Some core_type
      else
        None)

let apply_core_type_constraints = fun constraints scheme ->
  constraints
  |> List.fold_left
    (fun current (needle, replacement) ->
      replace_standalone_package_occurrences ~needle ~replacement current)
    scheme

let rec normalize_core_type_with_constraints = fun (interface: oracle_interface) constraints core_type ->
  match core_type with
  | Cst.CoreType.Parenthesized { inner; _ } ->
      normalize_core_type_with_constraints interface constraints inner
  | Cst.CoreType.Tuple { elements; _ } ->
      elements |> List.map
        (fun element ->
          match element with
          | Cst.CoreType.Arrow _ -> "("
          ^ normalize_core_type_with_constraints interface constraints element
          ^ ")"
          | _ -> normalize_core_type_with_constraints interface constraints element) |> String.concat
        " * "
  | Cst.CoreType.Arrow { label=None; parameter_type; result_type; _ } ->
      let (parameter_text, introduced_constraints) =
        match parameter_type with
        | Cst.CoreType.FirstClassModule { module_name; package_type; _ } -> normalize_package_type_with_constraints
          interface
          constraints
          (Option.map Cst.Token.text module_name)
          package_type
        |> (fun (text, introduced_constraints) -> ("(" ^ text ^ ")", introduced_constraints))
        | Cst.CoreType.Arrow _ -> (
          "(" ^ normalize_core_type_with_constraints interface constraints parameter_type ^ ")",
          []
        )
        | _ -> (normalize_core_type_with_constraints interface constraints parameter_type, [])
      in
      let result_text = normalize_core_type_with_constraints
        interface
        (constraints @ introduced_constraints)
        result_type in
      parameter_text ^ " -> " ^ result_text
  | Cst.CoreType.FirstClassModule { module_name; package_type; _ } ->
      let (text, _introduced_constraints) = normalize_package_type_with_constraints
        interface
        constraints
        (Option.map Cst.Token.text module_name)
        package_type in
      "(" ^ text ^ ")"
  | _ ->
      core_type_text interface.text core_type |> apply_core_type_constraints constraints |> String.trim

and normalize_package_type_constraints = fun (interface: oracle_interface) constraints (
  package_type: Cst.package_type
) ->
  package_type.constraints |> List.map
    (fun (constraint_: Cst.module_type_constraint) ->
      let type_name =
        match constraint_.constrained_type with
        | Cst.CoreType.Constr { constructor_path; arguments=[]; _ } -> Syn.Cst.Ident.name constructor_path
        |> Option.unwrap_or
          ~default:(core_type_text interface.text constraint_.constrained_type |> String.trim)
        | _ -> core_type_text interface.text constraint_.constrained_type |> String.trim
      in
      let replacement = normalize_core_type_with_constraints interface constraints constraint_.replacement_type in
      (type_name, replacement))

and normalize_package_type_with_constraints = fun (interface: oracle_interface) constraints module_name (
  package_type: Cst.package_type
) ->
  let package_constraints = normalize_package_type_constraints interface constraints package_type in
  let propagated_constraints =
    match module_name with
    | Some module_name -> package_constraints
    |> List.map (fun (type_name, replacement) -> (module_name ^ "." ^ type_name, replacement))
    | None -> []
  in
  let module_type_name = Syn.Cst.Ident.name package_type.module_type_path
  |> Option.unwrap_or ~default:(ident_text package_type.module_type_path) in
  match oracle_module_type_template interface module_type_name with
  | Some ({ parts; _ }: oracle_module_type_template) ->
      let values_text =
        parts.value_exports
        |> List.map
          (fun ({ name; scheme }: oracle_value_export) ->
            let scheme = scheme
            |> apply_package_constraints package_constraints
            |> apply_core_type_constraints constraints
            |> String.trim in
            "val " ^ name ^ " : " ^ scheme)
        |> String.concat "; "
      in
      ("module sig " ^ values_text ^ " end", propagated_constraints)
  | None ->
      let fallback = "module " ^ module_type_name in
      (fallback, propagated_constraints)

let package_rewrite = fun (interface: oracle_interface) segment ->
  let trimmed = String.trim segment in
  let (wrapped, core) =
    if String.length trimmed >= 2 && trimmed.[0] = '(' && trimmed.[String.length trimmed - 1] = ')' then
      (true, String.sub trimmed 1 (String.length trimmed - 2) |> String.trim)
    else
      (false, trimmed)
  in
  match trim_prefix_opt ~prefix:"module " core with
  | Some rest when not (String.starts_with ~prefix:"sig " (String.trim rest)) ->
      let (binder_name, rest) =
        match split_once rest ':' with
        | Some (left, right) when not (contains_substring ~needle:"with type" left)
        && not (contains_substring ~needle:" " left) -> (Some (String.trim left), String.trim right)
        | _ -> (None, String.trim rest)
      in
      let module_type_name =
        let rec find_end index =
          if index >= String.length rest then
            index
          else
            match rest.[index] with
            | ' '
            | '\t' -> index
            | _ -> find_end (index + 1)
        in
        String.sub rest 0 (find_end 0)
      in
      let constraints_text = String.sub
        rest
        (String.length module_type_name)
        (String.length rest - String.length module_type_name)
      |> String.trim in
      let constraints = package_constraint_entries constraints_text in
      oracle_module_type_template interface module_type_name |> Option.map
        (fun ({ parts; _ }: oracle_module_type_template) ->
          let rewritten_exports = parts.value_exports
          |> List.map
            (fun ({ name; scheme }: oracle_value_export) ->
              { name; scheme = apply_package_constraints constraints scheme }) in
          let rewritten_segment =
            let values_text = rewritten_exports
            |> List.map
              (fun ({ name; scheme }: oracle_value_export) -> "val " ^ name ^ " : " ^ scheme)
            |> String.concat "; " in
            let text = "module sig " ^ values_text ^ " end" in
            if wrapped then
              "(" ^ text ^ ")"
            else
              text
          in
          let propagated_constraints =
            match binder_name with
            | Some binder_name -> constraints
            |> List.map
              (fun (type_name, replacement) -> (binder_name ^ "." ^ type_name, replacement))
            | None -> []
          in
          (rewritten_segment, propagated_constraints))
  | _ -> None

let normalize_named_package_types = fun (interface: oracle_interface) scheme ->
  let segments = split_on_needle ~needle:"->" scheme |> List.map String.trim in
  let rec loop propagated_constraints acc = function
    | [] -> List.rev acc |> String.concat " -> "
    | segment :: rest ->
        let segment = propagated_constraints
        |> List.fold_left
          (fun current (needle, replacement) ->
            replace_standalone_package_occurrences ~needle ~replacement current)
          segment in
        (
          match package_rewrite interface segment with
          | Some (rewritten_segment, introduced_constraints) -> loop
            (propagated_constraints @ introduced_constraints)
            (rewritten_segment :: acc)
            rest
          | None -> loop propagated_constraints (segment :: acc) rest
        )
  in
  loop [] [] segments

let top_level_tuple_arg = fun scheme ->
  let length = String.length scheme in
  if length < 4 || scheme.[0] != '(' then
    None
  else
    let rec find_matching_paren index depth =
      if index >= length then
        None
      else
        match scheme.[index] with
        | '(' -> find_matching_paren (index + 1) (depth + 1)
        | ')' when depth = 1 -> Some index
        | ')' -> find_matching_paren (index + 1) (depth - 1)
        | _ -> find_matching_paren (index + 1) depth
    in
    match find_matching_paren 1 1 with
    | Some close_index when close_index + 3 <= length ->
        let suffix = String.sub scheme (close_index + 1) (length - close_index - 1) in
        if not (String.starts_with ~prefix:" ->" suffix) then
          None
        else
          let prefix = String.sub scheme 1 (close_index - 1) |> String.trim in
          if contains_substring ~needle:"*" prefix then
            Some (prefix, String.sub suffix 4 (String.length suffix - 4) |> String.trim)
          else
            None
    | _ -> None

type scheme_shape =
  | SchemeAtom
  | SchemeTuple
  | SchemeArrow

let scheme_shape = fun scheme ->
  let length = String.length scheme in
  let rec loop index depth =
    if index >= length then
      SchemeAtom
    else
      match scheme.[index] with
      | '(' -> loop (index + 1) (depth + 1)
      | ')' -> loop (index + 1) (max 0 (depth - 1))
      | '*' when depth = 0 -> SchemeTuple
      | '-' when depth = 0 && index + 1 < length && scheme.[index + 1] = '>' -> SchemeArrow
      | _ -> loop (index + 1) depth
  in
  loop 0 0

let next_significant_slice = fun text start_index ->
  let rec skip_spaces index =
    if index >= String.length text then
      None
    else if Char.equal text.[index] ' ' then
      skip_spaces (index + 1)
    else
      Some index
  in
  match skip_spaces start_index with
  | Some index when index + 1 < String.length text && text.[index] = '-' && text.[index + 1] = '>' -> Some "->"
  | Some index -> Some (String.make 1 text.[index])
  | None -> None

let previous_significant_slice = fun text end_index ->
  let rec skip_spaces index =
    if index < 0 then
      None
    else if Char.equal text.[index] ' ' then
      skip_spaces (index - 1)
    else
      Some index
  in
  match skip_spaces end_index with
  | Some index when index > 0 && text.[index - 1] = '-' && text.[index] = '>' -> Some "->"
  | Some index -> Some (String.make 1 text.[index])
  | None -> None

let strip_redundant_parentheses = fun scheme ->
  let length = String.length scheme in
  let rec find_matching_paren index depth =
    if index >= length then
      None
    else
      match scheme.[index] with
      | '(' -> find_matching_paren (index + 1) (depth + 1)
      | ')' when depth = 1 -> Some index
      | ')' -> find_matching_paren (index + 1) (depth - 1)
      | _ -> find_matching_paren (index + 1) depth
  in
  let rec loop index acc =
    if index >= length then
      List.rev acc |> String.concat ""
    else if not (Char.equal scheme.[index] '(') then
      loop (index + 1) (String.make 1 scheme.[index] :: acc)
    else
      match find_matching_paren (index + 1) 1 with
      | None -> loop (index + 1) (String.make 1 scheme.[index] :: acc)
      | Some close_index ->
          let inner = String.sub scheme (index + 1) (close_index - index - 1) in
          let previous = previous_significant_slice scheme (index - 1) in
          let next = next_significant_slice scheme (close_index + 1) in
          let remove =
            match scheme_shape (String.trim inner) with
            | SchemeAtom ->
                true
            | SchemeTuple -> (
                match (previous, next) with
                | (_, Some "->")
                | (Some "->", _) -> true
                | (None, None) -> true
                | _ -> false
              )
            | SchemeArrow ->
                false
          in
          if remove then
            loop (close_index + 1) (inner :: acc)
          else
            loop (close_index + 1) (String.sub scheme index (close_index - index + 1) :: acc)
  in
  loop 0 []

let normalize_scheme_notation = fun scheme ->
  let normalize_labeled_arrows text =
    let length = String.length text in
    let rec loop index acc =
      if index >= length then
        List.rev acc |> String.concat ""
      else if text.[index] = '~' then
        let rec consume_label end_index =
          if end_index < length then
            match text.[end_index] with
            | 'a' .. 'z'
            | 'A' .. 'Z'
            | '0' .. '9'
            | '_' -> consume_label (end_index + 1)
            | ':' -> Some end_index
            | _ -> None
          else
            None
        in
        (
          match consume_label (index + 1) with
          | Some label_end ->
              let label = String.sub text (index + 1) (label_end - index - 1) in
              loop (label_end + 1) (":" :: label :: acc)
          | None -> loop (index + 1) ("~" :: acc)
        )
      else
        loop (index + 1) (String.make 1 text.[index] :: acc)
    in
    loop 0 []
  in
  let rec loop current =
    let trimmed = String.trim current in
    match top_level_tuple_arg trimmed with
    | Some (tuple_argument, body) -> loop (tuple_argument ^ " -> " ^ body)
    | None ->
        let normalized = trimmed
        |> normalize_labeled_arrows
        |> strip_redundant_parentheses
        |> String.split_on_char ' '
        |> List.filter (fun part -> not (String.equal part ""))
        |> String.concat " " in
        if String.equal normalized trimmed then
          normalized
        else
          loop normalized
  in
  loop scheme

let normalize_typ_scheme = fun scheme ->
  match split_once scheme '.' with
  | Some (left, right) when String.starts_with ~prefix:"'" (String.trim left) -> normalize_scheme_notation
    right
  | _ -> normalize_scheme_notation scheme

let is_value_export_name = fun name ->
  let leaf =
    match List.rev (String.split_on_char '.' name) with
    | leaf :: _rest -> leaf
    | [] -> name
  in
  if String.length leaf = 0 then
    true
  else
    let first = String.get leaf 0 in
    not (first >= 'A' && first <= 'Z')

let typ_value_exports = fun (report: Check_result.t) ->
  FileSummary.exports report.file_summary
  |> List.filter (fun (name, _scheme) -> is_value_export_name (SurfacePath.to_string name))
  |> List.map
    (fun (name, scheme) ->
      {
        name = SurfacePath.to_string name;
        scheme = TypePrinter.scheme_to_string scheme |> normalize_typ_scheme
      })
  |> List.sort compare_value_export

let typ_type_names = fun (report: Check_result.t) ->
  FileSummary.type_decls report.file_summary |> List.map
    (fun ({ declaration; scope_path }: FileSummary.type_decl) ->
      if SurfacePath.is_empty scope_path then
        declaration.type_name
      else
        SurfacePath.append_name scope_path declaration.type_name |> SurfacePath.to_string) |> List.sort
    String.compare

let completeness_to_string = function
  | FileSummary.Complete -> "complete"
  | FileSummary.Partial -> "partial"

let export_to_json = fun (export: oracle_value_export) ->
  Data.Json.Object [
    ("name", Data.Json.String export.name);
    ("scheme", Data.Json.String export.scheme);
  ]

let normalized_value_exports = fun exports ->
  exports
  |> List.map
    (fun ({ name; scheme }: oracle_value_export) ->
      { name; scheme = normalize_scheme_notation scheme })
  |> List.sort compare_value_export

let is_type_path_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '\''
  | '.' -> true
  | _ -> false

let replace_standalone_occurrences = fun ~needle ~replacement haystack ->
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec find_from index =
    if needle_length = 0 then
      Some index
    else if index + needle_length > haystack_length then
      None
    else if String.sub haystack index needle_length = needle then
      let before_ok =
        if index = 0 then
          true
        else
          not (is_type_path_char haystack.[index - 1])
      in
      let after_index = index + needle_length in
      let after_ok =
        if after_index >= haystack_length then
          true
        else
          not (is_type_path_char haystack.[after_index])
      in
      if before_ok && after_ok then
        Some index
      else
        find_from (index + 1)
    else
      find_from (index + 1)
  in
  let rec loop start acc =
    match find_from start with
    | Some index ->
        let prefix = String.sub haystack start (index - start) in
        loop (index + needle_length) (replacement :: prefix :: acc)
    | None ->
        let suffix = String.sub haystack start (haystack_length - start) in
        List.rev (suffix :: acc) |> String.concat ""
  in
  if needle_length = 0 then
    haystack
  else
    loop 0 []

let export_scope = fun export_name ->
  match String.rindex_opt export_name '.' with
  | Some index -> Some (String.sub export_name 0 index)
  | None -> None

let qualify_local_type_names = fun ~scope type_names scheme ->
  let local_type_names =
    match scope with
    | Some prefix ->
        type_names |> List.filter_map
          (fun type_name ->
            if String.starts_with ~prefix:(prefix ^ ".") type_name then
              Some (
                String.sub
                  type_name
                  (String.length prefix + 1)
                  (String.length type_name - String.length prefix - 1),
                type_name
              )
            else
              None)
    | None ->
        type_names |> List.filter_map
          (fun type_name ->
            if contains_substring ~needle:"." type_name then
              None
            else
              Some (type_name, type_name))
  in
  local_type_names |> List.fold_left
    (fun current_scheme (bare_name, qualified_name) ->
      if String.equal bare_name qualified_name then
        current_scheme
      else
        replace_standalone_occurrences ~needle:bare_name ~replacement:qualified_name current_scheme)
    scheme

let expand_type_aliases = fun type_aliases scheme ->
  let rec loop remaining current =
    if remaining <= 0 then
      normalize_scheme_notation current
    else
      let next = type_aliases
      |> List.fold_left
        (fun current_scheme (type_name, manifest) ->
          replace_standalone_occurrences ~needle:type_name ~replacement:manifest current_scheme)
        current in
      if String.equal next current then
        normalize_scheme_notation current
      else
        loop (remaining - 1) next
  in
  loop 16 scheme

let normalize_export_scheme_with_interface = fun (interface: oracle_interface) (
  { name; scheme }: oracle_value_export
) ->
  (
    match oracle_value_export_type interface name with
    | Some core_type -> normalize_core_type_with_constraints interface [] core_type
    | None -> scheme |> normalize_named_package_types interface
  ) |> normalize_scheme_notation |> qualify_local_type_names ~scope:(export_scope name) interface.type_names |> expand_type_aliases
    interface.type_aliases

let normalized_oracle_exports_for_comparison = fun (interface: oracle_interface) exports ->
  exports
  |> List.map
    (fun ({ name; scheme=_ } as export) ->
      { name; scheme = normalize_export_scheme_with_interface interface export })
  |> List.sort compare_value_export

let json_string = fun json -> Data.Json.to_string_pretty json

let fail_json = fun ~label json ->
  panic (format Format.[ str label; str "\n"; str (json_string json); ])

let assert_json_equal = fun ~label ~expected ~actual ->
  if not (expected = actual) then
    fail_json ~label (Data.Json.Object [ ("expected", expected); ("actual", actual); ])

let diagnostics_json = fun ~parse ~lowering ~typing ->
  Data.Json.Object [
    ("parse", Data.Json.Array (List.map Syn.Diagnostic.to_json parse));
    ("lowering", Data.Json.Array (List.map Diagnostic.to_json lowering));
    ("typing", Data.Json.Array (List.map Diagnostic.to_json typing));
  ]

let report_json = fun (report: Check_result.t) (interface: oracle_interface) typedtree_text ->
  Data.Json.Object [
    (
      "comparison",
      Data.Json.Object [
        (
          "exports",
          Data.Json.Object [
            ("ocamlc", Data.Json.Array (List.map export_to_json interface.value_exports));
            ("typ", Data.Json.Array (List.map export_to_json (typ_value_exports report)));
          ]
        );
        (
          "normalized_exports",
          Data.Json.Object [
            (
              "ocamlc",
              Data.Json.Array (List.map
                export_to_json
                (normalized_oracle_exports_for_comparison interface interface.value_exports))
            );
            (
              "typ",
              Data.Json.Array (List.map
                export_to_json
                (normalized_oracle_exports_for_comparison interface (typ_value_exports report)))
            );
          ]
        );
        (
          "type_names",
          Data.Json.Object [
            (
              "ocamlc",
              Data.Json.Array (List.map (fun name -> Data.Json.String name) interface.type_names)
            );
            (
              "typ",
              Data.Json.Array (List.map (fun name -> Data.Json.String name) (typ_type_names report))
            );
          ]
        );
      ]
    );
    (
      "ocamlc",
      Data.Json.Object [
        ("interface_text", Data.Json.String interface.text);
        ("exports", Data.Json.Array (List.map export_to_json interface.value_exports));
        (
          "type_aliases",
          Data.Json.Array (List.map
            (fun (type_name, manifest) ->
              Data.Json.Object [
                ("name", Data.Json.String type_name);
                ("manifest", Data.Json.String manifest);
              ])
            interface.type_aliases)
        );
        (
          "type_names",
          Data.Json.Array (List.map (fun name -> Data.Json.String name) interface.type_names)
        );
        ("typedtree_text", Data.Json.String typedtree_text);
      ]
    );
    ("typ", Report.to_json report);
    (
      "typ_summary",
      Data.Json.Object [
        (
          "completeness",
          Data.Json.String (FileSummary.completeness report.file_summary |> completeness_to_string)
        );
        ("exports", Data.Json.Array (List.map export_to_json (typ_value_exports report)));
        (
          "type_names",
          Data.Json.Array (List.map (fun name -> Data.Json.String name) (typ_type_names report))
        );
      ]
    );
  ]

let has_error_diagnostics = fun diagnostics ->
  List.exists (fun diagnostic -> Diagnostic.severity diagnostic = Diagnostic.Error) diagnostics

let assert_no_error_diagnostics = fun (report: Check_result.t) ->
  if
    not (report.parse_diagnostics = [])
    || has_error_diagnostics report.lowering_diagnostics
    || has_error_diagnostics report.typing_diagnostics
  then
    fail_json
      ~label:"oracle fixture produced error diagnostics"
      (diagnostics_json
        ~parse:report.parse_diagnostics
        ~lowering:report.lowering_diagnostics
        ~typing:report.typing_diagnostics);
  if not (FileSummary.completeness report.file_summary = FileSummary.Complete) then
    fail_json
      ~label:"oracle fixture produced a partial file summary"
      (Data.Json.Object [
        (
          "completeness",
          Data.Json.String (FileSummary.completeness report.file_summary |> completeness_to_string)
        );
      ])

let with_oracle_stage = fun ~fixture_filename ~stage thunk ->
  try thunk () with
  | Stack_overflow -> panic
    (format
      Format.[
        str "stack overflow during oracle stage ";
        str stage;
        str " for ";
        str (Path.to_string fixture_filename);
      ])

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let fixture_filename = stable_fixture_filename ctx in
  let source_text =
    with_oracle_stage
      ~fixture_filename
      ~stage:"read_fixture"
      (fun () -> Fs.read ctx.fixture_path |> Result.expect ~msg:"oracle fixture should exist")
  in
  let report =
    with_oracle_stage
      ~fixture_filename
      ~stage:"typ_check_source"
      (fun () -> check_source_text ~filename:fixture_filename source_text)
  in
  let interface =
    with_oracle_stage
      ~fixture_filename
      ~stage:"ocamlc_interface_oracle"
      (fun () -> run_interface_oracle ~fixture_filename ~source_text)
  in
  let typedtree_text =
    with_oracle_stage
      ~fixture_filename
      ~stage:"ocamlc_typedtree_oracle"
      (fun () -> run_typedtree_oracle ~fixture_filename ~source_text)
  in
  let actual_json =
    with_oracle_stage
      ~fixture_filename
      ~stage:"report_json"
      (fun () -> report_json report interface typedtree_text)
  in
  with_oracle_stage
    ~fixture_filename
    ~stage:"assert_no_error_diagnostics"
    (fun () -> assert_no_error_diagnostics report);
  with_oracle_stage
    ~fixture_filename
    ~stage:"assert_export_equivalence"
    (fun () ->
      assert_json_equal
        ~label:"oracle exports mismatch"
        ~expected:(Data.Json.Array (List.map
          export_to_json
          (normalized_oracle_exports_for_comparison interface interface.value_exports)))
        ~actual:(Data.Json.Array (List.map
          export_to_json
          (normalized_oracle_exports_for_comparison interface (typ_value_exports report)))));
  with_oracle_stage
    ~fixture_filename
    ~stage:"assert_type_name_equivalence"
    (fun () ->
      assert_json_equal
        ~label:"oracle type names mismatch"
        ~expected:(Data.Json.Array (List.map (fun name -> Data.Json.String name) interface.type_names))
        ~actual:(Data.Json.Array (List.map
          (fun name -> Data.Json.String name)
          (typ_type_names report))));
  if skip_snapshot_assertion () then
    Ok ()
  else
    with_oracle_stage
      ~fixture_filename
      ~stage:"snapshot_assertion"
      (fun () -> Test.Snapshot.assert_json ~ctx:ctx.test ~actual:actual_json)

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:fixture_filter
          ~snapshot_path:(fun path -> Some (approved_snapshot_path path))
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"typ:oracle_fixtures" ~tests ~args)
    ~args:Std.Env.args
    ()
