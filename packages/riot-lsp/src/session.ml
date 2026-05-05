open Std
open Std.Data
open Std.Result.Syntax
open Typ.Diagnostics
open Typ.Model

let iter_fold = fun fold value ~fn ->
  fold
    value
    ~init:()
    ~fn:(fun item () ->
      fn item;
      Syn.Ast.Continue ())

module Vector = Std.Collections.Vector

let lint_rules = Riot_fix.Pipeline.default_rules ()

type typ_query_context = {
  text: string;
  context: Typ.Query.context;
}

type document = {
  uri: Lsp.Uri.t;
  version: int;
  text: string;
  path: Path.t option;
  typ_context: typ_query_context option;
}

type fixable_lint_diagnostic = {
  diagnostic: Riot_fix.Diagnostic.t;
  lsp_diagnostic: Lsp.Diagnostic.t;
  fix: Riot_fix.Fix.fix;
}

type t = {
  initialized: bool;
  shutdown_requested: bool;
  documents: document list;
  workspace_manager: Riot_model.Workspace_manager.t;
}

type outcome = {
  state: t;
  outbound: Json.t list;
  exit_code: int option;
  debug_events: string list;
}

let empty = {
  initialized = false;
  shutdown_requested = false;
  documents = [];
  workspace_manager = Riot_model.Workspace_manager.create ();
}

let uri_equal = fun left ->
  fun right -> String.equal (Lsp.Uri.to_string left) (Lsp.Uri.to_string right)

let upsert_document = fun state document -> {
  state with
  documents = document
  :: List.filter state.documents ~fn:(fun existing -> not (uri_equal existing.uri document.uri));
}

let remove_document = fun state uri -> {
  state with
  documents = List.filter state.documents ~fn:(fun document -> not (uri_equal document.uri uri));
}

let find_document = fun state uri ->
  List.find
    state.documents
    ~fn:(fun document -> uri_equal document.uri uri)

let response_error = fun ~id ~code ~message ?data () ->
  Lsp.error_response_to_json
    ~id
    Lsp.{ code; message; data }

let ok = fun ?(debug_events = []) state ?exit_code outbound ->
  {
    state;
    outbound;
    exit_code;
    debug_events;
  }

let filename_of_uri = fun uri ->
  match Lsp.Uri.to_path uri with
  | Ok path -> path
  | Error _ -> Path.v "buffer.ml"

let compare_paths = fun left right -> String.compare (Path.to_string left) (Path.to_string right)

let dedupe_paths = fun paths ->
  let sorted = List.sort paths ~compare:compare_paths in
  List.unique sorted ~compare:compare_paths

let document_path_key = fun (document: document) ->
  match document.path with
  | None -> None
  | Some path ->
      Some (
        Path.normalize path
        |> Path.to_string
      )

let document_in_root = fun root ->
  fun (document: document) ->
    match document.path with
    | None -> false
    | Some path ->
        let root = Path.normalize root in
        let path = Path.normalize path in
        Path.equal path root || match Path.strip_prefix path ~prefix:root with
        | Ok _ -> true
        | Error _ -> false

let filename_of_document = fun (document: document) ->
  match document.path with
  | Some path -> path
  | None -> filename_of_uri document.uri

let source_slice = fun text ->
  IO.IoVec.IoSlice.from_string text
  |> Result.expect ~msg:"failed to create LSP parser source slice"

let add_prepared_typ_source = fun session ->
  fun ~kind:_ ->
    fun ~origin:_ ->
      fun ~revision:_ -> fun ~text:_ -> fun ~parse_result:_ -> fun ~cst:_ -> (session, (), ())

let typ_source_origin_of_document = fun (document: document) ->
  match document.path with
  | Some _path -> None
  | None -> None

let package_scope_for_file = fun state path ->
  let start_dir = Path.dirname path in
  match Riot_model.Workspace_manager.scan state.workspace_manager start_dir with
  | Error _ -> None
  | Ok (workspace, _errors) -> (
      match Riot_model.Workspace_manifest.find_package_for_path workspace ~path with
      | None -> None
      | Some manifest ->
          Some (Riot_model.Workspace_manifest.realize_package
            ~intent:Riot_model.Package.Runtime
            workspace
            manifest)
    )

let package_source_files = fun (pkg: Riot_model.Package.t) ->
  ((pkg.sources.src @ pkg.sources.tests) @ pkg.sources.examples) @ pkg.sources.bench
  |> List.map ~fn:(fun relative -> Path.(pkg.path / relative))
  |> dedupe_paths

let package_typ_summary_source_files = fun (pkg: Riot_model.Package.t) ->
  pkg.sources.src
  |> List.map ~fn:(fun relative -> Path.(pkg.path / relative))
  |> dedupe_paths

let package_library_typ_source_files = fun (pkg: Riot_model.Package.t) ->
  match pkg.library with
  | None -> package_typ_summary_source_files pkg
  | Some { path = library_path } ->
      let interface_path = Path.(add_extension (remove_extension library_path) ~ext:"mli") in
      let files =
        match Fs.exists interface_path with
        | Ok true -> [ interface_path; library_path ]
        | Ok false
        | Error _ -> [ library_path ]
      in
      files
      |> dedupe_paths

let typ_target_files = fun state ->
  fun (document: document) ->
    match document.path with
    | None -> []
    | Some path -> (
        match package_scope_for_file state path with
        | Some pkg -> (
            let package_root = pkg.path in
            let package_files = package_source_files pkg in
            let open_documents =
              state.documents
              |> List.filter ~fn:(document_in_root package_root)
              |> List.filter_map ~fn:(fun document -> document.path)
            in
            dedupe_paths ((package_files @ open_documents) @ [ path ])
          )
        | None -> [ path ]
      )

let text_for_path = fun state path ->
  let key =
    Path.normalize path
    |> Path.to_string
  in
  state.documents
  |> List.find
    ~fn:(fun document ->
      match document_path_key document with
      | Some candidate -> String.equal candidate key
      | None -> false)
  |> fun __tmp1 ->
    match __tmp1 with
    | Some document -> Some document.text
    | None -> (
        match Fs.read path with
        | Ok text -> Some text
        | Error _ -> None
      )

let typ_config_for_document = fun _state -> fun (_document: document) -> ()

let typ_snapshot_for_document = fun _state -> fun (_document: document) -> None

let typ_query_context_for_document = fun ?(allow_parse_diagnostics = false) _state ->
  fun document ->
    let parse_result =
      Syn.parse ~filename:(filename_of_document document) (source_slice document.text)
    in
    if (not allow_parse_diagnostics) && not (Vector.is_empty parse_result.diagnostics) then
      None
    else
      let source = Typ.Model.Source.make ~text:document.text in
      match Typ.Ast.from_parse_result ~source parse_result with
      | Error _diagnostics -> None
      | Ok source_file ->
          let infer_result = Typ.Infer.check source_file in
          Some { text = document.text; context = Typ.Query.create ~source_file ~infer_result }

let refresh_document_typ_context = fun state document ->
  match typ_query_context_for_document ~allow_parse_diagnostics:true state document with
  | Some typ_context -> { document with typ_context = Some typ_context }
  | None -> document

let typ_analysis_for_document = fun _state -> fun _document -> None

let diagnostic_to_lsp = fun text ->
  fun (diagnostic: Syn.Diagnostic.t) ->
    {
      Lsp.Diagnostic.range = Lsp.Utf16.range_of_offsets
        text
        ~start_offset:diagnostic.span.start
        ~end_offset:diagnostic.span.end_;
      severity = Some Lsp.Diagnostic.Error;
      code = Some (Syn.Diagnostic.id diagnostic);
      source = Some "syn";
      message = Syn.Diagnostic.main_message diagnostic;
      tags = None;
      data = Some (Syn.Diagnostic.to_json diagnostic);
    }

let lint_diagnostic_severity = fun severity ->
  match severity with
  | Riot_fix.Diagnostic.Error -> Lsp.Diagnostic.Error
  | Riot_fix.Diagnostic.Warning -> Lsp.Diagnostic.Warning
  | Riot_fix.Diagnostic.Info -> Lsp.Diagnostic.Information
  | Riot_fix.Diagnostic.Hint -> Lsp.Diagnostic.Hint

let lint_diagnostic_to_lsp = fun text ->
  fun (diagnostic: Riot_fix.Diagnostic.t) ->
    let span = Riot_fix.Diagnostic.span diagnostic in
    {
      Lsp.Diagnostic.range = Lsp.Utf16.range_of_offsets
        text
        ~start_offset:span.start
        ~end_offset:span.end_;
      severity = Some (lint_diagnostic_severity (Riot_fix.Diagnostic.severity diagnostic));
      code = Some (Riot_fix.Rule_id.to_string (Riot_fix.Diagnostic.rule_id diagnostic));
      source = Some "riot-fix";
      message = Riot_fix.Diagnostic.message diagnostic;
      tags = None;
      data = Some (Riot_fix.Diagnostic.to_json diagnostic);
    }

let typ_diagnostic_severity = fun diagnostic ->
  match Typ.Diagnostics.Diagnostic.severity diagnostic with
  | Typ.Diagnostics.Diagnostic.Error -> Lsp.Diagnostic.Error
  | Typ.Diagnostics.Diagnostic.Warning -> Lsp.Diagnostic.Warning

let typ_diagnostic_to_lsp = fun text ->
  fun (diagnostic: Typ.Diagnostics.Diagnostic.t) ->
    let span = Typ.Diagnostics.Diagnostic.span diagnostic in
    {
      Lsp.Diagnostic.range = Lsp.Utf16.range_of_offsets
        text
        ~start_offset:span.start
        ~end_offset:span.end_;
      severity = Some (typ_diagnostic_severity diagnostic);
      code =
        Some (
          diagnostic
          |> Typ.Diagnostics.Diagnostic.id
          |> Typ.Diagnostics.Error.id_to_string
        );
      source = Some "typ";
      message = Typ.Diagnostics.Diagnostic.to_string diagnostic;
      tags = None;
      data = None;
    }

let analyze_document = fun document ->
  Riot_fix.Source_runner.run
    ~rules:lint_rules
    ~filename:(filename_of_document document)
    document.text

let compare_position = fun (left: Lsp.Position.t) ->
  fun (right: Lsp.Position.t) ->
    match Int.compare left.line right.line with
    | Order.EQ -> Int.compare left.character right.character
    | n -> n

let ranges_overlap = fun (left: Lsp.Range.t) ->
  fun (right: Lsp.Range.t) ->
    compare_position left.end_ right.start_ != Order.LT
    && compare_position right.end_ left.start_ != Order.LT

let same_range = fun (left: Lsp.Range.t) ->
  fun (right: Lsp.Range.t) ->
    compare_position left.start_ right.start_ = Order.EQ
    && compare_position left.end_ right.end_ = Order.EQ

let same_lsp_diagnostic = fun (left: Lsp.Diagnostic.t) ->
  fun (right: Lsp.Diagnostic.t) ->
    same_range left.range right.range
    && Option.equal left.code right.code ~fn:String.equal
    && Option.equal left.source right.source ~fn:String.equal
    && String.equal left.message right.message

let action_kind_allowed = fun only actual ->
  let actual_name = Lsp.Action_kind.to_string actual in
  match only with
  | None -> true
  | Some requested ->
      List.exists
        (fun requested_kind ->
          let requested_name = Lsp.Action_kind.to_string requested_kind in
          String.equal requested_name actual_name
          || String.starts_with ~prefix:(requested_name ^ ".") actual_name)
        requested

let lint_diagnostic_requested = fun context range diagnostic ->
  if List.is_empty context.Lsp.Text_document_methods.Code_action.diagnostics then
    ranges_overlap diagnostic.Lsp.Diagnostic.range range
  else
    List.exists (same_lsp_diagnostic diagnostic) context.diagnostics

let document_range = fun text ->
  Lsp.Utf16.range_of_offsets
    text
    ~start_offset:0
    ~end_offset:(String.length text)

let workspace_edit_of_text = fun document text -> {
  Lsp.Workspace_edit.changes = [
    (document.uri, [ { Lsp.Text_edit.range = document_range document.text; new_text = text } ]);
  ];
}

let maybe_format_text = fun document text ->
  let parse_result = Syn.parse ~filename:(filename_of_uri document.uri) (source_slice text) in
  if Vector.length parse_result.diagnostics > 0 then
    text
  else
    match Krasny.format parse_result with
    | Ok formatted -> formatted
    | Error _ -> text

let finalized_workspace_edit_of_text = fun document text ->
  let text = maybe_format_text document text in
  workspace_edit_of_text document text

let fixable_lint_diagnostics = fun document result ->
  result.Riot_fix.Source_runner.diagnostics
  |> List.filter_map
    ~fn:(fun diagnostic ->
      match Riot_fix.Diagnostic.fix diagnostic with
      | None -> None
      | Some fix ->
          Some {
            diagnostic;
            lsp_diagnostic = lint_diagnostic_to_lsp document.text diagnostic;
            fix;
          })

let quickfix_action_of_entry = fun document entry ->
  match Riot_fix.Fix.apply_fix ~source:document.text entry.fix with
  | Error _ -> None
  | Ok text ->
      Some (
        Lsp.Code_action_or_command.Action {
          Lsp.Code_action.title = Riot_fix.Fix.title entry.fix;
          kind = Some Lsp.Action_kind.Quick_fix;
          diagnostics = Some [ entry.lsp_diagnostic ];
          is_preferred = Some true;
          edit = Some (finalized_workspace_edit_of_text document text);
          command = None;
          data = None;
        }
      )

let fix_all_action = fun document entries ->
  match Riot_fix.Fix.apply_fixes
    ~source:document.text
    (List.map entries ~fn:(fun entry -> entry.fix)) with
  | Error _ -> None
  | Ok text ->
      Some (
        Lsp.Code_action_or_command.Action {
          Lsp.Code_action.title = "Fix all auto-fixable Riot diagnostics";
          kind = Some Lsp.Action_kind.Source_fix_all;
          diagnostics = Some (List.map entries ~fn:(fun entry -> entry.lsp_diagnostic));
          is_preferred = None;
          edit = Some (finalized_workspace_edit_of_text document text);
          command = None;
          data = None;
        }
      )

let typ_diagnostics = fun state ->
  fun document ->
    match typ_query_context_for_document state document with
    | None -> []
    | Some context ->
        let result = Typ.Query.infer_result context.context in
        result.diagnostics.items
        |> Vector.iter
        |> Iter.Iterator.map ~fn:(typ_diagnostic_to_lsp document.text)
        |> Iter.Iterator.to_list

let infer_interface_for_document = fun state ->
  fun document ->
    typ_query_context_for_document state document
    |> Option.map ~fn:(fun context -> (Typ.Query.infer_result context.context).intf)

let publish_diagnostics = fun state ->
  fun document ->
    let result = analyze_document document in
    let diagnostics =
      (List.map result.parse_diagnostics ~fn:(diagnostic_to_lsp document.text)
      @ List.map result.diagnostics ~fn:(lint_diagnostic_to_lsp document.text))
      @ typ_diagnostics state document
    in
    let params: Lsp.Text_document_methods.Publish_diagnostics.params = {
      uri = document.uri;
      version = Some document.version;
      diagnostics;
    }
    in
    Lsp.notification_to_json Lsp.Text_document_methods.Publish_diagnostics.notification params

let query_position_of_lsp_position = fun text position ->
  match Lsp.Utf16.offset_of_position text position with
  | Error _ -> None
  | Ok offset -> Some offset

let range_of_span = fun text (span: Syn.Span.t) ->
  Lsp.Utf16.range_of_offsets
    text
    ~start_offset:span.start
    ~end_offset:span.end_

let position_of_offset = fun text offset ->
  (Lsp.Utf16.range_of_offsets text ~start_offset:offset ~end_offset:offset).start_

let range_of_node = fun text node ->
  range_of_span
    text
    (Syn.Span.make ~start:(Syn.Ast.Node.span_start node) ~end_:(Syn.Ast.Node.span_end node))

let range_of_token = fun text token ->
  range_of_span
    text
    (Syn.Span.make ~start:(Syn.Ast.Token.span_start token) ~end_:(Syn.Ast.Token.span_end token))

let range_of_ident = fun text ident -> range_of_span text (Syn.Ast.Ident.span ident)

let range_of_tokens = fun text tokens ->
  match tokens with
  | [] -> Lsp.Utf16.range_of_offsets text ~start_offset:0 ~end_offset:0
  | first :: rest ->
      let last = List.fold_left rest ~init:first ~fn:(fun _ token -> token) in
      let start = Syn.Ast.Token.span_start first in
      let end_ = Syn.Ast.Token.span_end last in
      Lsp.Utf16.range_of_offsets text ~start_offset:start ~end_offset:end_

let text_of_name_tokens = fun tokens ->
  tokens
  |> List.map ~fn:Syn.Ast.Token.text
  |> String.concat ""

let vector_to_list = fun vector ->
  Vector.to_array vector
  |> Array.to_list

let collect_tokens = fun ~size collect ->
  let tokens = Vector.with_capacity ~size in
  collect ~fn:(fun token -> Vector.push tokens ~value:token);
  vector_to_list tokens

let rec binding_name_tokens_of_parameter = fun parameter ->
  match Syn.Ast.Parameter.view parameter with
  | Syn.Ast.Parameter.Param { pattern = Some pattern; _ } -> binding_name_tokens_of_pattern pattern
  | Syn.Ast.Parameter.Param { label = Syn.Ast.Parameter.Labeled { name = Some label }; _ }
  | Syn.Ast.Parameter.Param { label = Syn.Ast.Parameter.Optional { name = Some label; _ }; _ } ->
      Some [ label ]
  | Syn.Ast.Parameter.Param _
  | Syn.Ast.Parameter.Unknown _ -> None

and binding_name_tokens_of_pattern = fun pattern ->
  match Syn.Ast.Pattern.view pattern with
  | Syn.Ast.Pattern.Ident { ident } -> (
      match Syn.Ast.Ident.last_segment ident with
      | Some token -> Some [ token ]
      | None -> None
    )
  | Syn.Ast.Pattern.Alias { alias; _ }
  | Syn.Ast.Pattern.Constraint { pattern = alias; _ }
  | Syn.Ast.Pattern.Lazy { pattern = alias }
  | Syn.Ast.Pattern.Exception { pattern = alias } -> binding_name_tokens_of_pattern alias
  | Syn.Ast.Pattern.Unit
  | Syn.Ast.Pattern.Wildcard
  | Syn.Ast.Pattern.Literal _
  | Syn.Ast.Pattern.Constructor _
  | Syn.Ast.Pattern.Tuple _
  | Syn.Ast.Pattern.List _
  | Syn.Ast.Pattern.Array _
  | Syn.Ast.Pattern.Record _
  | Syn.Ast.Pattern.PolyVariant _
  | Syn.Ast.Pattern.FirstClassModule _
  | Syn.Ast.Pattern.Interval _
  | Syn.Ast.Pattern.Or _
  | Syn.Ast.Pattern.Cons _
  | Syn.Ast.Pattern.Error _
  | Syn.Ast.Pattern.Unknown _ -> None

let rec value_like_type_is_function = fun type_ ->
  match Syn.Ast.TypeExpr.view type_ with
  | Syn.Ast.TypeExpr.Arrow _ -> true
  | Syn.Ast.TypeExpr.Forall { body; _ } -> value_like_type_is_function body
  | _ -> false

let symbol_kind_of_type_member = fun member ->
  if Option.is_some (Syn.Ast.TypeDeclaration.Member.variant_type member) then
    Lsp.Symbol_kind.Enum
  else
    Lsp.Symbol_kind.Struct

let symbol_children = fun __tmp1 ->
  match __tmp1 with
  | [] -> None
  | children -> Some children

let document_symbol_of_named_item = fun
  ~text ~name ~kind ~syntax_node ~selection_range ?detail ?children () ->
  {
    Lsp.Document_symbol_item.name;
    detail;
    kind;
    range = range_of_node text syntax_node;
    selection_range;
    children;
  }

let let_binding_symbol = fun text ~range_node binding ->
  match Syn.Ast.LetBinding.pattern binding
  |> Option.and_then ~fn:binding_name_tokens_of_pattern with
  | None -> []
  | Some name_tokens ->
      let has_parameters = ref false in
      iter_fold Syn.Ast.LetBinding.fold_parameter binding ~fn:(fun _ -> has_parameters := true);
      let kind =
        match Syn.Ast.LetBinding.type_annotation binding with
        | Some type_ when value_like_type_is_function type_ -> Lsp.Symbol_kind.Function
        | _ when !has_parameters -> Lsp.Symbol_kind.Function
        | _ -> Lsp.Symbol_kind.Variable
      in
      [
        document_symbol_of_named_item
          ~text
          ~name:(text_of_name_tokens name_tokens)
          ~kind
          ~syntax_node:range_node
          ~selection_range:(range_of_tokens text name_tokens)
          ();
      ]

let let_declaration_symbols = fun text declaration ->
  let symbols = Vector.with_capacity ~size:2 in
  iter_fold
    Syn.Ast.LetDeclaration.fold_binding
    declaration
    ~fn:(fun binding ->
      let_binding_symbol text ~range_node:(Syn.Ast.LetDeclaration.as_node declaration) binding
      |> List.for_each ~fn:(fun symbol -> Vector.push symbols ~value:symbol));
  vector_to_list symbols

let type_declaration_symbols = fun text declaration ->
  Syn.Ast.TypeDeclaration.fold_members
    declaration
    []
    (fun acc member ->
      match Syn.Ast.TypeDeclaration.Member.name member with
      | None -> acc
      | Some name ->
          document_symbol_of_named_item
            ~text
            ~name:(Syn.Ast.Ident.text name)
            ~kind:(symbol_kind_of_type_member member)
            ~syntax_node:(Syn.Ast.TypeDeclaration.as_node declaration)
            ~selection_range:(range_of_ident text name)
            ()
          :: acc)
  |> List.reverse

let type_extension_symbols = fun text declaration ->
  match Syn.Ast.TypeExtensionDeclaration.name declaration with
  | None -> []
  | Some name ->
      [
        document_symbol_of_named_item
          ~text
          ~name:(Syn.Ast.Ident.text name)
          ~kind:Lsp.Symbol_kind.Enum
          ~syntax_node:(Syn.Ast.TypeExtensionDeclaration.as_node declaration)
          ~selection_range:(range_of_ident text name)
          ();
      ]

let rec collect_structure_symbols = fun text collect ->
  let items = Vector.with_capacity ~size:8 in
  collect ~fn:(fun item -> Vector.push items ~value:item);
  structure_item_symbols text (vector_to_list items)

and collect_signature_symbols = fun text collect ->
  let items = Vector.with_capacity ~size:8 in
  collect ~fn:(fun item -> Vector.push items ~value:item);
  signature_item_symbols text (vector_to_list items)

and module_declaration_children = fun text declaration ->
  match Syn.Ast.ModuleDeclaration.body declaration with
  | Syn.Ast.ModuleDeclaration.Expr { body } -> (
      match Syn.Ast.ModuleExpr.view body with
      | Syn.Ast.ModuleExpr.Structure _ ->
          collect_structure_symbols
            text
            (iter_fold Syn.Ast.ModuleDeclaration.fold_structure_item declaration)
      | Syn.Ast.ModuleExpr.Ident _
      | Syn.Ast.ModuleExpr.Functor _
      | Syn.Ast.ModuleExpr.Apply _
      | Syn.Ast.ModuleExpr.Constraint _
      | Syn.Ast.ModuleExpr.Opaque _
      | Syn.Ast.ModuleExpr.Error _
      | Syn.Ast.ModuleExpr.Unknown _ -> []
    )
  | Syn.Ast.ModuleDeclaration.Type { body } -> (
      match Syn.Ast.ModuleTypeExpr.view body with
      | Syn.Ast.ModuleTypeExpr.Signature _ ->
          collect_signature_symbols
            text
            (iter_fold Syn.Ast.ModuleDeclaration.fold_signature_item declaration)
      | Syn.Ast.ModuleTypeExpr.Ident _
      | Syn.Ast.ModuleTypeExpr.With _
      | Syn.Ast.ModuleTypeExpr.Typeof _
      | Syn.Ast.ModuleTypeExpr.Functor _
      | Syn.Ast.ModuleTypeExpr.Error _
      | Syn.Ast.ModuleTypeExpr.Unknown _ -> []
    )
  | Syn.Ast.ModuleDeclaration.Unsupported _ -> []

and module_declaration_symbols = fun text declaration ->
  Syn.Ast.ModuleDeclaration.fold_members
    declaration
    []
    (fun acc member ->
      match Syn.Ast.ModuleDeclaration.Member.name member with
      | None -> acc
      | Some name ->
          let children = module_declaration_children text declaration in
          document_symbol_of_named_item
            ~text
            ~name:(Syn.Ast.Ident.text name)
            ~kind:Lsp.Symbol_kind.Module
            ~syntax_node:(Syn.Ast.ModuleDeclaration.as_node declaration)
            ~selection_range:(range_of_ident text name)
            ?children:(symbol_children children)
            ()
          :: acc)
  |> List.reverse

and module_type_declaration_symbols = fun text declaration ->
  match Syn.Ast.ModuleTypeDeclaration.name declaration with
  | None -> []
  | Some name ->
      let children =
        match Syn.Ast.ModuleTypeDeclaration.body declaration with
        | Syn.Ast.ModuleTypeDeclaration.Abstract
        | Syn.Ast.ModuleTypeDeclaration.Unsupported _ -> []
        | Syn.Ast.ModuleTypeDeclaration.Manifest { body } -> (
            match Syn.Ast.ModuleTypeExpr.view body with
            | Syn.Ast.ModuleTypeExpr.Signature _ ->
                collect_signature_symbols
                  text
                  (iter_fold Syn.Ast.ModuleTypeDeclaration.fold_signature_item declaration)
            | Syn.Ast.ModuleTypeExpr.Ident _
            | Syn.Ast.ModuleTypeExpr.With _
            | Syn.Ast.ModuleTypeExpr.Typeof _
            | Syn.Ast.ModuleTypeExpr.Functor _
            | Syn.Ast.ModuleTypeExpr.Error _
            | Syn.Ast.ModuleTypeExpr.Unknown _ -> []
          )
      in
      [
        document_symbol_of_named_item
          ~text
          ~name:(Syn.Ast.Ident.text name)
          ~kind:Lsp.Symbol_kind.Interface
          ~syntax_node:(Syn.Ast.ModuleTypeDeclaration.as_node declaration)
          ~selection_range:(range_of_ident text name)
          ?children:(symbol_children children)
          ();
      ]

and exception_symbols = fun text declaration ->
  match Syn.Ast.ExceptionDeclaration.name declaration with
  | None -> []
  | Some name ->
      [
        document_symbol_of_named_item
          ~text
          ~name:(Syn.Ast.Ident.text name)
          ~kind:Lsp.Symbol_kind.Event
          ~syntax_node:(Syn.Ast.ExceptionDeclaration.as_node declaration)
          ~selection_range:(range_of_ident text name)
          ();
      ]

and value_like_declaration_symbols = fun text ~syntax_node ~name ~type_annotation ->
  match name with
  | None -> []
  | Some name ->
      let kind =
        match type_annotation with
        | Some type_ when value_like_type_is_function type_ -> Lsp.Symbol_kind.Function
        | _ -> Lsp.Symbol_kind.Variable
      in
      [
        document_symbol_of_named_item
          ~text
          ~name:(Syn.Ast.Ident.text name)
          ~kind
          ~syntax_node
          ~selection_range:(range_of_ident text name)
          ();
      ]

and value_declaration_symbols = fun text declaration ->
  value_like_declaration_symbols
    text
    ~syntax_node:(Syn.Ast.ValueDeclaration.as_node declaration)
    ~name:(Syn.Ast.ValueDeclaration.name declaration)
    ~type_annotation:(Syn.Ast.ValueDeclaration.type_annotation declaration)

and external_declaration_symbols = fun text declaration ->
  value_like_declaration_symbols
    text
    ~syntax_node:(Syn.Ast.ExternalDeclaration.as_node declaration)
    ~name:(Syn.Ast.ExternalDeclaration.name declaration)
    ~type_annotation:(Syn.Ast.ExternalDeclaration.type_annotation declaration)

and structure_item_symbols = fun text items ->
  items
  |> List.map
    ~fn:(fun item ->
      match Syn.Ast.StructureItem.view item with
      | Syn.Ast.StructureItem.Let declaration -> let_declaration_symbols text declaration
      | Syn.Ast.StructureItem.Type (
        Syn.Ast.TypeDeclarationItem declaration
      ) ->
          type_declaration_symbols text declaration
      | Syn.Ast.StructureItem.Type (
        Syn.Ast.TypeExtensionItem declaration
      ) ->
          type_extension_symbols text declaration
      | Syn.Ast.StructureItem.Module declaration -> module_declaration_symbols text declaration
      | Syn.Ast.StructureItem.ModuleType declaration ->
          module_type_declaration_symbols text declaration
      | Syn.Ast.StructureItem.External declaration -> external_declaration_symbols text declaration
      | Syn.Ast.StructureItem.Exception declaration -> exception_symbols text declaration
      | Syn.Ast.StructureItem.Open _
      | Syn.Ast.StructureItem.Include _
      | Syn.Ast.StructureItem.Extension _
      | Syn.Ast.StructureItem.Attribute _
      | Syn.Ast.StructureItem.Expr _
      | Syn.Ast.StructureItem.Error _
      | Syn.Ast.StructureItem.Unknown _ -> [])
  |> List.concat

and signature_item_symbols = fun text items ->
  items
  |> List.map
    ~fn:(fun item ->
      match Syn.Ast.SignatureItem.view item with
      | Syn.Ast.SignatureItem.Value declaration -> value_declaration_symbols text declaration
      | Syn.Ast.SignatureItem.Type (
        Syn.Ast.TypeDeclarationItem declaration
      ) ->
          type_declaration_symbols text declaration
      | Syn.Ast.SignatureItem.Type (
        Syn.Ast.TypeExtensionItem declaration
      ) ->
          type_extension_symbols text declaration
      | Syn.Ast.SignatureItem.Module declaration -> module_declaration_symbols text declaration
      | Syn.Ast.SignatureItem.ModuleType declaration ->
          module_type_declaration_symbols text declaration
      | Syn.Ast.SignatureItem.External declaration -> external_declaration_symbols text declaration
      | Syn.Ast.SignatureItem.Exception declaration -> exception_symbols text declaration
      | Syn.Ast.SignatureItem.Open _
      | Syn.Ast.SignatureItem.Include _
      | Syn.Ast.SignatureItem.Extension _
      | Syn.Ast.SignatureItem.Attribute _
      | Syn.Ast.SignatureItem.Error _
      | Syn.Ast.SignatureItem.Unknown _ -> [])
  |> List.concat

let document_symbols_for_document = fun document ->
  let parsed = Syn.parse ~filename:(filename_of_document document) (source_slice document.text) in
  if Vector.length parsed.Syn.Parser.diagnostics > 0 then
    Some []
  else
    let root = Syn.Ast.SourceFile.make parsed.Syn.Parser.tree in
    let symbols =
      match Syn.Ast.SourceFile.view root with
      | Syn.Ast.SourceFile.Implementation implementation ->
          let items = Vector.with_capacity ~size:16 in
          iter_fold
            Syn.Ast.Implementation.fold_item
            implementation
            ~fn:(fun item -> Vector.push items ~value:item);
          structure_item_symbols document.text (vector_to_list items)
      | Syn.Ast.SourceFile.Interface interface ->
          let items = Vector.with_capacity ~size:16 in
          iter_fold
            Syn.Ast.Interface.fold_item
            interface
            ~fn:(fun item -> Vector.push items ~value:item);
          signature_item_symbols document.text (vector_to_list items)
    in
    Some symbols

type hover_candidate = {
  hover_span: Syn.Span.t;
  hover_type: Typ.Ast.Type.t;
}

let span_contains_offset = fun (span: Syn.Span.t) offset ->
  offset >= span.start && offset <= span.end_

let hover_candidate_width = fun candidate -> Syn.Span.width candidate.hover_span

let better_hover_candidate = fun left right ->
  match (left, right) with
  | (None, candidate)
  | (candidate, None) -> candidate
  | (Some left, Some right) ->
      if hover_candidate_width right < hover_candidate_width left then
        Some right
      else
        Some left

let hover_candidate = fun offset origin type_ ->
  if span_contains_offset origin.Typ.Ast.span offset then
    Option.map type_ ~fn:(fun hover_type -> { hover_span = origin.span; hover_type })
  else
    None

let best_hover_candidate = fun candidates ->
  List.fold_left
    candidates
    ~init:None
    ~fn:better_hover_candidate

let rec hover_pattern = fun offset (pattern: Typ.Ast.pattern) ->
  let self = hover_candidate offset pattern.origin pattern.type_ in
  let children =
    match pattern.kind with
    | Typ.Ast.Wildcard
    | Typ.Ast.Bind _
    | Typ.Ast.Literal _
    | Typ.Ast.FirstClassModule _ -> []
    | Typ.Ast.Constructor { payload; _ } -> [ Option.and_then payload ~fn:(hover_pattern offset) ]
    | Typ.Ast.PolyVariant { payload; _ } -> [ Option.and_then payload ~fn:(hover_pattern offset) ]
    | Typ.Ast.Tuple patterns
    | Typ.Ast.List patterns -> List.map patterns ~fn:(hover_pattern offset)
    | Typ.Ast.Record fields ->
        List.map
          fields
          ~fn:(fun field -> Option.and_then field.Typ.Ast.pattern ~fn:(hover_pattern offset))
    | Typ.Ast.Or { left; right }
    | Typ.Ast.Cons { head = left; tail = right } ->
        [ hover_pattern offset left; hover_pattern offset right ]
    | Typ.Ast.Constraint { pattern; _ }
    | Typ.Ast.Attribute pattern -> [ hover_pattern offset pattern ]
    | Typ.Ast.Alias { pattern; alias } ->
        [ hover_pattern offset pattern; hover_pattern offset alias ]
  in
  best_hover_candidate (self :: children)

and hover_parameter = fun offset (parameter: Typ.Ast.parameter) ->
  best_hover_candidate
    [
      hover_pattern offset parameter.pattern;
      Option.and_then parameter.default ~fn:(hover_expression offset);
    ]

and hover_parameters = fun offset parameters ->
  parameters
  |> List.map ~fn:(hover_parameter offset)
  |> best_hover_candidate

and hover_function_body = fun offset body ->
  match body with
  | Typ.Ast.Body expression -> hover_expression offset expression
  | Typ.Ast.Cases cases -> hover_match_cases offset cases

and hover_match_case = fun offset (case: Typ.Ast.match_case) ->
  best_hover_candidate
    [
      hover_pattern offset case.pattern;
      Option.and_then case.guard ~fn:(hover_expression offset);
      hover_expression offset case.body;
    ]

and hover_match_cases = fun offset cases ->
  cases
  |> List.map ~fn:(hover_match_case offset)
  |> best_hover_candidate

and hover_argument = fun offset (argument: Typ.Ast.argument) ->
  match argument.kind with
  | Typ.Ast.Positional expression -> hover_expression offset expression
  | Typ.Ast.Labeled { value; _ }
  | Typ.Ast.Optional { value; _ } -> Option.and_then value ~fn:(hover_expression offset)

and hover_let_binding = fun offset (binding: Typ.Ast.let_binding) ->
  best_hover_candidate
    [ hover_pattern offset binding.pattern; hover_expression offset binding.expr; ]

and hover_expression = fun offset (expression: Typ.Ast.expression) ->
  let self = hover_candidate offset expression.origin expression.type_ in
  let children =
    match expression.kind with
    | Typ.Ast.Literal _
    | Typ.Ast.Ident _
    | Typ.Ast.FirstClassModule _ -> []
    | Typ.Ast.Constructor { payload; _ } ->
        [ Option.and_then payload ~fn:(hover_expression offset) ]
    | Typ.Ast.Tuple expressions
    | Typ.Ast.List expressions
    | Typ.Ast.Array expressions -> List.map expressions ~fn:(hover_expression offset)
    | Typ.Ast.PolyVariant { payload; _ } ->
        [ Option.and_then payload ~fn:(hover_expression offset) ]
    | Typ.Ast.Record { update; fields } ->
        Option.and_then update ~fn:(hover_expression offset)
        :: List.map fields ~fn:(fun field -> hover_expression offset field.Typ.Ast.value)
    | Typ.Ast.FieldAccess { receiver; _ } -> [ hover_expression offset receiver ]
    | Typ.Ast.Assign { target; value }
    | Typ.Ast.Sequence { left = target; right = value }
    | Typ.Ast.Infix { left = target; right = value; _ } ->
        [ hover_expression offset target; hover_expression offset value ]
    | Typ.Ast.If { condition; then_branch; else_branch } ->
        [
          hover_expression offset condition;
          hover_expression offset then_branch;
          Option.and_then else_branch ~fn:(hover_expression offset);
        ]
    | Typ.Ast.Match { scrutinee; cases } ->
        [ hover_expression offset scrutinee; hover_match_cases offset cases ]
    | Typ.Ast.Try { body; cases } ->
        [ hover_expression offset body; hover_match_cases offset cases ]
    | Typ.Ast.While { condition; body } ->
        [ hover_expression offset condition; hover_expression offset body ]
    | Typ.Ast.For {
        pattern;
        start_;
        stop;
        body;
      } ->
        [
          hover_pattern offset pattern;
          hover_expression offset start_;
          hover_expression offset stop;
          hover_expression offset body;
        ]
    | Typ.Ast.Function { parameters; body; _ } ->
        [ hover_parameters offset parameters; hover_function_body offset body ]
    | Typ.Ast.Apply { callee; arguments } ->
        hover_expression offset callee :: List.map arguments ~fn:(hover_argument offset)
    | Typ.Ast.Let { bindings; body; _ } ->
        hover_expression offset body :: List.map bindings ~fn:(hover_let_binding offset)
    | Typ.Ast.LetModule { body; _ }
    | Typ.Ast.LocalOpen { body; _ }
    | Typ.Ast.Assert body -> [ hover_expression offset body ]
  in
  best_hover_candidate (self :: children)

let rec hover_structure_item = fun offset (item: Typ.Ast.structure_item) ->
  match item.kind with
  | Typ.Ast.Let declaration ->
      declaration.bindings
      |> List.map ~fn:(hover_let_binding offset)
      |> best_hover_candidate
  | Typ.Ast.Expression expression -> hover_expression offset expression
  | Typ.Ast.Module modules ->
      modules
      |> List.map
        ~fn:(fun (module_: Typ.Ast.module_declaration) ->
          module_.items
          |> List.map ~fn:(hover_structure_item offset)
          |> best_hover_candidate)
      |> best_hover_candidate
  | Typ.Ast.Type _
  | Typ.Ast.TypeExtension _
  | Typ.Ast.External _
  | Typ.Ast.Exception _
  | Typ.Ast.ModuleType _
  | Typ.Ast.Include _ -> None

let hover_ast = fun offset (ast: Typ.Ast.t) ->
  match ast with
  | Typ.Ast.Implementation implementation ->
      implementation.items
      |> List.map ~fn:(hover_structure_item offset)
      |> best_hover_candidate
  | Typ.Ast.Interface _ -> None

let hover_candidate_of_query_node node =
  let rec loop node =
    match Typ.Query.Node.kind node with
    | Typ.Query.Node.Expression expression -> (
        match expression.type_ with
        | Some hover_type -> Some { hover_span = Typ.Query.Node.span node; hover_type }
        | None -> parent node
      )
    | Typ.Query.Node.Pattern pattern -> (
        match pattern.type_ with
        | Some hover_type -> Some { hover_span = Typ.Query.Node.span node; hover_type }
        | None -> parent node
      )
    | Typ.Query.Node.CoreType type_ -> (
        match type_.type_ with
        | Some hover_type -> Some { hover_span = Typ.Query.Node.span node; hover_type }
        | None -> parent node
      )
    | _ -> parent node
  and parent node =
    match Typ.Query.Node.parent node with
    | Some parent -> loop parent
    | None -> None
  in
  loop node

let hover_for_document = fun state ->
  fun document ->
    fun position ->
      match query_position_of_lsp_position document.text position with
      | None -> None
      | Some offset ->
          match typ_query_context_for_document state document with
          | None -> None
          | Some context -> (
              let cursor = Syn.Span.make ~start:offset ~end_:offset in
              match Typ.Query.node_at context.context cursor with
              | None -> None
              | Some node -> (
                  match hover_candidate_of_query_node node with
                  | None -> None
                  | Some candidate ->
                      Some {
                        Lsp.Hover_result.contents = {
                          Lsp.Markup_content.kind = Lsp.Markup_kind.Plain_text;
                          value = Typ.Ast.Type.to_string candidate.hover_type;
                        };
                        range = Some (range_of_span document.text candidate.hover_span);
                      }
                )
            )

let completion_item = fun ?detail kind label ->
  {
    Lsp.Completion_item.label;
    kind = Some kind;
    detail;
    documentation = None;
    insert_text = None;
  }

let completion_kind_of_value = fun (scheme: Typ.Infer.TypeScheme.t) ->
  match Typ.Infer.Unifier.resolve scheme.body with
  | Typ.Ast.Type.Arrow _ -> Lsp.Completion_item.Kind.Function
  | _ -> Lsp.Completion_item.Kind.Variable

let value_completion_item = fun (name, (scheme: Typ.Infer.TypeScheme.t)) ->
  completion_item
    ~detail:(Typ.Ast.Type.to_string scheme.body)
    (completion_kind_of_value scheme)
    (Typ.Model.Surface_path.to_string name)

let type_completion_item = fun (name, _declaration) ->
  completion_item
    ~detail:"type"
    Lsp.Completion_item.Kind.Struct
    (Typ.Model.Surface_path.to_string name)

let module_completion_item = fun (name, _module_) ->
  completion_item
    ~detail:"module"
    Lsp.Completion_item.Kind.Module
    (Typ.Model.Surface_path.to_string name)

let constructor_completion_item = fun owner (constructor: Typ.Ast.type_constructor) ->
  completion_item
    ~detail:(Typ.Model.Surface_path.to_string owner)
    Lsp.Completion_item.Kind.Constructor
    (Typ.Model.Surface_path.to_string constructor.name)

let constructor_completion_items_for_type = fun (_name, (declaration: Typ.Ast.type_declaration)) ->
  match declaration.definition.kind with
  | Typ.Ast.Variant constructors ->
      List.map constructors ~fn:(constructor_completion_item declaration.name)
  | _ -> []

let record_field_detail = fun (field: Typ.Ast.record_field_declaration) ->
  field.type_annotation.type_
  |> Option.map ~fn:Typ.Ast.Type.to_string

let record_field_completion_item = fun (field: Typ.Ast.record_field_declaration) ->
  completion_item
    ?detail:(record_field_detail field)
    Lsp.Completion_item.Kind.Field
    (Typ.Model.Surface_path.to_string field.name)

let value_completion_items_for_interface = fun (intf: Typ.Infer.ModuleInterface.t) ->
  intf
  |> Typ.Infer.ModuleInterface.values
  |> Iter.Iterator.map ~fn:value_completion_item
  |> Iter.Iterator.to_list

let type_completion_items_for_interface = fun (intf: Typ.Infer.ModuleInterface.t) ->
  intf
  |> Typ.Infer.ModuleInterface.types
  |> Iter.Iterator.map ~fn:type_completion_item
  |> Iter.Iterator.to_list

let constructor_completion_items_for_interface = fun (intf: Typ.Infer.ModuleInterface.t) ->
  intf
  |> Typ.Infer.ModuleInterface.types
  |> Iter.Iterator.map ~fn:constructor_completion_items_for_type
  |> Iter.Iterator.to_list
  |> List.concat

let module_completion_items_for_interface = fun (intf: Typ.Infer.ModuleInterface.t) ->
  intf
  |> Typ.Infer.ModuleInterface.modules
  |> Iter.Iterator.map ~fn:module_completion_item
  |> Iter.Iterator.to_list

let expression_completion_items_for_interface = fun intf ->
  let values = value_completion_items_for_interface intf in
  let constructors = constructor_completion_items_for_interface intf in
  let modules = module_completion_items_for_interface intf in
  (values @ constructors) @ modules

let type_position_completion_items_for_interface = fun intf ->
  type_completion_items_for_interface intf @ module_completion_items_for_interface intf

let type_ident_of_type = fun type_ ->
  match Typ.Infer.Unifier.resolve type_ with
  | Typ.Ast.Type.Apply { ident; _ } -> Some ident
  | _ -> None

let record_fields_for_interface = fun ?owner (intf: Typ.Infer.ModuleInterface.t) ->
  let fields = Vector.with_capacity ~size:8 in
  intf
  |> Typ.Infer.ModuleInterface.types
  |> Iter.Iterator.for_each
    ~fn:(fun (_name, (declaration: Typ.Ast.type_declaration)) ->
      let matches_owner =
        match owner with
        | None -> true
        | Some owner -> Typ.Model.Surface_path.equal owner declaration.name
      in
      if matches_owner then
        match declaration.definition.kind with
        | Typ.Ast.Record record_fields ->
            List.for_each record_fields ~fn:(fun field -> Vector.push fields ~value:field)
        | _ -> ());
  Vector.iter fields
  |> Iter.Iterator.map ~fn:record_field_completion_item
  |> Iter.Iterator.to_list

let record_completion_items = fun intf expression ->
  let owner = Option.and_then expression.Typ.Ast.type_ ~fn:type_ident_of_type in
  record_fields_for_interface ?owner intf

let field_access_completion_items = fun intf access ->
  match Option.and_then access.Typ.Ast.receiver.type_ ~fn:type_ident_of_type with
  | None -> []
  | Some owner -> record_fields_for_interface ~owner intf

let record_fields_for_receiver_expression = fun intf (receiver: Typ.Ast.expression) ->
  match Option.and_then receiver.type_ ~fn:type_ident_of_type with
  | None -> []
  | Some owner -> record_fields_for_interface ~owner intf

let last = fun values ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | [ value ] -> Some value
    | _ :: rest -> loop rest
  in
  loop values

let find_record_expression_for_completion = fun path ->
  match last path with
  | Some {
      Typ.Query.Node.kind = Typ.Query.Node.Expression ({ kind = Typ.Ast.Record _; _ } as expression);
      _;
    } ->
      Some expression
  | Some { Typ.Query.Node.kind = Typ.Query.Node.RecordExpressionField _; parent = Some parent; _ } -> (
      match Typ.Query.Node.kind parent with
      | Typ.Query.Node.Expression ({ kind = Typ.Ast.Record _; _ } as expression) -> Some expression
      | _ -> None
    )
  | _ -> None

let find_field_access_for_completion = fun offset path ->
  path
  |> List.reverse
  |> List.find
    ~fn:(fun node ->
      match Typ.Query.Node.kind node with
      | Typ.Query.Node.Expression { kind = Typ.Ast.FieldAccess access; _ } ->
          offset >= access.receiver.origin.span.end_
      | _ -> false)
  |> Option.and_then
    ~fn:(fun node ->
      match Typ.Query.Node.kind node with
      | Typ.Query.Node.Expression { kind = Typ.Ast.FieldAccess access; _ } -> Some access
      | _ -> None)

let rec expression_of_query_node = fun node ->
  match Typ.Query.Node.kind node with
  | Typ.Query.Node.Expression expression -> Some expression
  | _ -> (
      match Typ.Query.Node.parent node with
      | None -> None
      | Some parent -> expression_of_query_node parent
    )

let receiver_expression_for_dot_completion = fun offset node ->
  match expression_of_query_node node with
  | None -> None
  | Some { Typ.Ast.kind = Typ.Ast.FieldAccess access; _ } when offset
  >= access.receiver.origin.span.end_ -> Some access.receiver
  | Some expression -> Some expression

let node_at_cursor_or_previous_byte = fun context offset ->
  let cursor = Syn.Span.make ~start:offset ~end_:offset in
  match Typ.Query.node_at context cursor with
  | Some _ as found -> found
  | None ->
      if offset <= 0 then
        None
      else
        let cursor = Syn.Span.make ~start:(offset - 1) ~end_:(offset - 1) in
        Typ.Query.node_at context cursor

let dot_offset_for_completion = fun text offset ->
  let candidates =
    if offset > 1 then
      [ offset; offset - 1; offset - 2 ]
    else if offset > 0 then
      [ offset; offset - 1 ]
    else
      [ offset ]
  in
  List.find
    candidates
    ~fn:(fun candidate ->
      match String.get text ~at:candidate with
      | Some '.' -> true
      | _ -> false)

let completion_after_dot = fun text offset context intf ->
  match dot_offset_for_completion text offset with
  | None -> None
  | Some dot_offset -> (
      match node_at_cursor_or_previous_byte context dot_offset with
      | None -> Some []
      | Some node -> (
          match receiver_expression_for_dot_completion dot_offset node with
          | None -> Some []
          | Some receiver -> Some (record_fields_for_receiver_expression intf receiver)
        )
    )

let completion_path_is_type_position = fun path ->
  List.exists
    (fun node ->
      match Typ.Query.Node.kind node with
      | Typ.Query.Node.CoreType _
      | Typ.Query.Node.PackageType _
      | Typ.Query.Node.PackageTypeConstraint _ -> true
      | _ -> false)
    path

let completion_items_for_query_context = fun text context offset ->
  let cursor = Syn.Span.make ~start:offset ~end_:offset in
  let path = Typ.Query.path_at context cursor in
  let intf = (Typ.Query.infer_result context).intf in
  match completion_after_dot text offset context intf with
  | Some items -> items
  | None -> (
      match find_field_access_for_completion offset path with
      | Some access -> field_access_completion_items intf access
      | None -> (
          match find_record_expression_for_completion path with
          | Some expression -> record_completion_items intf expression
          | None ->
              if completion_path_is_type_position path then
                type_position_completion_items_for_interface intf
              else
                expression_completion_items_for_interface intf
        )
    )

let completion_for_document = fun state ->
  fun document ->
    fun position ->
      match query_position_of_lsp_position document.text position with
      | None -> Some []
      | Some offset -> (
          match typ_query_context_for_document ~allow_parse_diagnostics:true state document with
          | None ->
              Option.map
                document.typ_context
                ~fn:(fun context ->
                  completion_items_for_query_context
                    document.text
                    context.context
                    offset)
          | Some context ->
              Some (completion_items_for_query_context document.text context.context offset)
        )

let line_text_at = fun text requested_line ->
  let rec loop = fun line_number ->
    fun lines ->
      match lines with
      | [] -> None
      | line :: rest ->
          if line_number = requested_line then
            Some line
          else
            loop (line_number + 1) rest
  in
  loop 0 (String.split_on_char '\n' text)

let line_prefix_at = fun text position ->
  match line_text_at text position.Lsp.Position.line with
  | None -> "<line out of range>"
  | Some line ->
      let line_length = String.length line in
      let prefix_length =
        if position.character < line_length then
          position.character
        else
          line_length
      in
      String.sub line ~offset:0 ~len:prefix_length

type inlay_hint_context = {
  source_text: string;
  start_offset: int;
  end_offset: int;
  stable_until: int option;
  stability_span: Syn.Span.t option;
  type_printer: Typ.Ast.Type.Printer.printer;
}

let first_changed_offset = fun before after ->
  let before_length = String.length before in
  let after_length = String.length after in
  let limit = min before_length after_length in
  let rec loop offset =
    if offset >= limit then
      if Int.equal before_length after_length then
        None
      else
        Some limit
    else
      match (String.get before ~at:offset, String.get after ~at:offset) with
      | (Some left, Some right) when Char.equal left right -> loop (offset + 1)
      | _ -> Some offset
  in
  loop 0

let inlay_hint_in_range = fun ctx origin ->
  let hint_offset = origin.Typ.Ast.span.end_ in
  hint_offset >= ctx.start_offset && hint_offset <= ctx.end_offset

let inlay_hint_is_stable = fun ctx origin ->
  match ctx.stable_until with
  | None -> true
  | Some stable_until ->
      let span = Option.unwrap_or ctx.stability_span ~default:origin.Typ.Ast.span in
      span.end_ <= stable_until

let inlay_hint_for_pattern = fun ctx (pattern: Typ.Ast.pattern) ->
  match (pattern.kind, pattern.type_) with
  | (Typ.Ast.Bind _, Some type_) when inlay_hint_in_range ctx pattern.origin
  && inlay_hint_is_stable ctx pattern.origin ->
      Some {
        Lsp.Inlay_hint.position = position_of_offset ctx.source_text pattern.origin.span.end_;
        label = ": " ^ Typ.Ast.Type.Printer.to_string ctx.type_printer type_;
        kind = Some Lsp.Inlay_hint.Kind.Type;
        tooltip = None;
        padding_left = Some false;
        padding_right = Some false;
      }
  | _ -> None

let inlay_hint_for_expression = fun ctx (expression: Typ.Ast.expression) ->
  match (expression.kind, expression.type_) with
  | (Typ.Ast.Record _, Some type_) when inlay_hint_in_range ctx expression.origin
  && inlay_hint_is_stable ctx expression.origin ->
      Some {
        Lsp.Inlay_hint.position = position_of_offset ctx.source_text expression.origin.span.end_;
        label = ": " ^ Typ.Ast.Type.Printer.to_string ctx.type_printer type_;
        kind = Some Lsp.Inlay_hint.Kind.Type;
        tooltip = None;
        padding_left = Some false;
        padding_right = Some false;
      }
  | _ -> None

let rec inlay_hints_pattern = fun ctx (pattern: Typ.Ast.pattern) ->
  let children =
    match pattern.kind with
    | Typ.Ast.Wildcard
    | Typ.Ast.Bind _
    | Typ.Ast.Literal _
    | Typ.Ast.FirstClassModule _ -> []
    | Typ.Ast.Constructor { payload; _ } ->
        Option.unwrap_or (Option.map payload ~fn:(inlay_hints_pattern ctx)) ~default:[]
    | Typ.Ast.PolyVariant { payload; _ } ->
        Option.unwrap_or (Option.map payload ~fn:(inlay_hints_pattern ctx)) ~default:[]
    | Typ.Ast.Tuple patterns
    | Typ.Ast.List patterns ->
        patterns
        |> List.map ~fn:(inlay_hints_pattern ctx)
        |> List.concat
    | Typ.Ast.Record fields ->
        fields
        |> List.filter_map ~fn:(fun (field: Typ.Ast.record_pattern_field) -> field.pattern)
        |> List.map ~fn:(inlay_hints_pattern ctx)
        |> List.concat
    | Typ.Ast.Or { left; right }
    | Typ.Ast.Cons { head = left; tail = right } ->
        inlay_hints_pattern ctx left @ inlay_hints_pattern ctx right
    | Typ.Ast.Constraint { pattern; _ }
    | Typ.Ast.Attribute pattern -> inlay_hints_pattern ctx pattern
    | Typ.Ast.Alias { pattern; alias } ->
        inlay_hints_pattern ctx pattern @ inlay_hints_pattern ctx alias
  in
  match inlay_hint_for_pattern ctx pattern with
  | None -> children
  | Some hint -> hint :: children

and inlay_hints_parameter = fun ctx (parameter: Typ.Ast.parameter) ->
  let pattern_hints = inlay_hints_pattern ctx parameter.pattern in
  let default_hints =
    Option.unwrap_or (Option.map parameter.default ~fn:(inlay_hints_expression ctx)) ~default:[]
  in
  pattern_hints @ default_hints

and inlay_hints_parameters = fun ctx parameters ->
  parameters
  |> List.map ~fn:(inlay_hints_parameter ctx)
  |> List.concat

and inlay_hints_function_body = fun ctx body ->
  match body with
  | Typ.Ast.Body expression -> inlay_hints_expression ctx expression
  | Typ.Ast.Cases cases -> inlay_hints_match_cases ctx cases

and inlay_hints_match_case = fun ctx (case: Typ.Ast.match_case) ->
  inlay_hints_pattern ctx case.pattern
  @ Option.unwrap_or (Option.map case.guard ~fn:(inlay_hints_expression ctx)) ~default:[]
  @ inlay_hints_expression ctx case.body

and inlay_hints_match_cases = fun ctx cases ->
  cases
  |> List.map ~fn:(inlay_hints_match_case ctx)
  |> List.concat

and inlay_hints_argument = fun ctx (argument: Typ.Ast.argument) ->
  match argument.kind with
  | Typ.Ast.Positional expression -> inlay_hints_expression ctx expression
  | Typ.Ast.Labeled { value; _ }
  | Typ.Ast.Optional { value; _ } ->
      Option.unwrap_or (Option.map value ~fn:(inlay_hints_expression ctx)) ~default:[]

and inlay_hints_let_binding = fun ctx (binding: Typ.Ast.let_binding) ->
  let ctx = { ctx with stability_span = Some binding.origin.span } in
  inlay_hints_pattern ctx binding.pattern @ inlay_hints_expression ctx binding.expr

and inlay_hints_expression = fun ctx (expression: Typ.Ast.expression) ->
  let children =
    match expression.kind with
    | Typ.Ast.Literal _
    | Typ.Ast.Ident _
    | Typ.Ast.FirstClassModule _ -> []
    | Typ.Ast.Constructor { payload; _ } ->
        Option.unwrap_or (Option.map payload ~fn:(inlay_hints_expression ctx)) ~default:[]
    | Typ.Ast.Tuple expressions
    | Typ.Ast.List expressions
    | Typ.Ast.Array expressions ->
        expressions
        |> List.map ~fn:(inlay_hints_expression ctx)
        |> List.concat
    | Typ.Ast.PolyVariant { payload; _ } ->
        Option.unwrap_or (Option.map payload ~fn:(inlay_hints_expression ctx)) ~default:[]
    | Typ.Ast.Record { update; fields } ->
        Option.unwrap_or (Option.map update ~fn:(inlay_hints_expression ctx)) ~default:[]
        @ (
          fields
          |> List.map
            ~fn:(fun (field: Typ.Ast.record_expression_field) ->
              inlay_hints_expression
                ctx
                field.value)
          |> List.concat
        )
    | Typ.Ast.FieldAccess { receiver; _ } -> inlay_hints_expression ctx receiver
    | Typ.Ast.Assign { target; value }
    | Typ.Ast.Sequence { left = target; right = value }
    | Typ.Ast.Infix { left = target; right = value; _ } ->
        inlay_hints_expression ctx target @ inlay_hints_expression ctx value
    | Typ.Ast.If { condition; then_branch; else_branch } ->
        inlay_hints_expression ctx condition
        @ inlay_hints_expression ctx then_branch
        @ Option.unwrap_or (Option.map else_branch ~fn:(inlay_hints_expression ctx)) ~default:[]
    | Typ.Ast.Match { scrutinee; cases } ->
        inlay_hints_expression ctx scrutinee @ inlay_hints_match_cases ctx cases
    | Typ.Ast.Try { body; cases } ->
        inlay_hints_expression ctx body @ inlay_hints_match_cases ctx cases
    | Typ.Ast.While { condition; body } ->
        inlay_hints_expression ctx condition @ inlay_hints_expression ctx body
    | Typ.Ast.For {
        pattern;
        start_;
        stop;
        body;
      } ->
        inlay_hints_pattern ctx pattern
        @ inlay_hints_expression ctx start_
        @ inlay_hints_expression ctx stop
        @ inlay_hints_expression ctx body
    | Typ.Ast.Function { parameters; body; _ } ->
        inlay_hints_parameters ctx parameters @ inlay_hints_function_body ctx body
    | Typ.Ast.Apply { callee; arguments } ->
        inlay_hints_expression ctx callee
        @ (
          arguments
          |> List.map ~fn:(inlay_hints_argument ctx)
          |> List.concat
        )
    | Typ.Ast.Let { bindings; body; _ } ->
        (
          bindings
          |> List.map ~fn:(inlay_hints_let_binding ctx)
          |> List.concat
        )
        @ inlay_hints_expression ctx body
    | Typ.Ast.LetModule { body; _ }
    | Typ.Ast.LocalOpen { body; _ }
    | Typ.Ast.Assert body -> inlay_hints_expression ctx body
  in
  match inlay_hint_for_expression ctx expression with
  | None -> children
  | Some hint -> children @ [ hint ]

let rec inlay_hints_structure_item = fun ctx (item: Typ.Ast.structure_item) ->
  match item.kind with
  | Typ.Ast.Let declaration ->
      declaration.bindings
      |> List.map ~fn:(inlay_hints_let_binding ctx)
      |> List.concat
  | Typ.Ast.Expression expression -> inlay_hints_expression ctx expression
  | Typ.Ast.Module modules ->
      modules
      |> List.map
        ~fn:(fun (module_: Typ.Ast.module_declaration) ->
          module_.items
          |> List.map ~fn:(inlay_hints_structure_item ctx)
          |> List.concat)
      |> List.concat
  | Typ.Ast.Type _
  | Typ.Ast.TypeExtension _
  | Typ.Ast.External _
  | Typ.Ast.Exception _
  | Typ.Ast.ModuleType _
  | Typ.Ast.Include _ -> []

let inlay_hints_ast = fun text ?stable_until start_offset end_offset (ast: Typ.Ast.t) ->
  let ctx = {
    source_text = text;
    start_offset;
    end_offset;
    stable_until;
    stability_span = None;
    type_printer = Typ.Ast.Type.Printer.create ();
  }
  in
  match ast with
  | Typ.Ast.Implementation implementation ->
      implementation.items
      |> List.map ~fn:(inlay_hints_structure_item ctx)
      |> List.concat
  | Typ.Ast.Interface _ -> []

let inlay_hints_for_document = fun state ->
  fun document ->
    fun range ->
      match (
        Lsp.Utf16.offset_of_position document.text range.Lsp.Range.start_,
        Lsp.Utf16.offset_of_position document.text range.Lsp.Range.end_
      ) with
      | (Ok start_offset, Ok end_offset) ->
          let hints_for_context ?stable_until context =
            Typ.Query.source_file context.context
            |> inlay_hints_ast document.text ?stable_until start_offset end_offset
          in
          (
            match typ_query_context_for_document state document with
            | Some context -> Some (hints_for_context context)
            | None ->
                Option.map
                  document.typ_context
                  ~fn:(fun context ->
                    let stable_until = first_changed_offset context.text document.text in
                    hints_for_context ?stable_until context)
          )
      | _ -> None

let document_source_for_origin = fun state origin ->
  ignore state;
  ignore origin;
  None

let location_for_definition_site = fun state definition ->
  ignore state;
  ignore definition;
  None

let definition_for_document = fun state ->
  fun document ->
    fun position ->
      ignore state;
      ignore document;
      ignore position;
      None

let document_symbol_for_document = fun document -> document_symbols_for_document document

let clear_diagnostics = fun uri ->
  let params: Lsp.Text_document_methods.Publish_diagnostics.params = {
    uri;
    version = None;
    diagnostics = [];
  }
  in
  Lsp.notification_to_json Lsp.Text_document_methods.Publish_diagnostics.notification params

let splice_text = fun text ->
  fun range ->
    fun replacement ->
      let* start_offset = Lsp.Utf16.offset_of_position text range.Lsp.Range.start_ in
      let* end_offset = Lsp.Utf16.offset_of_position text range.end_ in
      if start_offset > end_offset then
        Error "invalid text edit range"
      else
        let prefix = String.sub text ~offset:0 ~len:start_offset in
        let suffix = String.sub text ~offset:end_offset ~len:(String.length text - end_offset) in
        Ok (prefix ^ replacement ^ suffix)

let apply_change = fun text ->
  fun (change: Lsp.Text_document.content_change_event) ->
    match change.range with
    | None -> Ok change.text
    | Some range -> splice_text text range change.text

let apply_changes = fun text ->
  fun changes ->
    List.fold_left
      changes
      ~init:(Ok text)
      ~fn:(fun acc change ->
        let* current = acc in
        apply_change current change)

let capabilities = {
  Lsp.Initialize.Server_capabilities.position_encoding = Some "utf-16";
  text_document_sync = Some (Lsp.Initialize.Server_capabilities.Sync_options {
    open_close = Some true;
    change = Some Lsp.Text_document.Sync_kind.Full;
    save = None;
  });
  document_formatting_provider = Some true;
  definition_provider = Some true;
  hover_provider = Some true;
  completion_provider = Some {
    resolve_provider = Some false;
    trigger_characters = Some [ "." ];
  };
  inlay_hint_provider = Some true;
  document_symbol_provider = Some true;
  code_action_provider = Some (Lsp.Initialize.Server_capabilities.Provider_options {
    code_action_kinds = Some [ Lsp.Action_kind.Quick_fix; Source_fix_all ];
    resolve_provider = Some false;
  });
  experimental = None;
}

let initialize_result: Lsp.Initialize.result = {
  capabilities;
  server_info = Some { Lsp.Server_info.name = "riot-lsp"; version = None };
}

let debug_json = fun state ->
  let documents =
    state.documents
    |> List.sort
      ~compare:(fun left right ->
        String.compare
          (Lsp.Uri.to_string left.uri)
          (Lsp.Uri.to_string right.uri))
    |> List.map
      ~fn:(fun document ->
        Json.obj
          [ ("uri", Lsp.Uri.to_json document.uri); ("version", Json.int document.version); ])
  in
  Json.obj
    [
      ("initialized", Json.bool state.initialized);
      ("shutdownRequested", Json.bool state.shutdown_requested);
      ("documents", Json.array documents);
    ]

let outcome_to_json = fun outcome ->
  Json.obj
    [
      ("outbound", Json.array outcome.outbound);
      ("exitCode", match outcome.exit_code with
      | None -> Json.Null
      | Some code -> Json.int code);
      ("state", debug_json outcome.state);
    ]

let handle_initialize = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Initialize.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, _params) ->
        if state.initialized then
          ok
            state
            [
              response_error
                ~id
                ~code:Lsp.Error_code.invalid_request
                ~message:"initialize was already called"
                ();
            ]
        else
          let state = { state with initialized = true; shutdown_requested = false } in
          ok state [ Lsp.response_to_json ~id Lsp.Initialize.request initialize_result ]

let handle_shutdown = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Shutdown.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, ()) ->
        let state = { state with shutdown_requested = true } in
        ok state [ Lsp.response_to_json ~id Lsp.Shutdown.request () ]

let handle_formatting = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Formatting.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None ->
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.invalid_params
                  ~message:"formatting requested for a document that is not open"
                  ();
              ]
        | Some document ->
            let parse_result =
              Syn.parse ~filename:(filename_of_uri document.uri) (source_slice document.text)
            in
            if Vector.length parse_result.diagnostics > 0 then
              ok
                state
                [ Lsp.response_to_json ~id Lsp.Text_document_methods.Formatting.request None ]
            else
              match Krasny.format parse_result with
              | Ok formatted ->
                  let result =
                    if String.equal formatted document.text then
                      Some []
                    else
                      Some [
                        {
                          Lsp.Text_edit.range = document_range document.text;
                          new_text = formatted;
                        };
                      ]
                  in
                  ok
                    state
                    [ Lsp.response_to_json ~id Lsp.Text_document_methods.Formatting.request result ]
              | Error error ->
                  ok
                    state
                    [
                      response_error
                        ~id
                        ~code:Lsp.Error_code.internal_error
                        ~message:(Krasny.format_error_to_string error)
                        ();
                    ]
      )

let handle_hover = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Hover.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None ->
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.invalid_params
                  ~message:"hover requested for a document that is not open"
                  ();
              ]
        | Some document ->
            ok
              state
              [
                Lsp.response_to_json
                  ~id
                  Lsp.Text_document_methods.Hover.request
                  (hover_for_document state document params.position);
              ]
      )

let handle_completion = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Completion.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None ->
            ok
              ~debug_events:[
                "completion "
                ^ Lsp.Uri.to_string params.text_document.uri
                ^ " line="
                ^ Int.to_string params.position.line
                ^ " character="
                ^ Int.to_string params.position.character
                ^ " unopened-document";
              ]
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.invalid_params
                  ~message:"completion requested for a document that is not open"
                  ();
              ]
        | Some document ->
            let result = completion_for_document state document params.position in
            let item_count =
              match result with
              | None -> 0
              | Some items -> List.length items
            in
            ok
              ~debug_events:[
                "completion "
                ^ Lsp.Uri.to_string document.uri
                ^ " line="
                ^ Int.to_string params.position.line
                ^ " character="
                ^ Int.to_string params.position.character
                ^ " version="
                ^ Int.to_string document.version
                ^ " prefix=\""
                ^ String.escaped (line_prefix_at document.text params.position)
                ^ "\""
                ^ " items="
                ^ Int.to_string item_count;
              ]
              state
              [ Lsp.response_to_json ~id Lsp.Text_document_methods.Completion.request result; ]
      )

let handle_inlay_hint = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Inlay_hint.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None ->
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.invalid_params
                  ~message:"inlay hints requested for a document that is not open"
                  ();
              ]
        | Some document ->
            ok
              state
              [
                Lsp.response_to_json
                  ~id
                  Lsp.Text_document_methods.Inlay_hint.request
                  (inlay_hints_for_document state document params.range);
              ]
      )

let handle_definition = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Definition.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None ->
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.invalid_params
                  ~message:"definition requested for a document that is not open"
                  ();
              ]
        | Some document ->
            ok
              state
              [
                Lsp.response_to_json
                  ~id
                  Lsp.Text_document_methods.Definition.request
                  (definition_for_document state document params.position);
              ]
      )

let handle_document_symbol = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Document_symbol.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None ->
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.invalid_params
                  ~message:"document symbols requested for a document that is not open"
                  ();
              ]
        | Some document ->
            ok
              state
              [
                Lsp.response_to_json
                  ~id
                  Lsp.Text_document_methods.Document_symbol.request
                  (document_symbol_for_document document);
              ]
      )

let handle_code_action = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Code_action.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None ->
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.invalid_params
                  ~message:"code actions requested for a document that is not open"
                  ();
              ]
        | Some document ->
            let analysis = analyze_document document in
            let fixable = fixable_lint_diagnostics document analysis in
            let actions = [] in
            let actions =
              if action_kind_allowed params.context.only Lsp.Action_kind.Quick_fix then
                actions
                @ (
                  fixable
                  |> List.filter
                    ~fn:(fun entry ->
                      lint_diagnostic_requested
                        params.context
                        params.range
                        entry.lsp_diagnostic)
                  |> List.filter_map ~fn:(quickfix_action_of_entry document)
                )
              else
                actions
            in
            let actions =
              if action_kind_allowed params.context.only Lsp.Action_kind.Source_fix_all then
                match fixable with
                | [] -> actions
                | _ -> (
                    match fix_all_action document fixable with
                    | Some action -> actions @ [ action ]
                    | None -> actions
                  )
              else
                actions
            in
            let result =
              match actions with
              | [] -> None
              | _ -> Some actions
            in
            ok
              state
              [ Lsp.response_to_json ~id Lsp.Text_document_methods.Code_action.request result ]
      )

let handle_request = fun state ->
  fun request ->
    fun payload ->
      if (not state.initialized) && not (String.equal request.Jsonrpc.method_ "initialize") then
        let id = Option.unwrap_or request.Jsonrpc.id ~default:Jsonrpc.Null in
        ok
          state
          [
            response_error
              ~id
              ~code:Lsp.Error_code.server_not_initialized
              ~message:"server not initialized"
              ();
          ]
      else
        match request.Jsonrpc.method_ with
        | "initialize" -> handle_initialize state payload
        | "shutdown" -> handle_shutdown state payload
        | "textDocument/definition" -> handle_definition state payload
        | "textDocument/documentSymbol" -> handle_document_symbol state payload
        | "textDocument/completion" -> handle_completion state payload
        | "textDocument/hover" -> handle_hover state payload
        | "textDocument/inlayHint" -> handle_inlay_hint state payload
        | "textDocument/formatting" -> handle_formatting state payload
        | "textDocument/codeAction" -> handle_code_action state payload
        | method_ ->
            let id = Option.unwrap_or request.Jsonrpc.id ~default:Jsonrpc.Null in
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.method_not_found
                  ~message:("unknown method `" ^ method_ ^ "`")
                  ();
              ]

let handle_did_open = fun state ->
  fun payload ->
    match Lsp.notification_of_json Lsp.Text_document_methods.Did_open.notification payload with
    | Error _reason -> ok state []
    | Ok params -> (
        match find_document state params.text_document.uri with
        | Some existing ->
            let document = {
              uri = params.text_document.uri;
              version = params.text_document.version;
              text = params.text_document.text;
              path = existing.path;
              typ_context = existing.typ_context;
            }
            in
            let document = refresh_document_typ_context state document in
            let state = upsert_document state document in
            ok state [ publish_diagnostics state document ]
        | None ->
            let path =
              match Lsp.Uri.to_path params.text_document.uri with
              | Ok path -> Some path
              | Error _ -> None
            in
            let document = {
              uri = params.text_document.uri;
              version = params.text_document.version;
              text = params.text_document.text;
              path;
              typ_context = None;
            }
            in
            let document = refresh_document_typ_context state document in
            let state = upsert_document state document in
            ok state [ publish_diagnostics state document ]
      )

let handle_did_change = fun state ->
  fun payload ->
    match Lsp.notification_of_json Lsp.Text_document_methods.Did_change.notification payload with
    | Error _reason -> ok state []
    | Ok params -> (
        match find_document state params.text_document.uri with
        | None -> ok state []
        | Some document -> (
            match apply_changes document.text params.content_changes with
            | Error _ -> ok state []
            | Ok text ->
                let document = {
                  uri = document.uri;
                  version = params.text_document.version;
                  text;
                  path = document.path;
                  typ_context = document.typ_context;
                }
                in
                let document = refresh_document_typ_context state document in
                let state = upsert_document state document in
                ok state [ publish_diagnostics state document ]
          )
      )

let handle_did_close = fun state ->
  fun payload ->
    match Lsp.notification_of_json Lsp.Text_document_methods.Did_close.notification payload with
    | Error _reason -> ok state []
    | Ok params -> (
        match find_document state params.text_document.uri with
        | None ->
            let state = remove_document state params.text_document.uri in
            ok state [ clear_diagnostics params.text_document.uri ]
        | Some _document ->
            let state = remove_document state params.text_document.uri in
            ok state [ clear_diagnostics params.text_document.uri ]
      )

let handle_notification = fun state ->
  fun request ->
    fun payload ->
      if not state.initialized then
        match request.Jsonrpc.method_ with
        | "exit" -> ok state ~exit_code:1 []
        | _ -> ok state []
      else
        match request.Jsonrpc.method_ with
        | "initialized" -> ok state []
        | "textDocument/didOpen" -> handle_did_open state payload
        | "textDocument/didChange" -> handle_did_change state payload
        | "textDocument/didClose" -> handle_did_close state payload
        | "exit" ->
            let exit_code =
              if state.shutdown_requested then
                0
              else
                1
            in
            ok state ~exit_code []
        | _ -> ok state []

let handle_payload = fun state ->
  fun payload ->
    match Json.from_string payload with
    | Error error ->
        ok
          state
          [
            response_error
              ~id:Jsonrpc.Null
              ~code:Lsp.Error_code.parse_error
              ~message:(Json.error_to_string error)
              ();
          ]
    | Ok json -> (
        match Jsonrpc.request_of_json json with
        | Error reason ->
            ok
              state
              [
                response_error
                  ~id:Jsonrpc.Null
                  ~code:Lsp.Error_code.invalid_request
                  ~message:reason
                  ();
              ]
        | Ok request -> (
            match request.Jsonrpc.id with
            | Some _ -> handle_request state request json
            | None -> handle_notification state request json
          )
      )
