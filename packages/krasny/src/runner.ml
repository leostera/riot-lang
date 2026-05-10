open Std
open Std.Iter
open Std.Collections

let iter_fold = fun fold value ~fn ->
  fold
    value
    ~init:()
    ~fn:(fun item () ->
      fn item;
      Syn.Ast.Continue ())

type run_mode =
  | Check
  | Verify
  | Format

type file_status =
  | Already_formatted
  | Needs_formatting
  | Would_reformat
  | Unsafe_to_format
  | Formatted
  | Failed

type file_result = {
  file: Path.t;
  status: file_status;
  needs_formatting: bool;
  error: string option;
  diagnostics: Syn.Diagnostic.t list option;
  duration: Time.Duration.t;
}

type summary = {
  total_files: int;
  already_formatted: int;
  needs_formatting: int;
  would_reformat: int;
  unsafe_to_format: int;
  formatted_files: int;
  failed_files: int;
  duration: Time.Duration.t;
}

type run_result = {
  files: file_result list;
  summary: summary;
}

type Message.t +=
  | ScannerDiscovered of {
      scanner_ref: unit Ref.t;
      file: Path.t;
    }
  | ScannerComplete of unit Ref.t
  | DispatchFileChecked of {
      result_ref: file_result Ref.t;
      result: file_result;
    }
  | StreamFileResult of {
      run_ref: unit Ref.t;
      result: file_result;
    }
  | StreamCompleted of unit Ref.t

let is_ocaml_source = fun path ->
  let path = Path.to_string path in
  String.ends_with ~suffix:".ml" path || String.ends_with ~suffix:".mli" path

let should_skip_directory = fun path ->
  let basename = Path.basename path in
  String.starts_with ~prefix:"." basename
  || String.equal basename "_build"
  || String.equal basename "target"

let compare_paths = fun left right -> String.compare (Path.to_string left) (Path.to_string right)

let walk_action = fun ~should_ignore ~seen (entry: Fs.Walker.FileItem.t) on_file ->
  let path = Fs.Walker.FileItem.path entry in
  let path_string = Path.to_string path in
  if HashSet.contains seen ~value:path_string then
    Fs.Walker.Skip_subtree
  else
    (
      let _ = HashSet.insert seen ~value:path_string in
      match Fs.Walker.FileItem.kind entry with
      | Directory ->
          if should_skip_directory path || should_ignore path then
            Fs.Walker.Skip_subtree
          else
            Fs.Walker.Continue
      | File ->
          if is_ocaml_source path && not (should_ignore path) then
            on_file path;
          Fs.Walker.Continue
      | Symlink
      | Other -> Fs.Walker.Continue
    )

let make_walker = fun ~roots ~should_ignore ->
  match Fs.Walker.create ~roots () with
  | Ok walker ->
      Fs.Walker.filter_entry
        walker
        ~f:(fun (entry: Fs.Walker.FileItem.t) ->
          let path = Fs.Walker.FileItem.path entry in
          not (should_skip_directory path || should_ignore path))
  | Error _ -> panic "krasny walker configuration should be valid"

let collect_ocaml_files = fun ?(should_ignore = fun _ -> false) ~roots () ->
  let seen = HashSet.create () in
  let files = ref [] in
  let iter =
    make_walker ~roots ~should_ignore
    |> Fs.Walker.into_iter
  in
  let rec loop iter =
    match Iterator.next iter with
    | (None, _) -> ()
    | (Some (Error _), iter') -> loop iter'
    | (Some (Ok (entry: Fs.Walker.FileItem.t)), iter') ->
        let _ = walk_action ~should_ignore ~seen entry (fun path -> files := path :: !files) in
        loop iter'
  in
  loop iter;
  !files
  |> List.sort ~compare:compare_paths
  |> List.unique ~compare:compare_paths

let syntax_hash = fun (result: Syn.Parser.parse_result) ->
  let module Ast = Syn.Ast in
  let module Kind = Syn.SyntaxKind in
  let buffer = IO.Buffer.create ~size:1_024 in
  let trivia_buffer = IO.Buffer.create ~size:256 in
  let write_kind kind = IO.Buffer.add_string buffer (Kind.to_string kind) in
  let write_trivia_kind kind = IO.Buffer.add_string trivia_buffer (Kind.to_string kind) in
  let normalized_token_text token =
    let kind = Ast.Token.kind token in
    let text = Ast.Token.text token in
    if Kind.is kind Kind.INT then
      Format_text.format_int_literal text
    else if Kind.is kind Kind.FLOAT then
      Format_text.format_float_literal text
    else
      text
  in
  let is_tuple_kind = fun __tmp1 ->
    match __tmp1 with
    | Kind.TUPLE_EXPR
    | Kind.TUPLE_PATTERN
    | Kind.TUPLE_TYPE -> true
    | _ -> false
  in
  let is_redundant_paren_inner_kind = fun __tmp1 ->
    match __tmp1 with
    | Kind.PATH_EXPR
    | Kind.FIELD_ACCESS_EXPR
    | Kind.POLY_VARIANT_EXPR
    | Kind.LITERAL_EXPR
    | Kind.PAREN_EXPR
    | Kind.TUPLE_EXPR
    | Kind.LIST_EXPR
    | Kind.ARRAY_EXPR
    | Kind.ARRAY_INDEX_EXPR
    | Kind.STRING_INDEX_EXPR
    | Kind.RECORD_EXPR
    | Kind.RECORD_UPDATE_EXPR
    | Kind.APPLY_EXPR
    | Kind.INFIX_EXPR
    | Kind.CONSTRUCT_PATTERN
    | Kind.TUPLE_PATTERN -> true
    | _ -> false
  in
  let should_skip_token ~parent_kind token =
    let token_text = Ast.Token.text token in
    match parent_kind with
    | Some Kind.PAREN_EXPR -> String.equal token_text "(" || String.equal token_text ")"
    | Some Kind.LIST_EXPR ->
        String.equal token_text "[" || String.equal token_text "]" || String.equal token_text ";"
    | Some Kind.LIST_PATTERN ->
        String.equal token_text "[" || String.equal token_text "]" || String.equal token_text ";"
    | Some Kind.TUPLE_EXPR -> String.equal token_text "(" || String.equal token_text ")"
    | Some Kind.TUPLE_PATTERN -> String.equal token_text "(" || String.equal token_text ")"
    | Some Kind.ARRAY_EXPR ->
        String.equal token_text "[|" || String.equal token_text "|]" || String.equal token_text ";"
    | Some Kind.ARRAY_PATTERN ->
        String.equal token_text "[|" || String.equal token_text "|]" || String.equal token_text ";"
    | Some Kind.RECORD_EXPR
    | Some Kind.RECORD_UPDATE_EXPR
    | Some Kind.RECORD_PATTERN
    | Some Kind.RECORD_TYPE
    | Some Kind.RECORD_FIELD
    | Some Kind.RECORD_EXPR_FIELD -> String.equal token_text ";"
    | Some Kind.TYPE_DECL
    | Some Kind.VARIANT_TYPE -> String.equal token_text "|"
    | Some Kind.VARIANT_CONSTRUCTOR -> String.equal token_text "|"
    | _ -> false
  in
  let tuple_node_has_paren_token node =
    let found = ref false in
    iter_fold
      Ast.Node.fold_child
      node
      ~fn:(fun child ->
        match child with
        | Syn.SyntaxTree.Token id ->
            let token: Ast.Token.t = { tree = result.tree; id } in
            let token_text = Ast.Token.text token in
            if String.equal token_text "(" || String.equal token_text ")" then
              found := true
        | Syn.SyntaxTree.Node _
        | Syn.SyntaxTree.Missing _ -> ());
    !found
  in
  let redundant_paren_child node =
    let meaningful_child = ref None in
    let meaningful_count = ref 0 in
    iter_fold
      Ast.Node.fold_child
      node
      ~fn:(fun child ->
        match child with
        | Syn.SyntaxTree.Token id ->
            let token: Ast.Token.t = { tree = result.tree; id } in
            let token_text = Ast.Token.text token in
            if not (String.equal token_text "(" || String.equal token_text ")") then (
              meaningful_count := Int.succ !meaningful_count;
              meaningful_child := Some child
            )
        | Syn.SyntaxTree.Node _
        | Syn.SyntaxTree.Missing _ ->
            meaningful_count := Int.succ !meaningful_count;
            meaningful_child := Some child);
    match (!meaningful_count, !meaningful_child) with
    | (1, Some (Syn.SyntaxTree.Node id as child)) ->
        let inner: Ast.Node.t = { tree = result.tree; id } in
        if is_redundant_paren_inner_kind (Ast.Node.kind inner) then
          Some child
        else
          None
    | _ -> None
  in
  let trailing_sequence_child node =
    let meaningful_child = ref None in
    let meaningful_count = ref 0 in
    iter_fold
      Ast.Node.fold_child
      node
      ~fn:(fun child ->
        match child with
        | Syn.SyntaxTree.Token id ->
            let token: Ast.Token.t = { tree = result.tree; id } in
            if not (String.equal (Ast.Token.text token) ";") then (
              meaningful_count := Int.succ !meaningful_count;
              meaningful_child := Some child
            )
        | Syn.SyntaxTree.Node _
        | Syn.SyntaxTree.Missing _ ->
            meaningful_count := Int.succ !meaningful_count;
            meaningful_child := Some child);
    match (!meaningful_count, !meaningful_child) with
    | (1, Some (Syn.SyntaxTree.Node _ as child)) -> Some child
    | _ -> None
  in
  let write_trivia = fun ~kind text ->
    (
      IO.Buffer.add_string trivia_buffer "R(";
      write_trivia_kind kind;
      IO.Buffer.add_string trivia_buffer ":";
      let length = String.length text in
      let rec write_text index at_line_start =
        if Int.(index < length) then
          let char = String.get_unchecked text ~at:index in
          if at_line_start && (Char.equal char ' ' || Char.equal char '\t') then
            write_text (Int.succ index) true
          else (
            IO.Buffer.add_char trivia_buffer char;
            write_text (Int.succ index) (Char.equal char '\n')
          )
      in
      write_text 0 true;
      IO.Buffer.add_string trivia_buffer ")"
    )
  in
  let node_child (node: Ast.Node.t) = Syn.SyntaxTree.Node node.Ast.id in
  let collect_match_cases expr =
    let cases = Vector.with_capacity ~size:(Ast.Expr.match_case_count expr) in
    iter_fold Ast.Expr.fold_match_case expr ~fn:(fun case -> Vector.push cases ~value:case);
    cases
  in
  let single_unlabeled_parameter_pattern expr =
    if not (Int.equal (Ast.Expr.parameter_count expr) 1) then
      None
    else
      Ast.Expr.fold_parameter
        expr
        ~init:None
        ~fn:(fun parameter _ ->
          match Ast.Parameter.view parameter with
          | Ast.Parameter.Param { label = Ast.Parameter.NoLabel; pattern = Some pattern } ->
              Ast.Return (Some pattern)
          | Ast.Parameter.Param _
          | Ast.Parameter.Unknown _ -> Ast.Return None)
  in
  let pattern_is_temp_arg pattern =
    match Ast.Pattern.view pattern with
    | Ast.Pattern.Ident { ident } -> String.equal (Ast.Ident.text ident) "__tmp1"
    | _ -> false
  in
  let expr_is_temp_arg expr =
    match Ast.Expr.view expr with
    | Ast.Expr.Ident { ident } -> String.equal (Ast.Ident.text ident) "__tmp1"
    | _ -> false
  in
  let rec write_node node =
    let node_kind = Ast.Node.kind node in
    IO.Buffer.add_string buffer "N(";
    write_kind node_kind;
    IO.Buffer.add_string buffer "[";
    iter_fold Ast.Node.fold_child node ~fn:(write_child ~parent_kind:(Some node_kind));
    IO.Buffer.add_string buffer "])"
  and write_token_trivia token =
    Ast.Token.fold_leading_trivia_item
      token
      ~init:()
      ~fn:(fun trivia () ->
        (
          match trivia with
          | Ast.Token.Whitespace -> ()
          | Ast.Token.Comment comment ->
              write_trivia ~kind:Kind.COMMENT (Formatter.normalize_comment comment)
          | Ast.Token.Docstring docstring ->
              write_trivia ~kind:Kind.DOCSTRING (Formatter.normalize_docstring docstring)
        );
        Ast.Continue ())
  and write_token token =
    write_token_trivia token;
    IO.Buffer.add_string buffer "T(";
    write_kind (Ast.Token.kind token);
    IO.Buffer.add_string buffer ":";
    IO.Buffer.add_string buffer (normalized_token_text token);
    IO.Buffer.add_string buffer ")"
  and write_redundant_paren node =
    iter_fold
      Ast.Node.fold_child
      node
      ~fn:(fun child ->
        match child with
        | Syn.SyntaxTree.Token id ->
            let token: Ast.Token.t = { tree = result.tree; id } in
            let token_text = Ast.Token.text token in
            if String.equal token_text "(" || String.equal token_text ")" then
              write_token_trivia token
        | Syn.SyntaxTree.Node _ -> write_child ~parent_kind:(Some Kind.PAREN_EXPR) child
        | Syn.SyntaxTree.Missing _ -> ())
  and write_trailing_sequence node =
    iter_fold
      Ast.Node.fold_child
      node
      ~fn:(fun child ->
        match child with
        | Syn.SyntaxTree.Token id ->
            let token: Ast.Token.t = { tree = result.tree; id } in
            if String.equal (Ast.Token.text token) ";" then
              write_token_trivia token
        | Syn.SyntaxTree.Node _ -> write_child ~parent_kind:(Some Kind.SEQUENCE_EXPR) child
        | Syn.SyntaxTree.Missing _ -> ())
  and write_direct_token_trivia node =
    iter_fold
      Ast.Node.fold_child
      node
      ~fn:(fun child ->
        match child with
        | Syn.SyntaxTree.Token id -> write_token_trivia ({ tree = result.tree; id }: Ast.Token.t)
        | Syn.SyntaxTree.Node _
        | Syn.SyntaxTree.Missing _ -> ())
  and write_canonical_function_case case =
    write_direct_token_trivia (Ast.MatchCase.as_node case);
    match Ast.MatchCase.view case with
    | Ast.MatchCase.Case { pattern; guard; body } ->
        IO.Buffer.add_string buffer "CASE(";
        write_element (node_child (Ast.Pattern.as_node pattern));
        IO.Buffer.add_string buffer "G(";
        (
          match guard with
          | Some guard -> write_element (node_child (Ast.Expr.as_node guard))
          | None -> ()
        );
        IO.Buffer.add_string buffer ")";
        write_element (node_child (Ast.Expr.as_node body));
        IO.Buffer.add_string buffer ")"
    | Ast.MatchCase.Unknown node -> write_element (node_child node)
  and write_canonical_function_cases cases =
    IO.Buffer.add_string buffer "N(FUNCTION_EXPR[";
    Vector.for_each cases ~fn:write_canonical_function_case;
    IO.Buffer.add_string buffer "])"
  and write_canonical_single_function pattern body =
    IO.Buffer.add_string buffer "N(FUNCTION_EXPR[";
    IO.Buffer.add_string buffer "CASE(";
    write_element (node_child (Ast.Pattern.as_node pattern));
    IO.Buffer.add_string buffer "G()";
    write_element (node_child (Ast.Expr.as_node body));
    IO.Buffer.add_string buffer ")";
    IO.Buffer.add_string buffer "])"
  and write_canonical_function_expr expr =
    match Ast.Expr.view expr with
    | Ast.Expr.Fun { parameters; return_annotation = None; body = Ast.Expr.Body_cases _ } when Vector.is_empty
      parameters ->
        write_direct_token_trivia (Ast.Expr.as_node expr);
        write_canonical_function_cases (collect_match_cases expr);
        true
    | Ast.Expr.Fun { parameters = _; return_annotation = None; body = Ast.Expr.Body_expr body } -> (
        match single_unlabeled_parameter_pattern expr with
        | Some pattern -> (
            match Ast.Expr.view body with
            | Ast.Expr.Match { scrutinee; first_case = _ } when pattern_is_temp_arg pattern
            && expr_is_temp_arg scrutinee ->
                write_direct_token_trivia (Ast.Expr.as_node expr);
                write_direct_token_trivia (Ast.Expr.as_node body);
                write_canonical_function_cases (collect_match_cases body);
                true
            | _ ->
                write_direct_token_trivia (Ast.Expr.as_node expr);
                write_canonical_single_function pattern body;
                true
          )
        | None -> false
      )
    | _ -> false
  and write_child ~parent_kind = fun __tmp1 ->
    match __tmp1 with
    | Syn.SyntaxTree.Node id ->
        let node: Ast.Node.t = { tree = result.tree; id } in
        let node_kind = Ast.Node.kind node in
        (
          match parent_kind with
          | Some parent_kind when Kind.(parent_kind = node_kind)
          && is_tuple_kind node_kind
          && not (tuple_node_has_paren_token node) ->
              iter_fold Ast.Node.fold_child node ~fn:(write_child ~parent_kind:(Some node_kind))
          | _ -> write_element (Syn.SyntaxTree.Node id)
        )
    | Syn.SyntaxTree.Token id ->
        let token: Ast.Token.t = { tree = result.tree; id } in
        if should_skip_token ~parent_kind token then
          write_token_trivia token
        else
          write_token token
    | Syn.SyntaxTree.Missing missing ->
        IO.Buffer.add_string buffer "M(";
        write_kind missing.kind;
        IO.Buffer.add_string buffer ")"
  and write_element = fun __tmp1 ->
    match __tmp1 with
    | Syn.SyntaxTree.Node id ->
        let node: Ast.Node.t = { tree = result.tree; id } in
        if Kind.(Ast.Node.kind node = FUN_EXPR || Ast.Node.kind node = FUNCTION_EXPR) then
          match Ast.Expr.cast node with
          | Ast.Node expr when write_canonical_function_expr expr -> ()
          | Ast.Node _
          | Ast.Unknown _
          | Ast.Error _ -> write_node node
        else if Kind.(Ast.Node.kind node = PAREN_EXPR || Ast.Node.kind node = PAREN_PATTERN) then
          match redundant_paren_child node with
          | Some _child -> write_redundant_paren node
          | None -> write_node node
        else if Kind.(Ast.Node.kind node = SEQUENCE_EXPR) then
          match trailing_sequence_child node with
          | Some _child -> write_trailing_sequence node
          | None -> write_node node
        else
          write_node node
    | Syn.SyntaxTree.Token id -> write_token ({ tree = result.tree; id }: Ast.Token.t)
    | Syn.SyntaxTree.Missing missing ->
        IO.Buffer.add_string buffer "M(";
        write_kind missing.kind;
        IO.Buffer.add_string buffer ")"
  in
  write_node (Ast.root result.tree);
  IO.Buffer.add_string buffer "|TRIVIA|";
  IO.Buffer.add_string buffer (IO.Buffer.contents trivia_buffer);
  IO.Buffer.contents buffer
  |> Crypto.Sha256.hash_string
  |> Crypto.Digest.hex

let vector_to_list = fun vector ->
  let items = ref [] in
  Vector.for_each vector ~fn:(fun item -> items := item :: !items);
  List.reverse !items

let finalize = fun file start ~status ~needs_formatting ~error ->
  {
    file;
    status;
    needs_formatting;
    error;
    diagnostics = None;
    duration = Time.Instant.elapsed start;
  }

let format_file = fun ~mode file ->
  let start = Time.Instant.now () in
  match Fs.read file with
  | Error _ ->
      finalize
        file
        start
        ~status:Failed
        ~needs_formatting:false
        ~error:(Some ("Failed to read " ^ Path.to_string file))
  | Ok source ->
      let parsed = Format_core.parse_source ~filename:file source in
      match Format_core.stream_format_to_string parsed ~width:100 with
      | Ok formatted ->
          let result =
            if String.equal source formatted then
              finalize file start ~status:Already_formatted ~needs_formatting:false ~error:None
            else
              match mode with
              | Check ->
                  finalize file start ~status:Needs_formatting ~needs_formatting:true ~error:None
              | Verify ->
                  let original_hash = syntax_hash parsed in
                  let reparsed = Format_core.parse_source ~filename:file formatted in
                  let formatted_hash = syntax_hash reparsed in
                  if String.equal original_hash formatted_hash then
                    finalize file start ~status:Would_reformat ~needs_formatting:true ~error:None
                  else
                    finalize
                      file
                      start
                      ~status:Unsafe_to_format
                      ~needs_formatting:true
                      ~error:(Some ("semantic-hash mismatch after formatting (original: "
                      ^ original_hash
                      ^ ", formatted: "
                      ^ formatted_hash
                      ^ ")"))
              | Format -> (
                  match Fs.write formatted file with
                  | Ok () ->
                      finalize file start ~status:Formatted ~needs_formatting:false ~error:None
                  | Error err ->
                      finalize
                        file
                        start
                        ~status:Failed
                        ~needs_formatting:false
                        ~error:(Some (IO.error_message err))
                )
          in
          result
      | Error err ->
          let diagnostics =
            match err with
            | Format_core.Cannot_parse diagnostics -> Some (vector_to_list diagnostics)
            | _ -> None
          in
          {
            (finalize
              file
              start
              ~status:Failed
              ~needs_formatting:false
              ~error:(Some (Format_core.format_error_to_string err))) with
            diagnostics;
          }

let check_file = fun file -> format_file ~mode:Check file

let verify_file = fun file -> format_file ~mode:Verify file

type scanner_state = {
  owner: Pid.t;
  scanner_ref: unit Ref.t;
  should_ignore: Path.t -> bool;
  seen: string HashSet.t;
}

let start_scanner = fun ~owner ~roots ~scanner_ref ~should_ignore ->
  let seen = HashSet.create () in
  let state = {
    owner;
    scanner_ref;
    should_ignore;
    seen;
  }
  in
  spawn
    (fun () ->
      let iter =
        make_walker ~roots ~should_ignore
        |> Fs.Walker.into_iter
      in
      let rec loop iter =
        match Iterator.next iter with
        | (None, _) ->
            send state.owner (ScannerComplete state.scanner_ref);
            Ok ()
        | (Some (Error _), iter') -> loop iter'
        | (Some (Ok (entry: Fs.Walker.FileItem.t)), iter') ->
            let _ =
              walk_action
                ~should_ignore:state.should_ignore
                ~seen:state.seen
                entry
                (fun file ->
                  send
                    state.owner
                    (ScannerDiscovered { scanner_ref = state.scanner_ref; file }))
            in
            loop iter'
      in
      loop iter)

type dispatch_state = {
  owner: Pid.t;
  run_ref: unit Ref.t;
  scanner_ref: unit Ref.t;
  pool: Path.t WorkerPool.DynamicWorkerPool.t;
  result_ref: file_result Ref.t;
  pending_files: Path.t Queue.t;
  idle_workers: Path.t WorkerPool.DynamicWorkerPool.worker Queue.t;
  mutable tasks_in_flight: int;
  mutable discovery_complete: bool;
}

let dispatch_ready_workers = fun state ->
  let rec loop () =
    match (Queue.front state.idle_workers, Queue.front state.pending_files) with
    | (Some _, Some _) ->
        let worker =
          Queue.pop state.idle_workers
          |> Option.expect ~msg:"idle worker should exist"
        in
        let file =
          Queue.pop state.pending_files
          |> Option.expect ~msg:"pending file should exist"
        in
        state.tasks_in_flight <- state.tasks_in_flight + 1;
        WorkerPool.DynamicWorkerPool.send_task state.pool worker file;
        loop ()
    | _ -> ()
  in
  loop ()

let is_dispatch_complete = fun state ->
  state.discovery_complete && state.tasks_in_flight = 0 && Queue.is_empty state.pending_files

let rec dispatch_loop = fun state ->
  if is_dispatch_complete state then (
    send state.owner (StreamCompleted state.run_ref);
    Ok ()
  ) else
    let selector:
      ([
        | `WorkerReady of Path.t WorkerPool.DynamicWorkerPool.worker
        | `ScannerDiscovered of Path.t
        | `ScannerComplete
        | `FileChecked of file_result
      ]) selector = fun __tmp1 ->
      match __tmp1 with
      | WorkerPool.DynamicWorkerPool.WorkerReady worker -> (
          match Ref.type_equal
            state.pool.task_ref
            (WorkerPool.DynamicWorkerPool.get_worker_task_ref worker) with
          | Some Type.Equal -> Select (`WorkerReady worker)
          | None -> Skip
        )
      | ScannerDiscovered { scanner_ref; file } when Ref.equal state.scanner_ref scanner_ref ->
          Select (`ScannerDiscovered file)
      | ScannerComplete scanner_ref when Ref.equal state.scanner_ref scanner_ref ->
          Select `ScannerComplete
      | DispatchFileChecked { result_ref; result } when Ref.equal state.result_ref result_ref ->
          Select (`FileChecked result)
      | _ -> Skip
    in
    match receive ~selector () with
    | `WorkerReady worker ->
        Queue.push state.idle_workers ~value:worker;
        dispatch_ready_workers state;
        dispatch_loop state
    | `ScannerDiscovered file ->
        Queue.push state.pending_files ~value:file;
        dispatch_ready_workers state;
        dispatch_loop state
    | `ScannerComplete ->
        state.discovery_complete <- true;
        dispatch_loop state
    | `FileChecked result ->
        state.tasks_in_flight <- max 0 (state.tasks_in_flight - 1);
        send state.owner (StreamFileResult { run_ref = state.run_ref; result });
        dispatch_ready_workers state;
        dispatch_loop state

let start_dispatcher = fun ~owner ~run_ref ~concurrency ~roots ~should_ignore ~check_fn ->
  let dispatcher_owner = self () in
  let scanner_ref = Ref.make () in
  let result_ref = Ref.make () in
  let worker_fn ~owner ~task =
    let result = check_fn task in
    send owner (DispatchFileChecked { result_ref; result })
  in
  let _scanner = start_scanner ~owner:dispatcher_owner ~roots ~scanner_ref ~should_ignore in
  let pool =
    WorkerPool.DynamicWorkerPool.start ~concurrency ~owner:dispatcher_owner ~worker_fn ()
  in
  let state = {
    owner;
    run_ref;
    scanner_ref;
    pool;
    result_ref;
    pending_files = Queue.create ();
    idle_workers = Queue.create ();
    tasks_in_flight = 0;
    discovery_complete = false;
  }
  in
  dispatch_loop state

let summarize = fun ~duration files ->
  List.fold_left
    files
    ~init:{
      total_files = 0;
      already_formatted = 0;
      needs_formatting = 0;
      would_reformat = 0;
      unsafe_to_format = 0;
      formatted_files = 0;
      failed_files = 0;
      duration;
    }
    ~fn:(fun acc result ->
      match result.status with
      | Failed ->
          { acc with total_files = acc.total_files + 1; failed_files = acc.failed_files + 1 }
      | Needs_formatting ->
          {
            acc with
            total_files = acc.total_files + 1;
            needs_formatting = acc.needs_formatting + 1;
          }
      | Would_reformat ->
          {
            acc with
            total_files = acc.total_files + 1;
            would_reformat = acc.would_reformat + 1;
          }
      | Unsafe_to_format ->
          {
            acc with
            total_files = acc.total_files + 1;
            unsafe_to_format = acc.unsafe_to_format + 1;
          }
      | Formatted ->
          {
            acc with
            total_files = acc.total_files + 1;
            formatted_files = acc.formatted_files + 1;
          }
      | Already_formatted ->
          {
            acc with
            total_files = acc.total_files + 1;
            already_formatted = acc.already_formatted + 1;
          })

let run_streaming = fun
  ~mode
  ?(concurrency = Thread.available_parallelism)
  ?(should_ignore = fun _ -> false)
  ~roots
  ~on_result
  () ->
  let concurrency = max 1 concurrency in
  let run_ref = Ref.make () in
  let owner = self () in
  let start = Time.Instant.now () in
  let check_fn =
    match mode with
    | Check -> check_file
    | Verify -> verify_file
    | Format -> format_file ~mode:Format
  in
  let _dispatcher =
    spawn (fun () -> start_dispatcher ~owner ~run_ref ~concurrency ~roots ~should_ignore ~check_fn)
  in
  let rec collect results_rev =
    let selector: ([`FileResult of file_result | `Completed]) selector = fun __tmp1 ->
      match __tmp1 with
      | StreamFileResult { run_ref = msg_ref; result } when Ref.equal run_ref msg_ref ->
          Select (`FileResult result)
      | StreamCompleted msg_ref when Ref.equal run_ref msg_ref -> Select `Completed
      | _ -> Skip
    in
    match receive ~selector () with
    | `FileResult result ->
        on_result result;
        collect (result :: results_rev)
    | `Completed ->
        let files = List.reverse results_rev in
        let duration = Time.Instant.elapsed start in
        { files; summary = summarize ~duration files }
  in
  collect []

let run_checks_streaming = fun ?concurrency ?should_ignore ~roots ~on_result () ->
  run_streaming
    ~mode:Check
    ?concurrency
    ?should_ignore
    ~roots
    ~on_result
    ()

let run_verify_streaming = fun ?concurrency ?should_ignore ~roots ~on_result () ->
  run_streaming
    ~mode:Verify
    ?concurrency
    ?should_ignore
    ~roots
    ~on_result
    ()

let run_format_streaming = fun ?concurrency ?should_ignore ~roots ~on_result () ->
  run_streaming
    ~mode:Format
    ?concurrency
    ?should_ignore
    ~roots
    ~on_result
    ()

let run_batch = fun
  ~mode ?(concurrency = Thread.available_parallelism) ?(should_ignore = fun _ -> false) files ->
  let concurrency = max 1 concurrency in
  let start = Time.Instant.now () in
  let check_fn =
    match mode with
    | Check -> check_file
    | Verify -> verify_file
    | Format -> format_file ~mode:Format
  in
  let files =
    files
    |> List.filter ~fn:(fun path -> not (should_ignore path))
    |> List.sort ~compare:compare_paths
  in
  let results =
    WorkerPool.SimpleWorkerPool.run ~concurrency ~tasks:files ~fn:check_fn ()
    |> List.map ~fn:(fun (_, result) -> result)
    |> List.sort ~compare:(fun left right -> compare_paths left.file right.file)
  in
  let duration = Time.Instant.elapsed start in
  { files = results; summary = summarize ~duration results }

let run_checks = fun ?concurrency ?should_ignore files ->
  run_batch
    ~mode:Check
    ?concurrency
    ?should_ignore
    files

let run_verify = fun ?concurrency ?should_ignore files ->
  run_batch
    ~mode:Verify
    ?concurrency
    ?should_ignore
    files

let run_format = fun ?concurrency ?should_ignore files ->
  run_batch
    ~mode:Format
    ?concurrency
    ?should_ignore
    files
