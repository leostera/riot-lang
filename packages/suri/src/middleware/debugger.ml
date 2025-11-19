open Std

(** Stack frame extracted from backtrace *)
type stack_frame = {
  file : string option;
  line : int option;
  char_range : (int * int) option;
  function_name : string option;
  raw : string;
}

(** Source code snippet with context *)
type source_snippet = {
  start_line : int;
  lines : (int * string) list;  (* line_num, content *)
  error_line : int;
  source_path : string;  (* Clean workspace-relative path *)
  found_via_tusk : bool;  (* Whether tusk server helped resolve *)
}

(** Resolved source path information *)
type resolved_path = {
  resolved_path : string;
  found_via_tusk : bool;
}

(** Sandbox path parsing result *)
type sandbox_info = {
  package_name : string;
  relative_path : string;
}

(** Find substring in string, returns start index *)
let string_index line pattern =
  let pattern_len = String.length pattern in
  let line_len = String.length line in
  let rec search pos =
    if pos + pattern_len > line_len then None
    else if String.sub line pos pattern_len = pattern then Some pos
    else search (pos + 1)
  in
  search 0

(** Parse sandbox path to extract package and relative path
    
    Input: /path/_build/debug/sandbox/suri-abc123/examples/file.ml
    Output: Some { package_name = "suri"; relative_path = "examples/file.ml" }
*)
let parse_sandbox_path path =
  match string_index path "/sandbox/" with
  | None -> None
  | Some idx ->
      let after_sandbox = idx + String.length "/sandbox/" in
      if after_sandbox >= String.length path then None
      else
        let rest = String.sub path after_sandbox (String.length path - after_sandbox) in
        (* Find first slash to separate package-hash from path *)
        (match String.index_opt rest '/' with
         | None -> None  (* No path after package, just package name *)
         | Some slash_pos ->
             let pkg_with_hash = String.sub rest 0 slash_pos in
             (* Remove hash suffix: "suri-abc123" -> "suri" *)
             let package_name =
               match String.rindex_opt pkg_with_hash '-' with
               | None -> pkg_with_hash
               | Some dash_pos -> String.sub pkg_with_hash 0 dash_pos
             in
             let after_slash = slash_pos + 1 in
             let relative_path = String.sub rest after_slash (String.length rest - after_slash) in
             Some { package_name; relative_path })

(** Try to connect to tusk server and get package sources *)
let get_package_sources package_name =
  (* Try to connect to tusk server on default port *)
  match Tusk_client.create ~host:"127.0.0.1" ~port:9001 with
  | Error err -> 
      Log.debug (String.concat "" ["Debugger: Cannot connect to tusk server: "; err]);
      None
  | Ok client ->
      (match Tusk_client.get_package_info client package_name with
       | Error err ->
           Log.debug (String.concat "" ["Debugger: Cannot get package info for "; package_name; ": "; err]);
           Tusk_client.close client;
           None
       | Ok package_detail ->
           let sources = package_detail.Tusk_protocol.WireProtocol.sources in
           Log.debug (String.concat "" ["Debugger: Got "; Int.to_string (List.length sources); " sources for package "; package_name]);
           Tusk_client.close client;
           Some sources)

(** Find actual source file path from sandbox path using tusk server *)
let find_source_via_tusk sandbox_info =
  match get_package_sources sandbox_info.package_name with
  | None -> None
  | Some sources ->
      (* Find source file that ends with the relative path *)
      List.find_opt (fun src_file ->
        String.ends_with ~suffix:sandbox_info.relative_path src_file
      ) sources

(** Resolve sandbox path to actual workspace source file
    
    Strategy:
    1. Parse sandbox path to get package + relative path
    2. Query tusk server for package sources
    3. Match against relative path
    4. Return clean workspace-relative path
*)
let resolve_source_path path =
  match parse_sandbox_path path with
  | None -> 
      (* Not a sandbox path, return as-is *)
      { resolved_path = path; found_via_tusk = false }
  | Some sandbox_info ->
      (* Try to find via tusk server *)
      (match find_source_via_tusk sandbox_info with
       | Some actual_path ->
           { resolved_path = actual_path; found_via_tusk = true }
       | None ->
           (* Fallback: construct expected path *)
           let fallback = String.concat "" [
             "packages/";
             sandbox_info.package_name;
             "/";
             sandbox_info.relative_path
           ] in
           { resolved_path = fallback; found_via_tusk = false })



(** Extract quoted string after a pattern *)
let extract_quoted line pattern =
  match String.index_opt line '"' with
  | None -> None
  | Some start_quote ->
      let after_quote = start_quote + 1 in
      (match String.index_from_opt line after_quote '"' with
       | None -> None
       | Some end_quote ->
           Some (String.sub line after_quote (end_quote - after_quote)))

(** Extract number after a pattern *)
let extract_number line pattern =
  match string_index line pattern with
  | None -> None
  | Some idx ->
      let after = idx + String.length pattern in
      let rec find_digits acc pos =
        if pos >= String.length line then acc
        else
          match line.[pos] with
          | '0'..'9' as c -> find_digits (acc ^ String.make 1 c) (pos + 1)
          | _ -> acc
      in
      let num_str = find_digits "" after in
      if num_str = "" then None
      else
        try Some (Int.of_string num_str)
        with Failure _ -> None

(** Parse a backtrace line into a stack frame
    
    OCaml backtrace format examples:
    - "Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33"
    - "Called from Mymodule.handler in file "handler.ml", line 42, characters 5-20"
    - "Re-raised at file "main.ml", line 100, characters 10-25"
*)
let parse_frame_line line =
  let file = 
    if String.contains line '"' then
      extract_quoted line "file"
    else None
  in
  
  let line_num = extract_number line "line " in
  
  let char_range =
    match extract_number line "characters " with
    | None -> None
    | Some start ->
        (* Find the dash and extract end number *)
        (match string_index line "characters " with
         | None -> None
         | Some idx ->
             let after = idx + String.length "characters " in
             let rest = String.sub line after (String.length line - after) in
             (match String.index_opt rest '-' with
              | None -> None
              | Some dash_pos ->
                  let after_dash = dash_pos + 1 in
                  let rec find_digits acc pos =
                    if pos >= String.length rest then acc
                    else
                      match rest.[pos] with
                      | '0'..'9' as c -> find_digits (acc ^ String.make 1 c) (pos + 1)
                      | _ -> acc
                  in
                  let end_str = find_digits "" after_dash in
                  if end_str = "" then None
                  else
                    try Some (start, Int.of_string end_str)
                    with Failure _ -> None))
  in
  
  let function_name =
    if String.starts_with ~prefix:"Raised at " line then
      let after = 10 in (* length of "Raised at " *)
      let rest = String.sub line after (String.length line - after) in
      (match String.index_opt rest ' ' with
       | Some space_pos -> Some (String.sub rest 0 space_pos |> String.trim)
       | None -> Some (String.trim rest))
    else if String.starts_with ~prefix:"Called from " line then
      let after = 12 in (* length of "Called from " *)
      let rest = String.sub line after (String.length line - after) in
      (match String.index_opt rest ' ' with
       | Some space_pos -> Some (String.sub rest 0 space_pos |> String.trim)
       | None -> Some (String.trim rest))
    else if String.starts_with ~prefix:"Re-raised at " line then
      Some "Re-raised"
    else None
  in
  
  { file; line = line_num; char_range; function_name; raw = line }

(** Parse full backtrace into list of stack frames *)
let parse_backtrace backtrace =
  String.split_on_char '\n' backtrace
  |> List.filter (fun line -> String.trim line != "")
  |> List.map parse_frame_line

(** Try to find and read a source file using tusk server resolution *)
let try_read_file file =
  (* First resolve the path using tusk server if it's a sandbox path *)
  let resolved = resolve_source_path file in
  
  (* Try to read from the resolved path *)
  match Fs.read_to_string (Path.v resolved.resolved_path) with
  | Ok content -> Some (content, resolved)
  | Error _ -> None

(** Read source file and extract lines around error location *)
let extract_source ~file ~line ~context =
  match try_read_file file with
  | None -> None
  | Some (content, resolved) ->
      let all_lines = String.split_on_char '\n' content in
      let total_lines = List.length all_lines in
      
      if line < 1 || line > total_lines then None
      else
        let start_line = max 1 (line - context) in
        let end_line = min total_lines (line + context) in
        
        (* Extract the relevant lines with line numbers *)
        let lines = List.init (end_line - start_line + 1) (fun i ->
          let line_num = start_line + i in
          let line_content = List.nth all_lines (line_num - 1) in
          (line_num, line_content)
        ) in
        
        Some { 
          start_line; 
          lines; 
          error_line = line;
          source_path = resolved.resolved_path;
          found_via_tusk = resolved.found_via_tusk;
        }

(** Render a single line of source code *)
let render_source_line ~error_line (line_num, content) =
  let open Component in
  let is_error = line_num = error_line in
  let line_class = if is_error then "source-line error" else "source-line" in
  
  div ~attrs:[class_ line_class] [
    span ~attrs:[class_ "line-num"] [text (Int.to_string line_num)];
    span ~attrs:[class_ "line-marker"] [text (if is_error then ">" else " ")];
    code ~attrs:[class_ "line-content"] [text content];
  ]

(** Render source code snippet *)
let render_snippet snippet =
  let open Component in
  div ~attrs:[class_ "source-snippet"] (
    List.map (render_source_line ~error_line:snippet.error_line) snippet.lines
  )

(** Extract module name from function name
    
    Examples:
    - "Debugger_test.process_user_request" -> "debugger_test"
    - "Suri__Middleware__Debugger.debugger" -> "debugger"
    - "Std__Global.panic" -> "global"
    - "Kernel__Global0.raise" -> "global0"
*)
let extract_module_from_function func_name =
  (* Split by dot to get module part *)
  match String.index_opt func_name '.' with
  | None -> None
  | Some dot_pos ->
      let module_part = String.sub func_name 0 dot_pos in
      (* Handle both Foo_bar and Foo__Bar__Baz formats *)
      (* For Foo__Bar__Baz, take the last component after splitting by __ *)
      let components = String.split_on_char '_' module_part in
      (* Filter out empty strings from double underscores *)
      let non_empty = List.filter (fun s -> s != "") components in
      (* Take the last component and lowercase it *)
      match List.rev non_empty with
      | [] -> None
      | last :: _ -> Some (String.lowercase_ascii last)

(** Find source file for a module using CodeDB via tusk server *)
let find_source_via_codedb module_name =
  Log.debug (String.concat "" ["Debugger: Querying CodeDB for module '"; module_name; "'"]);
  match Tusk_client.create ~host:"127.0.0.1" ~port:9001 with
  | Error err ->
      Log.debug (String.concat "" ["Debugger: Cannot connect to tusk server: "; err]);
      None
  | Ok client ->
      (* Create a Symbol.reference for the module *)
      (match Codedb.Model.Module_name.from_string module_name with
       | Error _ ->
           Log.debug (String.concat "" ["Debugger: Invalid module name '"; module_name; "'"]);
           Tusk_client.close client;
           None
       | Ok module_name_t ->
      let sym_ref : Codedb.Model.Symbol.reference = Module module_name_t in
      (match Tusk_client.get_symbol client sym_ref with
       | Error err ->
           Log.debug (String.concat "" ["Debugger: CodeDB query failed: "; err]);
           Tusk_client.close client;
           None
       | Ok None ->
           Log.debug (String.concat "" ["Debugger: Module '"; module_name; "' not found in CodeDB"]);
           Tusk_client.close client;
           None
        | Ok (Some symbol) ->
            let source_file = Path.to_string symbol.Codedb.Model.Symbol.file.path in
            let package_name = Codedb.Model.Package_name.to_string symbol.Codedb.Model.Symbol.package.name in
            Log.debug (String.concat "" [
              "Debugger: Found module '"; module_name; "' in CodeDB: ";
              source_file; " (package: "; package_name; ")"
            ]);
            Tusk_client.close client;
            Some source_file))

(** Find source file for a module using tusk server *)
let find_source_for_module package_name module_name =
  Log.debug (String.concat "" ["Debugger: Looking for module '"; module_name; "' in package '"; package_name; "'"]);
  
  (* First try CodeDB - it's fast and precise *)
  match find_source_via_codedb module_name with
  | Some path -> 
      Log.debug (String.concat "" ["Debugger: Found via CodeDB: "; path]);
      Some path
  | None ->
      (* Fallback to old method if CodeDB doesn't have it *)
      Log.debug "Debugger: Falling back to package sources heuristic";
      (match get_package_sources package_name with
       | None -> 
           Log.debug (String.concat "" ["Debugger: No sources for package "; package_name]);
           None
       | Some sources ->
           (* Look for a file ending with module_name.ml or module_name.mli *)
           let ml_name = module_name ^ ".ml" in
           let result = List.find_opt (fun src_file ->
             String.ends_with ~suffix:ml_name src_file
           ) sources in
           (match result with
            | Some path -> Log.debug (String.concat "" ["Debugger: Found source: "; path])
            | None -> Log.debug (String.concat "" ["Debugger: No match for "; ml_name; " in "; Int.to_string (List.length sources); " sources"]));
           result)

(** Render a single stack frame with optional source snippet *)
let render_stack_frame frame =
  let open Component in
  
  (* Strategy: Try to get source using file path first, then fall back to function name *)
  let snippet = match (frame.file, frame.line, frame.function_name) with
    | Some file, Some line, _ ->
        (* Try the file path first *)
        (match extract_source ~file ~line ~context:5 with
         | Some s -> Some s
         | None ->
             (* File path didn't work, try using function name *)
             (match frame.function_name with
              | None -> None
              | Some func_name ->
                  (* Extract module name from function *)
                  (match extract_module_from_function func_name with
                   | None -> None
                   | Some module_name ->
                       (* Try to guess package from file path *)
                       let package_guess = match parse_sandbox_path file with
                         | Some si -> si.package_name
                         | None -> "suri" (* Default guess *)
                       in
                       (* Find source file for this module *)
                       (match find_source_for_module package_guess module_name with
                        | None -> None
                        | Some source_file ->
                            extract_source ~file:source_file ~line ~context:5))))
    | _, Some line, Some func_name ->
        (* No file path, but we have function name and line *)
        (match extract_module_from_function func_name with
         | None -> None
         | Some module_name ->
             (* Try different packages *)
             let packages = ["suri"; "std"; "kernel"; "http"; "blink"] in
             let rec try_packages = function
               | [] -> None
               | pkg :: rest ->
                   (match find_source_for_module pkg module_name with
                    | None -> try_packages rest
                    | Some source_file ->
                        extract_source ~file:source_file ~line ~context:5)
             in
             try_packages packages)
    | _ -> None
  in
  
  (* Determine what file info to display *)
  let file_info, tusk_badge = match snippet with
    | Some s ->
        let path_display = s.source_path in
        let info = match frame.line with
          | Some l -> path_display ^ ":" ^ Int.to_string l
          | None -> path_display
        in
        let badge = if s.found_via_tusk then
          span ~attrs:[class_ "tusk-badge"; attr "title" "Path resolved via tusk server"] [text "✓"]
        else text ""
        in
        (info, badge)
    | None ->
        (* No snippet available - show function name as fallback *)
        let display = match frame.function_name with
          | Some fn -> fn
          | None ->
              (match frame.file with
               | Some f -> f
               | None -> "(unknown)")
        in
        let info = match frame.line with
          | Some l -> display ^ ":" ^ Int.to_string l
          | None -> display
        in
        (info, text "")
  in
  
  div ~attrs:[class_ "stack-frame"] [
    div ~attrs:[class_ "frame-header"] [
      span ~attrs:[class_ "frame-location"] [
        text file_info;
        text " ";
        tusk_badge;
      ];
      (match frame.function_name with
       | Some name -> span ~attrs:[class_ "frame-function"] [text (" in " ^ name)]
       | None -> text "");
    ];
    (match snippet with
     | Some s -> render_snippet s
     | None -> 
         (* No source available *)
         div ~attrs:[class_ "source-unavailable"] [
           text "Source file not available.";
         ]);
  ]

(** Render request inspector *)
let render_request conn =
  let open Component in
  let method_str = Conn.method_ conn |> Net.Http.Method.to_string in
  let path = Conn.path conn in
  let headers = Conn.headers conn in
  let params = Conn.params conn in
  let body_str = Conn.body conn in
  
  div ~attrs:[class_ "request-inspector"] [
    h2 [text "📨 Request"];
    
    div ~attrs:[class_ "request-line"] [
      strong [text (method_str ^ " ")];
      code [text path];
    ];
    
    (if Net.Http.Header.is_empty headers then text "" else
      Fragment [
        h3 [text "Headers"];
        table ~attrs:[class_ "headers-table"] (
          Net.Http.Header.to_list headers
          |> List.map (fun (name, value) ->
              tr [
                td ~attrs:[class_ "header-name"] [code [text name]];
                td ~attrs:[class_ "header-value"] [text value];
              ]
            )
        );
      ]);
    
    (if params = [] then text "" else
      Fragment [
        h3 [text "Parameters"];
        table ~attrs:[class_ "params-table"] (
          List.map (fun (name, value) ->
            tr [
              td ~attrs:[class_ "param-name"] [code [text name]];
              td ~attrs:[class_ "param-value"] [text value];
            ]
          ) params
        );
      ]);
    
    (if body_str = "" then text "" else
      Fragment [
        h3 [text "Body"];
        pre ~attrs:[class_ "request-body"] [text body_str];
      ]);
  ]

(** Render response inspector (shows partial response state) *)
let render_response conn =
  let open Component in
  let resp_headers = Conn.resp_headers conn in
  let response = Conn.to_response conn in
  let status = response.Web_server.Response.status in
  let status_code = Net.Http.Status.to_int status in
  let status_text = Net.Http.Status.to_string status in
  
  div ~attrs:[class_ "response-inspector"] [
    h2 [text "📤 Response (before error)"];
    
    div ~attrs:[class_ "response-status"] [
      strong [text "Status: "];
      code [text (Int.to_string status_code ^ " " ^ status_text)];
    ];
    
    (if resp_headers = [] then text "" else
      Fragment [
        h3 [text "Headers"];
        table ~attrs:[class_ "headers-table"] (
          List.map (fun (name, value) ->
            tr [
              td ~attrs:[class_ "header-name"] [code [text name]];
              td ~attrs:[class_ "header-value"] [text value];
            ]
          ) resp_headers
        );
      ]);
  ]

(** CSS styles for the error page *)
let error_page_styles = {|
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', sans-serif;
  background: #f5f5f5;
  color: #333;
  line-height: 1.6;
  height: 100vh;
  overflow: hidden;
}

.error-container {
  display: flex;
  flex-direction: column;
  height: 100vh;
}

.error-header {
  background: #fff;
  border-bottom: 1px solid #e0e0e0;
  padding: 20px 30px;
  flex-shrink: 0;
}

.error-header h1 {
  font-size: 18px;
  font-weight: 600;
  color: #e53935;
  margin-bottom: 8px;
  display: flex;
  align-items: center;
  justify-content: space-between;
}

.suri-brand {
  color: #1976d2;
  font-size: 24px;
  font-weight: 700;
  letter-spacing: -0.5px;
}

.error-header .exception-type {
  font-size: 14px;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  color: #666;
  margin-top: 4px;
  word-break: break-all;
}

.error-body {
  display: flex;
  flex: 1;
  overflow: hidden;
}

.left-column {
  flex: 3;
  background: #fff;
  overflow-y: auto;
  border-right: 1px solid #e0e0e0;
}

.right-column {
  flex: 2;
  background: #fafafa;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
}

.section {
  padding: 20px 30px;
}

.section h2 {
  color: #333;
  margin-bottom: 16px;
  font-size: 13px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  display: flex;
  align-items: center;
  gap: 8px;
}

.stack-frame {
  background: #f9f9f9;
  border-left: 3px solid #e53935;
  padding: 12px 16px;
  margin-bottom: 1px;
  font-size: 13px;
  cursor: pointer;
  transition: background 0.15s;
}

.stack-frame:hover {
  background: #f0f0f0;
}

.frame-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 8px;
}

.frame-location {
  color: #1976d2;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 12px;
  font-weight: 500;
}

.frame-function {
  color: #757575;
  font-size: 12px;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
}

.source-snippet {
  background: #fff;
  border: 1px solid #e0e0e0;
  border-radius: 3px;
  padding: 8px 0;
  overflow-x: auto;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 12px;
  margin-top: 8px;
}

.source-line {
  display: flex;
  padding: 2px 12px;
  align-items: baseline;
}

.source-line.error {
  background: #ffebee;
  border-left: 3px solid #e53935;
}

.line-num {
  color: #9e9e9e;
  min-width: 45px;
  text-align: right;
  padding-right: 16px;
  user-select: none;
  font-size: 11px;
}

.line-marker {
  color: #e53935;
  font-weight: bold;
  margin-right: 8px;
  user-select: none;
  width: 12px;
}

.line-content {
  flex: 1;
  color: #333;
  white-space: pre;
}

.frame-raw {
  background: #fff;
  border: 1px solid #e0e0e0;
  padding: 8px 12px;
  border-radius: 3px;
  color: #666;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 11px;
  overflow-x: auto;
  margin-top: 8px;
}

.request-inspector,
.response-inspector {
  padding: 0;
  border-bottom: 1px solid #e0e0e0;
}

.request-inspector {
  flex: 0 0 auto;
}

.response-inspector {
  flex: 1 0 auto;
}

.request-inspector h2,
.response-inspector h2 {
  color: #333;
  font-size: 13px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  margin-bottom: 12px;
  padding: 20px 30px 0;
}

.request-inspector h3,
.response-inspector h3 {
  color: #666;
  font-size: 11px;
  margin-top: 16px;
  margin-bottom: 8px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  font-weight: 600;
  padding: 0 30px;
}

.request-line {
  margin-bottom: 16px;
  font-size: 13px;
  padding: 0 30px;
}

.request-line strong {
  color: #e53935;
  font-weight: 600;
}

.request-line code {
  color: #1976d2;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  background: none;
  padding: 0;
}

.response-status {
  margin-bottom: 16px;
  font-size: 13px;
  padding: 0 30px;
}

.response-status code {
  color: #388e3c;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  background: none;
  padding: 0;
}

table {
  width: 100%;
  border-collapse: collapse;
  font-size: 12px;
}

tr {
  border-bottom: 1px solid #f0f0f0;
}

td {
  padding: 8px 30px;
  vertical-align: top;
}

.header-name,
.param-name {
  width: 35%;
  color: #757575;
  font-weight: 500;
}

.header-name code,
.param-name code {
  color: #757575;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 11px;
  background: none;
  padding: 0;
}

.header-value,
.param-value {
  color: #424242;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 11px;
  word-break: break-all;
}

.request-body {
  background: #fff;
  border: 1px solid #e0e0e0;
  padding: 12px;
  border-radius: 3px;
  color: #333;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 11px;
  overflow-x: auto;
  max-height: 300px;
  overflow-y: auto;
  margin: 0 30px 20px;
}

.tusk-badge {
  display: inline-block;
  color: #4caf50;
  font-size: 14px;
  font-weight: bold;
  margin-left: 6px;
  vertical-align: middle;
}

.source-unavailable {
  background: #fff3cd;
  border: 1px solid #ffc107;
  border-left: 3px solid #ffc107;
  padding: 12px 16px;
  border-radius: 3px;
  color: #856404;
  font-size: 12px;
  margin-top: 8px;
  font-style: italic;
}

code {
  background: none;
  padding: 0;
  border-radius: 0;
}

strong {
  font-weight: 600;
}

details {
  margin-top: 20px;
  padding: 16px;
  background: #f5f5f5;
  border: 1px solid #e0e0e0;
  border-radius: 4px;
}

summary {
  cursor: pointer;
  font-weight: 600;
  color: #666;
  font-size: 13px;
  user-select: none;
  margin-bottom: 12px;
}

summary:hover {
  color: #333;
}

details[open] summary {
  margin-bottom: 12px;
}

.raw-backtrace {
  background: #fff;
  border: 1px solid #e0e0e0;
  padding: 12px;
  border-radius: 3px;
  font-family: 'Monaco', 'Menlo', 'Courier New', monospace;
  font-size: 11px;
  color: #333;
  white-space: pre-wrap;
  word-break: break-all;
  overflow-x: auto;
}
|}

(** Main error page component *)
let render_error_page ~conn ~exn ~backtrace =
  let open Component in
  let exception_str = Exception.to_string exn in
  let frames = parse_backtrace backtrace in
  let method_str = Conn.method_ conn |> Net.Http.Method.to_string in
  let path = Conn.path conn in
  
  html [
    head [
      meta ~attrs:[attr "charset" "UTF-8"] ();
      meta ~attrs:[attr "viewport" "width=device-width, initial-scale=1.0"] ();
      title_ [text "500 Internal Server Error"];
      style_ [text error_page_styles];
    ];
    body [
      div ~attrs:[class_ "error-container"] [
        (* Header with exception info and Suri branding *)
        div ~attrs:[class_ "error-header"] [
          h1 [
            span [text (exception_str ^ " at " ^ method_str ^ " " ^ path)];
            span ~attrs:[class_ "suri-brand"] [text "SURI"];
          ];
          div ~attrs:[class_ "exception-type"] [text exception_str];
        ];
        
        (* Two-column layout: 2/3 stack trace, 1/3 request/response *)
        div ~attrs:[class_ "error-body"] [
          (* Left column: Stack trace (2/3) *)
          div ~attrs:[class_ "left-column"] [
            div ~attrs:[class_ "section"] [
              h2 [text "📚 Stack Trace"];
              Fragment (List.map render_stack_frame frames);
              
              (* Raw backtrace in collapsible details *)
              details [
                summary [text "🔍 Show Raw Backtrace"];
                div ~attrs:[class_ "raw-backtrace"] [text backtrace];
              ];
            ];
          ];
          
          (* Right column: Request/Response (1/3) *)
          div ~attrs:[class_ "right-column"] [
            render_request conn;
            render_response conn;
          ];
        ];
      ];
    ];
  ]

(** Debugger middleware - catches exceptions, displays error page, and logs *)
let debugger ~conn ~next =
  try
    next conn
  with exn ->
    (* Capture backtrace immediately *)
    let backtrace = Exception.get_backtrace () in
    let exception_str = Exception.to_string exn in
    let method_str = Conn.method_ conn |> Net.Http.Method.to_string in
    let path = Conn.path conn in
    
    (* Log the error *)
    Log.error (String.concat "" [
      method_str; " "; path; " -> Exception: "; exception_str
    ]);
    
    (* Build error page *)
    let error_page = render_error_page ~conn ~exn ~backtrace in
    
    (* Set 500 error response and return *)
    conn
      |> Conn.with_status InternalServerError
      |> Conn.with_header "Content-Type" "text/html; charset=utf-8"
      |> Conn.with_body (Component.to_html error_page)
      |> Conn.send
