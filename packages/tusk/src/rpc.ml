(** RPC protocol definitions for tusk server communication *)

open Miniriot

(** Request messages that clients can send to the server *)
type request =
  | Ping
  | GetWorkspace
  | GetBuildGraph
  | BuildPackage of { package : string; watch : bool }
  | BuildAll of { watch : bool }
  | GetPackageForFile of { file_path : string }
  | GetConfigForFile of { file_path : string }
  | GetBuildStatus
  | Clean
  | Restart
  | Shutdown

(** Response messages from the server *)
type response =
  | Pong
  | WorkspaceInfo of { packages : string list; root : string }
  | BuildGraphInfo of {
      packages : (string * string list) list; (* package name * dependencies *)
    }
  | BuildStarted of { id : string }
  | BuildProgress of {
      package : string;
      status : [ `Building | `Success | `Failed of string ];
    }
  | BuildComplete of { successful : int; failed : int }
  | PackageInfo of { name : string; path : string; dependencies : string list }
  | FileConfig of {
      source_paths : string list;
      build_paths : string list;
      flags : string list;
      stdlib_path : string;
    }
  | Error of { message : string }
  | Ok

(** Serialize a request to JSON-like string (temporary - will use proper JSON
    later) *)
let request_to_string = function
  | Ping -> "Ping"
  | GetWorkspace -> "GetWorkspace"
  | GetBuildGraph -> "GetBuildGraph"
  | BuildPackage { package; watch } ->
      Printf.sprintf "BuildPackage:%s:%b" package watch
  | BuildAll { watch } -> Printf.sprintf "BuildAll:%b" watch
  | GetPackageForFile { file_path } ->
      Printf.sprintf "GetPackageForFile:%s" file_path
  | GetConfigForFile { file_path } ->
      Printf.sprintf "GetConfigForFile:%s" file_path
  | GetBuildStatus -> "GetBuildStatus"
  | Clean -> "Clean"
  | Restart -> "Restart"
  | Shutdown -> "Shutdown"

(** Parse a request from string (temporary - will use proper JSON later) *)
let request_of_string s =
  let parts = String.split_on_char ':' s in
  match parts with
  | [ "Ping" ] -> Some Ping
  | [ "GetWorkspace" ] -> Some GetWorkspace
  | [ "GetBuildGraph" ] -> Some GetBuildGraph
  | [ "BuildPackage"; package; watch ] ->
      Some (BuildPackage { package; watch = bool_of_string watch })
  | [ "BuildAll"; watch ] -> Some (BuildAll { watch = bool_of_string watch })
  | [ "GetPackageForFile"; file_path ] -> Some (GetPackageForFile { file_path })
  | [ "GetConfigForFile"; file_path ] -> Some (GetConfigForFile { file_path })
  | [ "GetBuildStatus" ] -> Some GetBuildStatus
  | [ "Clean" ] -> Some Clean
  | [ "Restart" ] -> Some Restart
  | [ "Shutdown" ] -> Some Shutdown
  | _ -> None

(** Serialize a response to JSON-like string (temporary - will use proper JSON
    later) *)
let response_to_string = function
  | Pong -> "Pong"
  | WorkspaceInfo { packages; root } ->
      Printf.sprintf "WorkspaceInfo:%s:%s" (String.concat "," packages) root
  | BuildGraphInfo { packages } ->
      let pkg_strings =
        List.map
          (fun (name, deps) ->
            Printf.sprintf "%s[%s]" name (String.concat "," deps))
          packages
      in
      Printf.sprintf "BuildGraphInfo:%s" (String.concat ";" pkg_strings)
  | BuildStarted { id } -> Printf.sprintf "BuildStarted:%s" id
  | BuildProgress { package; status } ->
      let status_str =
        match status with
        | `Building -> "Building"
        | `Success -> "Success"
        | `Failed msg -> Printf.sprintf "Failed:%s" msg
      in
      Printf.sprintf "BuildProgress:%s:%s" package status_str
  | BuildComplete { successful; failed } ->
      Printf.sprintf "BuildComplete:%d:%d" successful failed
  | PackageInfo { name; path; dependencies } ->
      Printf.sprintf "PackageInfo:%s:%s:%s" name path
        (String.concat "," dependencies)
  | FileConfig { source_paths; build_paths; flags; stdlib_path } ->
      Printf.sprintf "FileConfig:%s:%s:%s:%s"
        (String.concat "," source_paths)
        (String.concat "," build_paths)
        (String.concat "," flags) stdlib_path
  | Error { message } -> Printf.sprintf "Error:%s" message
  | Ok -> "Ok"

(** Parse a response from string (temporary - will use proper JSON later) *)
let response_of_string s =
  let parts = String.split_on_char ':' s in
  match parts with
  | [ "Pong" ] -> Some Pong
  | [ "Ok" ] -> Some Ok
  | "WorkspaceInfo" :: packages_str :: root :: _ ->
      let packages = 
        if packages_str = "" then []
        else String.split_on_char ',' packages_str
      in
      Some (WorkspaceInfo { packages; root = String.concat ":" (root :: List.tl (List.tl parts)) })
  | "BuildGraphInfo" :: rest ->
      let graph_str = String.concat ":" rest in
      let pkg_strings = String.split_on_char ';' graph_str in
      let packages = 
        List.map (fun pkg_str ->
          match String.split_on_char '[' pkg_str with
          | [name; deps_with_bracket] ->
              let deps_str = String.sub deps_with_bracket 0 (String.length deps_with_bracket - 1) in
              let deps = 
                if deps_str = "" then []
                else String.split_on_char ',' deps_str
              in
              (name, deps)
          | _ -> (pkg_str, [])
        ) pkg_strings
      in
      Some (BuildGraphInfo { packages })
  | "BuildComplete" :: successful :: failed :: _ ->
      Some (BuildComplete { 
        successful = int_of_string successful; 
        failed = int_of_string failed 
      })
  | "Error" :: message -> Some (Error { message = String.concat ":" message })
  | _ -> None (* Unknown response format *)
