(** RPC protocol definitions for Tusk server communication

    This module defines the request/response protocol for communication between
    the Tusk server and various clients (CLI, LSP, MCP). *)

(** Request messages that clients can send to the server *)
type request =
  | Ping  (** Health check request *)
  | GetWorkspace  (** Request workspace information *)
  | GetBuildGraph  (** Request build graph structure *)
  | BuildPackage of { package : string; watch : bool }
      (** Build a specific package, optionally with file watching *)
  | BuildAll of { watch : bool }
      (** Build all packages, optionally with file watching *)
  | GetPackageForFile of { file_path : string }
      (** Find which package contains a given file *)
  | GetConfigForFile of { file_path : string }
      (** Get compilation configuration for a file (for LSP) *)
  | GetBuildStatus  (** Request current build status *)
  | Clean  (** Clean build artifacts *)
  | Restart  (** Restart the server *)
  | Shutdown  (** Shutdown the server *)

(** Response messages from the server *)
type response =
  | Pong  (** Health check response *)
  | WorkspaceInfo of { packages : string list; root : string }
      (** Workspace information *)
  | BuildGraphInfo of {
      packages : (string * string list) list;
          (** List of (package_name, dependencies) *)
    }  (** Build graph structure *)
  | BuildStarted of { id : string }  (** Build has started with given ID *)
  | BuildProgress of {
      package : string;
      status : [ `Building | `Success | `Failed of string ];
    }  (** Progress update for a package build *)
  | BuildComplete of { successful : int; failed : int }
      (** Build completed with statistics *)
  | PackageInfo of { name : string; path : string; dependencies : string list }
      (** Information about a specific package *)
  | FileConfig of {
      source_paths : string list;
      build_paths : string list;
      flags : string list;
      stdlib_path : string;
    }  (** Compilation configuration for a file *)
  | Error of { message : string }  (** Error response *)
  | Ok  (** Generic success response *)

(** {1 Serialization} *)

val request_to_string : request -> string
(** Serialize a request to a string. (Temporary implementation - will use proper
    JSON later) *)

val request_of_string : string -> request option
(** Parse a request from a string. Returns [None] if parsing fails. *)

val response_to_string : response -> string
(** Serialize a response to a string. (Temporary implementation - will use
    proper JSON later) *)

val response_of_string : string -> response option
(** Parse a response from a string. Returns [None] if parsing fails. *)
