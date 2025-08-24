(** Tusk JSON-RPC Protocol - JSON-RPC 2.0 compliant *)

val method_ping : string
(** Method names *)

val method_get_build_graph : string
val method_get_workspace_config : string
val method_build_package : string
val method_build_all : string
val method_restart : string
val method_shutdown : string
val method_build_event : string

(** IDE-like command methods *)
val method_find_definition : string
val method_find_usages : string
val method_add_dependency : string
val method_create_package : string
val method_explain_build_failure : string
val method_incremental_typecheck : string
val method_suggest_imports : string
val method_refactor_rename : string
val method_test_runner : string
val method_generate_interface : string
val method_module_graph : string
val method_dead_code_analysis : string
val method_type_hover : string
val method_workspace_stats : string

val build_package_params : string -> Jsonrpc.params
(** Helper to create method-specific parameters *)

module TuskProtocol : sig
  type build_node = {
    package_name : string;
    src_dir : string;
    out_dir : string;
    status : string;
    deps : string list;
  }

  type build_graph_response = { nodes : build_node list }

  type workspace_config = {
    workspace_root : string;
    toolchain : string;
    packages : string list;
  }

  type location = { file : string; line : int; column : int }
  
  type symbol_info = {
    name : string;
    kind : string; (* "function" | "type" | "module" | "value" *)
    location : location;
    signature : string option;
  }

  type dependency_info = {
    package : string;
    version : string option;
    source : string; (* "workspace" | "external" *)
  }

  type test_result = {
    name : string;
    package : string;
    passed : bool;
    duration_ms : int;
    output : string option;
  }

  type request =
    | Ping
    | GetBuildGraph
    | GetWorkspaceConfig
    | BuildPackage of string
    | BuildAll
    | Restart
    | Shutdown
    (* IDE-like commands *)
    | FindDefinition of { symbol : string; file : string }
    | FindUsages of { symbol : string; scope : string }
    | AddDependency of { package : string; dependency : string; version : string option }
    | CreatePackage of { name : string; path : string; kind : string }
    | ExplainBuildFailure of { package : string option }
    | IncrementalTypecheck of { file : string option; package : string option }
    | SuggestImports of { file : string; unbound_symbol : string }
    | RefactorRename of { old_name : string; new_name : string; kind : string }
    | TestRunner of { pattern : string option; package : string option; watch : bool }
    | GenerateInterface of { ml_file : string; expose_all : bool }
    | ModuleGraph of { package : string; format : string }
    | DeadCodeAnalysis of { scope : string; include_private : bool }
    | TypeHover of { file : string; line : int; column : int }
    | WorkspaceStats of { include_tests : bool; include_docs : bool }

  type build_stats = {
    duration_ms : int;
    packages_built : int;
    packages_failed : int;
    total_modules : int;
    cache_hits : int;
    cache_misses : int;
  }

  type workspace_stats = {
    total_loc : int;
    total_packages : int;
    total_modules : int;
    test_coverage : float option;
    doc_coverage : float option;
    dependencies : dependency_info list;
  }

  type response =
    | Pong
    | BuildGraph of build_graph_response
    | WorkspaceConfig of workspace_config
    | BuildStarted of { session_id : Session_id.t }
    | BuildEvent of { session_id : Session_id.t; log_event : Log.log_event }
    | BuildComplete of { session_id : Session_id.t; stats : build_stats }
    | BuildFailed of {
        session_id : Session_id.t;
        stats : build_stats;
        error : string;
      }
    | ShutdownAck
    | RestartAck
    (* IDE-like command responses *)
    | DefinitionFound of symbol_info
    | DefinitionNotFound of { symbol : string; reason : string }
    | UsagesFound of { symbol : string; usages : location list }
    | DependencyAdded of { package : string; dependency : string }
    | PackageCreated of { name : string; path : string }
    | BuildFailureExplanation of { 
        errors : Log.build_error list;
        suggestions : string list;
        missing_modules : string list;
      }
    | TypecheckResult of {
        errors : Log.build_error list;
        warnings : Log.build_error list;
      }
    | ImportSuggestions of {
        symbol : string;
        suggestions : (string * string) list; (* module * signature *)
      }
    | RefactorCompleted of {
        files_changed : int;
        changes : (string * int) list; (* file * count *)
      }
    | TestResults of {
        results : test_result list;
        total : int;
        passed : int;
        failed : int;
        duration_ms : int;
      }
    | InterfaceGenerated of { mli_file : string; content : string }
    | ModuleGraphResult of { graph : string }
    | DeadCodeFound of {
        unused : symbol_info list;
        total : int;
      }
    | TypeInfo of {
        type_signature : string;
        documentation : string option;
      }
    | WorkspaceStatsResult of workspace_stats
    | Error of string

  include
    Jsonrpc.ApplicationProtocol
      with type request := request
       and type response := response
end

(** Server module for RPC request handling *)
module Server : sig
  val create :
    Miniriot.Pid.t ->
    (TuskProtocol.request, TuskProtocol.response) Jsonrpc.Server.t
  (** Create a JSON-RPC server that handles tusk requests *)
end

(** Client module for RPC communication *)
module Client : sig
  type t

  (** Streaming build event *)
  type streaming_event =
    | BuildStarted of Session_id.t
    | BuildEvent of Log.log_event
    | BuildFinished of (unit, string) result

  (** Build request type *)
  type build_request = BuildPackage of string | BuildAll

  val create : host:string -> port:int -> (t, string) result

  val build_streaming :
    t ->
    build_request ->
    (streaming_event -> unit) ->
    (streaming_event, string) result

  val ping : t -> (unit, string) result
  val get_build_graph : t -> (TuskProtocol.build_graph_response, string) result
  val get_workspace_config : t -> (TuskProtocol.workspace_config, string) result
  val build_package : t -> string -> (TuskProtocol.response, string) result
  val build_all : t -> (TuskProtocol.response, string) result
  val restart : t -> (unit, string) result
  val shutdown : t -> (unit, string) result
  val close : t -> unit
end
