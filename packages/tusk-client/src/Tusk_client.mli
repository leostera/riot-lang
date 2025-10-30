open Std
open Tusk_model
open Tusk_protocol

type t

type error =
  | JsonrpcError of Jsonrpc.error
  | PackageNotFound of {
      package_name : string;
      available_packages : string list;
    }
  | UnexpectedEvent of { event : WireProtocol.response; reason : string }

val jsonrpc_error_to_string : Jsonrpc.error -> string

type streaming_event =
  | BuildStarted of Session_id.t
  | BuildEvent of Telemetry.event
  | BuildCompleted of {
      session_id : Session_id.t;
      completed_at : Datetime.t;
      stats : WireProtocol.build_stats;
      results : WireProtocol.build_result list;
    }
  | BuildFailed of {
      session_id : Session_id.t;
      failed_at : Datetime.t;
      stats : WireProtocol.build_stats;
      built : WireProtocol.build_result list;
      errors : WireProtocol.build_result list;
    }

(** Build request type *)
type build_target = BuildPackage of string | BuildAll

val create : host:string -> port:int -> (t, string) result

val build_streaming :
  t ->
  build_target ->
  (streaming_event -> unit) ->
  (streaming_event, error) result

val ping : t -> (unit, string) result
val get_package_graph : t -> (WireProtocol.package_graph_response, string) result
val get_workspace_config : t -> (WireProtocol.workspace_config, string) result

val get_package_info :
  t -> string -> (WireProtocol.package_detail, string) result

val find_executable : t -> string -> ((string * string) option, string) result

val find_artifact :
  t -> package:string -> kind:string -> name:string -> (string, string) result

val restart : t -> (unit, string) result
val shutdown : t -> (unit, string) result

val format_file :
  t -> file_path:string -> check_only:bool -> (string * bool, string) result

val format_code :
  t -> code:string -> file_path:string option -> (string * bool, string) result

val format_all :
  t ->
  mode:[ `check | `write ] ->
  (int * int * (string * string) list, string) result

val new_package :
  t ->
  path:string ->
  name:string ->
  is_library:bool ->
  (string * string, string) result

val create_package :
  t ->
  name:string ->
  deps:string list ->
  is_library:bool ->
  (string * string list, string) result

val create_module :
  t ->
  package:string ->
  module_name:string ->
  contents:string ->
  (string list, string) result

val close : t -> unit
