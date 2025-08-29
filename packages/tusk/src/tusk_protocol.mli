(** Protocol types for communication with the Tusk server *)

open Std
open Miniriot

(** Target for build operations *)
type target = All | Package of string

module BuildStats : sig
  type t

  val make : unit -> t
  val mark_started : t -> unit
  val mark_completed : t -> unit
  val inc_packages_built : t -> unit
  val inc_packages_failed : t -> unit
  val inc_cache_hits : t -> unit
  val inc_cache_misses : t -> unit
  val set_total_modules : t -> int -> unit
  val get_build_duration : t -> float (* seconds *)
  val get_packages_built : t -> int
  val get_packages_failed : t -> int
  val get_total_modules : t -> int
  val get_cache_hits : t -> int
  val get_cache_misses : t -> int
end

(** Request types that can be sent to the server *)
type request =
  | Build of {
      client_pid : Pid.t;
      target : target;
      session_id : Session_id.t option;
    }
  | Ping of { client_pid : Pid.t }
  | ScanWorkspace of { client_pid : Pid.t; current_dir : Path.t }
  | GetWorkspaceConfig of { client_pid : Pid.t }
  | GetPackageInfo of { client_pid : Pid.t; package_name : string }
  | GetBuildGraph of { client_pid : Pid.t }
  | FormatFile of { client_pid : Pid.t; file_path : Path.t; check_only : bool }
  | FormatCode of {
      client_pid : Pid.t;
      code : string;
      file_path : Path.t option;
    }
  | FormatAll of { client_pid : Pid.t; mode : [ `check | `write ] }
  | NewPackage of {
      client_pid : Pid.t;
      path : Path.t;
      name : string;
      is_library : bool;
    }

(** Response types from the server *)
type response =
  | Pong
  | BuildStarted of { session_id : Session_id.t; started_at : Datetime.t }
  | BuildCompleted of {
      session_id : Session_id.t;
      completed_At : Datetime.t;
      stats : BuildStats.t;
    }
  | CycleDetected of {
      session_id : Session_id.t;
      cycle_nodes : string list;
      detected_at : Datetime.t;
    }
  | WorkspaceConfig of {
      workspace : Workspace.t;
      toolchain : Toolchains.toolchain;
    }
  | PackageInfo of {
      package : Workspace.package;
      sources : Path.t list;
      dependencies : Build_node.t list;
    }
  | BuildGraph of { nodes : Build_node.t list }
  | FormatResult of { formatted_code : string; changed : bool }
  | FormatError of { error : string }
  | FormatAllResult of {
      files_formatted : int;
      files_failed : int;
      errors : (string * string) list;
    }
  | PackageCreated of { path : string; name : string }
  | PackageCreationError of { error : string }

(** Message types for server communication *)
type Message.t += ServerRequest of request | ServerResponse of response
