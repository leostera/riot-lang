open Std

(** `raml-native` owns the meaning of native artifacts.

    The caller provides one raw [Contentstore.t] through [Raml_core.Config].
    This module builds native-specific semantics on top of it: where assembly
    artifacts live, how they are keyed, and what metadata comes back when one
    is loaded. The outer API stays typed and native-shaped while the
    serialization details stay local here. *)
type t
type error =
  | Save_failed of { namespace: string; key: string; message: string }
  | Decode_failed of { namespace: string; key: string; message: string }
module Assembly_artifact: sig
  type t = {
    id: string;
    unit_name: string;
    target: string;
    assembly: string;
    payload: Std.Data.Json.t;
  }
end

module Link_plan_artifact: sig
  type t = {
    id: string;
    artifact: string;
    command: string;
    payload: Std.Data.Json.t;
  }
end

val create: Contentstore.t -> target:Raml_core.Target.t -> unit -> t

val from_config: Raml_core.Config.t -> t option

val target: t -> Raml_core.Target.t

val error_to_json: error -> Std.Data.Json.t

val save_assembly: t -> unit_name:string -> assembly:string -> (Assembly_artifact.t, error) result

val load_assembly: t -> id:string -> Assembly_artifact.t option

val find_assembly_by_unit_name: t -> unit_name:string -> Assembly_artifact.t option

val save_link_plan:
  t -> artifact:Linker.artifact -> command:string -> (Link_plan_artifact.t, error) result

val load_link_plan: t -> id:string -> Link_plan_artifact.t option
