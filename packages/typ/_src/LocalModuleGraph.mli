open Std
open Model

type visible_module_name =
  | InternalName of LocalModules.InternalName.t
  | AmbientName of LocalModules.AmbientName.t

type 'a input_source = {
  payload: 'a;
  source_id: SourceId.t;
  internal_name: LocalModules.InternalName.t;
  visible_names: visible_module_name list;
  required_names: LocalModules.RequiredName.t list;
}

type group_id = int

type dependency_set_id = int

type 'a graph_source = {
  input: 'a input_source;
  required_names: LocalModules.RequiredName.t array;
  dependency_set_id: dependency_set_id;
  unresolved_local_names: LocalModules.RequiredName.t array;
}

type 'a group = {
  id: group_id;
  internal_name: LocalModules.InternalName.t;
  visible_names: visible_module_name array;
  sources: 'a graph_source list;
  dependency_ids: group_id array;
}

type 'a t = {
  groups: 'a group array;
  candidate_ids_by_required_name: (LocalModules.RequiredName.t, group_id array) Collections.HashMap.t;
  dependency_local_ids_by_set_id: group_id array array;
  group_id_by_source_id: (int, group_id) Collections.HashMap.t;
}

type cycle = { module_ids: group_id list; module_names: string list; source_ids: SourceId.t list }

val visible_module_name_to_string: visible_module_name -> string

val required_names_of_parse_result: current_module_name:LocalModules.InternalName.t -> parse_result:Syn.Parser.parse_result -> implicit_opens:SurfacePath.t list -> LocalModules.RequiredName.t list

val create: ordered_sources:'a input_source list -> 'a t

val dependency_local_ids: 'a t -> dependency_set_id -> group_id array

val best_matching_local_module_ids: 'a t -> 'a group -> required_module_name:LocalModules.RequiredName.t -> group_id array

val ordered_group_ids: 'a t -> (group_id list, cycle) result

val ordered_subset_group_ids: 'a t -> group_ids:group_id list -> (group_id list, cycle) result

val closure_group_ids: 'a t -> roots:SourceId.t list -> group_id list

val ordered_closure_group_ids: 'a t -> roots:SourceId.t list -> (group_id list, cycle) result

val closure_source_ids: 'a t -> roots:SourceId.t list -> SourceId.t list
