open Std

module AmbientName: sig
  type t
  val of_string: string -> t

  val to_string: t -> string
end

module InternalName: sig
  type t
  val of_string: string -> t

  val to_string: t -> string
end

module RequiredName: sig
  type t
  val of_string: string -> t

  val of_ambient_name: AmbientName.t -> t

  val of_internal_name: InternalName.t -> t

  val to_string: t -> string
end

val split_internal_module_name: string -> string list

val should_include_implicit_open: current_module_name:InternalName.t -> module_name:string -> bool

val local_module_aliases_of_internal_name: InternalName.t -> AmbientName.t list

val ambient_name_of_internal_name: InternalName.t -> AmbientName.t

val matches_required_name: required_name:RequiredName.t -> InternalName.t -> bool

val contextual_match_depth:
  current_module_name:InternalName.t ->
  required_module_name:RequiredName.t ->
  candidate_module_name:InternalName.t ->
  int option

val ambient_names_of_internal_name: InternalName.t -> AmbientName.t list
