open Std

module Ranges = Ranges

module Term = Term

module Provider = Provider

module Incompatibility = Incompatibility

module Partial_solution = Partial_solution

module Solver = New_solver

(* SWAPPED TO NEW SOLVER! *)

module Report = Report

type version = Version.t
type 'v ranges = 'v Ranges.t
type package = string
val version_of_string : string -> (version, Version.parse_error) result

val version_to_string : version -> string

val version_compare : version -> version -> int

val make_version : major:int -> minor:int -> patch:int -> version

val zero : version

val one : version

val empty : 'v ranges

val full : 'v ranges

val singleton : 'v -> 'v ranges

val higher_than : 'v -> 'v ranges

val strictly_higher_than : 'v -> 'v ranges

val lower_than : 'v -> 'v ranges

val strictly_lower_than : 'v -> 'v ranges

val between : 'v -> 'v -> 'v ranges

val create_offline : unit -> Provider.offline

val add_package : Provider.offline -> package -> version -> Provider.dependency_list -> unit

val to_provider : Provider.offline -> string Provider.t

val solve : string Provider.t -> package -> version -> (Solver.solve_result, string) result

val explain_conflict : Incompatibility.t -> string
