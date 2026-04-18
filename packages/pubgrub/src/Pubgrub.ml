open Std
module Ranges = Ranges
module Term = Term
module Provider = Provider
module Incompatibility = Incompatibility
module Partial_solution = Partial_solution
module Solver = New_solver

module Trace = Trace
module Report = Report

type version = Version.t

type 'v ranges = 'v Ranges.t

type package = string

let version_of_string = Version.parse

let version_to_string = Version.to_string

let version_compare = fun a b ->
  match Version.compare a b with
  | Lt -> (-1)
  | Eq -> 0
  | Gt -> 1

let make_version = fun ~major ~minor ~patch -> Version.make ~major ~minor ~patch ()

let zero = make_version ~major:0 ~minor:0 ~patch:0

let one = make_version ~major:1 ~minor:0 ~patch:0

let empty = Ranges.empty

let full = Ranges.full

let singleton = Ranges.singleton

let higher_than = Ranges.higher_than

let strictly_higher_than = Ranges.strictly_higher_than

let lower_than = Ranges.lower_than

let strictly_lower_than = Ranges.strictly_lower_than

let between = Ranges.between

let create_offline = Provider.create_offline

let add_package = Provider.add_package

let to_provider = Provider.to_provider

let default_options = Solver.default_options

let solve_with_stats = Solver.solve_with_stats

let solve = Solver.solve

let explain_conflict = Report.explain_conflict
