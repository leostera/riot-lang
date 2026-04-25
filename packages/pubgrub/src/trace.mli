open Std

type package = string

type version = Version.t

type event =
  | Iteration of { iteration: int; next_package: package }
  | PickedPackage of { package: package; ranges: version Ranges.t }
  | ChoseVersion of { package: package; version: version }
  | NoVersionAvailable of { package: package; ranges: version Ranges.t }
  | DerivedConstraint of { package: package; incompatibility: Incompatibility.t; changed: bool }
  | ConflictResolvedDifferent of {
    package: package;
    previous_level: int;
    incompatibility: Incompatibility.t;
  }
  | ConflictResolvedSame of {
    package: package;
    incompatibility: Incompatibility.t;
    cause: Incompatibility.t;
    prior: Incompatibility.t;
  }
  | LearnedIncompatibility of { package: package; incompatibility: Incompatibility.t }
  | Solved of { solution: (package * version) list }

type t

val create: unit -> t

val record: t -> event -> unit

val events: t -> event list

val event_to_json: event -> Data.Json.t

val to_json: t -> Data.Json.t
