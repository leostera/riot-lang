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
      incompatibility: Incompatibility.t
    }
  | ConflictResolvedSame of {
      package: package;
      incompatibility: Incompatibility.t;
      cause: Incompatibility.t;
      prior: Incompatibility.t
    }
  | LearnedIncompatibility of { package: package; incompatibility: Incompatibility.t }
  | Solved of { solution: (package * version) list }

type t = {
  mutable events: event list;
}

let create = fun () -> { events = [] }

let record = fun t event -> t.events <- event :: t.events

let events = fun t -> List.reverse t.events

let json_string = Data.Json.string

let json_int = Data.Json.int

let json_bool = Data.Json.bool

let json_obj = Data.Json.obj

let json_array = Data.Json.array

let version_to_json = fun version -> json_string (Version.to_string version)

let bound_to_json = function
  | Ranges.Unbounded -> json_obj [ ("kind", json_string "unbounded"); ]
  | Ranges.Included version -> json_obj
    [ ("kind", json_string "included"); ("version", version_to_json version); ]
  | Ranges.Excluded version -> json_obj
    [ ("kind", json_string "excluded"); ("version", version_to_json version); ]

let ranges_to_json = fun ranges ->
  Ranges.segments ranges
  |> List.map
    ~fn:(fun (start, finish) ->
      json_obj [ ("start", bound_to_json start); ("end", bound_to_json finish); ])
  |> json_array

let term_to_json = fun term ->
  json_obj
    [
      ("package", json_string (Term.package term));
      ("positive", json_bool (Term.is_positive term));
      ("ranges", ranges_to_json (Term.ranges term));
    ]

let incompatibility_to_json = fun incompat ->
  json_obj
    [ ("terms", incompat |> Incompatibility.terms |> List.map ~fn:term_to_json |> json_array); ]

let solution_to_json = fun solution ->
  solution
  |> List.map
    ~fn:(fun (package, version) ->
      json_obj [ ("package", json_string package); ("version", version_to_json version); ])
  |> json_array

let event_to_json = function
  | Iteration { iteration; next_package } -> json_obj
    [
      ("type", json_string "iteration");
      ("iteration", json_int iteration);
      ("next_package", json_string next_package);
    ]
  | PickedPackage { package; ranges } -> json_obj
    [
      ("type", json_string "picked_package");
      ("package", json_string package);
      ("ranges", ranges_to_json ranges);
    ]
  | ChoseVersion { package; version } -> json_obj
    [
      ("type", json_string "chose_version");
      ("package", json_string package);
      ("version", version_to_json version);
    ]
  | NoVersionAvailable { package; ranges } -> json_obj
    [
      ("type", json_string "no_version_available");
      ("package", json_string package);
      ("ranges", ranges_to_json ranges);
    ]
  | DerivedConstraint { package; incompatibility; changed } -> json_obj
    [
      ("type", json_string "derived_constraint");
      ("package", json_string package);
      ("changed", json_bool changed);
      ("incompatibility", incompatibility_to_json incompatibility);
    ]
  | ConflictResolvedDifferent { package; previous_level; incompatibility } -> json_obj
    [
      ("type", json_string "conflict_resolved_different_levels");
      ("package", json_string package);
      ("previous_level", json_int previous_level);
      ("incompatibility", incompatibility_to_json incompatibility);
    ]
  | ConflictResolvedSame { package; incompatibility; cause; prior } -> json_obj
    [
      ("type", json_string "conflict_resolved_same_level");
      ("package", json_string package);
      ("incompatibility", incompatibility_to_json incompatibility);
      ("cause", incompatibility_to_json cause);
      ("prior", incompatibility_to_json prior);
    ]
  | LearnedIncompatibility { package; incompatibility } -> json_obj
    [
      ("type", json_string "learned_incompatibility");
      ("package", json_string package);
      ("incompatibility", incompatibility_to_json incompatibility);
    ]
  | Solved { solution } -> json_obj
    [ ("type", json_string "solved"); ("solution", solution_to_json solution); ]

let to_json = fun t -> json_obj [ ("events", events t |> List.map ~fn:event_to_json |> json_array); ]
