open Std

type package = string
type version = Version.t

type assignment =
  | Decision of package * version
  | Derivation of package * version Ranges.t * Incompatibility.t

type t = {
  assignments : assignment list;
  decisions : (package, version) Collections.HashMap.t;
}

let empty () = { assignments = []; decisions = Collections.HashMap.create () }

let add_decision solution pkg ver =
  ignore (Collections.HashMap.insert solution.decisions pkg ver);
  { solution with assignments = Decision (pkg, ver) :: solution.assignments }

let add_derivation solution pkg ranges incompat =
  {
    solution with
    assignments = Derivation (pkg, ranges, incompat) :: solution.assignments;
  }

let get_decision solution pkg = Collections.HashMap.get solution.decisions pkg
let extract_solution solution = Collections.HashMap.to_list solution.decisions
