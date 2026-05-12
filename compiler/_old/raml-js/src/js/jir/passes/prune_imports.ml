open Std
module Jir = Types
module Analysis = Analysis
module Entity_set = Analysis.Entity_set

let program = fun ~context:_ (program: Jir.Program.t) ->
  let used = Analysis.program_read_entities program in
  let imports =
    List.filter
      program.imports
      ~fn:(fun (import: Jir.Imports.requirement) ->
        Entity_set.mem (Jir.Binder.entity_id import.local) used)
  in
  { program with imports }
