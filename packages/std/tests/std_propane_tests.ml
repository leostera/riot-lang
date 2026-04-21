open Std
open Propane

let list_reverse_property =
  property
    "std list reverse is involutive"
    Arbitrary.(list int)
    (fun values -> List.reverse (List.reverse values) = values)

let option_map_property =
  property "std option map preserves none" Arbitrary.(option int)
    (fun value ->
      match value with
      | None -> Option.map value ~fn:(( + ) 1) = None
      | Some _ -> true)

let tests = [ list_reverse_property; option_map_property ]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"std_propane" ~tests ~args ()) ~args:Env.args ()
