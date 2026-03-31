open Std
open Propane

let list_reverse_property =
  property
    "std list reverse is involutive"
    Arbitrary.(list int)
    (fun values -> List.rev (List.rev values) = values)

let option_map_property =
  property "std option map preserves none" Arbitrary.(option int)
    (fun value ->
      match value with
      | None -> Option.map (( + ) 1) value = None
      | Some _ -> true)

let tests = [ list_reverse_property; option_map_property ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"std_propane" ~tests ~args) ~args:Env.args ()
