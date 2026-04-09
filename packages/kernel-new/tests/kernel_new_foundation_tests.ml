open Std
module Test = Std.Test
module Kernel = Kernel_new

let test_bool_to_string_uses_stable_lowercase_literals = fun _ctx ->
  if
    Kernel.String.equal (Kernel.Bool.to_string Kernel.Bool.true_) "true"
    && Kernel.String.equal (Kernel.Bool.to_string Kernel.Bool.false_) "false"
  then
    Ok ()
  else
    Error "expected Bool.to_string to use stable lowercase literals"

let test_char_of_int_checks_bounds = fun _ctx ->
  match (Kernel.Char.of_int 65, Kernel.Char.of_int (-1), Kernel.Char.of_int 256) with
  | (Some value, None, None) when Kernel.Char.to_int value = 65 -> Ok ()
  | _ -> Error "expected Char.of_int to accept only byte-sized values"

let test_array_init_builds_in_index_order = fun _ctx ->
  let seen = Kernel.Array.make 4 (-1) in
  let next = ref 0 in
  let built =
    Kernel.Array.init 4
      (fun index ->
        Kernel.Array.set seen !next index;
        next := !next + 1;
        index * 2)
  in
  if
    !next = 4
    && Kernel.Array.get seen 0 = 0
    && Kernel.Array.get seen 1 = 1
    && Kernel.Array.get seen 2 = 2
    && Kernel.Array.get seen 3 = 3
    && Kernel.Array.get built 0 = 0
    && Kernel.Array.get built 1 = 2
    && Kernel.Array.get built 2 = 4
    && Kernel.Array.get built 3 = 6
  then
    Ok ()
  else
    Error "expected Array.init to visit each index once from left to right"

let test_option_map_leaves_none_unforced = fun _ctx ->
  let called = ref false in
  let value =
    Kernel.Option.map
      (fun _ ->
        called := true;
        1)
      None
  in
  if not !called && Kernel.Option.is_none value && Kernel.Option.unwrap_or value ~default:3 = 3 then
    Ok ()
  else
    Error "expected Option.map to leave None untouched and avoid calling its mapper"

let test_result_and_then_short_circuits_errors = fun _ctx ->
  let called = ref false in
  let value =
    Kernel.Result.and_then (Kernel.Result.Error "boom")
      (fun _ ->
        called := true;
        Kernel.Result.Ok 1)
  in
  match value with
  | Kernel.Result.Error "boom" when not !called -> Ok ()
  | _ -> Error "expected Result.and_then to leave Error untouched and skip the next step"

let tests = [
  Test.case "Bool.to_string uses stable lowercase literals" test_bool_to_string_uses_stable_lowercase_literals;
  Test.case "Char.of_int checks bounds" test_char_of_int_checks_bounds;
  Test.case "Array.init builds in index order" test_array_init_builds_in_index_order;
  Test.case "Option.map leaves None unforced" test_option_map_leaves_none_unforced;
  Test.case "Result.and_then short-circuits errors" test_result_and_then_short_circuits_errors;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_foundation_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
