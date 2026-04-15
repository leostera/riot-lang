open Std

let of_build_runtime_event = fun (event: Build_runtime.build_event) ->
  Some event
