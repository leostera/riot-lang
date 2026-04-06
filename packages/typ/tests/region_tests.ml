open Std
open Typ

let test_local_reachable_vars_include_nested_escaped_vars = fun _ctx ->
  let regions = Region.create () in
  Region.with_region regions
    (fun outer ->
      let outer_ty = Region.fresh_var regions 0 in
      let escaped_ty =
        Region.with_region regions
          (fun _inner ->
            let escaped_ty = Region.fresh_var regions 1 in
            let _dead_ty = Region.fresh_var regions 2 in
            let lowered = TypeRepr.occurs_or_lower ~needle:0 ~level:1 escaped_ty in
            if lowered then
              raise (Failure "escaping var unexpectedly occurred in the outer variable")
            else
              escaped_ty)
      in
      let locals = Region.local_reachable_vars
        regions
        outer
        (TypeRepr.Tuple [ outer_ty; escaped_ty ]) in
      if locals = [ 0; 1 ] then
        Ok ()
      else
        Error ("expected outer region locals [0; 1], got ["
        ^ String.concat ", " (List.map string_of_int locals)
        ^ "]"))

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "local reachable vars include nested escaped vars" test_local_reachable_vars_include_nested_escaped_vars;
      ] in
      Test.Cli.main ~name:"typ:region" ~tests ~args)
    ~args:Env.args
    ()
