open Std
open Typ
open Typ.Analysis
open Typ.Diagnostics
open Typ.Infer
open Typ.Model
open Typ.Session

let test_local_reachable_vars_include_nested_escaped_vars = fun _ctx ->
  let regions = Region.create () in
  Region.with_region regions
    (fun outer ->
      let outer_ty = TypeRepr.make_var ~level:1 0 |> Region.track_node regions in
      let escaped_ty =
        Region.with_region regions
          (fun _inner ->
            let escaped_ty = TypeRepr.make_var ~level:2 1 |> Region.track_node regions in
            let _dead_ty = TypeRepr.make_var ~level:2 2 |> Region.track_node regions in
            let lowered =
              TypeRepr.occurs_or_lower
                ~generation:(Region.next_mark regions)
                ~needle:0
                ~level:1
                ~on_lower:(fun ty -> Region.add_to_pool regions ~level:(TypeRepr.level ty) ty |> ignore)
                escaped_ty
            in
            if lowered then
              raise (Failure "escaping var unexpectedly occurred in the outer variable")
            else
              escaped_ty)
      in
      let locals = Region.local_reachable_vars
        regions
        outer
        (TypeRepr.tuple [ outer_ty; escaped_ty ]) in
      if locals = [ 0; 1 ] then
        Ok ()
      else
        Error (format
          Format.[
            str "expected outer region locals [0; 1], got [";
            str (String.concat ", " (List.map string_of_int locals));
            str "]";
          ]))

let main ~args =
  let tests = [
    Test.case "local reachable vars include nested escaped vars" test_local_reachable_vars_include_nested_escaped_vars;
  ] in
  Test.Cli.main ~name:"typ:region" ~tests ~args ()

let () = Runtime.run ~main ~args:Std.Env.args ()
