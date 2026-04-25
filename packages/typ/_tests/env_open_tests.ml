open Std
open Typ
open Typ.Analysis
open Typ.Infer
open Typ.Model

module Std_env = Std.Env

let int_to_string_scheme = TypeScheme.of_type (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.string)

let rgb_blend_scheme = TypeScheme.of_type (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:(TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.int))

let make_id =
  let next_local_id = ref 0 in
  fun path ->
    let local_id = !next_local_id in
    (next_local_id := local_id + 1);
    BindingId.local ~stamp:local_id ~name:(SurfacePath.to_string path)

let lookup_name = fun env name -> Env.lookup env (EntityId.of_string name) |> Option.map Env.Binding.name

let test_local_open_exposes_nested_modules = fun _ctx ->
  let ambient =
    Env.of_entries ~make_id ~provenance:Env.Binding.Ambient
      [
        SurfacePath.of_string "Colors.to_string", int_to_string_scheme;
        SurfacePath.of_string "Colors.RGB.blend", rgb_blend_scheme;
      ]
  in
  let opened = Env.with_local_open ambient (SurfacePath.of_name "Colors") in
  let to_string_name = lookup_name opened "to_string" in
  let blend_name = lookup_name opened "RGB.blend" in
  if not (Option.equal String.equal to_string_name (Some "to_string")) then
    Error (format Format.[ str "expected to_string after open Colors, got "; str (Option.unwrap_or ~default:"<none>" to_string_name) ])
  else
    if not (Option.equal String.equal blend_name (Some "blend")) then
      Error (format Format.[ str "expected RGB.blend after open Colors, got "; str (Option.unwrap_or ~default:"<none>" blend_name) ])
    else Ok ()

let main ~args =
  let tests = [ Test.case "local open exposes nested modules from ambient module exports" test_local_open_exposes_nested_modules ] in Test.Cli.main ~name:"typ:env_open" ~tests ~args ()

let () = Runtime.run ~main ~args:Std_env.args ()
