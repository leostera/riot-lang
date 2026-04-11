open Std
open Infer
open Model

let rec to_env = fun scope ->
  let local_env =
    Env.bind
      (Env.of_entries
        ~make_id:BindingId.persistent
        ~provenance:Env.Binding.Ambient
        (CompiledScope.exports scope |> List.map (fun (path, scheme) -> (path, scheme))))
      (Env.of_type_decls (CompiledScope.type_decls scope))
  in
  local_env
  |> Env.without_summary
