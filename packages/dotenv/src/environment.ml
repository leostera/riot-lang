open Std
open Types

let should_set = fun ~on_existing binding ->
  match on_existing with
  | OverwriteExisting -> true
  | PreserveExisting -> (
      match Env.var Env.String ~name:binding.key with
      | Some _ -> false
      | None -> true
    )

let apply_collect = fun ?(on_existing = PreserveExisting) bindings ->
  let rec loop bindings applied =
    match bindings with
    | [] -> List.rev applied
    | binding :: rest ->
        if should_set ~on_existing binding then (
          ignore (Env.set ~var:binding.key ~value:binding.value);
          loop rest (binding :: applied)
        ) else
          loop rest applied
  in
  loop bindings []

let apply = fun ?(on_existing = PreserveExisting) bindings ->
  ignore
    (apply_collect ~on_existing bindings)
