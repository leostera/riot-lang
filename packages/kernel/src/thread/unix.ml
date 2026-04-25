open Prelude

let available_parallelism =
  let count = Caml_runtime.recommended_domain_count () in
  if count < 1 then
    1
  else count
