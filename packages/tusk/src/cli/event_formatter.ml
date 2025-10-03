open Std
open Core

(** Format an event for cargo-style output *)
let format (event : Event.t) =
  match event.kind with
  | PackageStarted { package = _ } ->
      "" (* Don't show on start - wait for cache status *)
  | PackageComplete { package; success; errors; _ } ->
      if success then "" (* Already shown as "Compiling" *)
      else if errors = [] then ""
        (* Skipped due to dependency failure, don't show *)
      else format "   \027[1;31mFailed\027[0m %s" package
  | PackageSkipped _ -> "" (* Don't show skipped packages *)
  | CacheHit { package; _ } ->
      format "   \027[1;32mCompiling\027[0m %s \027[1;90m(cached)\027[0m"
        package
  | CacheMiss { package; _ } ->
      format "   \027[1;32mCompiling\027[0m %s" package
  | CompileError { package = _; error } ->
      (* Just display the raw compiler output for best fidelity *)
      error.raw
  | BuildComplete { duration_ms; succeeded; failed; _ } ->
      if List.length failed = 0 then
        format "   \027[1;32mFinished\027[0m in %.2fs"
          (float_of_int duration_ms /. 1000.0)
      else
        format "   \027[1;31mFailed\027[0m with %d errors" (List.length failed)
  | _ -> ""
