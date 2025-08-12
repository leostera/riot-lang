let enabled = ref false
let enable () = enabled := true
let disable () = enabled := false

let trace fmt =
  if !enabled then
    Printf.ksprintf (fun s -> Printf.eprintf "[TRACE] %s\n%!" s) fmt
  else Printf.ksprintf (fun _ -> ()) fmt
