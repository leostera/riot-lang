(** OS Process operations for Kernel *)

let sigterm = Sys.sigterm
let environment () = Unix.environment ()
let getpid () = Unix.getpid ()
let kill pid signal = Unix.kill pid signal
let system cmd = Unix.system cmd
let execv prog args = Unix.execv prog args

let create_process prog args stdin stdout stderr =
  Unix.create_process prog args stdin stdout stderr

let open_process_in cmd = Unix.open_process_in cmd
let close_process_in ic = Unix.close_process_in ic
let open_process_full cmd env = Unix.open_process_full cmd env

let close_process_full (ic_out, oc_in, ic_err) =
  Unix.close_process_full (ic_out, oc_in, ic_err)
