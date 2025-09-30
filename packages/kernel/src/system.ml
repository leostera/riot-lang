(** System-level operations for Kernel *)

let os_type = Sys.os_type
let unix = Sys.unix
let win32 = Sys.win32
let cygwin = Sys.cygwin
let word_size = Sys.word_size
let int_size = Sys.int_size
let big_endian = Sys.big_endian
let max_string_length = Sys.max_string_length
let max_array_length = Sys.max_array_length
let max_floatarray_length = Sys.max_floatarray_length
let runtime_variant () = Sys.runtime_variant ()
let runtime_parameters () = Sys.runtime_parameters ()

let signal signum handler =
  let old_handler = Sys.signal signum (Sys.Signal_handle handler) in
  match old_handler with
  | Sys.Signal_default -> fun _ -> ()
  | Sys.Signal_ignore -> fun _ -> ()
  | Sys.Signal_handle h -> h

let set_signal signum behavior = Sys.set_signal signum behavior
let sigabrt = Sys.sigabrt
let sigalrm = Sys.sigalrm
let sigfpe = Sys.sigfpe
let sighup = Sys.sighup
let sigill = Sys.sigill
let sigint = Sys.sigint
let sigkill = Sys.sigkill
let sigpipe = Sys.sigpipe
let sigquit = Sys.sigquit
let sigsegv = Sys.sigsegv
let sigterm = Sys.sigterm
let sigusr1 = Sys.sigusr1
let sigusr2 = Sys.sigusr2
let sigchld = Sys.sigchld
let sigcont = Sys.sigcont
let sigstop = Sys.sigstop
let sigtstp = Sys.sigtstp
let sigttin = Sys.sigttin
let sigttou = Sys.sigttou
let sigvtalrm = Sys.sigvtalrm
let sigprof = Sys.sigprof
let sigbus = Sys.sigbus
let sigpoll = Sys.sigpoll
let sigsys = Sys.sigsys
let sigtrap = Sys.sigtrap
let sigurg = Sys.sigurg
let sigxcpu = Sys.sigxcpu
let sigxfsz = Sys.sigxfsz

exception Break = Sys.Break

let catch_break = Sys.catch_break
let ocaml_version = Sys.ocaml_version
let enable_runtime_warnings = Sys.enable_runtime_warnings
let runtime_warnings_enabled = Sys.runtime_warnings_enabled
let opaque_identity = Sys.opaque_identity
let executable_name = Sys.executable_name
let argv () = Sys.argv
