type t = Kernel.System.OS.t =
  | Unix
  | Win32
  | Cygwin

let current = Kernel.System.OS.current

let to_string = Kernel.System.OS.to_string

let is_unix = Kernel.System.OS.is_unix

let is_win32 = Kernel.System.OS.is_win32

let is_cygwin = Kernel.System.OS.is_cygwin
