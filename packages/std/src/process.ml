include Kernel.Process

let execv = fun ~program ~args ->
  Kernel.Process.execv program args

let id = fun () -> Int32.from_int (current_pid ())
