include Kernel.Process

let execv = Kernel.Process.execv

let id = fun () -> Int32.of_int (current_pid ())
