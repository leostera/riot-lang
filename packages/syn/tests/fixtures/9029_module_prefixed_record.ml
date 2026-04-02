(* Test: Module-prefixed record expressions *)

let x = M.{ a = 1; b = 2 }

let stdio = OsProcess.{ stdin = `Null; stdout = `Pipe; stderr = `Pipe }
