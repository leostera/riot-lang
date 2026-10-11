// Stage 0 type signatures for the compiler-provided Node host.
use Prelude.{List, Nil, Result, Ok}

let args = fn () -> (Nil: List<string>)
let read_file = fn (path: string) -> (Ok(""): Result<string, string>)
let write_file = fn (path: string) -> fn (contents: string) -> (Ok(()): Result<unit, string>)
let write_stdout = fn (message: string) -> ()
let write_stderr = fn (message: string) -> ()
let run_process = fn (program: string) -> fn (arguments: List<string>) -> (Ok(0): Result<i32, string>)
