open Std
open Std.Data
module Target = RamlCore.Target

let ( let* ) = Result.and_then

type artifact =
  | Executable
  | Object

type error =
  | UnsupportedHost of { host: Target.t }
  | UnsupportedTarget of { target: Target.t }
  | LinkFailed of { command: string; status: int; stderr: string }
  | SpawnFailed of { command: string; message: string }

type plan = {
  program: string;
  args: string list;
}

let supports_aarch64_apple_darwin = fun target ->
  String.equal (Target.to_string target) "aarch64-apple-darwin"

let artifact_to_string = fun artifact ->
  match artifact with
  | Executable -> "executable"
  | Object -> "object"

let error_to_json = fun error ->
  match error with
  | UnsupportedHost { host } -> Json.obj
    [ ("kind", Json.string "unsupported_host"); ("host", Target.to_json host); ]
  | UnsupportedTarget { target } -> Json.obj
    [ ("kind", Json.string "unsupported_target"); ("target", Target.to_json target); ]
  | LinkFailed { command; status; stderr } -> Json.obj
    [
      ("kind", Json.string "link_failed");
      ("command", Json.string command);
      ("status", Json.int status);
      ("stderr", Json.string stderr);
    ]
  | SpawnFailed { command; message } -> Json.obj
    [
      ("kind", Json.string "spawn_failed");
      ("command", Json.string command);
      ("message", Json.string message);
    ]

let make_plan = fun ~artifact ~input ~output ->
  let input = Path.to_string input in
  let output = Path.to_string output in
  match artifact with
  | Executable -> { program = "clang"; args = [ "-arch"; "arm64"; input; "-o"; output ] }
  | Object -> { program = "clang"; args = [ "-arch"; "arm64"; "-c"; input; "-o"; output ] }

let plan = fun ~host ~target ~artifact ~input ~output ->
  if supports_aarch64_apple_darwin host then
    if supports_aarch64_apple_darwin target then
      Ok (make_plan ~artifact ~input ~output)
    else
      Error (UnsupportedTarget { target })
  else
    Error (UnsupportedHost { host })

let to_command = fun plan -> Command.make plan.program ~args:plan.args

let plan_to_string = fun plan -> Command.to_string (to_command plan)

let link = fun ~host ~target ~artifact ~input ~output ->
  let* plan = plan ~host ~target ~artifact ~input ~output in
  let command = to_command plan in
  let rendered = Command.to_string command in
  match Command.output command with
  | Error (Command.SystemError message) -> Error (SpawnFailed { command = rendered; message })
  | Ok output when output.status = 0 -> Ok ()
  | Ok output -> Error (LinkFailed {
    command = rendered;
    status = output.status;
    stderr = output.stderr
  })
