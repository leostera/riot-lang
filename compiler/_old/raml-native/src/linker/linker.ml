open Std
open Std.Data
open Std.Result.Syntax
module Compiler_target = Raml_core.Target
module Target_profile = Target_profile

type artifact =
  | Executable
  | Object

type error =
  | UnsupportedHostArchitecture of {
      host: Compiler_target.t;
      supported_hosts: Compiler_target.t list
    }
  | UnsupportedTargetArchitecture of {
      host: Compiler_target.t;
      supported_targets: Compiler_target.t list
    }
  | LinkFailed of { command: string; status: int; stderr: string }
  | SpawnFailed of { command: string; message: string }

type plan = {
  program: string;
  args: string list;
}

let artifact_to_string = fun artifact ->
  match artifact with
  | Executable -> "executable"
  | Object -> "object"

let error_to_json = fun error ->
  match error with
  | UnsupportedHostArchitecture { host; supported_hosts } -> Json.obj
    [
      ("kind", Json.string "unsupported_host_architecture");
      ("host", Compiler_target.to_json host);
      ("supported_hosts", Json.array (List.map supported_hosts ~fn:Compiler_target.to_json));
    ]
  | UnsupportedTargetArchitecture { host; supported_targets } -> Json.obj
    [
      ("kind", Json.string "unsupported_target_architecture");
      ("host", Compiler_target.to_json host);
      ("supported_targets", Json.array (List.map supported_targets ~fn:Compiler_target.to_json));
    ]
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

let make_plan = fun ~profile ~artifact ~input ~output ->
  let input = Path.to_string input in
  let output = Path.to_string output in
  match artifact with
  | Executable -> {
    program = "clang";
    args = [ "-arch"; profile.Target_profile.clang_arch; input; "-o"; output ]
  }
  | Object -> {
    program = "clang";
    args = [ "-arch"; profile.Target_profile.clang_arch; "-c"; input; "-o"; output ]
  }

let plan = fun ~host ~target ~artifact ~input ~output ->
  match (Target_profile.from_target host, Target_profile.from_target target) with
  | (Some host_profile, Some target_profile) when Target_profile.matches_target host_profile target -> Ok (make_plan
    ~profile:target_profile
    ~artifact
    ~input
    ~output)
  | (None, _) -> Error (UnsupportedHostArchitecture {
    host;
    supported_hosts = Target_profile.supported_hosts ()
  })
  | (_, None) -> Error (UnsupportedTargetArchitecture {
    host;
    supported_targets = Target_profile.supported_targets ()
  })
  | (Some _, Some _) -> Error (UnsupportedTargetArchitecture {
    host;
    supported_targets = Target_profile.supported_targets ()
  })

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
