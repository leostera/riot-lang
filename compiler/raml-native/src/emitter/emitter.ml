open Std
open Std.Data
module Compiler_target = Raml_core.Target
module Target_profile = Target_profile

type error =
  | UnsupportedTargetArchitecture of {
      target: Compiler_target.t;
      supported_targets: Compiler_target.t list
    }
  | Aarch64_apple_darwin of Aarch64_apple_darwin.error

let error_to_json = fun error ->
  match error with
  | UnsupportedTargetArchitecture { target; supported_targets } -> Json.obj
    [
      ("kind", Json.string "unsupported_target_architecture");
      ("target", Compiler_target.to_json target);
      ("supported_targets", Json.array (List.map supported_targets ~fn:Compiler_target.to_json));
    ]
  | Aarch64_apple_darwin error -> Json.obj
    [
      ("kind", Json.string "aarch64_apple_darwin");
      ("error", Aarch64_apple_darwin.error_to_json error);
    ]

let emit_program = fun ~host:_ ~target program ->
  match Target_profile.from_target target with
  | Some { kind=Target_profile.Aarch64_apple_darwin; _ } ->
      Result.map_err
        (Aarch64_apple_darwin.emit_program program)
        ~fn:(fun error -> Aarch64_apple_darwin error)
  | None -> Error (UnsupportedTargetArchitecture {
    target;
    supported_targets = Target_profile.supported_targets ()
  })
