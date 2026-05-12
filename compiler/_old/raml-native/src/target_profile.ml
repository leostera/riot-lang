(** Native target profiles centralize the target-owned register and toolchain
    policy that several native passes need to agree on.

    The algorithm here is intentionally simple: classify a target triple into a
    known native profile, then hand back the register sets and toolchain knobs
    that the rest of the native backend needs.

    The effect is that allocation, legalization, calling convention lowering,
    emission, and linking all read one source of truth for target policy.

    The rationale is the same one behind [asmcomp]'s [Proc] and target modules:
    target facts should not be scattered across unrelated passes. *)
open Std
module Compiler_target = Raml_core.Target
module Compilation_context = Raml_core.Compilation_context

type kind =
  | Aarch64_apple_darwin

type t = {
  kind: kind;
  target: Compiler_target.t;
  argument_registers: string list;
  return_register: string;
  value_scratch_register: string;
  address_scratch_register: string;
  callee_scratch_register: string;
  caller_saved_allocatable_registers: string list;
  callee_saved_allocatable_registers: string list;
  clang_arch: string;
}

let is_aarch64_apple_darwin = fun (target: Compiler_target.t) ->
  String.equal target.architecture "aarch64"
  && String.equal target.vendor "apple"
  && String.equal target.system "darwin"

let aarch64_apple_darwin = fun target ->
  {
    kind = Aarch64_apple_darwin;
    target;
    argument_registers = [ "x0"; "x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7" ];
    return_register = "x0";
    value_scratch_register = "x9";
    address_scratch_register = "x10";
    callee_scratch_register = "x16";
    caller_saved_allocatable_registers = [ "x11"; "x12"; "x13"; "x14"; "x15"; "x17" ];
    callee_saved_allocatable_registers =
      [
        "x19";
        "x20";
        "x21";
        "x22";
        "x23";
        "x24";
        "x25";
        "x26";
        "x27";
        "x28"
      ];
    clang_arch = "arm64";
  }

let from_target = fun target ->
  if is_aarch64_apple_darwin target then
    Some (aarch64_apple_darwin target)
  else
    None

let from_context = fun ctx -> from_target (Compilation_context.target ctx)

let matches_target = fun profile target ->
  match (profile.kind, from_target target) with
  | (Aarch64_apple_darwin, Some { kind=Aarch64_apple_darwin; _ }) -> true
  | _ -> false

let supported_targets = fun () -> [ Compiler_target.aarch64_apple_darwin ]

let supported_hosts = supported_targets
