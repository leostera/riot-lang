(** Native target profiles centralize the target-owned register and toolchain
    policy that several native passes need to agree on.

    Today this module only exposes the current AArch64 Darwin slice, but the
    shape is deliberate: allocation, legalization, calling convention, the
    emitter, and the linker all need the same target facts, and duplicating
    them across those layers drifts quickly.

    The effect is that target-specific passes can ask one module for their
    calling convention, scratch registers, and allocatable pools instead of
    baking those choices into each pass separately. *)
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
val from_target: Compiler_target.t -> t option

val from_context: Compilation_context.t -> t option

val matches_target: t -> Compiler_target.t -> bool

val supported_targets: unit -> Compiler_target.t list

val supported_hosts: unit -> Compiler_target.t list
