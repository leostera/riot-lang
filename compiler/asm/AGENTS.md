# asm AGENTS

`compiler/asm` is Riot's typed assembly document package.

It should own:

- generic assembly document structure
- per-ISA instruction vocabularies
- rendering rules for those vocabularies

Compiler packages own:

- compiler lowering policy
- runtime layout decisions
- backend selection

Those stay in compiler packages like `compiler/raml`.

## Current Shape

Today `asm` is intentionally narrow:

- `Asm.Doc` owns sections, directives, labels, comments, and document rendering
- `Asm.MachO` and `Asm.Elf` own format and assembler-convention helpers
- `Asm.AArch64` owns the first typed instruction set
- `Asm.Wasm` owns the first typed Wasm text-format instruction set
- `Asm.X86_64` is a placeholder seam for the next ISA family
- `Asm.Target.*` owns concrete target profiles that combine ISA plus format policy

## Rules

1. Keep `Asm.Doc` ISA-neutral.
2. Keep ISA-specific spellings in the ISA modules, not in compiler emitters.
3. Preserve useful ISA differences in typed assembly structures.
4. Prefer typed operands and typed instructions over stringly-typed helpers.
5. If rendering rules move, update this file and the consuming emitter docs in
   the same change.
