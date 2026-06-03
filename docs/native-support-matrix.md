# TinyML Native Support Matrix

This matrix tracks the parser/checker/Lambda surface that the native SSA and
AArch64 pipeline must carry without normal `unsupported` placeholders.

## Parser AST expression forms

| Form | Parser | Checker/TIR | Lambda | Native SSA | AArch64 |
| --- | --- | --- | --- | --- | --- |
| literals: unit/bool/int | yes | yes | `Constant` | `Const` | direct immediates |
| literals: string | yes | yes | `Constant` | `StringLiteral` | `tinyml_rt_string_new` |
| literals: float | yes | yes | `Constant` | `FloatLiteral` | `tinyml_rt_float_new` |
| variable/global | yes | yes | `Var` | local `ValueId` / `Global` | stack/global call path |
| function | yes | yes | `Function` | top-level `Function`, nested closure helper | direct function / closure helper |
| application | yes | yes | `Apply` | `Call` | direct call / closure helper |
| primitive arithmetic | yes | yes | `Prim` | `Prim` | arithmetic instructions |
| let/block | yes | yes | `Let`/`Block` | SSA bindings/statements | stack homes |
| tuple construction | yes | yes | `Tuple` | `MakeTuple` | `tinyml_rt_tuple_new` |
| record construction | yes | yes | `Record` | `MakeRecord` | `tinyml_rt_record_new` |
| constructor | yes | yes | `Construct` | `Const(ConstructorTag)` / `MakeVariant` | immediate / `tinyml_rt_variant_new` |
| match | yes | yes | `Match` | `RuntimeMatch` placeholder / future blocks | runtime match helper path |

## Lambda pattern forms

| Form | Lambda | Native SSA binding strategy |
| --- | --- | --- |
| wildcard | `Wildcard` | ignore |
| variable | `Var` | env bind |
| constant | `Constant` | no binding, match lowering will test |
| constructor | `Construct` | `VariantGet` payload bindings |
| tuple | `Tuple` | `TupleGet` field bindings |
| record | `Record` | `RecordGet` field bindings by order |

## Native representation

Primitive scalar values use concrete `MachineRepr` variants. Runtime-managed or
not-yet-specialized values use `MachineRepr::Ptr`; accepted TinyML programs must
not normally lower to an unsupported representation.
