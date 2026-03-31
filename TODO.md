# TODO

## Loop

Token-first compiler-lead cleanup loop:

1. Start in `packages/krasny/src/lower.ml`.
2. In the formatter branch, ask for the exact original token you want to render.
3. Let the compiler fail on the missing CST field or accessor.
4. Add that token to the `Syn.Cst` type in `packages/syn/src/cst.mli` and `packages/syn/src/cst.ml`.
5. Let the compiler fail in `packages/syn/src/cst_builder.ml`.
6. Populate the token from the original syntax node.
7. Rebuild with `timeout 120 tusk build syn krasny fixme tusk-fix`.
8. Repeat until `krasny` no longer synthesizes punctuation it could render from CST-owned tokens.

Rules:

- Do not reintroduce `owned_trivia`.
- Prefer original tokens over duplicated trivia fields.
- If a formatter branch needs a token, make `krasny` ask for it first.
- Update this file and commit every slice with conventional commits

## Landed

Token-backed so far:

- record field `:`
- record field `;`
- object type field `:`
- object type field `;`
- type constraint `=`
- type declaration manifest `=`
- type declaration definition `=`
- type extension `+=`
- record pattern field `=`
- record expression field `=`
- object override field `=`
- record definition field `:`
- typed pattern `:`
- expression type ascription `:`
- expression coercion `:>`
- variant constructor payload/result separator
- arrow label `:`
- class/class-type constraint `=`
- module declaration `=`
- class declaration `:`
- class definition `:`
- class definition `=`
- class expression constraint `:`
- module type of `of`
- module type constraint `=` / `:=`
- object member type annotation `:`
- class type value/method `:`
- class member type annotation `:`
- class type declaration `=`
- object member body `=`
- class member body `=`
- for loop `=`
- let module expression `=`
- named parameter alias `:`
- optional parameter alias `:`
- optional parameter default `=`
- polymorphic expression `:`
- local binding annotation `:`
- first-class module pattern `:`
- module pack expression `:`
- module unpack expression `:`
- module declaration `:`
- functor parameter `:`
- module expression constraint `:`
- polyvariant leading `|`
- variant constructor leading `|`

## Next Targets

Remaining synthetic tokens / fallback punctuation to drive out of `krasny`:

- declaration `=` sites still rendered from synthetic `equals`
  - local bindings / let-style renderers
  - type alias / type definition renderers
  - module declarations
  - class declarations / definitions
  - record/object field assignment renderers

- declaration `:` sites still rendered from synthetic `Doc.colon` / `annotation_colon`
  - type ascriptions
  - constructor result types
  - label / parameter shells
  - class/object constraint renderers

- variant / polyvariant shell punctuation
  - variant constructor fallback `kw_of`
  - variant constructor fallback `Doc.colon`
  - synthesized leading `|`
  - polyvariant fallback `kw_of`

- type extension `+=`

- remaining synthetic `;` joins in structural shells
  - record / object / class field lists
  - list / array / pattern list shells where source tokens should be available

- remaining synthetic delimiters that should come from CST tokens if available
  - `{ ... }`
  - `[ ... ]`
  - `(module ...)`
  - pattern local-open `.(...)`

## Audit: lower.ml

High-confidence synthetic punctuation sites in `packages/krasny/src/lower.ml`:

- `=`:
  - `638`
  - `1479`
  - `1498`
  - `2287`
  - `2600`
  - `2815`
  - `4054`
  - `4956`
  - `5156`
  - `5207`
  - `5209`
  - `5617`
  - `5622`
  - `5671`

- `:`:
  - `504`
  - `507`
  - `973`
  - `1290`
  - `1370`
  - `1656`
  - `1738`
  - `2506`
  - `4341`
  - `4376`

- `|`:
  - `1054`
  - `1230`
  - `1734`
  - `3517`

- `of`:
  - `752`
  - `1089`
  - `1099`
  - `1356`
  - `4983`

- `+=`:
  - `1544`
  - `1555`

- assignment `=` in field/update shells:
  - `1704`
  - `2581`
  - `3107`

- class/object `constraint ... = ...` still synthetic:
  - `2732`
  - `2864`

Medium-confidence synthetic delimiter / separator sites:

- structural `;` joins:
  - `949`
  - `1016`
  - `1717`
  - `1726`
  - `3116`
  - `3129`

- structural `{}` wrappers:
  - `947`
  - `1007`
  - `1013`
  - `1715`
  - `1723`
  - `3165`
  - `3182`

- polyvariant brackets:
  - `1125`
  - `1127`
  - `1129`
  - `1168`

- `(module ...)` delimiters:
  - `918`
  - `1421`

- pattern local-open `.(...)` shell:
  - `1749`

## Known Warnings

- `packages/krasny/src/lower.ml`
  - `render_first_class_module_type_doc` is still non-exhaustive for `ModuleType.Signature _`
  - redundant `BeginEnd` parenthesized-expression subpattern
