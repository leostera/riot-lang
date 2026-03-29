# TODO

This file is _yours_. Keep it up to date after every big change.

## How You Work

1. Read this file from top to bottom and pick the next unchecked item that is unblocked.
2. Work until it is complete.
3. Mark a task complete here only after the listed verification has passed.
4. Commit after every slice with a conventional commit message.
5. Prefer `ceibo` / `syn` changes over new `krasny/lower.ml` trivia heuristics.

## Mission

- [ ] Make trivia, comments, and docstrings first-class at the token layer so the CST can derive reliable ownership and `krasny` can become a renderer again

## Non-Negotiables

- [ ] Use token `leading_trivia` only; do not add token `trailing_trivia`
- [ ] Preserve trivia losslessly in `syn`, even when the source placement is weird
- [ ] Keep token spans as token-body-only spans; trivia carries its own spans/text
- [ ] Stop storing trivia as standalone syntax/tree children
- [ ] Derive ownership from token order and item/member sequences, not source-gap archaeology
- [ ] Normalize ugly comment placement in `krasny`, not in `syn`

## Ralph Loop

For every slice below:

- [ ] Add or update the smallest regression first
- [ ] Land the smallest code change that moves ownership closer to the token layer
- [ ] Run focused tests first
- [ ] Run the slice build command
- [ ] Commit the slice
- [ ] Update this file before starting the next slice

## Definition Of Done

- [x] `ceibo` stores trivia on tokens instead of as standalone tree children
- [x] `syn` lexer/parser consume token-attached trivia cleanly
- [x] Top-level CST item ownership comes from token boundaries
- [x] Nested `sig ... end` / `struct ... end` ownership uses the same model
- [x] Member ownership is reliable for constructors, fields, `and` groups, and nested modules
- [x] Doc kind is explicit in the CST
- [x] `krasny/lower.ml` no longer does normal-path trivia archaeology
- [ ] `./packages/krasny/tests/test_runner.py --verify-workspace --fail-fast` passes

## Execution Plan

### 1. Ceibo Token Trivia Model

- [x] Introduce a first-class trivia type
  - represent whitespace, comments, and docstrings distinctly
  - keep raw text and spans on each trivia entry
  - verification:
    - focused token/trivia tests
    - `timeout 120 tusk build ceibo syn`

- [x] Add `leading_trivia` to Ceibo tokens
  - update green token representation
  - update red token representation
  - keep token spans token-body-only
  - verification:
    - focused token construction tests
    - `timeout 120 tusk build ceibo syn`

- [x] Add EOF-owned trailing file trivia
  - introduce an EOF token/sentinel whose `leading_trivia` owns trailing file trivia
  - keep file-end comments/docstrings lossless
  - verification:
    - focused EOF/file-end tests
    - `timeout 120 tusk build ceibo syn`

- [x] Update Ceibo docs/helpers to describe token-attached trivia
  - remove assumptions that trivia exists as tree children
  - verification:
    - docs match the landed representation

### 2. Remove Trivia-As-Children From Trees

- [x] Stop constructing standalone trivia children in the green tree
  - tree children should be non-trivia syntax only
  - verification:
    - `timeout 120 tusk build ceibo syn`

- [x] Stop exposing standalone trivia children in the red tree
  - traversal helpers should not rely on filtering trivia out of child lists
  - verification:
    - focused Ceibo traversal tests
    - `timeout 120 tusk build ceibo syn`

- [x] Keep `syn print-ceibo` lossless after the representation change
  - verification:
    - direct before/after print-ceibo checks on files with mixed comments/docstrings
    - `timeout 120 tusk build ceibo syn`

### 3. Syn Lexer / Parser Migration

- [x] Make the lexer accumulate trivia onto the next real token
  - `Whitespace` / `Comment` / `Docstring` stop behaving like ordinary parser tokens
  - preserve exact trivia ordering
  - verification:
    - focused lexer tests with mixed blank lines/comments/docstrings
    - `timeout 120 tusk build syn`

- [x] Remove explicit parser trivia consumption
  - delete parser flows that splice trivia into green children
  - keep offsets and diagnostics stable
  - verification:
    - focused parser tests on awkward comment placements
    - `timeout 120 tusk build syn`

- [x] Update token cursor / parser helper APIs for token-attached trivia
  - helpers should work on real tokens directly
  - verification:
    - focused parser helper tests if needed
    - `timeout 120 tusk build syn`

### 4. Rebuild Top-Level CST Ownership

- [x] Derive standalone comments/docstrings from token boundaries
  - stop reparsing source gaps for top-level ownership
  - verification:
    - `timeout 180 tusk test syn:cst_tests`

- [x] Build a generic ordered-item ownership pass
  - use one pass for structure items and signature items
  - attach leading doc blocks
  - preserve standalone headings/docs
  - avoid double-owning comments/docstrings
  - verification:
    - focused CST tests for leading docs, standalone headings, repeated docs, mixed comments/docstrings
    - `timeout 120 tusk build syn krasny`

- [x] Retire the old file-level span-exclusion/source-gap path where possible
  - verification:
    - targeted formatter fixtures for module overviews, section headings, repeated docstrings
    - `timeout 120 tusk build syn krasny`

### 5. Rebuild Nested Body Ownership

- [x] Reify nested `sig ... end` bodies as normalized item lists
  - preserve original syntax nodes for losslessness/debugging
  - verification:
    - nested signature CST tests
    - `timeout 120 tusk build syn krasny`

- [x] Reify nested `struct ... end` bodies as normalized item lists
  - preserve original syntax nodes for losslessness/debugging
  - verification:
    - nested structure CST tests
    - `timeout 120 tusk build syn krasny`

- [x] Use the same ordered-item ownership pass for nested and top-level bodies
  - verification:
    - nested CST tests for headings/docs/comments/repeated docs
    - `timeout 120 tusk build syn krasny`

### 6. Rebuild Member-Level Ownership

- [x] Make type declaration ownership come from token order
  - avoid ad hoc span surgery around grouped `and` declarations
  - verification:
    - CST tests for grouped type docs/headings
    - `timeout 120 tusk build syn krasny`

- [x] Make variant constructor ownership reliable
  - constructor docs should not drift to the next type or the parent type header
  - verification:
    - CST tests for constructor docs vs next type docs
    - targeted formatter fixtures in `jsonrpc.mli` / `mcp.mli`

- [x] Make record field ownership reliable
  - field docs should stay with fields, not the enclosing type or following declarations
  - verification:
    - CST tests for record field docs
    - targeted formatter fixtures in `sse.mli` / similar cases

- [x] Decide which grammars need explicit member-item streams
  - candidates:
    - variant constructors
    - record fields
    - object fields
    - exception constructors
  - verification:
    - design documented in code/comments/tests

- [x] Add nested member-heading coverage
  - headings/docs inside nested modules should not duplicate or drift
  - verification:
    - targeted CST tests for `ceibo.mli`

### 7. Make Doc Kind Explicit

- [x] Represent ordinary docs vs section docs explicitly in the CST/trivia model
  - stop reclassifying headings from raw text in normal paths
  - verification:
    - CST tests for ordinary docs vs section docs vs plain comments
    - `timeout 120 tusk build syn krasny`

### 8. Simplify Krasny

- [x] Delete top-level trivia archaeology from `krasny/lower.ml`
  - remove normal-path source-gap parsing once CST ownership is correct
  - verification:
    - `timeout 180 tusk test krasny:format_tests`
    - targeted workspace regressions

- [x] Delete nested-body trivia repair heuristics from `krasny/lower.ml`
  - nested signatures/modules should render from CST ownership directly
  - verification:
    - `timeout 180 tusk test krasny:format_tests`
    - targeted nested formatter fixtures

- [x] Keep only layout/rendering logic in `lower.ml`
  - if `lower.ml` is still deciding ownership from spans/source gaps, the job is not done
  - verification:
    - code review of `lower.ml`
    - focused formatter tests still pass

### 9. Regression Inventory

- [x] Keep adding direct `syn:cst_tests` for ownership, not just formatter fixtures
  - top-level module overviews
  - first type after `open`
  - repeated adjacent docstrings
  - section headings between declarations
  - constructor docs vs next type docs
  - record field docs
  - nested module headings
  - plain comments mixed with docstrings

- [ ] Keep formatter fixtures only for renderer problems after CST ownership is correct
  - avoid new `lower.ml`-only fixtures for issues that belong in `syn:cst_tests`

### 10. Final Validation

- [ ] `timeout 120 tusk build ceibo syn krasny`
- [ ] `timeout 180 tusk test syn:cst_tests`
- [ ] `timeout 180 tusk test krasny:format_tests`
- [ ] `./packages/krasny/tests/test_runner.py --verify-workspace --fail-fast`

## Current Notes

- [ ] Treat the existing `owned_trivia` expansion as a migration aid, not the final model
- [ ] Current state: Ceibo docs and builder helpers now describe token-attached trivia explicitly, including EOF-owned trailing file trivia
- [ ] Current state: Ceibo green/red tokens have token-attached `leading_trivia`; `syn` now also uses EOF `leading_trivia` to own trailing file trivia
- [ ] Current state: `Parser.parse_result.tokens` and `syn print-ceibo` preserve the original lexer token stream, and `Syn.build_cst` now uses that token stream for top-level standalone comment/docstring ownership instead of re-lexing `source`
- [ ] Current state: `Lexer.tokenize` now emits only real tokens plus EOF, with all non-EOF trivia attached to the next real token
- [ ] Current state: `Token_cursor` now works on real tokens directly and exposes helper APIs for consuming the current token's leading trivia without reintroducing trivia tokens into the main parser stream
- [ ] Current state: the parser still has explicit `consume_trivia` flows, but they no longer splice trivia into green children: `tokens_to_green` drops trivia tokens entirely and real token construction still rehydrates original token `leading_trivia`
- [ ] Current state: top-level structure/signature item ownership now runs through one generic ordered-item pass, so type/value doc attachment and standalone heading preservation no longer fork by item family
- [ ] Current state: top-level standalone item ordering no longer subtracts item spans from the file token stream; it walks each item’s first-token trivia plus `EOF.leading_trivia` from the original parser tokens
- [ ] Current state: nested `sig ... end` and `struct ... end` bodies now expose normalized item lists through `CstBuilder.signature_items_of_module_type` and `CstBuilder.structure_items_of_module_expression`, while still preserving raw syntax-node anchors for losslessness/debugging
- [ ] Current state: nested helper APIs are now pinned against the same ordered-item ownership behavior as the top level, including grouped-type headings, repeated docs, and standalone doc/comment ordering after `open`
- [ ] Current state: grouped `type ... and ...` ownership now normalizes each member from its own `TYPE_DECL` token stream before reassembling the public group, so doc/header ownership no longer depends on redistributing trivia out of the grouped parent node by source spans
- [ ] Current state: constructor doc ownership is now leading-only: docstrings between constructors attach to the next constructor, postfix comments may still stay with the previous constructor, and terminal constructor docstrings remain standalone instead of being stolen by the last constructor
- [ ] Current state: record field doc ownership is now leading-only: docstrings between fields attach to the next field, postfix comments may still stay with the previous field, and terminal `}`-owned docs/comments are preserved inside the record body; `blink/src/sse.mli` plus fixtures `0744` and `0915` pin the formatter output
- [ ] Current state: explicit member-item streams are only needed for repeated member grammars with public member `owned_trivia` today, namely variant constructors and record fields; exception declarations stay on the ordinary ordered-item pass, and object type fields remain syntax-only until they grow a public owned-trivia/rendering contract
- [ ] Current state: nested heading/doc ordering is now pinned against the real `ceibo.mli` `Green` signature, including repeated `## Construction` section docs staying standalone while the following `make_trivia` API doc stays declaration-owned
- [ ] Current state: top-level `krasny` source-file rendering now consumes `SourceFile.items` plus per-item `owned_trivia` directly, so standalone comments/docstrings and declaration-owned docs/comments no longer come from raw source-gap parsing in the normal top-level path
- [ ] Current state: `render_structure_top_level_items` and `render_signature_top_level_items` are now layout-only joins over ordered item streams; the dead `pending` / `leading_doc` / `parse_between` scaffolding and optional per-entry owned-trivia toggles are gone, while phrase-separator and expression-run preservation stay intact
- [ ] Current state: top-level formatter fixtures `0910`, `0914`, `0915`, `0916`, and `0917` now pin the leading-only doc rule in `krasny`: inter-item docs render with the next declaration, while terminal postfix docs/comments stay after the last declaration
- [ ] Current state: ordinary docstrings before signature `val` declarations now normalize onto the next declaration's `owned_trivia.leading`, while terminal postfix `val` docstrings stay standalone instead of being rescued onto the previous declaration
- [ ] Current state: nested `CstBuilder.structure_items_of_module_expression` / `signature_items_of_module_type` streams now walk token order like the top level: they surface standalone comments/docstrings from each nested item's first-token `leading_trivia` plus the closing `end` token's `leading_trivia`, so nested item consumers no longer need raw-gap recovery for inter-item or terminal body trivia
- [ ] Current state: nested `krasny` structure/signature rendering now consumes those helper item streams directly; fixtures `0738`, `0918`, and `0919` pin section headings, terminal nested docstrings, and terminal nested comments without nested-only trailing-comment recovery
- [ ] Current state: doc kind is now explicit on `Cst.Docstring`, and normal ownership/rendering paths in `syn` and `krasny` use that explicit section-vs-ordinary distinction instead of reparsing docstring text
- [ ] Current state: grouped `type ... and ...` members now inherit `and`-token leading trivia during `syn` normalization, and `krasny` renders grouped member docs/comments from per-member `owned_trivia`; fixtures `0920`, `0921`, and `0743` pin the `and`-member comment/doc and adjacent standalone-doc spacing behavior
- [ ] Current state: dead `krasny/lower.ml` member-level docstring archaeology is gone: the old trailing-docstring scanners, `allow_terminal_docstrings`/`render_remaining_trivia` type-declaration knobs, and grouped-type nontrivia-end helpers were deleted without changing formatter output on the focused grouped/nested/doc-spacing regressions
- [ ] Current state: `CstBuilder.record_field_items_of_fields` now exposes record bodies as ordered `RecordField`/`Comment`/`Docstring` streams sourced from field first-token trivia plus the closing `}` token's remaining `leading_trivia`, so `krasny` no longer interleaves raw record children or recovers terminal `}` trivia by hand; fixtures `0744` and `0922` pin top-level and inline-record terminal doc cases
- [ ] Current state: the direct `syn:cst_tests` ownership inventory now covers top-level module overviews, the first type after `open`, repeated adjacent ordinary docstrings, section headings between declarations, constructor/record member ownership, nested module headings, and plain comments mixed with docstrings, so those cases no longer depend only on `krasny` fixtures
- [ ] Current state: module/class top-level keyword probes, bracket attribute/extension probes, functor application / `(val ...)` module-expression probes, parenthesized module-type lookahead in module expressions, declaration-local `module type of` probes, application/infix/tuple/assign/sequence expression continuations, paren and bracket local-open vs index expression disambiguation, postfix custom-index/operator-like probes, dotted module/module-type/type-name/qualified-field path continuations, local-open core type path lookahead, `include module type of`, `let open` expression detection, the polymorphic/local-open/local-abstract type probes, tuple/as/cons/or pattern continuations, local-open pattern path disambiguation, literal range-pattern probes, and grouped structure/signature type-declaration uppercase-body disambiguation no longer rely on trivia-skipping control flow; inline-comment alias-vs-variant, grouped GADT, and `module type of` declaration cases are pinned in `syn:cst_tests`
- [ ] Current state: top-level file loops and nested `struct`/`sig` body loops no longer thread trivia through `tokens_to_green []`
- [ ] Current state: red traversal now follows the same contract as green for parser-built trees; leading trivia lives on tokens and `SyntaxNode.tokens` stays trivia-free
- [ ] Current state: `print-ceibo` fixture coverage now includes a mixed comment/docstring bridge case
- [ ] The next concrete slice is the section 9 formatter-fixture audit: make sure new formatter fixtures stay reserved for renderer/layout problems now that the ownership cases are pinned directly in `syn:cst_tests`
