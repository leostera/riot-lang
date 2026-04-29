# pretext AGENTS

`pretext` owns the tiny text-only pretty-printing document model and layout engine.

## Rules

1. Keep `pretext` pure and text-first. Browser, terminal-renderer, and font-measurement concerns belong in downstream renderers.
2. Measure user-visible width with Unicode-aware display columns (`Std.String.width`), not UTF-8 byte counts.
3. Keep the public surface tiny and combinator-oriented. Add new primitives only when a concrete formatting shape cannot be expressed with the existing ones.
4. Root-level formatting implicitly groups the incoming document so `[str "hello"; brk; str "world"]` can stay inline when it fits.
5. Hard line breaks must remain explicit and stable; width heuristics should preserve `line` / `hardline`.
6. Prefer deterministic layout over clever heuristics. If a formatting choice is surprising, make it explicit in the document structure.
