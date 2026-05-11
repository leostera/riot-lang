# Changelog

All notable changes to `fixme` are documented here.

## 0.0.26 - 2026-04-28

### Changed

- Rule authors now get `Syn.Ast`-based traversal and matching helpers for expressions, patterns, let bindings, parameters, match cases, applications, identifiers, and spans.
- Provider rules use the same typed traversal shape as built-in rules, which removes the old dependency on the removed CST API and makes custom rule providers easier to keep compatible with Syn.
- Rule helpers expose source spans directly from typed Ast handles, so diagnostics and fixes no longer need to recover locations by scanning raw syntax nodes.
