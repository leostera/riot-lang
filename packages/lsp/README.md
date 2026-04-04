# lsp

Language Server Protocol types and codecs.

`lsp` is Riot's typed vocabulary for speaking the Language Server Protocol. It
gives you the request, notification, and payload types needed to build or test
LSP-compatible tools without hard-coding raw JSON shapes everywhere.

## Install

```sh
riot add lsp
```

## When to use it

Use `lsp` when you need:

- typed LSP request and notification payloads;
- codecs for turning those payloads into wire messages;
- a reusable protocol layer that can be shared by servers, tests, and editor
  integrations.

If you want the actual Riot language server, use `riot-lsp`. This package is
the protocol substrate below it.

## Where to start

- `src/lsp.ml` is the public entrypoint.
- `tests/protocol_fixture_tests.ml` shows the intended wire shapes.
- `tests/utf16_tests.ml` covers the text-position semantics editors care about.
