# minttea AGENTS

`minttea` is the Elm-style TUI framework layer.

## Rules

1. The model-update-view loop is the core contract. Preserve that shape when refactoring.
2. UI primitives belong in `gooey`; terminal mechanics belong in `tty`.
3. Keep commands and side effects explicit rather than hidden in rendering code.

## Validate

`timeout 30 tusk build minttea`
