# minttea AGENTS

`minttea` is the Elm-style TUI framework layer.

## Rules

1. The model-update-view loop is the core contract. Preserve that shape when refactoring.
2. UI primitives belong in `gooey`; terminal mechanics belong in `tty`.
3. Model commands and side effects explicitly outside rendering code.
