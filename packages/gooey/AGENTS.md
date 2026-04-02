# gooey AGENTS

`gooey` provides terminal UI primitives on top of `tty` and `colors`.

## Rules

1. Keep the package focused on reusable view primitives, not application state machines.
2. Terminal control belongs in `tty`; higher-level app architecture belongs in `minttea`.
3. Favor small composable widgets over framework-like global behavior.

## Validate

`timeout 30 riot build gooey`
