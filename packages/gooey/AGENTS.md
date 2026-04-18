# gooey AGENTS

`gooey` provides terminal UI primitives on top of `tty` and `colors`.

## Rules

1. Keep the package focused on reusable view primitives, not application state machines.
2. Terminal control belongs in `tty`; higher-level app architecture belongs in `minttea`.
3. Favor small composable widgets over framework-like global behavior.
4. Treat Gooey as terminal-cell based: sizing, measurement, wrapping, and renderer behavior should align with visible terminal cell width.
5. Keep container helpers honest: `Element.row` and `Element.column` set direction only, while grow behavior must be explicit in style.
6. Prefer explicit, renderer-backed API surface. If a public style or render feature exists, tests should exercise the layout and terminal behavior it promises.

## Validate

`timeout 30 riot build gooey`
`timeout 60 riot test -p gooey`
`timeout 60 riot bench -p gooey --json`
