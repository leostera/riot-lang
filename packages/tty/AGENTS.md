# tty AGENTS

`tty` owns terminal control primitives.

## Rules

1. Keep low-level control sequences and terminal capability behavior here.
2. Rendering policy belongs in `gooey` or `minttea`, not in this package.
3. Favor deterministic output and narrow abstractions.

## Validate

- `./riot-old build tty`
- `./riot-old test -p tty --json`
- `./riot-old bench -p tty --json`
