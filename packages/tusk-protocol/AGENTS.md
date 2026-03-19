# tusk-protocol AGENTS

`tusk-protocol` is the remaining protocol-shaped layer in tusk.

## Rules

1. Keep this package small. It should justify its existence with shared types or helpers, not historical transport baggage.
2. If a type is only used locally by one package, move it closer to that package instead of growing this one.
3. Prefer simplification over adding new abstractions here.

## Validate

`timeout 30 tusk build tusk-protocol`
