# mcp AGENTS

`mcp` owns Model Context Protocol types and transport helpers.

## Rules

1. Keep MCP-specific modeling here. Do not reintroduce tusk-specific coupling.
2. Favor clear protocol types and capability boundaries over dynamic dictionaries.
3. Re-check `jsonrpc` interactions when changing message shapes.

## Validate

`timeout 30 tusk build mcp`
