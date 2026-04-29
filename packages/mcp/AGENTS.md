# mcp AGENTS

`mcp` owns Model Context Protocol types and transport helpers.

## Rules

1. Keep MCP-specific modeling here and route Riot-specific behavior to caller packages.
2. Favor clear protocol types and capability boundaries over dynamic dictionaries.
3. Re-check `jsonrpc` interactions when changing message shapes.
