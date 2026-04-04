# mcp

Model Context Protocol transports and types.

`mcp` provides Riot's typed surface for speaking MCP. It is the package you use
when you want to model MCP requests, responses, capabilities, and transport
messages in OCaml instead of treating the protocol as an untyped JSON blob.

## Install

```sh
riot add mcp
```

## What it is for

- building MCP servers or clients in Riot;
- sharing protocol types between transport code and application logic;
- writing tests against real protocol shapes instead of ad-hoc JSON.

## What it is not

`mcp` is not an agent framework by itself. It gives you the protocol language.
You still need to decide how you host tools, route requests, and integrate with
your application.

## Related packages

- `jsonrpc` is the closest neighbor conceptually.
- `lsp` is another example of Riot packaging a protocol as typed OCaml data
  rather than raw JSON.
