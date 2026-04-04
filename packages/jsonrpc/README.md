# jsonrpc

JSON-RPC framing, codecs, and client/server helpers.

`jsonrpc` gives Riot tooling a typed home for JSON-RPC messages, request/response
handling, and transport-facing helpers. It is useful for editor integrations,
language tooling, daemon protocols, and internal RPC surfaces that want a
simple JSON-RPC 2.0 contract.

## Install

```sh
riot add jsonrpc
```

## Use `jsonrpc` when

- you are building a protocol that already speaks JSON-RPC 2.0;
- you need typed request, response, notification, and error shapes;
- you want shared framing logic for both client and server code;
- you are working on adjacent packages such as `lsp` or `mcp`.

## What you get

- core JSON-RPC types and encoding/decoding;
- client and server helpers built around the same message model;
- a small surface you can compose into bigger tools without bringing in a full
  transport stack.

## Example

```ocaml
open Std
open Jsonrpc

let request =
  Jsonrpc.request
    ~method_:"ping"
    ~params:(Named [ ("client", Data.Json.string "riot") ])
    ~id:(Number 1)
    ()

let json = Jsonrpc.request_to_json request
let () = println (Data.Json.to_string_pretty json)
```

A runnable example is included:

```sh
riot run -p jsonrpc request_json
```

## Related packages

- `lsp` builds the Language Server Protocol on top of the same underlying
  message shape.
- `mcp` solves a similar problem for the Model Context Protocol.
