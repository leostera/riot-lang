# mime

MIME parsing and rendering helpers.

`mime` is a focused package for dealing with media types, parameters, and MIME
syntax. It is useful anywhere Riot code needs to understand or produce content
types cleanly instead of passing raw strings around.

## Install

```sh
riot add mime
```

## Typical uses

- parsing `Content-Type` headers in HTTP code;
- rendering MIME values back to strings;
- working with parameters such as `charset=utf-8` or multipart boundaries;
- centralizing MIME handling in clients, servers, and tooling.

## Example

```ocaml
open Std
open Mime

let headers = [
  ("Content-Type", "text/plain");
  ("Content-Transfer-Encoding", "base64");
] in

let body = "SGVsbG8gV29ybGQ=" in

match Mime.parse ~headers ~body with
| Ok (SinglePart part) ->
    let decoded = Mime.get_decoded_content part |> Result.expect ~msg:"example should decode" in
    println decoded
| Ok (MultiPart _) ->
    panic "expected a single part"
| Error err ->
    panic err
```

A runnable example is included:

```sh
riot run -p mime decode_attachment
```

## Where to look next

- `src/Mime.mli` is the public API.
- `tests/rfc2231_tests.ml` covers parameter-handling edge cases.
- the RFC notes in `src/` explain the standards the implementation is following.
