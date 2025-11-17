# URI Normalization Specification

## Executive Summary

**FROZEN FOREVER**: These normalization rules must NEVER change once data exists.

URI IDs are generated as `SHA-256(normalized_uri)`. Changing normalization breaks everything:
- Old facts use `SHA256(old_normalization(uri))`
- New facts use `SHA256(new_normalization(uri))`  
- Same logical URI → Two different entity IDs → CORRUPTION

**This document is the canonical, immutable specification.**

---

## Normalization Rules (RFC 3986 + Extensions)

### Rule 1: Case Normalization

**Scheme and host are case-insensitive** (RFC 3986 §6.2.2.1)

```
HTTP://Example.Com/Path  →  http://example.com/Path
```

**Implementation**:
```ocaml
let normalize_scheme_host uri =
  (* Lowercase scheme *)
  let scheme = String.lowercase_ascii (Uri.scheme uri) in
  (* Lowercase host *)
  let host = String.lowercase_ascii (Uri.host uri) in
  Uri.with_scheme uri scheme |> Uri.with_host host
```

### Rule 2: Percent-Encoding Normalization

**Decode unreserved characters** (RFC 3986 §2.3)

Unreserved: `A-Z a-z 0-9 - . _ ~`

```
http://example.com/%7Efoo  →  http://example.com/~foo
http://example.com/f%6F%6F  →  http://example.com/foo
```

**KEEP encoded**:
- Reserved characters: `: / ? # [ ] @ ! $ & ' ( ) * + , ; =`
- Characters outside unreserved set

**Implementation**:
```ocaml
let is_unreserved c =
  match c with
  | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

let normalize_percent_encoding s =
  (* Decode %XX where XX is unreserved *)
  (* Keep %XX where XX is reserved or non-ASCII *)
  decode_unreserved s
```

### Rule 3: Path Normalization

**Remove dot segments** (RFC 3986 §6.2.2.3)

```
http://example.com/a/./b     →  http://example.com/a/b
http://example.com/a/../b    →  http://example.com/b
http://example.com/a/b/..    →  http://example.com/a
```

**Remove trailing slash EXCEPT for root**:

```
http://example.com/path/     →  http://example.com/path
http://example.com/          →  http://example.com/   (keep!)
```

**Implementation**:
```ocaml
let normalize_path path =
  let segments = String.split_on_char '/' path in
  let rec remove_dots acc = function
    | [] -> List.rev acc
    | "." :: rest -> remove_dots acc rest
    | ".." :: rest ->
        (match acc with
        | [] -> remove_dots acc rest  (* Can't go above root *)
        | _ :: acc' -> remove_dots acc' rest)
    | seg :: rest -> remove_dots (seg :: acc) rest
  in
  let normalized = remove_dots [] segments in
  let joined = String.concat "/" normalized in
  (* Remove trailing slash unless it's root *)
  if String.length joined > 1 && String.ends_with ~suffix:"/" joined then
    String.sub joined 0 (String.length joined - 1)
  else
    joined
```

### Rule 4: Default Port Removal

**Remove default ports** (RFC 3986 §6.2.3)

```
http://example.com:80/path   →  http://example.com/path
https://example.com:443/path →  https://example.com/path
ftp://example.com:21/path    →  ftp://example.com/path
```

**Custom ports are preserved**:
```
http://example.com:8080/path →  http://example.com:8080/path
```

**Implementation**:
```ocaml
let default_ports = [
  ("http", 80);
  ("https", 443);
  ("ftp", 21);
  ("ssh", 22);
]

let normalize_port uri =
  match Uri.scheme uri, Uri.port uri with
  | Some scheme, Some port ->
      (match List.assoc_opt scheme default_ports with
      | Some default when port = default -> Uri.with_port uri None
      | _ -> uri)
  | _ -> uri
```

### Rule 5: Fragment Removal

**Always remove fragments** (not part of resource identity)

```
http://example.com/page#section  →  http://example.com/page
```

**Implementation**:
```ocaml
let normalize_fragment uri =
  Uri.with_fragment uri None
```

### Rule 6: Query Parameter Sorting

**Sort query parameters alphabetically** (for canonical form)

```
http://example.com/search?b=2&a=1  →  http://example.com/search?a=1&b=2
```

**Implementation**:
```ocaml
let normalize_query uri =
  match Uri.query uri with
  | [] -> uri
  | params ->
      let sorted = List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) params in
      Uri.with_query uri sorted
```

### Rule 7: UTF-8 NFC Normalization

**Normalize to NFC** (Unicode canonical composition)

```
"café" (decomposed: c a f e ́)  →  "café" (composed: c a f é)
```

**Implementation**: Requires `uunf` library (defer for MVP)

For MVP: **Assume all URIs are already NFC** or reject non-ASCII URIs.

---

## Combined Normalization Pipeline

**Apply rules in order**:

```ocaml
let normalize uri =
  uri
  |> normalize_scheme_host     (* Rule 1 *)
  |> normalize_percent_encoding (* Rule 2 *)
  |> normalize_path             (* Rule 3 *)
  |> normalize_port             (* Rule 4 *)
  |> normalize_fragment         (* Rule 5 *)
  |> normalize_query            (* Rule 6 *)
  (* Rule 7 deferred to post-MVP *)
```

---

## SHA-256 Hash Generation

```ocaml
let uri_to_id uri =
  let normalized = normalize uri in
  let canonical_string = Uri.to_string normalized in
  let hash = SHA256.hash (Bytes.of_string canonical_string) in
  (* Return first 8 bytes as int64 for fixed-width ID *)
  Int64.of_bytes hash 0
```

---

## Test Cases (Normalization Invariants)

```ocaml
let test_cases = [
  (* Case normalization *)
  ("HTTP://Example.COM/Path", "http://example.com/Path");
  
  (* Percent encoding *)
  ("http://example.com/%7Efoo", "http://example.com/~foo");
  ("http://example.com/f%6F%6F", "http://example.com/foo");
  
  (* Path normalization *)
  ("http://example.com/a/./b", "http://example.com/a/b");
  ("http://example.com/a/../b", "http://example.com/b");
  ("http://example.com/path/", "http://example.com/path");
  ("http://example.com/", "http://example.com/");  (* Keep root slash *)
  
  (* Default port removal *)
  ("http://example.com:80/path", "http://example.com/path");
  ("https://example.com:443/path", "https://example.com/path");
  ("http://example.com:8080/path", "http://example.com:8080/path");  (* Keep custom *)
  
  (* Fragment removal *)
  ("http://example.com/page#section", "http://example.com/page");
  
  (* Query sorting *)
  ("http://example.com/search?b=2&a=1", "http://example.com/search?a=1&b=2");
]
```

---

## Implementation Checklist

- [ ] Implement `Uri.normalize : Uri.t -> Uri.t`
- [ ] Implement each rule as separate function
- [ ] Add comprehensive test suite (all invariants)
- [ ] Add `Uri.to_id : Uri.t -> int64` (SHA-256 first 8 bytes)
- [ ] Document "FROZEN FOREVER" in code comments
- [ ] Verify all test cases pass
- [ ] Generate 1000 random URIs, verify normalization is idempotent

---

## Migration Strategy

### For New Databases

Just use normalized URIs from the start.

### For Existing Databases (IMPOSSIBLE)

**YOU CANNOT CHANGE NORMALIZATION RULES AFTER DATA EXISTS.**

If normalization needs to change:
1. Create new database
2. Re-ingest all facts with new normalization
3. Abandon old database

**This is why these rules are frozen forever.**

---

## Decision Log

| Rule | Rationale |
|------|-----------|
| Lowercase scheme/host | RFC 3986 §6.2.2.1 |
| Decode unreserved % | RFC 3986 §2.3 |
| Remove dot segments | RFC 3986 §6.2.2.3 |
| Remove default ports | RFC 3986 §6.2.3 |
| Remove fragments | Fragments ≠ resource identity |
| Sort query params | Canonical form for same query |
| UTF-8 NFC | Unicode canonical form |
| Defer NFC to v2 | Requires `uunf` library |

---

## Status: FROZEN ❄️

**These rules are now canonical and immutable.**

Any changes require a major version bump and full data migration.

