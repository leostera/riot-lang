# MIME Implementation Guide

Based on RFC 2045, RFC 2231, and RFC 6532

## Key Concepts from RFCs

### RFC 2045 - Core MIME Structure

1. **Content-Type Header** (Section 5)
   - Format: `type/subtype; param1=value1; param2=value2`
   - Types: text, image, audio, video, application, message, multipart
   - Parameters are case-insensitive names with case-sensitive values
   - Boundary parameter required for multipart types

2. **Content-Transfer-Encoding** (Section 6)
   - Values: 7bit, 8bit, binary, quoted-printable, base64
   - Multipart MUST only use 7bit, 8bit, or binary (no QP or base64 on multipart itself)
   - Individual parts within multipart can use any encoding

3. **Multipart Structure** (Section 7 of RFC 2046, referenced)
   - Delimiter: `--{boundary}`
   - End delimiter: `--{boundary}--`
   - Each part has its own headers and body
   - Preamble before first boundary and epilogue after final boundary should be ignored

### RFC 2231 - Parameter Extensions

1. **Parameter Continuations** (Section 3)
   - Long parameters split with `*0`, `*1`, `*2`, etc.
   - Example: `filename*0="first"; filename*1="second"`
   - Values concatenated in order

2. **Character Set and Language** (Section 4)
   - Format: `param*=charset'language'encoded-value`
   - Example: `title*=utf-8'en'Hello%20World`
   - Percent encoding for non-ASCII octets
   - Single quotes MUST be present even if charset/language blank

3. **Combined Continuations + Encoding** (Section 4.1)
   - Format: `param*0*=charset'lang'value; param*1*=value; param*2=value`
   - Only first segment has charset/language
   - Can mix encoded and unencoded segments

### RFC 6532 - Internationalization

1. **UTF-8 in Headers**
   - Allows direct UTF-8 in header values (not just encoded-words)
   - Applies to atoms, quoted strings, domains
   - NFC normalization recommended

2. **message/global Media Type**
   - Like message/rfc822 but allows UTF-8
   - Can have content-transfer-encoding applied

## Current Implementation Status

### ✅ Implemented

1. **Typed Header API**
   - Type-safe header variants (ContentType, ContentDisposition, etc.)
   - Pattern matching with exhaustiveness checks
   - Automatic parsing from raw string headers
   - Content-Type with media_type, subtype, and parameters
   - Content-Disposition (Inline | Attachment) with optional filename
   - Content-Transfer-Encoding variants

2. **Parameter Parsing** (RFC 2231)
   - ✅ Handle parameter continuations (`*0`, `*1`, etc.)
   - ✅ Decode charset/language parameters (`param*=`)
   - ✅ Percent-decode encoded values
   - ✅ Handle quoted vs unquoted values correctly
   - ✅ Combine continuation segments using HashMap

3. **Content-Transfer-Encoding**
   - ✅ Decode based on Content-Transfer-Encoding header
   - ✅ Support quoted-printable with soft line breaks
   - ✅ Support base64 with whitespace handling
   - ✅ Handle 7bit/8bit/binary (pass-through)
   - ✅ Type-safe encoding detection via variants

4. **Multipart Handling**
   - ✅ Handle preamble/epilogue correctly
   - ✅ Recursive parsing of nested multipart
   - ✅ Graceful handling of missing end delimiter
   - ✅ Filter empty parts from output

5. **Attachment Detection**
   - ✅ Content-Disposition variant-based detection
   - ✅ Parse disposition parameters (filename, etc.)
   - ✅ Handle RFC 2231 encoded filenames
   - ✅ Support filename* parameter with charset

### ⚠️ Future Enhancements

1. **Header Parsing**
   - [ ] Properly unfold header continuation lines
   - [ ] Handle comments in header values (RFC 5322 CFWS)
   - [ ] Parse more structured headers (MIME-Version, etc.)

2. **Validation**
   - [ ] Validate boundary format per RFC
   - [ ] Validate media type format
   - [ ] Warn on malformed headers

## Architecture Notes

- **No dependency on email package** - Works with raw headers/body
- **Delegates encoding** - Use existing `encoding` package for base64/QP
- **Standalone** - Can be used by HTTP multipart, email, etc.

## Example Usage

```ocaml
(* Parse a MIME message *)
let headers = [("Content-Type", "multipart/mixed; boundary=foo")] in
let body = "--foo\r\n..." in
match Mime.parse ~headers ~body with
| Ok (Mime.MultiPart { boundary; parts }) ->
    (* Extract attachments *)
    let attachments = Mime.attachments mime in
    List.iter (fun part ->
      match Mime.get_filename part with
      | Some name -> println "Attachment: %s" name
      | None -> ()
    ) attachments
| _ -> ()
```

## Next Steps

1. Implement RFC 2231 parameter parsing
2. Add Content-Transfer-Encoding decoding
3. Improve multipart boundary handling
4. Add comprehensive tests for RFC edge cases
5. Consider supporting message/global (RFC 6532)
