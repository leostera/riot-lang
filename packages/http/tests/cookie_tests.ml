open Std
open Http

module Cookie = Http1.Cookie

let test_make_accepts_safe_cookie = fun _ctx ->
  match Cookie.make ~name:"session_id" ~value:"abc123" () with
  | Ok cookie ->
      if Cookie.to_set_cookie cookie != "session_id=abc123; Path=/; HttpOnly; SameSite=Lax" then
        Result.Error "validated cookie serialized with unexpected defaults"
      else
        Result.Ok ()
  | Error error -> Result.Error ("safe cookie rejected: " ^ Cookie.validation_error_to_string error)

let test_make_rejects_empty_name = fun _ctx ->
  match Cookie.make ~name:"" ~value:"abc123" () with
  | Error Cookie.EmptyName -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "empty cookie name was accepted"

let test_make_rejects_invalid_name_character = fun _ctx ->
  match Cookie.make ~name:"session id" ~value:"abc123" () with
  | Error (Cookie.InvalidNameCharacter { index = 7; character = ' ' }) -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "cookie name with a space was accepted"

let test_make_rejects_value_semicolon = fun _ctx ->
  match Cookie.make ~name:"session" ~value:"abc;123" () with
  | Error (
    Cookie.InvalidValueCharacter { index = 3; character = ';'; reason = Cookie.Semicolon }
  ) -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "cookie value with semicolon was accepted"

let test_make_rejects_value_delete_character = fun _ctx ->
  match Cookie.make ~name:"session" ~value:"abc\x7f123" () with
  | Error (
    Cookie.InvalidValueCharacter { index = 3; character = '\x7f'; reason = Cookie.DeleteCharacter }
  ) -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "cookie value with delete character was accepted"

let test_make_validated_alias_accepts_safe_cookie = fun _ctx ->
  match Cookie.make_validated ~name:"session_id" ~value:"abc123" () with
  | Ok _ -> Result.Ok ()
  | Error error ->
      Result.Error ("make_validated alias rejected safe cookie: "
      ^ Cookie.validation_error_to_string error)

let test_make_rejects_same_site_none_without_secure = fun _ctx ->
  match Cookie.make ~name:"session" ~value:"abc123" ~same_site:Cookie.None () with
  | Error Cookie.SameSiteNoneRequiresSecure -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "SameSite=None without Secure was accepted"

let test_make_rejects_secure_prefix_without_secure = fun _ctx ->
  match Cookie.make ~name:"__Secure-session" ~value:"abc123" () with
  | Error Cookie.SecurePrefixRequiresSecure -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "__Secure- cookie without Secure was accepted"

let test_make_rejects_host_prefix_with_domain = fun _ctx ->
  match Cookie.make ~name:"__Host-session" ~value:"abc123" ~secure:true ~domain:"example.com" () with
  | Error Cookie.HostPrefixRequiresNoDomain -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "__Host- cookie with Domain was accepted"

let test_make_rejects_host_prefix_non_root_path = fun _ctx ->
  match Cookie.make ~name:"__Host-session" ~value:"abc123" ~secure:true ~path:"/app" () with
  | Error Cookie.HostPrefixRequiresRootPath -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "__Host- cookie without Path=/ was accepted"

let test_make_rejects_path_semicolon = fun _ctx ->
  match Cookie.make ~name:"session" ~value:"abc123" ~path:"/app;admin" () with
  | Error (
    Cookie.InvalidAttributeCharacter {
      attribute = Cookie.Path;
      index = 4;
      character = ';';
      reason = Cookie.AttributeSemicolon
    }
  ) -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "cookie path with semicolon was accepted"

let test_parse_set_cookie_accepts_safe_cookie = fun _ctx ->
  match Cookie.parse_set_cookie "session=abc123; Max-Age=3600; Path=/app; Secure; SameSite=None" with
  | Some cookie ->
      if cookie.name != "session" then
        Result.Error "parsed cookie had wrong name"
      else if cookie.value != "abc123" then
        Result.Error "parsed cookie had wrong value"
      else if cookie.max_age != Some 3_600 then
        Result.Error "parsed cookie had wrong Max-Age"
      else if cookie.path != "/app" then
        Result.Error "parsed cookie had wrong Path"
      else if not cookie.secure then
        Result.Error "parsed cookie did not set Secure"
      else if cookie.same_site != Some Cookie.None then
        Result.Error "parsed cookie had wrong SameSite"
      else
        Result.Ok ()
  | Option.None -> Result.Error "safe Set-Cookie header was rejected"

let test_parse_set_cookie_result_accepts_safe_cookie = fun _ctx ->
  match Cookie.parse_set_cookie_result
    "session=abc123; Max-Age=3600; Path=/app; Secure; SameSite=None" with
  | Ok cookie ->
      if cookie.name != "session" then
        Result.Error "parsed cookie had wrong name"
      else if cookie.max_age != Some 3_600 then
        Result.Error "parsed cookie had wrong Max-Age"
      else
        Result.Ok ()
  | Error error ->
      Result.Error ("safe Set-Cookie header was rejected: "
      ^ Cookie.parse_set_cookie_error_to_string error)

let test_parse_set_cookie_rejects_header_injection_value = fun _ctx ->
  match Cookie.parse_set_cookie "session=abc\r\nSet-Cookie: evil=1; Path=/" with
  | Option.None -> Result.Ok ()
  | Some _ -> Result.Error "Set-Cookie value with CRLF was accepted"

let test_parse_set_cookie_result_reports_header_injection_value = fun _ctx ->
  match Cookie.parse_set_cookie_result "session=abc\r\nSet-Cookie: evil=1; Path=/" with
  | Error (
    Cookie.InvalidCookie (
      Cookie.InvalidValueCharacter { index = 3; character = '\r'; reason = Cookie.ControlCharacter }
    )
  ) -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong parse error: " ^ Cookie.parse_set_cookie_error_to_string error)
  | Ok _ -> Result.Error "Set-Cookie value with CRLF was accepted"

let test_parse_set_cookie_rejects_same_site_none_without_secure = fun _ctx ->
  match Cookie.parse_set_cookie "session=abc; SameSite=None" with
  | Option.None -> Result.Ok ()
  | Some _ -> Result.Error "SameSite=None without Secure was accepted"

let test_parse_set_cookie_result_reports_invalid_max_age = fun _ctx ->
  match Cookie.parse_set_cookie_result "session=abc; Max-Age=forever; Path=/" with
  | Error (Cookie.InvalidMaxAge (
    Cookie.InvalidMaxAgeCharacter { code; index = 0 }
  )) when code = Char.to_int 'f' -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong parse error: " ^ Cookie.parse_set_cookie_error_to_string error)
  | Ok _ -> Result.Error "Set-Cookie with invalid Max-Age was accepted"

let test_parse_set_cookie_result_reports_empty_max_age = fun _ctx ->
  match Cookie.parse_set_cookie_result "session=abc; Max-Age=; Path=/" with
  | Error (Cookie.InvalidMaxAge Cookie.EmptyMaxAge) -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong parse error: " ^ Cookie.parse_set_cookie_error_to_string error)
  | Ok _ -> Result.Error "Set-Cookie with empty Max-Age was accepted"

let test_parse_set_cookie_result_reports_negative_max_age = fun _ctx ->
  match Cookie.parse_set_cookie_result "session=abc; Max-Age=-1; Path=/" with
  | Error (Cookie.InvalidMaxAge Cookie.NegativeMaxAge) -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong parse error: " ^ Cookie.parse_set_cookie_error_to_string error)
  | Ok _ -> Result.Error "Set-Cookie with negative Max-Age was accepted"

let test_parse_set_cookie_result_reports_max_age_overflow = fun _ctx ->
  let header = "session=abc; Max-Age=" ^ String.make ~len:32 ~char:'9' ^ "; Path=/" in
  match Cookie.parse_set_cookie_result header with
  | Error (Cookie.InvalidMaxAge Cookie.MaxAgeOverflow) -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong parse error: " ^ Cookie.parse_set_cookie_error_to_string error)
  | Ok _ -> Result.Error "Set-Cookie with overflowing Max-Age was accepted"

let test_parse_set_cookie_result_reports_invalid_same_site = fun _ctx ->
  match Cookie.parse_set_cookie_result "session=abc; SameSite=Maybe" with
  | Error (Cookie.InvalidSameSite (Cookie.UnknownSameSite { value = "Maybe" })) -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong parse error: " ^ Cookie.parse_set_cookie_error_to_string error)
  | Ok _ -> Result.Error "Set-Cookie with invalid SameSite was accepted"

let test_parse_set_cookie_result_reports_empty_same_site = fun _ctx ->
  match Cookie.parse_set_cookie_result "session=abc; SameSite=" with
  | Error (Cookie.InvalidSameSite Cookie.EmptySameSite) -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong parse error: " ^ Cookie.parse_set_cookie_error_to_string error)
  | Ok _ -> Result.Error "Set-Cookie with empty SameSite was accepted"

let test_parse_set_cookie_result_reports_missing_separator = fun _ctx ->
  match Cookie.parse_set_cookie_result "session; Path=/" with
  | Error Cookie.MissingNameValueSeparator -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong parse error: " ^ Cookie.parse_set_cookie_error_to_string error)
  | Ok _ -> Result.Error "Set-Cookie without name=value was accepted"

let tests =
  Test.[
    case "make accepts safe cookie" test_make_accepts_safe_cookie;
    case "make rejects empty name" test_make_rejects_empty_name;
    case "make rejects invalid name character" test_make_rejects_invalid_name_character;
    case "make rejects value semicolon" test_make_rejects_value_semicolon;
    case "make rejects value delete character" test_make_rejects_value_delete_character;
    case "make_validated alias accepts safe cookie" test_make_validated_alias_accepts_safe_cookie;
    case "make rejects SameSite None without Secure" test_make_rejects_same_site_none_without_secure;
    case "make rejects Secure prefix without Secure" test_make_rejects_secure_prefix_without_secure;
    case "make rejects Host prefix with Domain" test_make_rejects_host_prefix_with_domain;
    case "make rejects Host prefix non-root Path" test_make_rejects_host_prefix_non_root_path;
    case "make rejects Path semicolon" test_make_rejects_path_semicolon;
    case "parse Set-Cookie accepts safe cookie" test_parse_set_cookie_accepts_safe_cookie;
    case
      "parse Set-Cookie result accepts safe cookie"
      test_parse_set_cookie_result_accepts_safe_cookie;
    case
      "parse Set-Cookie rejects header injection value"
      test_parse_set_cookie_rejects_header_injection_value;
    case
      "parse Set-Cookie result reports header injection value"
      test_parse_set_cookie_result_reports_header_injection_value;
    case
      "parse Set-Cookie rejects SameSite None without Secure"
      test_parse_set_cookie_rejects_same_site_none_without_secure;
    case
      "parse Set-Cookie result reports invalid Max-Age"
      test_parse_set_cookie_result_reports_invalid_max_age;
    case
      "parse Set-Cookie result reports empty Max-Age"
      test_parse_set_cookie_result_reports_empty_max_age;
    case
      "parse Set-Cookie result reports negative Max-Age"
      test_parse_set_cookie_result_reports_negative_max_age;
    case
      "parse Set-Cookie result reports Max-Age overflow"
      test_parse_set_cookie_result_reports_max_age_overflow;
    case
      "parse Set-Cookie result reports invalid SameSite"
      test_parse_set_cookie_result_reports_invalid_same_site;
    case
      "parse Set-Cookie result reports empty SameSite"
      test_parse_set_cookie_result_reports_empty_same_site;
    case
      "parse Set-Cookie result reports missing separator"
      test_parse_set_cookie_result_reports_missing_separator;
  ]

let main ~args:_ = Test.Cli.main ~name:"http:cookie" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
