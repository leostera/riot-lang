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
  | Error (Cookie.InvalidValueCharacter { index = 3; character = ';'; reason = Cookie.Semicolon }) ->
      Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "cookie value with semicolon was accepted"

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
  | Error (Cookie.InvalidAttributeCharacter {
    attribute = Cookie.Path;
    index = 4;
    character = ';';
    reason = Cookie.AttributeSemicolon
  }) -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "cookie path with semicolon was accepted"

let tests =
  Test.[
    case "make accepts safe cookie" test_make_accepts_safe_cookie;
    case "make rejects empty name" test_make_rejects_empty_name;
    case "make rejects invalid name character" test_make_rejects_invalid_name_character;
    case "make rejects value semicolon" test_make_rejects_value_semicolon;
    case "make_validated alias accepts safe cookie" test_make_validated_alias_accepts_safe_cookie;
    case "make rejects SameSite None without Secure" test_make_rejects_same_site_none_without_secure;
    case "make rejects Secure prefix without Secure" test_make_rejects_secure_prefix_without_secure;
    case "make rejects Host prefix with Domain" test_make_rejects_host_prefix_with_domain;
    case "make rejects Host prefix non-root Path" test_make_rejects_host_prefix_non_root_path;
    case "make rejects Path semicolon" test_make_rejects_path_semicolon;
  ]

let main ~args:_ = Test.Cli.main ~name:"http:cookie" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
