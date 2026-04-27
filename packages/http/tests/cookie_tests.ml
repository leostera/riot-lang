open Std
open Http

module Cookie = Http1.Cookie

let test_make_validated_accepts_safe_cookie = fun _ctx ->
  match Cookie.make_validated ~name:"session_id" ~value:"abc123" () with
  | Ok cookie ->
      if Cookie.to_set_cookie cookie != "session_id=abc123; HttpOnly; SameSite=Lax" then
        Result.Error "validated cookie serialized with unexpected defaults"
      else
        Result.Ok ()
  | Error error -> Result.Error ("safe cookie rejected: " ^ Cookie.validation_error_to_string error)

let test_make_validated_rejects_empty_name = fun _ctx ->
  match Cookie.make_validated ~name:"" ~value:"abc123" () with
  | Error Cookie.EmptyName -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "empty cookie name was accepted"

let test_make_validated_rejects_invalid_name_character = fun _ctx ->
  match Cookie.make_validated ~name:"session id" ~value:"abc123" () with
  | Error (Cookie.InvalidNameCharacter { index = 7; character = ' ' }) -> Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "cookie name with a space was accepted"

let test_make_validated_rejects_value_semicolon = fun _ctx ->
  match Cookie.make_validated ~name:"session" ~value:"abc;123" () with
  | Error (Cookie.InvalidValueCharacter { index = 3; character = ';'; reason = Cookie.Semicolon }) ->
      Result.Ok ()
  | Error error ->
      Result.Error ("wrong validation error: " ^ Cookie.validation_error_to_string error)
  | Ok _ -> Result.Error "cookie value with semicolon was accepted"

let tests =
  Test.[
    case "make_validated accepts safe cookie" test_make_validated_accepts_safe_cookie;
    case "make_validated rejects empty name" test_make_validated_rejects_empty_name;
    case
      "make_validated rejects invalid name character"
      test_make_validated_rejects_invalid_name_character;
    case "make_validated rejects value semicolon" test_make_validated_rejects_value_semicolon;
  ]

let main ~args:_ = Test.Cli.main ~name:"http:cookie" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
