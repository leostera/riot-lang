open Std

(** {1 Token Generation} *)

let secure_equal = fun s1 s2 ->
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  let result = ref 0 in
  for i = 0 to max len1 len2 - 1 do
    let c1 =
      if i < len1 then
        Char.code (String.get_unchecked s1 ~at:i)
      else
        0
    in
    let c2 =
      if i < len2 then
        Char.code (String.get_unchecked s2 ~at:i)
      else
        0
    in
    result := !result lor (c1 lxor c2)
  done;
  result := !result lor (len1 lxor len2);
  !result = 0

let fresh_rng = fun () ->
  match Random.Rng.standard () with
  | Ok rng -> rng
  | Error error ->
      panic ("Failed to initialize CSRF random generator: " ^ Random.error_to_string error)

(** Generate random bytes *)
let random_bytes = fun length ->
  let rng = fresh_rng () in
  let bytes = IO.Bytes.create ~size:length in
  for i = 0 to length - 1 do
    let byte =
      Random.int ~rng 256
      |> Result.expect ~msg:"Failed to generate CSRF random byte"
    in
    IO.Bytes.set_unchecked bytes ~at:i ~char:(Char.chr byte)
  done;
  IO.Bytes.to_string bytes

(** Convert bytes to hex string *)
let bytes_to_hex = fun bytes ->
  let hex_chars = "0123456789abcdef" in
  let len = String.length bytes in
  let hex = IO.Bytes.create ~size:(len * 2) in
  for i = 0 to len - 1 do
    let byte = Char.code (String.get_unchecked bytes ~at:i) in
    IO.Bytes.set_unchecked
      hex
      ~at:(i * 2)
      ~char:(String.get_unchecked hex_chars ~at:(byte lsr 4));
    IO.Bytes.set_unchecked
      hex
      ~at:(i * 2 + 1)
      ~char:(String.get_unchecked hex_chars ~at:(byte land 0x0f))
  done;
  IO.Bytes.to_string hex

(** Convert hex string to bytes *)
let hex_to_bytes = fun hex ->
  let len = String.length hex in
  if not (len mod 2 = 0) then
    Option.none
  else
    let hex_value c =
      match c with
      | '0' .. '9' -> Option.some (Char.code c - Char.code '0')
      | 'a' .. 'f' -> Option.some (Char.code c - Char.code 'a' + 10)
      | 'A' .. 'F' -> Option.some (Char.code c - Char.code 'A' + 10)
      | _ -> Option.none
    in
    let bytes = IO.Bytes.create ~size:(len / 2) in
    let valid = ref true in
    for i = 0 to (len / 2) - 1 do
      if !valid then
        match (
          hex_value (String.get_unchecked hex ~at:(i * 2)),
          hex_value (String.get_unchecked hex ~at:(i * 2 + 1))
        ) with
        | (Option.Some hi, Option.Some lo) ->
            IO.Bytes.set_unchecked bytes ~at:i ~char:(Char.chr ((hi lsl 4) lor lo))
        | _ -> valid := false
    done;
  if !valid then
    Option.some (IO.Bytes.to_string bytes)
  else
    Option.none

(** Generate a cryptographically random CSRF token (32 bytes as 64 hex chars) *)
let generate_token = fun () -> bytes_to_hex (random_bytes 32)

(** {1 Token Masking (BREACH Attack Protection)} *)

(** Mask token to prevent BREACH attacks *)
let mask_token = fun raw_token_hex ->
  match hex_to_bytes raw_token_hex with
  | Option.None -> raw_token_hex
  | Option.Some raw_bytes ->
      (* Generate 32-byte one-time pad *)
      let pad = random_bytes 32 in
      (* XOR pad with raw token bytes *)
      let masked = IO.Bytes.create ~size:32 in
      for i = 0 to 31 do
        let pad_byte = Char.code (String.get_unchecked pad ~at:i) in
        let raw_byte = Char.code (String.get_unchecked raw_bytes ~at:i) in
        IO.Bytes.set_unchecked masked ~at:i ~char:(Char.chr (pad_byte lxor raw_byte))
      done;
      (* Combine pad + masked (64 bytes total) and base64 encode *)
      let combined = pad ^ IO.Bytes.to_string masked in
      Encoding.Base64.encode combined

(** Unmask token received from client *)
let unmask_token = fun masked_b64 ->
  match Encoding.Base64.decode masked_b64 with
  | Result.Error `Invalid_base64 -> Option.none
  | Result.Ok decoded ->
      let len = String.length decoded in
      if not (len = 64) then
        begin
          Option.none
        end
      else
        (* Split into pad and masked parts (32 bytes each) *)
        let pad = String.sub decoded ~offset:0 ~len:32 in
        let masked = String.sub decoded ~offset:32 ~len:32 in
        (* XOR to recover original bytes *)
        let raw_bytes = IO.Bytes.create ~size:32 in
        for i = 0 to 31 do
          let pad_byte = Char.code (String.get_unchecked pad ~at:i) in
          let masked_byte = Char.code (String.get_unchecked masked ~at:i) in
          IO.Bytes.set_unchecked raw_bytes ~at:i ~char:(Char.chr (pad_byte lxor masked_byte))
        done;
      (* Convert back to hex *)
      let unmasked_hex = bytes_to_hex (IO.Bytes.to_string raw_bytes) in
      Option.some unmasked_hex

let is_hex_char = function
  | '0' .. '9'
  | 'a' .. 'f'
  | 'A' .. 'F' -> true
  | _ -> false

let is_raw_token = fun token -> String.length token = 64 && String.for_all token ~fn:is_hex_char

(** {1 Token Storage and Verification} *)

(** Session key for CSRF token *)
let csrf_token_key = "_csrf_token"

let missing_session_body =
  "CSRF middleware requires Suri.Middleware.Session.middleware to run before it"

let missing_session = fun conn ->
  conn
  |> Conn.respond ~status:Net.Http.Status.InternalServerError ~body:missing_session_body
  |> Conn.halt

(** Get or create token from session *)
let get_or_create_token = fun session ->
  match Session.get_value csrf_token_key session with
  | Option.Some token -> token
  | Option.None ->
      let token = generate_token () in
      Session.put csrf_token_key token session;
      token

(** Verify token from request matches session *)
let verify_token = fun session request_token ->
  match Session.get_value csrf_token_key session with
  | Option.None -> false
  | Option.Some stored_token ->
      is_raw_token stored_token && match unmask_token request_token with
      | Option.Some unmasked -> is_raw_token unmasked && secure_equal unmasked stored_token
      | Option.None -> is_raw_token request_token && secure_equal request_token stored_token

(** {1 HTTP Method Classification} *)

(** Check if request method is safe (no CSRF protection needed) *)
let is_safe_method = fun method_ ->
  match method_ with
  | Net.Http.Method.Get
  | Head
  | Options -> true
  | _ -> false

(** {1 Middleware} *)

(** CSRF protection middleware *)
let middleware = fun
  ?(param_name = "_csrf_token")
  ?(header_name = "x-csrf-token")
  ?(skip_safe_methods = true)
  ?skip
  () ->
  fun ~conn ~next ->
    (* Check if we should skip CSRF for this request *)
    let should_skip =
      match skip with
      | Option.Some f -> f conn
      | Option.None -> false
    in
    if should_skip then
      next conn
      (* Skip safe methods if configured *)
    else if skip_safe_methods && is_safe_method (Conn.method_ conn) then
      next conn
    else
      match Session.find conn with
      | Option.None -> missing_session conn
      | Option.Some session ->
          (* Get token from request (body_params, params, or header) *)
          let req_headers = Conn.headers conn in
          let request_token =
            (* Try body_params first (parsed by body_parser middleware) *)
            match Std.Collections.Proplist.get (Conn.body_params conn) ~key:param_name with
            | Option.Some token -> Option.some token
            | Option.None -> (
                match Std.Collections.Proplist.get (Conn.params conn) ~key:param_name with
                | Option.Some token -> Option.some token
                | Option.None ->
                    (* Try header as last resort *)
                    Net.Http.Header.get req_headers header_name
              )
          in
          match request_token with
          | Option.None -> (
              (* No token provided *)
              conn
              |> Conn.respond ~status:Net.Http.Status.Forbidden ~body:"CSRF token missing"
              |> Conn.halt
            )
          | Option.Some token ->
              (* Verify token *)
              if verify_token session token then
                begin
                  (* Valid token - continue *)
                  next conn
                end
              else
                begin
                  (* Invalid token *)
                  conn
                  |> Conn.respond ~status:Net.Http.Status.Forbidden ~body:"CSRF token invalid"
                  |> Conn.halt
                end

(** {1 View Helpers} *)

(** Get current CSRF token for use in views *)
let get_token = fun conn ->
  let session = Session.get conn in
  get_or_create_token session

(** Generate HTML hidden field for forms *)
let hidden_field = fun conn ->
  let token = get_token conn in
  let masked = mask_token token in
  Component.input
    ~attrs:[ Component.type_ "hidden"; Component.name "_csrf_token"; Component.value masked ]
    ()

(** Generate HTML meta tag for AJAX *)
let meta_tag = fun conn ->
  let token = get_token conn in
  let masked = mask_token token in
  Component.meta ~attrs:[ Component.name "csrf-token"; Component.attr "content" masked ] ()

module For_testing = struct
  let generate_token = generate_token

  let mask_token = mask_token

  let unmask_token = unmask_token

  let is_raw_token = is_raw_token

  let secure_equal = secure_equal

  let missing_session_body = missing_session_body
end
