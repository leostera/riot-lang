open Std

(** {1 Token Generation} *)

(** Generate random bytes *)
let random_bytes = fun length ->
  let bytes = IO.Bytes.create ~size:length in
  for i = 0 to length - 1 do
    let byte = Random.int 256 |> Result.expect ~msg:"Failed to generate random byte" in
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
    IO.Bytes.set_unchecked hex ~at:(i * 2) ~char:(String.get_unchecked hex_chars ~at:(byte lsr 4));
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
        | (Option.Some hi, Option.Some lo) -> IO.Bytes.set_unchecked
          bytes
          ~at:i
          ~char:(Char.chr ((hi lsl 4) lor lo))
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

(** {1 Token Storage and Verification} *)

(** Session key for CSRF token *)
let csrf_token_key = "_csrf_token"

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
      (* Try unmasking first, fallback to direct comparison *)
      match unmask_token request_token with
      | Option.Some unmasked ->
          let matches = String.equal unmasked stored_token in
          if not matches then
            ()
          else
            ();
          matches
      | Option.None -> String.equal request_token stored_token

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
let middleware = fun ?(param_name = "_csrf_token") ?(header_name = "x-csrf-token") ?(skip_safe_methods = true) ?skip () ->
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
      (* Get session *)
      let session = Session.get conn in
      (* Get token from request (body_params, params, or header) *)
      let req_headers = Conn.headers conn in
      let request_token =
        (* Try body_params first (parsed by body_parser middleware) *)
        match Conn.body_params conn |> List.assoc_opt param_name with
        | Option.Some token -> Option.some token
        | Option.None -> (* Try URL parameters *)
          (
            match Conn.params conn |> List.assoc_opt param_name with
            | Option.Some token -> Option.some token
            | Option.None ->
                (* Try header as last resort *)
                Net.Http.Header.get req_headers header_name
          )
      in
      match request_token with
      | Option.None -> begin
          (* No token provided *)
          conn |> Conn.respond ~status:Net.Http.Status.Forbidden ~body:"CSRF token missing" |> Conn.halt
        end
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
    ~attrs:[ Component.type_ "hidden"; Component.name "_csrf_token"; Component.value masked; ]
    ()

(** Generate HTML meta tag for AJAX *)
let meta_tag = fun conn ->
  let token = get_token conn in
  let masked = mask_token token in
  Component.meta ~attrs:[ Component.name "csrf-token"; Component.attr "content" masked; ] ()
