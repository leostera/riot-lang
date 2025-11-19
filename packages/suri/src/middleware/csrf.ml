open Std

(** {1 Token Generation} *)

(** Generate random hex string of given length *)
let random_hex_string length =
  let hex_chars = "0123456789abcdef" in
  let chars = ref [] in
  for _ = 1 to length * 2 do  (* Each byte = 2 hex chars *)
    let idx = Random.int 16 in
    let c = String.get hex_chars idx in
    chars := c :: !chars
  done;
  String.of_seq (List.to_seq (List.rev !chars))

(** Generate a cryptographically random CSRF token (64 hex chars = 32 bytes) *)
let generate_token () =
  random_hex_string 32

(** {1 Token Masking (BREACH Attack Protection)} *)

(** Mask token to prevent BREACH attacks *)
let mask_token raw_token =
  (* Generate one-time pad (32 bytes = 64 hex chars) *)
  let pad = random_hex_string 32 in
  
  (* Convert hex strings to char lists for XOR *)
  let pad_chars = String.to_seq pad |> List.of_seq in
  let raw_chars = String.to_seq raw_token |> List.of_seq in
  
  (* XOR each character *)
  let masked_chars = 
    List.map2 (fun p r -> 
      Char.chr (Char.code p lxor Char.code r)
    ) pad_chars raw_chars
  in
  
  (* Combine pad + masked and base64 encode *)
  let combined = String.of_seq (List.to_seq (pad_chars @ masked_chars)) in
  Data.Base64.encode combined

(** Unmask token received from client *)
let unmask_token masked_b64 =
  match Data.Base64.decode masked_b64 with
  | Result.Error _ -> Option.none
  | Result.Ok decoded ->
      let len = String.length decoded in
      if len = 128 then
        begin
          (* Split into pad and masked parts *)
          let pad = String.sub decoded 0 64 in
          let masked = String.sub decoded 64 64 in
          
          (* XOR to recover original *)
          let pad_chars = String.to_seq pad |> List.of_seq in
          let masked_chars = String.to_seq masked |> List.of_seq in
          
          let raw_chars =
            List.map2 (fun p m ->
              Char.chr (Char.code p lxor Char.code m)
            ) pad_chars masked_chars
          in
          
          Option.some (String.of_seq (List.to_seq raw_chars))
        end
      else
        Option.none

(** {1 Token Storage and Verification} *)

(** Session key for CSRF token *)
let csrf_token_key = "_csrf_token"

(** Get or create token from session *)
let get_or_create_token session =
  match Session.get_value csrf_token_key session with
  | Option.Some token -> token
  | Option.None ->
      let token = generate_token () in
      Session.put csrf_token_key token session;
      token

(** Verify token from request matches session *)
let verify_token session request_token =
  match Session.get_value csrf_token_key session with
  | Option.None -> false
  | Option.Some stored_token ->
      (* Try unmasking first, fallback to direct comparison *)
      (match unmask_token request_token with
       | Option.Some unmasked -> String.equal unmasked stored_token
       | Option.None -> String.equal request_token stored_token)

(** {1 HTTP Method Classification} *)

(** Check if request method is safe (no CSRF protection needed) *)
let is_safe_method method_ =
  match method_ with
  | Net.Http.Method.Get | Head | Options -> true
  | _ -> false

(** {1 Middleware} *)

(** CSRF protection middleware *)
let middleware 
    ?(param_name = "_csrf_token")
    ?(header_name = "x-csrf-token")
    ?(skip_safe_methods = true)
    ?skip
    () =
  
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
      
      (* Get token from request (param, body, or header) *)
      let req_headers = Conn.headers conn in
      
      (* Helper to parse form data from body *)
      let parse_form_body body =
        String.split_on_char '&' body
        |> List.filter_map (fun pair ->
            match String.split_on_char '=' pair with
            | [k; v] -> Option.some (k, v)
            | _ -> Option.none)
      in
      
      let request_token =
        (* Try body_params first (parsed by body_parser middleware) *)
        (match Conn.body_params conn |> List.assoc_opt param_name with
         | Option.Some token -> Option.some token
         | Option.None ->
             (* Try URL parameters *)
             (match Conn.params conn |> List.assoc_opt param_name with
              | Option.Some token -> Option.some token
              | Option.None ->
                  (* Try form body manually (fallback if no body_parser) *)
                  let body = Conn.body conn in
                  let form_params = parse_form_body body in
                  (match List.assoc_opt param_name form_params with
                   | Option.Some token -> Option.some token
                   | Option.None ->
                       (* Try header as last resort *)
                       Net.Http.Header.get req_headers header_name)))
      in
      
      match request_token with
      | Option.None -> begin
          (* No token provided *)
          let _ = Log.warn (String.concat "" [
            "[CSRF] Missing token for ";
            Net.Http.Method.to_string (Conn.method_ conn);
            " ";
            Conn.path conn
          ]) in
          conn
          |> Conn.respond ~status:Net.Http.Status.Forbidden 
               ~body:"CSRF token missing"
          |> Conn.halt
        end
      
      | Option.Some token ->
          (* Verify token *)
          if verify_token session token then
            (* Valid token - continue *)
            next conn
          else begin
            (* Invalid token *)
            let _ = Log.warn (String.concat "" [
              "[CSRF] Invalid token for ";
              Net.Http.Method.to_string (Conn.method_ conn);
              " ";
              Conn.path conn
            ]) in
            conn
            |> Conn.respond ~status:Net.Http.Status.Forbidden 
                 ~body:"CSRF token invalid"
            |> Conn.halt
          end

(** {1 View Helpers} *)

(** Get current CSRF token for use in views *)
let get_token conn =
  let session = Session.get conn in
  get_or_create_token session

(** Generate HTML hidden field for forms *)
let hidden_field conn =
  let token = get_token conn in
  let masked = mask_token token in
  Component.input ~attrs:[
    Component.type_ "hidden";
    Component.name "_csrf_token";
    Component.value masked;
  ] ()

(** Generate HTML meta tag for AJAX *)
let meta_tag conn =
  let token = get_token conn in
  let masked = mask_token token in
  Component.meta ~attrs:[
    Component.name "csrf-token";
    Component.attr "content" masked;
  ] ()
