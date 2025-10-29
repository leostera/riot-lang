open Email
open Std
open Std.Data

let read_file path =
  match Fs.read (Path.v path) with
  | Ok content -> content
  | Error _ -> failwith (format "Failed to read file: %s" path)

let parse_expected_address json =
  match Json.of_string json with
  | Ok (Json.Object fields) ->
      let get_string key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.String s -> Some s
          | _ -> None)
      in
      let get_opt_string key =
        match List.assoc_opt key fields with
        | Some (Json.String s) -> Some (Some s)
        | Some Json.Null -> Some None
        | None -> Some None
        | _ -> None
      in
      Option.map
        (fun display_name ->
          ( display_name,
            get_string "local_part",
            get_string "domain",
            get_string "address" ))
        (get_opt_string "display_name")
  | _ -> None

let parse_expected_message json =
  match Json.of_string json with
  | Ok (Json.Object fields) ->
      let get_object key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.Object obj -> Some obj
          | _ -> None)
      in
      let get_string key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.String s -> Some s
          | _ -> None)
      in
      Option.map
        (fun headers_obj ->
          let headers =
            List.filter_map
              (fun (k, v) ->
                match v with Json.String s -> Some (k, s) | _ -> None)
              headers_obj
          in
          (headers, get_string "body"))
        (get_object "headers")
  | _ -> None

let parse_expected_imap_capability json =
  match Json.of_string json with
  | Ok (Json.Object fields) -> (
      match List.assoc_opt "capabilities" fields with
      | Some (Json.Array caps) ->
          Some
            (List.filter_map
               (function Json.String s -> Some s | _ -> None)
               caps)
      | _ -> None)
  | _ -> None

let parse_expected_imap_select json =
  match Json.of_string json with
  | Ok (Json.Object fields) ->
      let get_array key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.Array arr -> Some arr
          | _ -> None)
      in
      let get_int key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.Int n -> Some n
          | _ -> None)
      in
      let get_bool key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.Bool b -> Some b
          | _ -> None)
      in
      let flags =
        Option.map
          (List.filter_map (function Json.String s -> Some s | _ -> None))
          (get_array "flags")
      in
      Some
        ( flags,
          get_int "exists",
          get_int "recent",
          get_int "unseen",
          get_int "uid_validity",
          get_int "uid_next",
          get_bool "read_write" )
  | _ -> None

let parse_expected_imap_fetch json =
  match Json.of_string json with
  | Ok (Json.Object fields) ->
      let get_int key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.Int n -> Some n
          | _ -> None)
      in
      let get_array key =
        Option.and_then (List.assoc_opt key fields) (function
          | Json.Array arr -> Some arr
          | _ -> None)
      in
      let flags =
        Option.map
          (List.filter_map (function Json.String s -> Some s | _ -> None))
          (get_array "flags")
      in
      Some (get_int "message", get_int "uid", flags, get_int "size")
  | _ -> None

let parse_expected_imap_list json =
  match Json.of_string json with
  | Ok (Json.Object fields) -> (
      match List.assoc_opt "mailboxes" fields with
      | Some (Json.Array mailboxes) ->
          Some
            (List.filter_map
               (function
                 | Json.Object mb -> (
                     let get_string k =
                       Option.and_then (List.assoc_opt k mb) (function
                         | Json.String s -> Some s
                         | _ -> None)
                     in
                     let get_array k =
                       Option.and_then (List.assoc_opt k mb) (function
                         | Json.Array a -> Some a
                         | _ -> None)
                     in
                     match
                       ( get_string "name",
                         get_string "delimiter",
                         get_array "flags" )
                     with
                     | Some name, Some delim, Some flags ->
                         let flag_strs =
                           List.filter_map
                             (function Json.String s -> Some s | _ -> None)
                             flags
                         in
                         Some (name, delim, flag_strs)
                     | _ -> None)
                 | _ -> None)
               mailboxes)
      | _ -> None)
  | _ -> None

let parse_expected_imap_search json =
  match Json.of_string json with
  | Ok (Json.Object fields) -> (
      match List.assoc_opt "message_ids" fields with
      | Some (Json.Array ids) ->
          Some
            (List.filter_map (function Json.Int n -> Some n | _ -> None) ids)
      | _ -> None)
  | _ -> None

let test_address (name, input_file, expected_file) =
  Test.case (format "Address: %s" name) (fun () ->
      let input = read_file input_file in
      let expected_json = read_file expected_file in

      match parse_expected_address expected_json with
      | None ->
          Error (format "Failed to parse expected JSON for %s" expected_file)
      | Some
          (exp_display_name, Some exp_local, Some exp_domain, Some exp_address)
        -> (
          match Address.of_string input with
          | Error e -> Error (format "Parse error: %s" e)
          | Ok addr ->
              let actual_display = Address.display_name addr in
              let actual_local = Address.local_part addr in
              let actual_domain = Address.domain addr in
              let actual_address = Address.address addr in

              if actual_display <> exp_display_name then
                Error
                  (format "Display name mismatch: expected %s, got %s"
                     (Option.map_or ~default:"null" Fun.id exp_display_name)
                     (Option.map_or ~default:"null" Fun.id actual_display))
              else if actual_local <> exp_local then
                Error
                  (format "Local part mismatch: expected %s, got %s" exp_local
                     actual_local)
              else if actual_domain <> exp_domain then
                Error
                  (format "Domain mismatch: expected %s, got %s" exp_domain
                     actual_domain)
              else if actual_address <> exp_address then
                Error
                  (format "Address mismatch: expected %s, got %s" exp_address
                     actual_address)
              else Ok ())
      | _ -> Error "Invalid expected format")

let test_message (name, eml_file, expected_file) =
  Test.case (format "Message: %s" name) (fun () ->
      let input = read_file eml_file in
      let expected_json = read_file expected_file in

      match parse_expected_message expected_json with
      | None ->
          Error (format "Failed to parse expected JSON for %s" expected_file)
      | Some (exp_headers, Some exp_body) -> (
          match Message.of_string input with
          | Error e -> Error (format "Parse error: %s" e)
          | Ok msg -> (
              let actual_headers = Message.headers msg in
              let actual_body = Message.body msg in

              let header_map =
                List.fold_left
                  (fun acc (k, v) ->
                    match List.assoc_opt k acc with
                    | None -> (k, [ v ]) :: acc
                    | Some vs -> (k, v :: vs) :: List.remove_assoc k acc)
                  [] actual_headers
              in

              let rec check_headers = function
                | [] -> Ok ()
                | (name, exp_value) :: rest -> (
                    match List.assoc_opt name header_map with
                    | None -> Error (format "Missing header: %s" name)
                    | Some values ->
                        if List.mem exp_value values then check_headers rest
                        else
                          Error
                            (format
                               "Header %s mismatch: expected '%s', got '%s'"
                               name exp_value
                               (String.concat ", " values)))
              in

              match check_headers exp_headers with
              | Error e -> Error e
              | Ok () ->
                  if actual_body = exp_body then Ok ()
                  else
                    Error
                      (format "Body mismatch: expected '%s', got '%s'" exp_body
                         actual_body)))
      | _ -> Error "Invalid expected format")

let test_imap_response (name, imap_file, expected_file) =
  Test.case (format "IMAP: %s" name) (fun () ->
      let _input = read_file imap_file in
      let _expected_json = read_file expected_file in
      Error "IMAP parsing not implemented yet")

let load_fixtures base_path suffix =
  let fixtures_path = Path.v base_path in
  match Fs.read_dir fixtures_path with
  | Error _ -> []
  | Ok iter ->
      let entries = Std.Iter.MutIterator.to_list iter in
      let fixtures =
        List.filter_map
          (fun path ->
            let name = Path.basename path in
            if String.ends_with ~suffix name then
              let base =
                String.sub name 0 (String.length name - String.length suffix)
              in
              let input_file = format "%s/%s" base_path name in
              let expected_file = format "%s/%s.expected" base_path base in
              match Fs.exists (Path.v expected_file) with
              | Ok true -> Some (base, input_file, expected_file)
              | _ -> None
            else None)
          entries
      in
      List.sort (fun (a, _, _) (b, _, _) -> String.compare a b) fixtures

let test_header (name, input_file, expected_file) =
  Test.case (format "Header: %s" name) (fun () ->
      let _input = read_file input_file in
      let _expected_json = read_file expected_file in
      Error "Header parsing not implemented yet")

let test_mime (name, eml_file, expected_file) =
  Test.case (format "MIME: %s" name) (fun () ->
      let _input = read_file eml_file in
      let _expected_json = read_file expected_file in
      Error "MIME parsing not implemented yet")

let test_smtp (name, smtp_file, expected_file) =
  Test.case (format "SMTP: %s" name) (fun () ->
      let _input = read_file smtp_file in
      let _expected_json = read_file expected_file in
      Error "SMTP parsing not implemented yet")

let test_encoding (name, input_file, expected_file) =
  Test.case (format "Encoding: %s" name) (fun () ->
      let input = read_file input_file in
      let expected_json = read_file expected_file in

      match Json.of_string expected_json with
      | Error _ -> Error "Failed to parse expected JSON"
      | Ok (Json.Object fields) -> (
          match
            (List.assoc_opt "encoding" fields, List.assoc_opt "decoded" fields)
          with
          | Some (Json.String enc_name), Some (Json.String expected_decoded)
            -> (
              match Encoding.of_string enc_name with
              | Error e -> Error (format "Invalid encoding: %s" e)
              | Ok encoding -> (
                  match Encoding.decode encoding input with
                  | Error e -> Error (format "Decode error: %s" e)
                  | Ok decoded ->
                      if decoded = expected_decoded then Ok ()
                      else
                        Error
                          (format "Decoded mismatch: expected '%s', got '%s'"
                             expected_decoded decoded)))
          | _ -> Error "Invalid expected format")
      | _ -> Error "Expected JSON object")

let test_real_world (name, eml_file, expected_file) =
  Test.case (format "Real-world: %s" name) (fun () ->
      let _input = read_file eml_file in
      let _expected_json = read_file expected_file in
      Error "Real-world message parsing not implemented yet")

let () =
  Miniriot.run
    ~main:(fun ~args:_ ->
      let address_fixtures =
        load_fixtures "packages/email/tests/fixtures/address" ".txt"
      in
      let message_fixtures =
        load_fixtures "packages/email/tests/fixtures/message" ".eml"
      in
      let imap_fixtures =
        load_fixtures "packages/email/tests/fixtures/imap/responses" ".imap"
      in
      let header_fixtures =
        load_fixtures "packages/email/tests/fixtures/headers" ".txt"
      in
      let mime_fixtures =
        load_fixtures "packages/email/tests/fixtures/mime" ".eml"
      in
      let smtp_fixtures =
        load_fixtures "packages/email/tests/fixtures/smtp" ".smtp"
      in
      let encoding_fixtures =
        load_fixtures "packages/email/tests/fixtures/encoding" ".txt"
      in
      let real_world_fixtures =
        load_fixtures "packages/email/tests/fixtures/real_world" ".eml"
      in

      let address_tests = List.map test_address address_fixtures in
      let message_tests = List.map test_message message_fixtures in
      let imap_tests = List.map test_imap_response imap_fixtures in
      let header_tests = List.map test_header header_fixtures in
      let mime_tests = List.map test_mime mime_fixtures in
      let smtp_tests = List.map test_smtp smtp_fixtures in
      let encoding_tests = List.map test_encoding encoding_fixtures in
      let real_world_tests = List.map test_real_world real_world_fixtures in

      let all_tests =
        address_tests @ message_tests @ imap_tests @ header_tests @ mime_tests
        @ smtp_tests @ encoding_tests @ real_world_tests
      in

      Test.Cli.main ~name:"email" ~tests:all_tests ~args:Env.args)
    ~args:Env.args ()
