open Std

(** Edge case tests for gRPC protocol implementation *)

(** {1 Message Edge Cases} *)

let test_message_empty_payload () =
  Test.case "Message: empty payload" (fun () ->
      let payload = IO.Bytes.of_string "" in
      let encoded = Grpc.Message.encode ~compressed:false ~payload in
      match Grpc.Message.decode encoded with
      | Error _ -> Error "Failed to decode empty payload"
      | Ok (msg, _) ->
          if IO.Bytes.length msg.payload = 0 then Ok ()
          else Error "Empty payload not preserved")

let test_message_max_size () =
  Test.case "Message: at maximum size (4MB)" (fun () ->
      (* Create a 4MB payload *)
      let max_size = 4 * 1024 * 1024 in
      let payload = IO.Bytes.create max_size in
      match Grpc.Message.validate_size max_size ~max_size:None with
      | Error _ -> Error "4MB should be valid"
      | Ok () -> Ok ())

let test_message_exceeds_max_size () =
  Test.case "Message: exceeds maximum size" (fun () ->
      let too_large = 5 * 1024 * 1024 in
      match Grpc.Message.validate_size too_large ~max_size:None with
      | Ok () -> Error "Should reject size > 4MB"
      | Error (Grpc.Message.Message_size_exceeds_maximum _) -> Ok ()
      | Error _ -> Error "Wrong error type")

let test_message_boundary_size () =
  Test.case "Message: at boundary (4MB - 1)" (fun () ->
      let boundary = 4 * 1024 * 1024 - 1 in
      match Grpc.Message.validate_size boundary ~max_size:None with
      | Error _ -> Error "4MB-1 should be valid"
      | Ok () -> Ok ())

let test_message_custom_max_size () =
  Test.case "Message: custom max size" (fun () ->
      let size = 1000 in
      let max_size = Some 500 in
      match Grpc.Message.validate_size size ~max_size with
      | Ok () -> Error "Should reject size > custom max"
      | Error (Grpc.Message.Message_size_exceeds_maximum { size = s; max_size = m }) ->
          if s = size && m = 500 then Ok ()
          else Error ("Size mismatch: expected size=1000 max=500, got size=" ^ Int.to_string s ^ " max=" ^ Int.to_string m)
      | Error _ -> Error "Wrong error type")

let test_message_incomplete_header () =
  Test.case "Message: incomplete header (3 bytes)" (fun () ->
      let partial = IO.Bytes.of_string "\x00\x00\x00" in
      match Grpc.Message.decode partial with
      | Ok _ -> Error "Should fail on incomplete header"
      | Error (Grpc.Message.Incomplete_header { have }) ->
          if have = 3 then Ok ()
          else Error ("Wrong have count: " ^ Int.to_string have)
      | Error _ -> Error "Wrong error type")

let test_message_header_only () =
  Test.case "Message: header only, no payload" (fun () ->
      (* Valid header claiming 10 bytes, but no payload *)
      let header = IO.Bytes.of_string "\x00\x00\x00\x00\x0a" in
      match Grpc.Message.decode header with
      | Ok _ -> Error "Should fail with incomplete message"
      | Error (Grpc.Message.Incomplete_message _) -> Ok ()
      | Error _ -> Error "Wrong error type")

let test_message_multiple_sequential () =
  Test.case "Message: decode multiple sequential messages" (fun () ->
      let payload1 = IO.Bytes.of_string "first" in
      let payload2 = IO.Bytes.of_string "second" in
      let encoded1 = Grpc.Message.encode ~compressed:false ~payload:payload1 in
      let encoded2 = Grpc.Message.encode ~compressed:false ~payload:payload2 in
      
      (* Concatenate them *)
      let len1 = IO.Bytes.length encoded1 in
      let len2 = IO.Bytes.length encoded2 in
      let combined = IO.Bytes.create (len1 + len2) in
      IO.Bytes.blit encoded1 0 combined 0 len1;
      IO.Bytes.blit encoded2 0 combined len1 len2;
      
      (* Decode first *)
      match Grpc.Message.decode combined with
      | Error _ -> Error "Failed to decode first message"
      | Ok (msg1, remaining) ->
          if not (IO.Bytes.equal msg1.payload payload1) then
            Error "First message payload mismatch"
          else
            (* Decode second from remaining *)
            match Grpc.Message.decode remaining with
            | Error _ -> Error "Failed to decode second message"
            | Ok (msg2, final_remaining) ->
                if IO.Bytes.equal msg2.payload payload2 && IO.Bytes.length final_remaining = 0 then
                  Ok ()
                else Error "Second message payload mismatch")

(** {1 Metadata Edge Cases} *)

let test_metadata_empty () =
  Test.case "Metadata: empty metadata" (fun () ->
      let meta = Grpc.Metadata.empty in
      match Grpc.Metadata.get meta ~key:"any-key" with
      | None -> Ok ()
      | Some _ -> Error "Empty metadata should return None")

let test_metadata_long_value () =
  Test.case "Metadata: very long header value" (fun () ->
      let long_value = String.make 10000 'x' in
      let meta = Grpc.Metadata.empty |> Grpc.Metadata.add ~key:"long" ~value:long_value in
      match Grpc.Metadata.get meta ~key:"long" with
      | None -> Error "Failed to retrieve long value"
      | Some v -> if String.equal v long_value then Ok () else Error "Long value corrupted")

let test_metadata_unicode () =
  Test.case "Metadata: unicode in header value" (fun () ->
      let unicode = "こんにちは 世界 🌍" in
      let meta = Grpc.Metadata.empty |> Grpc.Metadata.add ~key:"greeting" ~value:unicode in
      match Grpc.Metadata.get meta ~key:"greeting" with
      | None -> Error "Failed to retrieve unicode value"
      | Some v -> if String.equal v unicode then Ok () else Error "Unicode value corrupted")

let test_metadata_all_timeout_units () =
  Test.case "Metadata: all timeout units" (fun () ->
      let units = [
        (`Hours, "H");
        (`Minutes, "M");
        (`Seconds, "S");
        (`Milliseconds, "m");
        (`Microseconds, "u");
        (`Nanoseconds, "n");
      ] in
      let test_unit (unit_val, unit_char) =
        let timeout = { Grpc.Metadata.value = 42; unit = unit_val } in
        let (_key, encoded) = Grpc.Metadata.timeout timeout in
        (* Should end with the unit character *)
        if String.ends_with ~suffix:unit_char encoded then
          match Grpc.Metadata.parse_timeout encoded with
          | None -> Error ("Failed to parse timeout with unit " ^ unit_char)
          | Some parsed ->
              if parsed.value = 42 && parsed.unit = unit_val then Ok ()
              else Error ("Timeout unit mismatch for " ^ unit_char)
        else Error ("Timeout encoding doesn't end with " ^ unit_char)
      in
      List.fold_left
        (fun acc unit_spec ->
          match acc with
          | Error _ -> acc
          | Ok () -> test_unit unit_spec)
        (Ok ())
        units)

let test_metadata_all_status_codes () =
  Test.case "Metadata: all status codes parse correctly" (fun () ->
      let codes = [
        (0, Grpc.Status.OK);
        (1, Grpc.Status.Cancelled);
        (2, Grpc.Status.Unknown);
        (3, Grpc.Status.InvalidArgument);
        (4, Grpc.Status.DeadlineExceeded);
        (5, Grpc.Status.NotFound);
        (6, Grpc.Status.AlreadyExists);
        (7, Grpc.Status.PermissionDenied);
        (8, Grpc.Status.ResourceExhausted);
        (9, Grpc.Status.FailedPrecondition);
        (10, Grpc.Status.Aborted);
        (11, Grpc.Status.OutOfRange);
        (12, Grpc.Status.Unimplemented);
        (13, Grpc.Status.Internal);
        (14, Grpc.Status.Unavailable);
        (15, Grpc.Status.DataLoss);
        (16, Grpc.Status.Unauthenticated);
      ] in
      let test_code (code, expected_status) =
        let code_str = Int.to_string code in
        match Grpc.Metadata.parse_status code_str with
        | None -> Error ("Failed to parse status code " ^ code_str)
        | Some status ->
            if status = expected_status then Ok ()
            else Error ("Status mismatch for code " ^ code_str)
      in
      List.fold_left
        (fun acc code_spec ->
          match acc with
          | Error _ -> acc
          | Ok () -> test_code code_spec)
        (Ok ())
        codes)

let test_metadata_malformed_timeout () =
  Test.case "Metadata: malformed timeout strings" (fun () ->
      let malformed = ["abc"; "123"; "123X"; ""; "  "] in
      let all_fail =
        List.for_all
          (fun s -> match Grpc.Metadata.parse_timeout s with None -> true | Some _ -> false)
          malformed
      in
      if all_fail then Ok () else Error "Some malformed timeouts were parsed")

let test_metadata_malformed_status () =
  Test.case "Metadata: malformed status codes" (fun () ->
      let malformed = ["abc"; "99"; "-1"; ""; "  "] in
      let all_fail =
        List.for_all
          (fun s -> match Grpc.Metadata.parse_status s with None -> true | Some _ -> false)
          malformed
      in
      if all_fail then Ok () else Error "Some malformed status codes were parsed")

(** {1 Status Edge Cases} *)

let test_status_all_codes_to_int () =
  Test.case "Status: all status codes map to correct integers" (fun () ->
      let codes = [
        (Grpc.Status.OK, 0);
        (Grpc.Status.Cancelled, 1);
        (Grpc.Status.Unknown, 2);
        (Grpc.Status.InvalidArgument, 3);
        (Grpc.Status.DeadlineExceeded, 4);
        (Grpc.Status.NotFound, 5);
        (Grpc.Status.AlreadyExists, 6);
        (Grpc.Status.PermissionDenied, 7);
        (Grpc.Status.ResourceExhausted, 8);
        (Grpc.Status.FailedPrecondition, 9);
        (Grpc.Status.Aborted, 10);
        (Grpc.Status.OutOfRange, 11);
        (Grpc.Status.Unimplemented, 12);
        (Grpc.Status.Internal, 13);
        (Grpc.Status.Unavailable, 14);
        (Grpc.Status.DataLoss, 15);
        (Grpc.Status.Unauthenticated, 16);
      ] in
      let test_code (status, expected) =
        let actual = Grpc.Status.to_int status in
        if actual = expected then Ok ()
        else Error ("Status code mismatch: expected " ^ Int.to_string expected ^ " got " ^ Int.to_string actual)
      in
      List.fold_left
        (fun acc code_spec ->
          match acc with
          | Error _ -> acc
          | Ok () -> test_code code_spec)
        (Ok ())
        codes)

let test_status_http_mappings () =
  Test.case "Status: HTTP status code mappings" (fun () ->
      let mappings = [
        (Grpc.Status.OK, 200);
        (Grpc.Status.Cancelled, 499);
        (Grpc.Status.Unknown, 500);
        (Grpc.Status.InvalidArgument, 400);
        (Grpc.Status.DeadlineExceeded, 504);
        (Grpc.Status.NotFound, 404);
        (Grpc.Status.AlreadyExists, 409);
        (Grpc.Status.PermissionDenied, 403);
        (Grpc.Status.ResourceExhausted, 429);
        (Grpc.Status.FailedPrecondition, 400);
        (Grpc.Status.Aborted, 409);
        (Grpc.Status.OutOfRange, 400);
        (Grpc.Status.Unimplemented, 501);
        (Grpc.Status.Internal, 500);
        (Grpc.Status.Unavailable, 503);
        (Grpc.Status.DataLoss, 500);
        (Grpc.Status.Unauthenticated, 401);
      ] in
      let test_mapping (status, expected_http) =
        let actual = Grpc.Status.to_http_status status in
        if actual = expected_http then Ok ()
        else Error ("HTTP mapping mismatch for status " ^ Int.to_string (Grpc.Status.to_int status))
      in
      List.fold_left
        (fun acc mapping_spec ->
          match acc with
          | Error _ -> acc
          | Ok () -> test_mapping mapping_spec)
        (Ok ())
        mappings)

let test_status_retriability () =
  Test.case "Status: retriability classification" (fun () ->
      let retriable = [
        Grpc.Status.Unavailable;
        Grpc.Status.DeadlineExceeded;
        Grpc.Status.ResourceExhausted;
      ] in
      let non_retriable = [
        Grpc.Status.OK;
        Grpc.Status.InvalidArgument;
        Grpc.Status.NotFound;
        Grpc.Status.AlreadyExists;
        Grpc.Status.PermissionDenied;
        Grpc.Status.FailedPrecondition;
        Grpc.Status.Unimplemented;
        Grpc.Status.DataLoss;
        Grpc.Status.Unauthenticated;
      ] in
      let all_retriable = List.for_all Grpc.Status.is_retriable retriable in
      let none_retriable = List.for_all (fun s -> not (Grpc.Status.is_retriable s)) non_retriable in
      if all_retriable && none_retriable then Ok ()
      else Error "Retriability classification incorrect")

(** {1 Call Edge Cases} *)

let test_call_streaming_methods () =
  Test.case "Call: all streaming method types" (fun () ->
      let unary = Grpc.Call.unary_method ~service:"s" ~method_:"m" in
      let client_stream = Grpc.Call.client_streaming_method ~service:"s" ~method_:"m" in
      let server_stream = Grpc.Call.server_streaming_method ~service:"s" ~method_:"m" in
      let bidi = Grpc.Call.bidi_streaming_method ~service:"s" ~method_:"m" in
      
      (* All should produce same path *)
      let path = Grpc.Call.method_path unary in
      if String.equal path (Grpc.Call.method_path client_stream)
         && String.equal path (Grpc.Call.method_path server_stream)
         && String.equal path (Grpc.Call.method_path bidi)
      then Ok ()
      else Error "Streaming methods produce different paths")

let test_call_empty_service_name () =
  Test.case "Call: empty service name" (fun () ->
      let method_def = Grpc.Call.unary_method ~service:"" ~method_:"Method" in
      let path = Grpc.Call.method_path method_def in
      (* Empty service results in //Method due to string concatenation *)
      if String.equal path "//Method" then Ok ()
      else Error ("Empty service path wrong: " ^ path))

let test_call_empty_method_name () =
  Test.case "Call: empty method name" (fun () ->
      let method_def = Grpc.Call.unary_method ~service:"Service" ~method_:"" in
      let path = Grpc.Call.method_path method_def in
      if String.equal path "/Service/" then Ok ()
      else Error ("Empty method path wrong: " ^ path))

let test_call_long_names () =
  Test.case "Call: very long service and method names" (fun () ->
      let long_service = String.make 1000 'S' in
      let long_method = String.make 1000 'M' in
      let method_def = Grpc.Call.unary_method ~service:long_service ~method_:long_method in
      let path = Grpc.Call.method_path method_def in
      let expected = "/" ^ long_service ^ "/" ^ long_method in
      if String.equal path expected then Ok ()
      else Error "Long names not handled correctly")

(** {1 Test Suite} *)

let tests =
  Test.[
    (* Message edge cases *)
    test_message_empty_payload ();
    test_message_max_size ();
    test_message_exceeds_max_size ();
    test_message_boundary_size ();
    test_message_custom_max_size ();
    test_message_incomplete_header ();
    test_message_header_only ();
    test_message_multiple_sequential ();
    (* Metadata edge cases *)
    test_metadata_empty ();
    test_metadata_long_value ();
    test_metadata_unicode ();
    test_metadata_all_timeout_units ();
    test_metadata_all_status_codes ();
    test_metadata_malformed_timeout ();
    test_metadata_malformed_status ();
    (* Status edge cases *)
    test_status_all_codes_to_int ();
    test_status_http_mappings ();
    test_status_retriability ();
    (* Call edge cases *)
    test_call_streaming_methods ();
    test_call_empty_service_name ();
    test_call_empty_method_name ();
    test_call_long_names ();
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"grpc:edge_case" ~tests ~args:Env.args)
    ~args:Env.args ()
