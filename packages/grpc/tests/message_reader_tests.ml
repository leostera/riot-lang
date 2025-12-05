open Std

(** Tests for gRPC Message_reader (streaming parser) *)

(** {1 API Surface Tests} *)

let test_reader_create_default () =
  Test.case "Message_reader: create with default max size" (fun () ->
      let parser = Grpc.Message_reader.create () in
      (* Just verify it was created *)
      let buffered = Grpc.Message_reader.buffered_bytes parser in
      if buffered = 0 then Ok ()
      else Error "New parser should have no buffered bytes")

let test_reader_create_custom_max () =
  Test.case "Message_reader: create with custom max size" (fun () ->
      let parser = Grpc.Message_reader.create ~max_message_size:1000 () in
      (* Just verify it was created *)
      let buffered = Grpc.Message_reader.buffered_bytes parser in
      if buffered = 0 then Ok ()
      else Error "New parser should have no buffered bytes")

let test_reader_reset () =
  Test.case "Message_reader: reset parser" (fun () ->
      let parser = Grpc.Message_reader.create () in
      (* Reset should not crash *)
      Grpc.Message_reader.reset parser;
      let buffered = Grpc.Message_reader.buffered_bytes parser in
      if buffered = 0 then Ok ()
      else Error "Reset parser should have no buffered bytes")

let test_reader_buffered_bytes_initial () =
  Test.case "Message_reader: buffered_bytes initially zero" (fun () ->
      let parser = Grpc.Message_reader.create () in
      let buffered = Grpc.Message_reader.buffered_bytes parser in
      if buffered = 0 then Ok ()
      else Error ("Initial buffered should be 0, got " ^ Int.to_string buffered))

(** {1 Streaming Tests} *)

let test_reader_parse_complete_message () =
  Test.case "Message_reader: parse complete message from reader" (fun () ->
      let payload = IO.Bytes.of_string "Hello, streaming!" in
      let encoded = Grpc.Message.encode ~compressed:false ~payload in
      let reader = IO.Reader.from_bytes encoded in
      let parser = Grpc.Message_reader.create () in
      
      match Grpc.Message_reader.parse parser reader with
      | Grpc.Message_reader.Message msg ->
          if IO.Bytes.equal msg.payload payload && not msg.compressed then Ok ()
          else Error "Message mismatch"
      | Grpc.Message_reader.Need_more -> Error "Should not need more data"
      | Grpc.Message_reader.Error _ -> Error "Parse failed")

let test_reader_parse_empty_payload () =
  Test.case "Message_reader: parse empty payload from reader" (fun () ->
      let payload = IO.Bytes.of_string "" in
      let encoded = Grpc.Message.encode ~compressed:false ~payload in
      let reader = IO.Reader.from_bytes encoded in
      let parser = Grpc.Message_reader.create () in
      
      match Grpc.Message_reader.parse parser reader with
      | Grpc.Message_reader.Message msg ->
          if IO.Bytes.length msg.payload = 0 then Ok ()
          else Error "Empty payload not preserved"
      | Grpc.Message_reader.Need_more -> Error "Should not need more data"
      | Grpc.Message_reader.Error _ -> Error "Parse failed")

let test_reader_parse_compressed () =
  Test.case "Message_reader: parse compressed message" (fun () ->
      let payload = IO.Bytes.of_string "compressed" in
      let encoded = Grpc.Message.encode ~compressed:true ~payload in
      let reader = IO.Reader.from_bytes encoded in
      let parser = Grpc.Message_reader.create () in
      
      match Grpc.Message_reader.parse parser reader with
      | Grpc.Message_reader.Message msg ->
          if msg.compressed && IO.Bytes.equal msg.payload payload then Ok ()
          else Error "Compression flag or payload mismatch"
      | Grpc.Message_reader.Need_more -> Error "Should not need more data"
      | Grpc.Message_reader.Error _ -> Error "Parse failed")

let test_reader_parse_multiple_messages () =
  Test.case "Message_reader: parse multiple messages sequentially" (fun () ->
      let payload1 = IO.Bytes.of_string "first" in
      let payload2 = IO.Bytes.of_string "second" in
      let payload3 = IO.Bytes.of_string "third" in
      
      let encoded1 = Grpc.Message.encode ~compressed:false ~payload:payload1 in
      let encoded2 = Grpc.Message.encode ~compressed:false ~payload:payload2 in
      let encoded3 = Grpc.Message.encode ~compressed:false ~payload:payload3 in
      
      (* Concatenate all three messages *)
      let len1 = IO.Bytes.length encoded1 in
      let len2 = IO.Bytes.length encoded2 in
      let len3 = IO.Bytes.length encoded3 in
      let total = IO.Bytes.create (len1 + len2 + len3) in
      IO.Bytes.blit encoded1 0 total 0 len1;
      IO.Bytes.blit encoded2 0 total len1 len2;
      IO.Bytes.blit encoded3 0 total (len1 + len2) len3;
      
      let reader = IO.Reader.from_bytes total in
      let parser = Grpc.Message_reader.create () in
      
      (* Parse first message *)
      match Grpc.Message_reader.parse parser reader with
      | Grpc.Message_reader.Message msg1 ->
          if not (IO.Bytes.equal msg1.payload payload1) then
            Error "First message mismatch"
          else
            (* Parse second message *)
            match Grpc.Message_reader.parse parser reader with
            | Grpc.Message_reader.Message msg2 ->
                if not (IO.Bytes.equal msg2.payload payload2) then
                  Error "Second message mismatch"
                else
                  (* Parse third message *)
                  match Grpc.Message_reader.parse parser reader with
                  | Grpc.Message_reader.Message msg3 ->
                      if IO.Bytes.equal msg3.payload payload3 then Ok ()
                      else Error "Third message mismatch"
                  | _ -> Error "Failed to parse third message"
            | _ -> Error "Failed to parse second message"
      | _ -> Error "Failed to parse first message")

let test_reader_exceeds_max_size () =
  Test.case "Message_reader: reject message exceeding max size" (fun () ->
      (* Create header claiming 10MB payload *)
      let header = IO.Bytes.of_string "\x00\x00\x98\x96\x80" in (* 10MB *)
      let reader = IO.Reader.from_bytes header in
      let parser = Grpc.Message_reader.create ~max_message_size:(1 * 1024 * 1024) () in
      
      match Grpc.Message_reader.parse parser reader with
      | Grpc.Message_reader.Error (Grpc.Message_reader.Message_size_exceeds_maximum { size; max_size }) ->
          if size = 10_000_000 && max_size = 1_048_576 then Ok ()
          else Error ("Size values wrong: size=" ^ Int.to_string size ^ " max=" ^ Int.to_string max_size)
      | Grpc.Message_reader.Message _ -> Error "Should reject oversized message"
      | Grpc.Message_reader.Need_more -> Error "Should error, not need more")

let test_reader_need_more_incomplete () =
  Test.case "Message_reader: returns Need_more on incomplete message" (fun () ->
      (* Only 3 bytes of 5-byte header *)
      let partial = IO.Bytes.of_string "\x00\x00\x00" in
      let reader = IO.Reader.from_bytes partial in
      let parser = Grpc.Message_reader.create () in
      
      match Grpc.Message_reader.parse parser reader with
      | Grpc.Message_reader.Need_more -> Ok ()
      | Grpc.Message_reader.Message _ -> Error "Should not parse incomplete header"
      | Grpc.Message_reader.Error _ -> Error "Should return Need_more, not Error")

(** {1 Test Suite} *)

let tests =
  Test.[
    (* API surface tests *)
    test_reader_create_default ();
    test_reader_create_custom_max ();
    test_reader_reset ();
    test_reader_buffered_bytes_initial ();
    (* Streaming tests *)
    test_reader_parse_complete_message ();
    test_reader_parse_empty_payload ();
    test_reader_parse_compressed ();
    test_reader_parse_multiple_messages ();
    test_reader_exceeds_max_size ();
    test_reader_need_more_incomplete ();
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"grpc:message_reader" ~tests ~args:Env.args)
    ~args:Env.args ()
