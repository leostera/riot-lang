open Std
open Std.Sync
open Email

let entry_to_json entry include_attachments =
  let msg_json = Message.to_json entry.Mbox.message in

  let msg_json_with_attachments =
    if include_attachments then
      let headers = Message.headers entry.Mbox.message in
      let body = Message.body entry.Mbox.message in
      match Mime.parse ~headers ~body with
      | Error _ -> msg_json
      | Ok mime -> (
          let attachments = Mime.attachments mime in
          let attachment_list =
            List.map
              (fun part ->
                let filename = Mime.get_filename part in
                let content_type = Mime.get_content_type part in
                let encoding = Mime.get_encoding part in
                let decoded_size =
                  match Mime.get_decoded_content part with
                  | Ok decoded -> String.length decoded
                  | Error _ -> String.length part.content
                in
                Data.Json.Object
                  [
                    ( "filename",
                      match filename with
                      | Some f -> Data.Json.String f
                      | None -> Data.Json.Null );
                    ( "content_type",
                      match content_type with
                      | Some ct ->
                          Data.Json.String
                            (ct.Mime.media_type ^ "/" ^ ct.Mime.subtype)
                      | None -> Data.Json.Null );
                    ( "encoding",
                      match encoding with
                      | Some Mime.Base64 -> Data.Json.String "base64"
                      | Some Mime.QuotedPrintable ->
                          Data.Json.String "quoted-printable"
                      | Some Mime.SevenBit -> Data.Json.String "7bit"
                      | Some Mime.EightBit -> Data.Json.String "8bit"
                      | Some Mime.Binary -> Data.Json.String "binary"
                      | Some (Mime.Other s) -> Data.Json.String s
                      | None -> Data.Json.Null );
                    ("size", Data.Json.Int decoded_size);
                    ("encoded_size", Data.Json.Int (String.length part.content));
                  ])
              attachments
          in
          match msg_json with
          | Data.Json.Object fields ->
              Data.Json.Object
                (("attachments", Data.Json.Array attachment_list) :: fields)
          | _ -> msg_json)
    else msg_json
  in

  let fields = [ ("message", msg_json_with_attachments) ] in
  let fields =
    match entry.Mbox.envelope_from with
    | Some addr -> ("envelope_from", Data.Json.String addr) :: fields
    | None -> fields
  in
  let fields =
    match entry.Mbox.envelope_date with
    | Some date -> ("envelope_date", Data.Json.String date) :: fields
    | None -> fields
  in
  Data.Json.Object (List.rev fields)

let export_entry export_dir index entry =
  let filename = 
    let index_str = Int.to_string index in
    let padded = String.make (4 - String.length index_str) '0' ^ index_str in
    padded ^ ".eml" 
  in
  let filepath = Path.join export_dir (Path.v filename) in

  match Fs.File.create filepath with
  | Error _ ->
      println ("Error: failed to create " ^ (Path.to_string filepath));
      Error ()
  | Ok file -> (
      let content = Message.to_string entry.Mbox.message in
      match Fs.File.write_all file content with
      | Error _ ->
          println ("Error: failed to write to " ^ (Path.to_string filepath));
          let _ = Fs.File.close file in
          Error ()
      | Ok () ->
          let _ = Fs.File.close file in
          Ok ())

let list_messages mbox_path =
      match Fs.File.open_read mbox_path with
      | Error _ ->
          println ("Error opening file: " ^ (Path.to_string mbox_path));
          exit 1
      | Ok file -> (
          match Mbox.of_file file with
          | Error e ->
              println ("Error: " ^ e);
          exit 1
      | Ok mbox ->
          let iter = Mbox.into_mut_iter mbox in
          let count = Cell.create 0 in

          let rec process () =
            match Iter.MutIterator.next iter with
            | None -> ()
            | Some entry ->
                Cell.update count (fun n -> n + 1);
                println ("\n=== Message " ^ (Int.to_string (Cell.get count)) ^ " ===");

                (match entry.envelope_from with
                | Some addr -> println ("Envelope-From: " ^ addr)
                | None -> ());

                (match entry.envelope_date with
                | Some date -> println ("Envelope-Date: " ^ date)
                | None -> ());

                let headers = Message.headers entry.message in
                List.iter
                  (fun (name, value) -> println (name ^ ": " ^ value))
                  headers;

                println "";
                println (Message.body entry.message);
                process ()
          in
          process ();
          println ("\nTotal messages: " ^ (Int.to_string (Cell.get count)));
          let _ = Fs.File.close file in
          Ok ())

let query_messages mbox_path query_str json_output =
  let query =
    match Query.parse query_str with
    | Error e ->
        println ("Error parsing query: " ^ e);
        exit 1
    | Ok q -> q
  in

  match Fs.File.open_read mbox_path with
  | Error _ ->
      println ("Error opening file: " ^ (Path.to_string mbox_path));
      exit 1
  | Ok file -> (
      match Mbox.of_file file with
      | Error e ->
          println ("Error: " ^ e);
          exit 1
      | Ok mbox ->
          let iter = Mbox.into_mut_iter mbox in
          let count = Cell.create 0 in
          let matched = Cell.create 0 in

          let rec process () =
            match Iter.MutIterator.next iter with
            | None -> ()
            | Some entry ->
                Cell.update count (fun n -> n + 1);

                if Query.matches_entry query entry then (
                  Cell.update matched (fun n -> n + 1);
                  if json_output then
                    let json = entry_to_json entry true in
                    println (Data.Json.to_string json)
                  else (
                    println ("\n=== Match " ^ (Int.to_string (Cell.get matched)) ^
                      " (Message " ^ (Int.to_string (Cell.get count)) ^ ") ===");

                    (match entry.envelope_from with
                    | Some addr -> println ("Envelope-From: " ^ addr)
                    | None -> ());

                    (match entry.envelope_date with
                    | Some date -> println ("Envelope-Date: " ^ date)
                    | None -> ());

                    let headers = Message.headers entry.message in
                    List.iter
                      (fun (name, value) -> println (name ^ ": " ^ value))
                      headers;

                    println "";
                    let body = Message.body entry.message in
                    let decoded_body =
                      match Mime.parse ~headers ~body with
                      | Ok (Mime.SinglePart part) -> (
                          match Mime.get_decoded_content part with
                          | Ok decoded -> decoded
                          | Error _ -> body)
                      | _ -> body
                    in
                    println decoded_body));

                process ()
          in
          process ();

          if not json_output then
            println ("\nProcessed: " ^ (Int.to_string (Cell.get count)) ^
              " messages, Matched: " ^ (Int.to_string (Cell.get matched)));

          let _ = Fs.File.close file in
          Ok ())

let export_messages mbox_path export_dir filter_str =
  let query =
    match Query.parse filter_str with
    | Error e ->
        println ("Error parsing filter: " ^ e);
        exit 1
    | Ok q -> q
  in

  match Fs.create_dir_all export_dir with
  | Error _ ->
      println ("Error: failed to create export directory " ^
        (Path.to_string export_dir));
      exit 1
  | Ok () -> (
      match Fs.File.open_read mbox_path with
      | Error _ ->
          println ("Error opening file: " ^ (Path.to_string mbox_path));
          exit 1
      | Ok file -> (
      match Mbox.of_file file with
      | Error e ->
          println ("Error: " ^ e);
              exit 1
          | Ok mbox ->
              let iter = Mbox.into_mut_iter mbox in
              let count = Cell.create 0 in
              let exported = Cell.create 0 in

              let rec process () =
                match Iter.MutIterator.next iter with
                | None -> ()
                | Some entry ->
                    Cell.update count (fun n -> n + 1);

                    if Query.matches_entry query entry then (
                      println ("Exporting message " ^ (Int.to_string (Cell.get count)) ^ "...");
                      match export_entry export_dir (Cell.get count) entry with
                      | Ok () -> Cell.update exported (fun n -> n + 1)
                      | Error () -> ());

                    process ()
              in
              process ();
              println ("\nProcessed: " ^ (Int.to_string (Cell.get count)) ^ " messages");
              println ("Exported: " ^ (Int.to_string (Cell.get exported)) ^ " messages");
              let _ = Fs.File.close file in
              Ok ()))

let () =
  Miniriot.run
    ~main:(fun ~args:_ ->
      let query_cmd =
        ArgParser.command "query"
        |> ArgParser.about "Query and filter emails from MBOX file"
        |> ArgParser.args
             [
               ArgParser.Arg.positional "mbox_file"
               |> ArgParser.Arg.required true
               |> ArgParser.Arg.help "Path to MBOX file";
               ArgParser.Arg.positional "query"
               |> ArgParser.Arg.required true
               |> ArgParser.Arg.help
                    "Query string (e.g., 'has:attachment AND from:alice')";
               ArgParser.Arg.flag "json" |> ArgParser.Arg.long "json"
               |> ArgParser.Arg.help "Output results as JSON";
             ]
      in

      let export_cmd =
        ArgParser.command "export"
        |> ArgParser.about "Export emails from MBOX file"
        |> ArgParser.args
             [
               ArgParser.Arg.positional "mbox_file"
               |> ArgParser.Arg.required true
               |> ArgParser.Arg.help "Path to MBOX file";
               ArgParser.Arg.positional "export_dir"
               |> ArgParser.Arg.required true
               |> ArgParser.Arg.help "Directory to export emails to";
               ArgParser.Arg.option "filter"
               |> ArgParser.Arg.long "filter"
               |> ArgParser.Arg.value_name "FILTER"
               |> ArgParser.Arg.help
                    "Filter: has:attachment, from:addr, to:addr, subject:text, \
                     contains:text, AND, OR, ?, ()";
             ]
      in

      let cmd =
        ArgParser.command "mbox_reader"
        |> ArgParser.about "Stream-process large MBOX files"
        |> ArgParser.version "0.1.0"
        |> ArgParser.subcommands [ query_cmd; export_cmd ]
        |> ArgParser.args
             [
               ArgParser.Arg.positional "mbox_file"
               |> ArgParser.Arg.help "Path to MBOX file (for listing)";
             ]
      in

      let matches =
        match ArgParser.get_matches cmd Env.args with
        | Error _ ->
            ArgParser.print_help cmd;
            exit 1
        | Ok m -> m
      in

      match ArgParser.get_subcommand matches with
      | Some ("query", sub_matches) ->
          let mbox_path =
            match ArgParser.get_one sub_matches "mbox_file" with
            | None ->
                println "Error: mbox_file is required";
                exit 1
            | Some p -> Path.v p
          in
          let query_str =
            match ArgParser.get_one sub_matches "query" with
            | None ->
                println "Error: query is required";
                exit 1
            | Some q -> q
          in
          let json_output = ArgParser.get_flag sub_matches "json" in
          query_messages mbox_path query_str json_output
      | Some ("export", sub_matches) ->
          let mbox_path =
            match ArgParser.get_one sub_matches "mbox_file" with
            | None ->
                println "Error: mbox_file is required";
                exit 1
            | Some p -> Path.v p
          in
          let export_dir =
            match ArgParser.get_one sub_matches "export_dir" with
            | None ->
                println "Error: export_dir is required";
                exit 1
            | Some p -> Path.v p
          in
          let filter =
            match ArgParser.get_one sub_matches "filter" with
            | None -> ""
            | Some f -> f
          in
          export_messages mbox_path export_dir filter
      | _ ->
          let path =
            match ArgParser.get_one matches "mbox_file" with
            | None ->
                ArgParser.print_help cmd;
                exit 1
            | Some p -> Path.v p
          in
          list_messages path)
    ~args:Env.args ()
