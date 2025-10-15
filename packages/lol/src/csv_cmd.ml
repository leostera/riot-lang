open Std

let to_json_stream input_path =
  let iter = Data.Csv.read input_path in

  let headers =
    match Iter.MutIterator.next iter with
    | Some (Ok row) -> row
    | Some (Error err) ->
        Log.error "Failed to parse CSV headers: %s"
          (Data.Csv.error_to_string err);
        []
    | None ->
        Log.error "Empty CSV file";
        []
  in

  let rec process_rows () =
    match Iter.MutIterator.next iter with
    | Some (Ok row) ->
        let fields =
          List.combine headers row
          |> List.map (fun (k, v) -> (k, Data.Json.string v))
        in
        let json = Data.Json.obj fields in
        println "%s" (Data.Json.to_string json);
        process_rows ()
    | Some (Error err) ->
        Log.error "Failed to parse CSV row: %s" (Data.Csv.error_to_string err)
    | None -> ()
  in

  process_rows ()
