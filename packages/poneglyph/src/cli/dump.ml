open Std
module Bytes = Kernel.IO.Bytes

let command =
  let open ArgParser in
  let open Arg in
  command "dump"
  |> about "Dump entries from an SSTable file"
  |> args [
      positional "path" |> help "Path to SSTable file";
      option "limit" |> default "20" |> help "Maximum number of entries to display";
    ]

let run matches =
  let open ArgParser in
  let path = get_one matches "path" |> Option.expect ~msg:"path required" in
  let limit_str = get_one matches "limit" |> Option.unwrap_or ~default:"20" in
  let limit = match int_of_string_opt limit_str with
    | Some n -> n
    | None ->
        println "Error: limit must be a number";
        exit 1
  in
  
  match Storage.Lsm.Sstable.open_read ~path with
  | Error e ->
      println ("Error opening SSTable: " ^ e);
      Error (Failure e)
  | Ok reader ->
      let entry_count = Storage.Lsm.Sstable.entry_count reader in
      let first_key = Storage.Lsm.Sstable.first_key reader in
      let last_key = Storage.Lsm.Sstable.last_key reader in
      
      let first_hex = Data.Base16.encode_bytes first_key in
      let last_hex = Data.Base16.encode_bytes last_key in
      
      println ("\nSSTable: " ^ path);
      println ("  Total entries: " ^ string_of_int entry_count);
      println ("  First key: " ^ String.sub first_hex 0 (min 16 (String.length first_hex)) ^ "...");
      println ("  Last key:  " ^ String.sub last_hex 0 (min 16 (String.length last_hex)) ^ "...");
      println ("\nShowing first " ^ string_of_int (min limit entry_count) ^ " entries:");
      println "";
      
      let count = ref 0 in
      let high_byte_count = ref 0 in
      let total_iterated = ref 0 in
      
      Storage.Lsm.Sstable.iter reader ~f:(fun ~key ~value ->
        total_iterated := !total_iterated + 1;
        let first_byte = Bytes.get key 0 |> Char.code in
        if first_byte >= 0x80 then
          high_byte_count := !high_byte_count + 1;
        
        if !count < limit then begin
          let key_hex = Data.Base16.encode_bytes key in
          println ("  " ^ string_of_int (!count + 1) ^ ". Key: " ^ String.sub key_hex 0 (min 16 (String.length key_hex)) ^ "...");
          println ("     Value size: " ^ string_of_int (Bytes.length value) ^ " bytes");
          
          (* Try to show value if it looks like text (small value) *)
          if Bytes.length value < 200 then begin
            (* Try to interpret as UTF-8 string *)
            let str = Bytes.to_string value in
            (* Check if it's printable ASCII/UTF-8 (rough heuristic) *)
            let is_text = try
              String.iter (fun c ->
                let code = Char.code c in
                if code < 32 && code != 10 && code != 13 && code != 9 then raise Exit
              ) str;
              true
            with Exit -> false in
            
            if is_text then
              println ("     Value: " ^ (if String.length str > 60 then String.sub str 0 60 ^ "..." else str))
            else
              println ("     (Binary data)")
          end else begin
            println ("     (Large binary data)")
          end;
          
          count := !count + 1;
          println ""
        end
      );
      
      if !count = 0 then
        println "  (No entries found)";
      
      println "";
      println ("Statistics:");
      println ("  Total entries iterated: " ^ string_of_int !total_iterated ^ " (expected: " ^ string_of_int entry_count ^ ")");
      println ("  High-byte keys (>= 0x80): " ^ string_of_int !high_byte_count ^ " (" ^ 
        string_of_int (!high_byte_count * 100 / max 1 !total_iterated) ^ "%)");
      println "";
      
      Storage.Lsm.Sstable.close reader;
      Ok ()
