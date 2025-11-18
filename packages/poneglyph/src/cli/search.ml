open Std
module Bytes = Kernel.IO.Bytes

let command =
  let open ArgParser in
  let open Arg in
  command "search"
  |> about "Search for a specific key in the database"
  |> args [
      positional "db" |> help "Database directory";
      positional "key" |> help "Key to search (hex string)";
    ]

let search_in_index ~db_path ~index_name ~key =
  let index_dir = db_path ^ "/" ^ index_name in
  
  match Fs.exists (Path.v index_dir) with
  | Error _ | Ok false ->
      println ("  " ^ index_name ^ ": INDEX NOT FOUND");
      ()
  | Ok true ->
      (* List all .sst files *)
      match Fs.read_dir (Path.v index_dir) with
      | Error _ ->
          println ("  " ^ index_name ^ ": ERROR reading directory")
      | Ok entries_iter ->
          let entries = Iter.MutIterator.to_list entries_iter in
          
          (* Filter for .sst files and sort *)
          let sst_files = List.filter (fun path ->
            String.ends_with ~suffix:".sst" (Path.to_string path)
          ) entries in
          let sst_files = List.sort (fun a b -> 
            String.compare (Path.to_string a) (Path.to_string b)
          ) sst_files in
          
          if List.length sst_files = 0 then begin
            println ("  " ^ index_name ^ ": NO SSTABLES");
          end else begin
            (* Find which SSTable(s) should contain this key based on range *)
            let found_in_range = ref false in
            let found_value = ref false in
            
            List.iter (fun sst_path ->
              let full_path = index_dir ^ "/" ^ Path.to_string sst_path in
              match Storage.Lsm.Sstable.open_read ~path:full_path with
              | Error e ->
                  println ("  " ^ index_name ^ "/" ^ Path.basename sst_path ^ ": ERROR - " ^ e)
              | Ok reader ->
                  let first_key = Storage.Lsm.Sstable.first_key reader in
                  let last_key = Storage.Lsm.Sstable.last_key reader in
                  
                  (* Check if key is in range *)
                  let in_range = Bytes.compare key first_key >= 0 && Bytes.compare key last_key <= 0 in
                  
                  if in_range then begin
                    found_in_range := true;
                    
                    let first_hex = Data.Base16.encode_bytes first_key in
                    let last_hex = Data.Base16.encode_bytes last_key in
                    
                    println ("  " ^ index_name ^ "/" ^ Path.basename sst_path ^ ": IN RANGE");
                    println ("    Range: " ^ String.sub first_hex 0 8 ^ "... → " ^ String.sub last_hex 0 8 ^ "...");
                    
                    (* Try to actually get the key *)
                    match Storage.Lsm.Sstable.get reader ~key with
                    | Some value ->
                        found_value := true;
                        println ("    Result: ✓ FOUND (value size: " ^ string_of_int (Bytes.length value) ^ " bytes)");
                        
                        (* Try to decode the value if it's from URIS index *)
                        if index_name = "uris" then begin
                          (* Value IS the URI string *)
                          let uri_str = Bytes.to_string value in
                          println ("    URI: " ^ uri_str)
                        end
                    | None ->
                        println ("    Result: ✗ NOT FOUND");
                        println ("    (Key is in range but not found - bloom filter or missing data)")
                  end;
                  
                  Storage.Lsm.Sstable.close reader
            ) sst_files;
            
            if not !found_in_range then
              println ("  " ^ index_name ^ ": NOT IN RANGE (checked " ^ string_of_int (List.length sst_files) ^ " SSTables)");
            
            if !found_in_range && not !found_value then
              println ("  " ^ index_name ^ ": IN RANGE BUT NOT FOUND!");
          end

let run matches =
  let open ArgParser in
  let db_path = get_one matches "db" |> Option.expect ~msg:"db required" in
  let key_hex = get_one matches "key" |> Option.expect ~msg:"key required" in
  
  (* Parse hex key *)
  let key = match Data.Base16.decode key_hex with
    | Ok k -> String.to_bytes k
    | Error _ ->
        println "Error: Invalid hex key";
        exit 1
  in
  
  println ("\nSearching for key: " ^ String.sub key_hex 0 (min 16 (String.length key_hex)) ^ "...");
  println ("Key length: " ^ string_of_int (Bytes.length key) ^ " bytes");
  println "";
  
  (* Search in each index *)
  let indices = ["eavt"; "avet"; "vaet"; "fact"; "uris"] in
  List.iter (fun index_name ->
    search_in_index ~db_path ~index_name ~key
  ) indices;
  
  println "";
  Ok ()
