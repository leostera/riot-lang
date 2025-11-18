open Std
module Bytes = Kernel.IO.Bytes

module Sstable = struct
  let command =
    let open ArgParser in
    let open Arg in
    command "inspect-sstable"
    |> about "Inspect SSTable file metadata"
    |> args [
        positional "path" |> help "Path to SSTable file";
      ]

  let run matches =
    let open ArgParser in
    let path = get_one matches "path" |> Option.expect ~msg:"path required" in
    
    match Storage.Lsm.Sstable.open_read ~path with
    | Error e ->
        println ("Error opening SSTable: " ^ e);
        Error (Failure e)
    | Ok reader ->
        let first_key = Storage.Lsm.Sstable.first_key reader in
        let last_key = Storage.Lsm.Sstable.last_key reader in
        let entry_count = Storage.Lsm.Sstable.entry_count reader in
        let block_count = Storage.Lsm.Sstable.block_count reader in
        
        let first_hex = Data.Base16.encode_bytes first_key in
        let last_hex = Data.Base16.encode_bytes last_key in
        
        (* Get file size *)
        let file_size = match Fs.metadata (Path.v path) with
          | Ok meta -> Fs.Metadata.len meta
          | Error _ -> 0
        in
        
        println ("\nSSTable: " ^ path);
        println ("  First key: " ^ String.sub first_hex 0 (min 16 (String.length first_hex)) ^ "...");
        println ("  Last key:  " ^ String.sub last_hex 0 (min 16 (String.length last_hex)) ^ "...");
        println ("  Entries:   " ^ string_of_int entry_count);
        println ("  Blocks:    " ^ string_of_int block_count);
        println ("  Size:      " ^ string_of_int file_size ^ " bytes");
        println "";
        
        Storage.Lsm.Sstable.close reader;
        Ok ()
end

module Index = struct
  let command =
    let open ArgParser in
    let open Arg in
    command "inspect-index"
    |> about "Inspect all SSTables in a database index"
    |> args [
        positional "db" |> help "Database directory";
        positional "index" |> help "Index name (eavt, avet, vaet, fact, uris)";
      ]

  let run matches =
    let open ArgParser in
    let db_path = get_one matches "db" |> Option.expect ~msg:"db required" in
    let index_name = get_one matches "index" |> Option.expect ~msg:"index required" in
    
    let index_dir = db_path ^ "/" ^ index_name in
    
    (* Check if directory exists *)
    match Fs.exists (Path.v index_dir) with
    | Error _ ->
        println ("Error checking directory: " ^ index_dir);
        Error (Failure "directory check failed")
    | Ok false ->
        println ("Index directory does not exist: " ^ index_dir);
        Error (Failure "directory not found")
    | Ok true ->
        (* List all .sst files *)
        match Fs.read_dir (Path.v index_dir) with
        | Error _ ->
            println ("Error reading directory: " ^ index_dir);
            Error (Failure "directory read failed")
        | Ok entries_iter ->
            let entries = Iter.MutIterator.to_list entries_iter in
            
            (* Filter for .sst files and sort *)
            let sst_files = List.filter (fun path ->
              String.ends_with ~suffix:".sst" (Path.to_string path)
            ) entries in
            let sst_files = List.sort (fun a b -> 
              String.compare (Path.to_string a) (Path.to_string b)
            ) sst_files in
            
            println ("\nIndex: " ^ index_name ^ " (" ^ string_of_int (List.length sst_files) ^ " SSTables)");
            println "";
            
            (* Inspect each SSTable *)
            List.iter (fun sst_path ->
              let full_path = index_dir ^ "/" ^ Path.to_string sst_path in
              match Storage.Lsm.Sstable.open_read ~path:full_path with
              | Error e ->
                  println ("  ERROR: " ^ Path.basename sst_path ^ " - " ^ e)
              | Ok reader ->
                  let first_key = Storage.Lsm.Sstable.first_key reader in
                  let last_key = Storage.Lsm.Sstable.last_key reader in
                  let entry_count = Storage.Lsm.Sstable.entry_count reader in
                  
                  let first_hex = Data.Base16.encode_bytes first_key in
                  let last_hex = Data.Base16.encode_bytes last_key in
                  
                  println ("  " ^ Path.basename sst_path ^ ": " ^ 
                    String.sub first_hex 0 (min 8 (String.length first_hex)) ^ "... → " ^
                    String.sub last_hex 0 (min 8 (String.length last_hex)) ^ "... " ^
                    "(" ^ string_of_int entry_count ^ " entries)");
                  
                  Storage.Lsm.Sstable.close reader
            ) sst_files;
            
            println "";
            Ok ()
end
