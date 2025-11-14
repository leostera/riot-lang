(** SSTable - Sorted String Table for persistent storage *)

open Std
open Std.Collections
open Std.Sync

module Bytes = Kernel.IO.Bytes
module Block = Block
module File = Fs.File

(** Footer constants *)
let footer_size = 256
let magic = "SST1"
let version = 1

(** Index entry: maps first key of a block to its file offset *)
type index_entry = {
  first_key : bytes;
  offset : int;  (* byte offset in file where block starts *)
}

(** SSTable builder state *)
type builder = {
  path : string;
  file : File.t Cell.t;  (* file handle *)
  current_block : Block.t Cell.t;  (* current block being filled *)
  index : index_entry Vector.t;  (* index of all blocks *)
  mutable blocks_written : int;  (* number of blocks written *)
  mutable entries_written : int;  (* total entries across all blocks *)
  mutable first_key_opt : bytes option;  (* first key in entire SSTable *)
  mutable last_key : bytes;  (* last key added *)
  mutable file_pos : int;  (* current write position in file *)
}

(** SSTable reader state *)
type reader = {
  path : string;
  file : File.t Cell.t;
  index : index_entry Vector.t;  (* loaded into memory *)
  first_key : bytes;
  last_key : bytes;
  block_count : int;
  entry_count : int;
  index_offset : int;
}

(** Create a new SSTable builder *)
let create_builder ~path =
  (* Open file for writing *)
  let file = File.create (Path.v path) |> Result.expect ~msg:"Failed to create SSTable file" in
  {
    path;
    file = cell file;
    current_block = cell (Block.create ());
    index = Vector.create ();
    blocks_written = 0;
    entries_written = 0;
    first_key_opt = None;
    last_key = Bytes.create 0;
    file_pos = 0;
  }

(** Flush current block to disk *)
let flush_block builder =
  let block = Cell.get builder.current_block in
  
  if Block.is_empty block then Ok ()
  else (
    (* Serialize block *)
    let block_bytes = Block.to_bytes block in
    let block_size = Bytes.length block_bytes in
    
    (* Record index entry (first key of this block → file offset) *)
    let first_key = Block.first_key block |> Option.expect ~msg:"block has first key" in
    Vector.push builder.index { first_key; offset = builder.file_pos };
    
    (* Write block to file *)
    let file = Cell.get builder.file in
    let _ = File.write file block_bytes ~offset:0 ~len:(Bytes.length block_bytes) 
            |> Result.expect ~msg:"Failed to write block" in
    
    (* Update state *)
    builder.blocks_written <- builder.blocks_written + 1;
    builder.entries_written <- builder.entries_written + Block.count block;
    builder.file_pos <- builder.file_pos + block_size;
    
    (* Reset current block *)
    Cell.set builder.current_block (Block.create ());
    
    Ok ()
  )

(** Add a key-value pair to the SSTable *)
let add builder ~key ~value =
  (* Check key ordering (must be strictly increasing) *)
  let is_out_of_order =
    match builder.first_key_opt with
    | None -> false  (* First key, can't be out of order *)
    | Some _ when Bytes.length builder.last_key = 0 -> false  (* Shouldn't happen *)
    | Some _ -> Bytes.compare key builder.last_key <= 0
  in
  
  if is_out_of_order then
    Error "Keys must be added in strictly increasing order"
  else (
    (* Track first key *)
    (match builder.first_key_opt with
     | None -> builder.first_key_opt <- Some key
     | Some _ -> ());
    
    builder.last_key <- key;
    
    (* Try to add to current block *)
    let block = Cell.get builder.current_block in
    match Block.add block ~key ~value with
    | Ok _ ->
        (* Successfully added to current block *)
        Ok builder
    | Error _ ->
        (* Block is full, flush it and create new block *)
        match flush_block builder with
        | Error e -> Error e
        | Ok () ->
            (* Add to new block *)
            let new_block = Block.create () in
            match Block.add new_block ~key ~value with
            | Error e -> Error ("Failed to add to new block: " ^ e)
            | Ok _ ->
                Cell.set builder.current_block new_block;
                Ok builder
  )

(** Write index block to file
    
    Index format:
    [count: 4 bytes]
    [entry_0: first_key (41 bytes) + offset (8 bytes)]
    [entry_1: first_key (41 bytes) + offset (8 bytes)]
    ...
*)
let write_index (builder : builder) =
  let index_count = Vector.len builder.index in
  let entry_size = 41 + 8 in  (* key + offset *)
  let index_size = 4 + (index_count * entry_size) in
  
  let index_buf = Bytes.create index_size in
  
  (* Write count *)
  Bytes.set_int32_be index_buf 0 (Int32.of_int index_count);
  
  (* Write entries *)
  for i = 0 to index_count - 1 do
    let entry = Vector.get builder.index i |> Option.expect ~msg:"index entry exists" in
    let pos = 4 + (i * entry_size) in
    
    (* Write key (41 bytes) *)
    Bytes.blit entry.first_key 0 index_buf pos 41;
    
    (* Write offset (8 bytes) *)
    Bytes.set_int64_be index_buf (pos + 41) (Int64.of_int entry.offset);
  done;
  
  (* Write to file *)
  let file = Cell.get builder.file in
  let _ = File.write file index_buf ~offset:0 ~len:(Bytes.length index_buf)
          |> Result.expect ~msg:"Failed to write index" in
  
  let index_offset = builder.file_pos in
  builder.file_pos <- builder.file_pos + index_size;
  
  (index_offset, index_size)

(** Write footer to file
    
    Footer format (256 bytes):
    [magic: 4 bytes]
    [version: 1 byte]
    [block_count: 4 bytes]
    [entry_count: 8 bytes]
    [first_key: 41 bytes]
    [last_key: 41 bytes]
    [index_offset: 8 bytes]
    [index_size: 4 bytes]
    [checksum: 8 bytes]
    [reserved: 177 bytes]
*)
let write_footer (builder : builder) ~index_offset ~index_size =
  let footer = Bytes.create footer_size in
  
  (* Magic *)
  Bytes.blit_string magic 0 footer 0 4;
  
  (* Version *)
  Bytes.set_uint8 footer 4 version;
  
  (* Block count *)
  Bytes.set_int32_be footer 5 (Int32.of_int builder.blocks_written);
  
  (* Entry count *)
  Bytes.set_int64_be footer 9 (Int64.of_int builder.entries_written);
  
  (* First key *)
  (match builder.first_key_opt with
   | Some k -> Bytes.blit k 0 footer 17 41
   | None -> ());  (* Leave as zeros if empty *)
  
  (* Last key *)
  if builder.entries_written > 0 then
    Bytes.blit builder.last_key 0 footer 58 41;
  
  (* Index offset *)
  Bytes.set_int64_be footer 99 (Int64.of_int index_offset);
  
  (* Index size *)
  Bytes.set_int32_be footer 107 (Int32.of_int index_size);
  
  (* Checksum (simple: XOR of all previous bytes) *)
  let checksum = cell 0L in
  for i = 0 to 110 do
    let byte = Int64.of_int (Bytes.get_uint8 footer i) in
    Cell.set checksum (Int64.logxor (Cell.get checksum) byte);
  done;
  Bytes.set_int64_be footer 111 (Cell.get checksum);
  
  (* Write footer *)
  let file = Cell.get builder.file in
  let _ = File.write file footer ~offset:0 ~len:(Bytes.length footer)
          |> Result.expect ~msg:"Failed to write footer" in
  ()

(** Finalize the SSTable *)
let finalize builder =
  (* Flush current block if not empty *)
  match flush_block builder with
  | Error e -> Error e
  | Ok () ->
      (* Write index *)
      let (index_offset, index_size) = write_index builder in
      
      (* Write footer *)
      write_footer builder ~index_offset ~index_size;
      
      (* Close file *)
      let file = Cell.get builder.file in
      let _ = File.close file in
      
      Ok builder.entries_written

(** Read footer from file *)
let read_footer file =
  (* Get file size via metadata *)
  let metadata = File.metadata file |> Result.expect ~msg:"Failed to get file metadata" in
  let file_size = Int64.of_int metadata.st_size in
  
  (* Seek to footer (last 256 bytes) *)
  let _ = File.seek file (Int64.sub file_size (Int64.of_int footer_size))
          |> Result.expect ~msg:"Failed to seek to footer" in
  
  (* Read footer *)
  let footer = Bytes.create footer_size in
  let _ = File.read_exact file footer ~offset:0 ~len:footer_size
          |> Result.expect ~msg:"Failed to read footer" in
  
  (* Validate magic *)
  let magic_bytes = Bytes.sub footer 0 4 in
  if Bytes.to_string magic_bytes != magic then
    Error "Invalid SSTable magic number"
  else
    (* Validate version *)
    let ver = Bytes.get_uint8 footer 4 in
    if ver != version then
      Error ("Unsupported SSTable version: " ^ string_of_int ver)
    else
      (* Verify checksum *)
      let stored_checksum = Bytes.get_int64_be footer 111 in
      let computed = cell 0L in
      for i = 0 to 110 do
        let byte = Int64.of_int (Bytes.get_uint8 footer i) in
        Cell.set computed (Int64.logxor (Cell.get computed) byte);
      done;
      
      if Cell.get computed != stored_checksum then
        Error "SSTable footer checksum mismatch"
      else
        (* Extract metadata *)
        let block_count = Int32.to_int (Bytes.get_int32_be footer 5) in
        let entry_count = Int64.to_int (Bytes.get_int64_be footer 9) in
        let first_key = Bytes.sub footer 17 41 in
        let last_key = Bytes.sub footer 58 41 in
        let index_offset = Int64.to_int (Bytes.get_int64_be footer 99) in
        let index_size = Int32.to_int (Bytes.get_int32_be footer 107) in
        
        Ok (block_count, entry_count, first_key, last_key, index_offset, index_size)

(** Read index from file *)
let read_index file ~offset ~size =
  let _ = File.seek file (Int64.of_int offset)
          |> Result.expect ~msg:"Failed to seek to index" in
  
  let index_buf = Bytes.create size in
  let _ = File.read_exact file index_buf ~offset:0 ~len:size
          |> Result.expect ~msg:"Failed to read index" in
  
  (* Read count *)
  let count = Int32.to_int (Bytes.get_int32_be index_buf 0) in
  
  let index = Vector.create () in
  let entry_size = 41 + 8 in
  
  for i = 0 to count - 1 do
    let pos = 4 + (i * entry_size) in
    
    (* Read key *)
    let first_key = Bytes.sub index_buf pos 41 in
    
    (* Read offset *)
    let offset = Int64.to_int (Bytes.get_int64_be index_buf (pos + 41)) in
    
    Vector.push index { first_key; offset };
  done;
  
  Ok index

(** Open SSTable for reading *)
let open_read ~path =
  match Fs.metadata (Path.v path) with
  | Error _ -> Error ("SSTable file not found: " ^ path)
  | Ok _ ->
      match File.open_read (Path.v path) with
      | Error _ -> Error ("Failed to open SSTable file: " ^ path)
      | Ok file ->
      
          match read_footer file with
          | Error e ->
              let _ = File.close file in
              Error e
          | Ok (block_count, entry_count, first_key, last_key, index_offset, index_size) ->
              match read_index file ~offset:index_offset ~size:index_size with
              | Error e ->
                  let _ = File.close file in
                  Error e
              | Ok index ->
                  Ok {
                    path;
                    file = cell file;
                    index;
                    first_key;
                    last_key;
                    block_count;
                    entry_count;
                    index_offset;
                  }

(** Find the block that might contain the key using binary search on index *)
let find_block_index reader ~key =
  let rec search low high =
    if low > high then
      (* Key might be in the last block we checked *)
      if low = 0 then None
      else Some (low - 1)
    else
      let mid = low + (high - low) / 2 in
      let entry = Vector.get reader.index mid |> Option.expect ~msg:"index entry exists" in
      
      match Bytes.compare key entry.first_key with
      | 0 -> Some mid  (* Exact match on first key *)
      | n when n < 0 ->
          (* Key is less than this block's first key, search earlier blocks *)
          search low (mid - 1)
      | _ ->
          (* Key is greater, might be in this block or later *)
          search (mid + 1) high
  in
  search 0 (Vector.len reader.index - 1)

(** Read a specific block from the file *)
let read_block reader ~block_idx =
  let entry = Vector.get reader.index block_idx |> Option.expect ~msg:"block index valid" in
  
  (* Seek to block offset *)
  let file = Cell.get reader.file in
  let _ = File.seek file (Int64.of_int entry.offset)
          |> Result.expect ~msg:"Failed to seek to block" in
  
  (* Determine block size *)
  let next_offset =
    if block_idx = Vector.len reader.index - 1 then
      (* Last block: ends at index start *)
      reader.index_offset
    else
      (* Not last: next block's offset *)
      let next_entry = Vector.get reader.index (block_idx + 1) 
                       |> Option.expect ~msg:"next block exists" in
      next_entry.offset
  in
  
  let block_size = next_offset - entry.offset in
  let block_bytes = Bytes.create block_size in
  let _ = File.read_exact file block_bytes ~offset:0 ~len:block_size
          |> Result.expect ~msg:"Failed to read block" in
  
  Block.from_bytes block_bytes

(** Get a value by key *)
let get reader ~key =
  (* Quick range check *)
  if Bytes.compare key reader.first_key < 0 || Bytes.compare key reader.last_key > 0 then
    None
  else
    (* Find which block might contain the key *)
    match find_block_index reader ~key with
    | None -> None
    | Some block_idx ->
        (* Read and search the block *)
        match read_block reader ~block_idx with
        | Error _ -> None  (* Block read error *)
        | Ok block -> Block.get block ~key

(** Iterate over all entries *)
let iter reader ~f =
  for i = 0 to Vector.len reader.index - 1 do
    match read_block reader ~block_idx:i with
    | Ok block -> Block.iter block ~f
    | Error _ -> ()  (* Skip corrupted blocks *)
  done

(** Accessors *)
let first_key reader = reader.first_key
let last_key reader = reader.last_key
let entry_count reader = reader.entry_count
let block_count reader = reader.block_count

let close reader =
  let file = Cell.get reader.file in
  let _ = File.close file in
  ()

let in_range reader ~key =
  Bytes.compare key reader.first_key >= 0 && Bytes.compare key reader.last_key <= 0
