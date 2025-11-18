open Std
open Std.IO
open Std.UUID
open Model

type t = { mem : Inmemory.t; mutable filename : string option }

let create () = { mem = Inmemory.create (); filename = None }

let create_with_file filename =
  { mem = Inmemory.create (); filename = Some filename }

let escape_json_string s =
  let buffer = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | c -> Buffer.add_char buffer c)
    s;
  Buffer.contents buffer

let fact_to_json (fact : Fact.t) =
  let value_str =
    match fact.value with
    | Fact.String s -> "{\"String\":\"" ^ escape_json_string s ^ "\"}"
    | Fact.Int i -> "{\"Int\":" ^ string_of_int i ^ "}"
    | Fact.Bool b -> "{\"Bool\":" ^ string_of_bool b ^ "}"
    | Fact.Float f -> "{\"Float\":" ^ string_of_float f ^ "}"
    | Fact.Uri u -> "{\"Uri\":\"" ^ Uri.to_string u ^ "\"}"
    | Fact.DateTime dt ->
        "{\"DateTime\":\"" ^ string_of_float (Datetime.to_timestamp dt) ^ "\"}"
  in
  "{\"e\":\"" ^ Uri.to_string fact.entity ^ "\",\"a\":\"" ^ Uri.to_string fact.attribute ^ "\",\"s\":\"" ^ Uri.to_string fact.source_uri ^ "\",\"v\":" ^ value_str ^ ",\"fact_uri\":\"" ^ Uri.to_string fact.fact_uri ^ "\",\"stated_at\":\"" ^ string_of_float (Datetime.to_timestamp fact.stated_at) ^ "\",\"tx_id\":\"" ^ UUID.to_string fact.tx_id ^ "\",\"retracted\":" ^ string_of_bool fact.retracted ^ "}"

let json_to_fact line =
  (* Very simple JSON parser - just enough for our needs *)
  let extract_string key line =
    let pattern = "\"" ^ key ^ "\":\"" in
    match String.split_on_char '"' line with
    | _ ->
        let start_idx = String.index line '"' in
        let rec find_key idx =
          if idx >= String.length line then panic ("Key not found: " ^ key)
          else if idx + String.length pattern <= String.length line && 
                  String.sub line idx (String.length pattern) = pattern then
            let value_start = idx + String.length pattern in
            let rec find_end i =
              if i >= String.length line then panic "Unterminated string"
              else if String.get line i = '"' && String.get line (i - 1) != '\\'
              then i
              else find_end (i + 1)
            in
            let value_end = find_end value_start in
            String.sub line value_start (value_end - value_start)
          else find_key (idx + 1)
        in
        find_key 0
  in
  let extract_int key line =
    let pattern = "\"" ^ key ^ "\":" in
    let idx = String.index line '"' in
    let rec find_key i =
      if i >= String.length line then panic ("Key not found: " ^ key)
      else if i + String.length pattern <= String.length line &&
              String.sub line i (String.length pattern) = pattern then
        let value_start = i + String.length pattern in
        let rec find_end j =
          if j >= String.length line then panic "Number not found"
          else
            match String.get line j with
            | '0' .. '9' | '-' -> find_end (j + 1)
            | _ -> j
        in
        let value_end = find_end value_start in
        int_of_string (String.sub line value_start (value_end - value_start))
      else find_key (i + 1)
    in
    find_key idx
  in
  let extract_bool key line =
    let pattern = "\"" ^ key ^ "\":true" in
    let rec contains_substr s sub =
      let len_s = String.length s in
      let len_sub = String.length sub in
      let rec check_at i =
        if i + len_sub > len_s then false
        else if String.sub s i len_sub = sub then true
        else check_at (i + 1)
      in
      check_at 0
    in
    contains_substr line pattern
  in

  let entity = Uri.of_string (extract_string "e" line) in
  let attribute = Uri.of_string (extract_string "a" line) in
  let source_uri = Uri.of_string (extract_string "s" line) in
  let fact_uri = Uri.of_string (extract_string "fact_uri" line) in
  let stated_at_ts = float_of_string (extract_string "stated_at" line) in
  let stated_at = Datetime.from_unix_time stated_at_ts in
  let tx_id_str = extract_string "tx_id" line in
  let tx_id = match UUID.of_string tx_id_str with
    | Ok uuid -> uuid
    | Error _ -> panic ("Invalid UUID for tx_id: " ^ tx_id_str)
  in
  let retracted = extract_bool "retracted" line in

  (* Parse value - find the "v":{...} part *)
  let contains_substr s sub =
    let len_s = String.length s in
    let len_sub = String.length sub in
    let rec check_at i =
      if i + len_sub > len_s then false
      else if String.sub s i len_sub = sub then true
      else check_at (i + 1)
    in
    check_at 0
  in
  let value =
    if contains_substr line "\"String\"" then
      Fact.String (extract_string "String" line)
    else if contains_substr line "\"Int\"" then
      Fact.Int (extract_int "Int" line)
    else if contains_substr line "\"Bool\"" then
      Fact.Bool (extract_bool "Bool" line)
    else if contains_substr line "\"Float\"" then
      Fact.Float (float_of_string (extract_string "Float" line))
    else if contains_substr line "\"Uri\"" then
      Fact.Uri (Uri.of_string (extract_string "Uri" line))
    else if contains_substr line "\"DateTime\"" then
      let ts = float_of_string (extract_string "DateTime" line) in
      Fact.DateTime (Datetime.from_unix_time ts)
    else panic "Unknown value type"
  in

  { Fact.fact_uri; source_uri; entity; attribute; value; stated_at; tx_id; retracted }

let load filename =
  let path = Path.v filename in
  match Fs.exists path with
  | Ok false | Error _ -> { mem = Inmemory.create (); filename = Some filename }
  | Ok true ->
      let content = match Fs.read path with Ok s -> s | Error _ -> "" in
      let lines = String.split_on_char '\n' content in
      let facts =
        List.filter_map
          (fun line ->
            if String.trim line = "" then None
            else try Some (json_to_fact line) with _ ->
              (* JSON parsing failed - skip this line *)
              None)
          lines
      in
      let mem = Inmemory.with_facts (Inmemory.create ()) facts in
      { mem; filename = Some filename }

let save _store _filename =
  (* Save is a no-op for SimpleFile - we append on every state/retract *)
  (* If we wanted to compact the file (remove retracted facts), we could do it here *)
  ()

let state store facts =
  let tx_id = Inmemory.state store.mem facts in
  (match store.filename with
  | Some filename ->
      let lines = List.map fact_to_json facts in
      let content = String.concat "\n" lines ^ "\n" in
      let path = Path.v filename in
      let file =
        Fs.File.open_append path
        |> Result.expect ~msg:("Failed to open " ^ filename)
      in
      Fs.File.write_all file content
      |> Result.expect ~msg:("Failed to append to " ^ filename);
      Fs.File.close file |> ignore
  | None -> ());
  tx_id

let retract store ~fact_uri =
  Inmemory.retract store.mem ~fact_uri;
  match store.filename with
  | Some filename ->
      (* After retraction, get the fact again to get its updated state *)
      let all_facts = Inmemory.get_all_facts store.mem ~entity:fact_uri 
        |> Iter.MutIterator.to_list in
      let retracted_fact =
        List.find_opt (fun f -> Uri.equal f.Fact.fact_uri fact_uri) all_facts
        |> Option.expect ~msg:"Fact not found"
      in
      let line = fact_to_json retracted_fact ^ "\n" in
      let path = Path.v filename in
      let file =
        Fs.File.open_append path
        |> Result.expect ~msg:("Failed to open " ^ filename)
      in
      Fs.File.write_all file line
      |> Result.expect ~msg:("Failed to append to " ^ filename);
      Fs.File.close file |> ignore
  | None -> ()

let get store ~entity ~attr = Inmemory.get store.mem ~entity ~attr
let get_all_facts store ~entity = Inmemory.get_all_facts store.mem ~entity

let get_current_facts store ~entity =
  Inmemory.get_current_facts store.mem ~entity

let exists store entity = Inmemory.exists store.mem entity
let get_kind store entity = Inmemory.get_kind store.mem entity
let list_schemas store = Inmemory.list_schemas store.mem
let get_all_current_facts store = Inmemory.get_all_current_facts store.mem
let find_entities_by_attr_value store ~attr ~value = 
  Inmemory.find_entities_by_attr_value store.mem ~attr ~value
let entity_count store = Inmemory.entity_count store.mem
let fact_count store = Inmemory.fact_count store.mem
let current_fact_count store = Inmemory.current_fact_count store.mem
