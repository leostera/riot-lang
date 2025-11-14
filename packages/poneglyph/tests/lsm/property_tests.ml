(** Property-Based Tests for LSM Storage Layers
    
    These tests use Propane to verify properties that should hold
    for all inputs, with automatic shrinking to find minimal counter-examples.
*)

open Std
open Std.Collections
open Std.Sync
open Propane
open Poneglyph

module Encoding = Poneglyph.Storage.Lsm.Encoding
module Key = Poneglyph.Storage.Lsm.Key
module Block = Poneglyph.Storage.Lsm.Block
module SSTable = Poneglyph.Storage.Lsm.Sstable
module Bytes = Kernel.IO.Bytes

(** {1 Helpers} *)

(** Take first n elements from a list *)
let list_take n lst =
  let rec take acc n lst =
    if n <= 0 then List.rev acc
    else match lst with
    | [] -> List.rev acc
    | x :: xs -> take (x :: acc) (n - 1) xs
  in
  take [] n lst

(** {1 Custom Arbitraries} *)

(** Generate random int64 values *)
let arb_int64 =
  Arbitrary.make
    ~print:Int64.to_string
    Generator.(map Int64.of_int int)

(** Generate random float values (avoiding NaN/infinity for now) *)
let arb_finite_float =
  Arbitrary.make
    ~print:string_of_float
    Generator.(map (fun x -> float_of_int x /. 10.0) int)

(** Generate random DateTime - limit range to avoid precision issues *)
let arb_datetime =
  Arbitrary.make
    ~print:(fun dt -> string_of_float (Datetime.to_timestamp dt))
    Generator.(map (fun secs -> Datetime.from_unix_time (float_of_int secs))
               (int_range 0 1000000))  (* Limited range for tests *)

(** Generate random 41-byte keys (simplified: just use entity_id variation) *)
let arb_eavt_key =
  Arbitrary.make
    ~print:(fun k ->
      let bytes = Key.encode_eavt k in
      "Key(entity=" ^ Int64.to_string k.entity_id ^ ")")
    Generator.(map (fun entity_id ->
      let k : Key.eavt_key = {
        entity_id = Int64.of_int entity_id;
        attr_id = 1L;
        value_kind = Encoding.VK_Int;
        value_repr = 42L;
        tx_id = 1L;
        fact_id = Int64.of_int entity_id;
      } in
      k
    ) (int_range 1 10000))

(** Generate random bytes of a given length *)
let arb_bytes len =
  Arbitrary.make
    ~print:(fun b -> "Bytes(" ^ string_of_int (Bytes.length b) ^ ")")
    Generator.(map Bytes.of_string (string_size (return len) char))

(** Generate sorted list of EAVT keys *)
let arb_sorted_keys =
  Arbitrary.make
    ~print:(fun ks -> "SortedKeys(" ^ string_of_int (List.length ks) ^ ")")
    Generator.(
      map (fun entity_ids ->
        (* Sort and deduplicate *)
        let sorted = List.sort compare entity_ids in
        let deduped = List.fold_left (fun acc x ->
          match acc with
          | [] -> [x]
          | hd :: _ when hd = x -> acc
          | _ -> x :: acc
        ) [] sorted |> List.rev in
        List.map (fun entity_id ->
          let k : Key.eavt_key = {
            entity_id = Int64.of_int entity_id;
            attr_id = 1L;
            value_kind = Encoding.VK_Int;
            value_repr = 42L;
            tx_id = 1L;
            fact_id = Int64.of_int entity_id;
          } in
          k
        ) deduped
      ) (list (int_range 1 10000))
    )

(** {1 Layer 1: Encoding Properties} *)

(** Property: ID encoding is a round-trip *)
let prop_id_roundtrip =
  property "ID encoding round-trip"
    arb_int64
    (fun id ->
      let encoded = Encoding.encode_id id in
      let decoded = Encoding.decode_id encoded in
      decoded = id
    )

(** Property: Float encoding is deterministic *)
let prop_float_deterministic =
  property "Float encoding is deterministic"
    arb_finite_float
    (fun f ->
      let e1 = Encoding.encode_float f in
      let e2 = Encoding.encode_float f in
      e1 = e2
    )

(** Property: Float encoding round-trip *)
let prop_float_roundtrip =
  property "Float encoding round-trip"
    arb_finite_float
    (fun f ->
      let encoded = Encoding.encode_float f in
      let decoded = Encoding.decode_float encoded in
      (* Allow small floating point errors *)
      let diff = if f < decoded then decoded -. f else f -. decoded in
      diff < 1e-10
    )

(** Property: DateTime encoding round-trip *)
let prop_datetime_roundtrip =
  property "DateTime encoding round-trip"
    arb_datetime
    (fun dt ->
      let encoded = Encoding.encode_datetime dt in
      let decoded = Encoding.decode_datetime encoded in
      (* Allow 1 microsecond tolerance *)
      let diff = if Datetime.to_timestamp dt < Datetime.to_timestamp decoded
                 then Datetime.to_timestamp decoded -. Datetime.to_timestamp dt
                 else Datetime.to_timestamp dt -. Datetime.to_timestamp decoded in
      diff < 0.000001
    )

(** Property: EAVT key encoding round-trip *)
let prop_eavt_key_roundtrip =
  property "EAVT key encoding round-trip"
    arb_eavt_key
    (fun key ->
      let encoded = Key.encode_eavt key in
      let decoded = Key.decode_eavt encoded in
      decoded.entity_id = key.entity_id &&
      decoded.attr_id = key.attr_id &&
      decoded.value_repr = key.value_repr &&
      decoded.tx_id = key.tx_id &&
      decoded.fact_id = key.fact_id
    )

(** {1 Layer 2: Block Properties} *)

(** Property: Block serialization round-trip (empty block) *)
let prop_block_empty_roundtrip =
  property "Empty block serialization round-trip"
    Arbitrary.int  (* Dummy input, we ignore it *)
    (fun _ ->
      let block = Block.create () in
      let bytes = Block.to_bytes block in
      match Block.from_bytes bytes with
      | Error _ -> false
      | Ok block2 ->
          Block.is_empty block2 &&
          Block.count block2 = 0
    )

(** Property: Block preserves key-value pairs *)
let prop_block_preserves_data =
  property "Block preserves key-value data"
    Arbitrary.(pair arb_eavt_key (arb_bytes 10))
    (fun (key_record, value) ->
      let block = Block.create () in
      let key = Key.encode_eavt key_record in
      
      match Block.add block ~key ~value with
      | Error _ -> assume_fail ()  (* Skip if doesn't fit *)
      | Ok block ->
          match Block.get block ~key with
          | None -> false
          | Some retrieved -> Bytes.compare retrieved value = 0
    )

(** Property: Block iteration gives back all added keys *)
let prop_block_iteration_complete =
  property "Block iteration returns all added entries"
    arb_sorted_keys
    (fun keys ->
      (* Limit to keys that will fit in a block *)
      let keys = list_take 10 keys in
      assume (List.length keys > 0);
      
      let block = cell (Block.create ()) in
      let expected_count = cell 0 in
      
      (* Add all keys *)
      let rec add_all ks =
        match ks with
        | [] -> true
        | k :: rest ->
            let key = Key.encode_eavt k in
            let value = Bytes.of_string ("v_" ^ Int64.to_string k.entity_id) in
            match Block.add (Cell.get block) ~key ~value with
            | Error _ -> false  (* Shouldn't fail with small list *)
            | Ok b ->
                Cell.set block b;
                Cell.set expected_count (Cell.get expected_count + 1);
                add_all rest
      in
      
      if not (add_all keys) then false
      else
        (* Count entries via iteration *)
        let actual_count = cell 0 in
        Block.iter (Cell.get block) ~f:(fun ~key:_ ~value:_ ->
          Cell.set actual_count (Cell.get actual_count + 1)
        );
        
        Cell.get actual_count = Cell.get expected_count
    )

(** {1 Layer 3: SSTable Properties} *)

(** Property: SSTable write/read round-trip for single entry *)
let prop_sstable_single_roundtrip =
  property "SSTable single entry round-trip"
    Arbitrary.(pair arb_eavt_key (arb_bytes 20))
    (fun (key_record, value) ->
      (* Use a unique temp file *)
      let path = "/tmp/propane_sst_" ^ string_of_int (Random.int 1000000) ^ ".sst" in
      
      let key = Key.encode_eavt key_record in
      let builder = SSTable.create_builder ~path in
      
      match SSTable.add builder ~key ~value with
      | Error _ -> false
      | Ok builder ->
          match SSTable.finalize builder with
          | Error _ -> false
          | Ok count ->
              if count != 1 then false
              else
                match SSTable.open_read ~path with
                | Error _ -> false
                | Ok reader ->
                    let result = match SSTable.get reader ~key with
                      | None -> false
                      | Some retrieved -> Bytes.compare retrieved value = 0
                    in
                    SSTable.close reader;
                    (* Clean up *)
                    let _ = Fs.remove_file (Path.v path) in
                    result
    )

(** Property: SSTable query matches what was written *)
let prop_sstable_query_matches_write =
  property "SSTable query returns written data"
    arb_sorted_keys
    (fun keys ->
      (* Limit number of keys *)
      let keys = list_take 20 keys in
      assume (List.length keys > 0);
      
      let path = "/tmp/propane_sst_" ^ string_of_int (Random.int 1000000) ^ ".sst" in
      let builder = cell (SSTable.create_builder ~path) in
      
      (* Build expected map *)
      let expected = Collections.HashMap.create () in
      
      (* Add all keys to SSTable and expected map *)
      let rec add_all ks =
        match ks with
        | [] -> true
        | k :: rest ->
            let key = Key.encode_eavt k in
            let value = Bytes.of_string ("val_" ^ Int64.to_string k.entity_id) in
            Collections.HashMap.insert expected key value;
            
            match SSTable.add (Cell.get builder) ~key ~value with
            | Error _ -> false
            | Ok b ->
                Cell.set builder b;
                add_all rest
      in
      
      if not (add_all keys) then false
      else
        match SSTable.finalize (Cell.get builder) with
        | Error _ -> false
        | Ok _count ->
            match SSTable.open_read ~path with
            | Error _ -> false
            | Ok reader ->
                (* Verify all keys *)
                let all_match = List.for_all (fun k ->
                  let key = Key.encode_eavt k in
                  match (SSTable.get reader ~key, Collections.HashMap.get expected key) with
                  | (Some got, Some expected_val) -> Bytes.compare got expected_val = 0
                  | _ -> false
                ) keys in
                
                SSTable.close reader;
                let _ = Fs.remove_file (Path.v path) in
                all_match
    )

(** {1 Test Suite} *)

let tests =
  [
    (* Encoding properties *)
    prop_id_roundtrip;
    prop_float_deterministic;
    prop_float_roundtrip;
    prop_datetime_roundtrip;
    prop_eavt_key_roundtrip;
    
    (* Block properties *)
    prop_block_empty_roundtrip;
    prop_block_preserves_data;
    prop_block_iteration_complete;
    
    (* SSTable properties *)
    prop_sstable_single_roundtrip;
    prop_sstable_query_matches_write;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args ->
      Test.Cli.main ~name:"poneglyph/lsm/properties" ~tests ~args)
    ~args:Env.args ()
