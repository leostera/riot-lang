(** Explicit compaction command for CLI users *)

open Std

let command =
  let open ArgParser in
  let open Arg in
  command "compact"
  |> about "Aggressively compact SSTables until database is fully compacted"
  |> args [
      positional "db" |> help "Database path";
      option "tier" |> long "tier" |> help "Specific tier to compact (default: all tiers)";
      option "threshold" |> long "threshold" |> default "1" |> help "Min files to trigger (default: 1 for aggressive)";
      option "max-merge" |> long "max-merge" |> default "1000" |> help "Max files per merge (default: 1000 for aggressive)";
      flag "all" |> long "all" |> help "Compact all tiers (default behavior)";
    ]

let run matches =
  let open ArgParser in
  let db = get_one matches "db" |> Option.expect ~msg:"db required" in
  let specific_tier = get_one matches "tier" |> Option.map int_of_string in
  let threshold = get_one matches "threshold" 
    |> Option.map int_of_string 
    |> Option.unwrap_or ~default:1 in
  let max_merge = get_one matches "max-merge" 
    |> Option.map int_of_string 
    |> Option.unwrap_or ~default:1000 in
  
  println ("Aggressively compacting database: " ^ db);
  println ("  Threshold: " ^ string_of_int threshold);
  println ("  Max merge: " ^ string_of_int max_merge);
  println ("  (Will compact all tiers repeatedly until fully compacted)");
  
  match Graph_store.open_exclusive ~data_dir:db () with
  | Error e ->
      println ("Error: Failed to open database: " ^ e);
      println "Note: Database may be locked by another process (e.g., a concurrent query or write)";
      Error (Failure e)
  | Ok graph ->
      (* Compact all tiers by default, or specific tier if requested *)
      let compact_result = match specific_tier with
      | Some tier ->
          println ("  Compacting tier: " ^ string_of_int tier);
          (* Keep compacting until done *)
          let rec compact_until_done iteration =
            println ("  Iteration " ^ string_of_int iteration ^ "...");
            
            match Graph_store.compact_tier graph ~tier ~threshold ~max_merge () with
            | Error e ->
                println ("  Error: " ^ e);
                Error (Failure e)
            | Ok false ->
                println ("  No more compaction needed");
                Ok ()
            | Ok true ->
                println ("  Compacted some SSTables");
                compact_until_done (iteration + 1)
          in
          compact_until_done 1
          
      | None ->
          println ("  Aggressively compacting all tiers until fully compacted...");
          
          (* Aggressive compaction: repeatedly compact ALL tiers until nothing changes *)
          let rec compact_pass pass_num =
            println ("  === Pass " ^ string_of_int pass_num ^ " ===");
            let any_work = ref false in
            
            (* Compact each tier in this pass - support up to tier 10 for heavily compacted DBs *)
            let rec compact_tier_in_pass tier =
              if tier > 10 then !any_work
              else begin
                (* Keep compacting this tier until it's done *)
                let rec compact_tier_loop iteration =
                  match Graph_store.compact_tier graph ~tier ~threshold ~max_merge () with
                  | Ok true -> 
                      if iteration = 1 then
                        println ("  Tier " ^ string_of_int tier ^ ": compacting...");
                      any_work := true;
                      compact_tier_loop (iteration + 1)
                  | Ok false -> 
                      if iteration > 1 then
                        println ("  Tier " ^ string_of_int tier ^ ": done (" ^ string_of_int (iteration - 1) ^ " iterations)");
                      ()
                  | Error e -> 
                      println ("  Tier " ^ string_of_int tier ^ ": error: " ^ e);
                      ()
                in
                
                compact_tier_loop 1;
                compact_tier_in_pass (tier + 1)
              end
            in
            
            let had_work = compact_tier_in_pass 0 in
            
            if had_work then begin
              println ("  Pass " ^ string_of_int pass_num ^ " completed, checking for more work...");
              compact_pass (pass_num + 1)
            end else begin
              println ("  No more work in pass " ^ string_of_int pass_num ^ " - fully compacted!");
              Ok ()
            end
          in
          
          compact_pass 1
      in
      
      match compact_result with
      | Ok () ->
          println "✓ Compaction complete";
          
          (* Always cleanup orphaned SST files *)
          println "";
          println "Cleaning up orphaned SST files...";
          
          Graph_store.cleanup_orphaned_files graph;
          
          println "✓ Cleanup complete";
          
          Graph_store.close graph;
          Ok ()
      | Error e ->
          Graph_store.close graph;
          Error e
