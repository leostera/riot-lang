(** Compaction - Merge multiple SSTables to reduce read amplification.

    Compaction is the process of merging multiple sorted SSTables into a single
    SSTable, removing duplicates and reclaiming space from deleted entries.

    This implementation uses a simplified size-tiered strategy:
    - Merge N SSTables into 1 larger SSTable
    - Last-write-wins for duplicate keys
    - Output is sorted by key
*)

open Std

(** {1 Core Operations} *)

(** [merge_sstables ~inputs ~output] merges multiple SSTables into one.

    The merge process:
    1. Opens all input SSTables
    2. Performs a K-way merge sort on entries
    3. Deduplicates keys (later inputs win)
    4. Writes sorted, deduplicated entries to output SSTable

    @param inputs List of SSTable file paths to merge. Order matters: later
                  SSTables have higher priority, so their values win on conflicts.
    @param output Path for the new merged SSTable.
    @return Ok () on success, Error msg on failure.

    Example:
    {[
      match Compaction.merge_sstables
        ~inputs:["old1.sst"; "old2.sst"; "old3.sst"]
        ~output:"merged.sst"
      with
      | Ok () -> Log.info "Compaction successful"
      | Error err -> Log.error "Compaction failed: %s" err
    ]}

    Invariants:
    - Output SSTable is sorted by key
    - For duplicate keys, value from the last input SSTable is kept
    - All unique keys from inputs appear in output
*)
val merge_sstables : inputs:string list -> output:string -> (unit, string) result

(** [compact ~inputs ~output ~delete_inputs] performs compaction with optional cleanup.

    This is a convenience wrapper around {!merge_sstables} that can also
    delete the input SSTables after a successful merge.

    @param inputs List of SSTable paths to merge
    @param output Path for the merged SSTable
    @param delete_inputs If true, delete input SSTables after successful merge
    @return Ok () on success, Error msg on failure

    Example:
    {[
      (* Merge and delete old SSTables *)
      match Compaction.compact
        ~inputs:["level0_1.sst"; "level0_2.sst"]
        ~output:"level1.sst"
        ~delete_inputs:true
      with
      | Ok () -> Log.info "Compacted and cleaned up"
      | Error err -> Log.error "Compaction failed: %s" err
    ]}

    Note: If delete_inputs is true but the merge succeeds, the inputs are deleted
    even if the function later fails (e.g., due to disk errors during deletion).
*)
val compact :
  inputs:string list -> output:string -> delete_inputs:bool -> (unit, string) result
