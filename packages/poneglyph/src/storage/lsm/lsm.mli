(** LSM Storage Engine - Layer-by-layer implementation *)

module Ref_store = Ref_store
(** Reference store - Ground truth oracle for testing *)

module Encoding = Encoding
(** Binary encoding for values and keys *)

module Key = Key
(** Fixed-width 41-byte key encoding *)

module Block = Block
(** Fixed-size sorted data blocks (16KB) *)

module Sstable = Sstable
(** Sorted String Tables - persistent on-disk storage *)

module Memtable = Memtable
(** In-memory sorted write buffer *)

module Wal = Wal
(** Write-Ahead Log for durability *)

module Compaction = Compaction
(** Compaction - Merge SSTables to reduce read amplification *)

module Engine = Engine
(** LSM Engine - Complete LSM storage system *)
