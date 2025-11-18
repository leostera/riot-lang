(** LSM Storage Engine - Layer-by-layer implementation *)

module Ref_store = Ref_store
(** Reference store - Ground truth oracle for testing *)

module Encoding = Encoding
(** Binary encoding for values and keys *)

module Key = Key
(** Fixed-width 41-byte key encoding *)

module Block = Block
(** Fixed-size sorted data blocks (16KB) *)

module Bloom_filter = Bloom_filter
(** Bloom filters - Probabilistic membership test for fast negative lookups *)

module Sstable = Sstable
(** Sorted String Tables - persistent on-disk storage *)

module Skiplist = Skiplist
(** SkipList - Probabilistic balanced search structure *)

module Memtable = Memtable
(** In-memory sorted write buffer *)

module Wal = Wal
(** Write-Ahead Log for durability *)

module Compaction = Compaction
(** Compaction - Merge SSTables to reduce read amplification *)

module Manifest = Manifest
(** Manifest - SSTable metadata tracking across process boundaries *)

module Lockfile = Lockfile
(** Lockfile - Write lock for single-writer concurrency control *)

module Engine = Engine
(** LSM Engine - Complete LSM storage system *)

module Multi_store = Multi_store
(** Multi-Index Store - Atomic writes across EAVT, AVET, FACT, SOURCE indices *)
