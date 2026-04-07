(** # Collections - Core data structures
    
    Essential collections available at the Kernel layer. These are the modern,
    performant APIs that form the foundation for all higher-level code.
    
    ## Available Collections
    
    - {!Array} - Built-in arrays
    - {!List} - Enhanced list utilities
    - {!Stream} - Lazy sequences (renamed from Seq)
    - {!Vector} - Dynamic arrays with O(1) indexing
    - {!Map} - Persistent ordered maps
    - {!HashMap} - Hash tables with O(1) average lookups
    - {!HashSet} - Sets with O(1) average membership testing
    - {!Queue} - FIFO queues
*)

module Array = Array

module List = List

module Stream = Stream

module Vector = Vector

module Map = Map

module HashMap = Hashmap

module HashSet = Hashset

module Queue = Queue
