module HashMap = Hashmap
module HashSet = Hashset
module Queue = Queue
module Deque = Deque
module Vector = Vector
module Heap = Heap

type 'v vec = 'v Vector.t

let vec = Vector.of_list

type ('k, 'v) map = ('k, 'v) HashMap.t

let map = HashMap.of_list

type 'v set = 'v HashSet.t

let set = HashSet.of_list
