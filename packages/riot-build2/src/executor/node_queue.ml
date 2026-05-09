open Std

module Queue = Collections.Queue

type t = Work_node.t Queue.t

let create = Queue.create

let push = fun t node -> Queue.push t ~value:node

let pop = Queue.pop

let is_empty = Queue.is_empty
