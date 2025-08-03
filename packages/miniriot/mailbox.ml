type t = { 
  mutable queue : Message.envelope Queue.t;
  mutable size : int 
}

let create () = { 
  queue = Queue.create (); 
  size = 0 
}

let queue t msg =
  Queue.push msg t.queue;
  t.size <- t.size + 1

let next t =
  if Queue.is_empty t.queue then
    None
  else
    let msg = Queue.pop t.queue in
    t.size <- t.size - 1;
    Some msg

let size t = t.size
let is_empty t = Queue.is_empty t.queue