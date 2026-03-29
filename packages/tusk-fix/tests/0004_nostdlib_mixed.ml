open Unix

let home = Unix.getenv "HOME"

let queue : int Queue.t = Queue.create ()
