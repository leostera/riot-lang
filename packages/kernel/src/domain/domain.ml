type 'value t = 'value Thread.t

let spawn = Thread.spawn

let join = Thread.join

module DLS = Thread.DLS
