type counter = { mutable count : int }

let incr c = c.count <- c.count + 1
