type 'a t = 'a Stdlib.Domain.t

let spawn = Stdlib.Domain.spawn

let join = Stdlib.Domain.join

module DLS = struct
  type 'a key = 'a Stdlib.Domain.DLS.key

  let new_key = Stdlib.Domain.DLS.new_key

  let get = Stdlib.Domain.DLS.get

  let set = Stdlib.Domain.DLS.set
end
