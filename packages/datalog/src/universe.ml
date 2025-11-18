open Std

module Make (S : Storage.STORAGE) = struct
  type t = {
    storage : S.t;
  }
  
  let create storage = {
    storage;
  }
  
  let get_facts_matching universe ~predicate ~pattern =
    S.get_facts_matching universe.storage ~predicate ~pattern
  
  let storage universe = universe.storage
end

(* InMemory universe for testing *)
module InMemory = struct
  include Make(Inmemory_storage)
  
  let create_empty () =
    create (Inmemory_storage.create ())
  
  let of_facts facts_list =
    create (Inmemory_storage.of_facts facts_list)
end
