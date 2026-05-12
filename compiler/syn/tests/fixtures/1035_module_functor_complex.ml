module Make (Ord : OrderedType): Set with type elt = Ord.t = struct
  type elt = Ord.t

  type t = elt list
end
