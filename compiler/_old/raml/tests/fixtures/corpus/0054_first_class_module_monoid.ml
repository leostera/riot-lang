(* First-class modules. *)
module type MONOID = sig
  type t
  val empty : t
  val append : t -> t -> t
  val show : t -> string
end

module String_monoid = struct
  type t = string
  let empty = ""
  let append = ( ^ )
  let show x = x
end

let fold_monoid (type a) (module M : MONOID with type t = a) xs =
  List.fold_left M.append M.empty xs |> M.show

let () =
  print_endline (fold_monoid (module String_monoid) [ "ra"; "ml" ])
