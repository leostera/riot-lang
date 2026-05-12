(* Local module with type definitions *)

let process data =
  let module Types = struct
    type t = int

    type result =
      Success of t
      | Failure of string
  end in
  Types.Success data
