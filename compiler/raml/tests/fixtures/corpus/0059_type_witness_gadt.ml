(* GADT type witnesses. *)
type _ ty =
  | Int : int ty
  | String : string ty
  | Pair : 'a ty * 'b ty -> ('a * 'b) ty

let rec show : type a. a ty -> a -> string =
 fun ty x ->
  match ty with
  | Int -> string_of_int x
  | String -> x
  | Pair (ta, tb) ->
      let a, b = x in
      "(" ^ show ta a ^ "," ^ show tb b ^ ")"

let () =
  print_endline (show (Pair (Int, String)) (42, "raml"))
