(* Small higher-order interpreter with closures. *)
type expr =
  | Int of int
  | Var of string
  | Add of expr * expr
  | Let of string * expr * expr
  | Lam of string * expr
  | App of expr * expr

type value =
  | VInt of int
  | VClosure of string * expr * env

and env = (string * value) list

let rec eval env = function
  | Int n -> VInt n
  | Var x -> List.assoc x env
  | Add (a, b) ->
      begin match eval env a, eval env b with
      | VInt x, VInt y -> VInt (x + y)
      | _ -> failwith "type error"
      end
  | Let (x, e, body) ->
      let v = eval env e in
      eval ((x, v) :: env) body
  | Lam (x, body) -> VClosure (x, body, env)
  | App (f, arg) ->
      begin match eval env f with
      | VClosure (x, body, closure_env) ->
          let v = eval env arg in
          eval ((x, v) :: closure_env) body
      | _ -> failwith "not a function"
      end

let program =
  Let
    ( "inc",
      Lam ("x", Add (Var "x", Int 1)),
      App (Var "inc", Int 41) )

let () =
  match eval [] program with
  | VInt n -> Printf.printf "%d\n" n
  | VClosure _ -> print_endline "<fun>"
