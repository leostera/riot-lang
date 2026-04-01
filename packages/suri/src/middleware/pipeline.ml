open Std

type middleware = conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t

type t = middleware list

let rec run_pipeline = fun t conn ->
  match t with
  | [] -> conn
  | middleware :: rest ->
      let next conn' =
        if Conn.halted conn' || Conn.sent conn' then
          conn'
        else
          run_pipeline rest conn'
      in
      middleware ~conn ~next

let run = fun conn t ->
  if Conn.halted conn then
    conn
  else
    run_pipeline t conn
