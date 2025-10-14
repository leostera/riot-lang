type middleware = Conn.t -> Conn.t
type t = middleware list

let rec run_pipeline t conn =
  match t with
  | [] -> conn
  | middleware :: rest ->
      let conn = middleware conn in
      if Conn.halted conn then conn else run_pipeline rest conn

let run conn t = if Conn.halted conn then conn else run_pipeline t conn
