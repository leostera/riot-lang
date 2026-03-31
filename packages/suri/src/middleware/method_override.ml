open Std

(** Parse method string to Method.t, returning None for invalid/disallowed methods *)
let parse_override_method = fun str ->
  let upper = String.uppercase_ascii str in
  match upper with
  | "PUT" -> Some Net.Http.Method.Put
  | "PATCH" -> Some Patch
  | "DELETE" -> Some Delete
  | _ -> None

(* Only allow PUT, PATCH, DELETE *)

(** Method override middleware *)
let middleware = fun ?(param = "_method") ~conn ~next ->
  (* Only override POST requests *)
  match Conn.method_ conn with
  | Net.Http.Method.Post -> (
      (* Check body params for _method parameter *)
      let body_params = Conn.body_params conn in
      match List.assoc_opt param body_params with
      | Some method_str -> (
          (* Try to parse as valid override method *)
          match parse_override_method method_str with
          | Some new_method ->
              (* Override the method and continue *)
              let conn' = Conn.with_method new_method conn in
              next conn'
          | None ->
              (* Invalid method - ignore and continue as POST *)
              next conn
        )
      | None ->
          (* No _method parameter - continue as POST *)
          next conn
    )
  | _ ->
      (* Not a POST - pass through unchanged *)
      next conn
