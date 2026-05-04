open Std

type override_error =
  | MissingOverrideMethod
  | MethodNotAllowed of {
      method_: Net.Http.Method.t;
      allowed: Net.Http.Method.t list;
    }

let allowed_override_methods = [ Net.Http.Method.Put; Patch; Delete; ]

let override_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | MissingOverrideMethod -> "method override parameter is empty"
  | MethodNotAllowed { method_; allowed } ->
      "method override is not allowed: "
      ^ Net.Http.Method.to_string method_
      ^ "; allowed methods: "
      ^ (
        allowed
        |> List.map ~fn:Net.Http.Method.to_string
        |> String.concat ", "
      )

(** Parse method string to Method.t, rejecting invalid/disallowed methods. *)
let parse_override_method = fun str ->
  let upper =
    str
    |> String.trim
    |> String.uppercase_ascii
  in
  if String.equal upper "" then
    Error MissingOverrideMethod
  else
    let method_ = Net.Http.Method.from_string upper in
    if List.contains allowed_override_methods ~value:method_ then
      Ok method_
    else
      Error (MethodNotAllowed { method_; allowed = allowed_override_methods })

(* Only allow PUT, PATCH, DELETE *)
(** Method override middleware *)

let middleware = fun ?(param = "_method") () ~conn ~next ->
  (* Only override POST requests *)
  match Conn.method_ conn with
  | Net.Http.Method.Post -> (
      (* Check body params for _method parameter *)
      let body_params = Conn.body_params conn in
      match Std.Collections.Proplist.get body_params ~key:param with
      | Some method_str -> (
          (* Try to parse as valid override method *)
          match parse_override_method method_str with
          | Ok new_method ->
              (* Override the method and continue *)
              let conn' = Conn.with_method new_method conn in
              next conn'
          | Error error ->
              conn
              |> Conn.respond
                ~status:Net.Http.Status.BadRequest
                ~body:(override_error_to_string error)
              |> Conn.halt
        )
      | None ->
          (* No _method parameter - continue as POST *)
          next conn
    )
  | _ ->
      (* Not a POST - pass through unchanged *)
      next conn
