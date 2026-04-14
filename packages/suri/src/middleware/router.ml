open Std

type handler = Conn.t -> Web_server.Request.t -> Conn.t

type route_method =
  | SpecificMethod of Net.Http.Method.t
  | AnyMethod

type route =
  | Route of { meth: route_method; path: string; handler: handler }
  | Scope of { prefix: string; routes: route list }

type t = route list

let route = fun meth path handler -> Route { meth = SpecificMethod meth; path; handler }

let any = fun path handler -> Route { meth = AnyMethod; path; handler }

let get = fun path handler -> route Get path handler

let post = fun path handler -> route Post path handler

let put = fun path handler -> route Put path handler

let patch = fun path handler -> route Patch path handler

let delete = fun path handler -> route Delete path handler

let head = fun path handler -> route Head path handler

let scope = fun prefix routes -> Scope { prefix; routes }

let websocket = fun (type a s) path ((module H : Channel.Handler.Intf with type args = a and type state = s)) (
  args: a
) ->
  (* This creates a route that's meant to be used with a Handler.t-based server *)
  (* It won't work correctly with the middleware-only routing we currently have *)
  (* TODO: This needs to be integrated properly with the handler-based approach *)
  get path (fun conn req -> conn |> Conn.with_status Net.Http.Status.SwitchingProtocols |> Conn.send)

let normalize_path = fun path ->
  let path =
    if String.starts_with ~prefix:"/" path then
      path
    else
      "/" ^ path
  in
  let path =
    if String.ends_with ~suffix:"/" path && String.length path > 1 then
      String.sub path ~offset:0 ~len:(String.length path - 1)
    else
      path
  in
  path

module Matcher = struct
  type segment =
    Literal of string
    | Param of string

  let parse_path = fun path ->
    let parts = String.split_on_char '/' path in
    List.filter_map
      ~fn:(fun part ->
      if part = "" then
          None
        else if String.starts_with ~prefix:":" part then
          Some (Param (String.sub part ~offset:1 ~len:(String.length part - 1)))
        else
          Some (Literal part))
      parts

  let match_segments = fun pattern_segs path_segs ->
    let rec go acc pattern_segs path_segs =
      match (pattern_segs, path_segs) with
      | [], [] -> Some (List.rev acc)
      | Literal p :: ps, h :: hs when p = h -> go acc ps hs
      | Param name :: ps, value :: hs -> go ((name, value) :: acc) ps hs
      | _, _ -> None
    in
    go [] pattern_segs path_segs

  let match_path = fun pattern path ->
    let pattern_segs = parse_path pattern in
    let path_parts = String.split_on_char '/' path |> List.filter ~fn:(fun s -> s != "") in
    match_segments pattern_segs path_parts
end

let rec flatten_routes = fun prefix routes ->
  List.flat_map
    ~fn:(fun route ->
      match route with
      | Route { meth; path; handler } ->
          let full_path = normalize_path (prefix ^ path) in
          [ (meth, full_path, handler) ]
      | Scope { prefix=scope_prefix; routes=scope_routes } ->
          let new_prefix = prefix ^ scope_prefix in
          flatten_routes new_prefix scope_routes)
    routes

let middleware = fun routes ->
  let flat_routes = flatten_routes "" routes in
  fun ~conn ~next ->
    if Conn.sent conn || Conn.halted conn then
      conn
    else
      let meth = Conn.method_ conn in
      let path = normalize_path (Conn.path conn) in
      let req = Conn.request conn in
      let rec try_routes = function
        | [] -> next conn
        | (route_meth, route_path, handler) :: rest ->
            let method_matches =
              match route_meth with
              | SpecificMethod m -> m = meth
              | AnyMethod -> true
            in
            if method_matches then
              match Matcher.match_path route_path path with
              | Some params ->
                  let conn = Conn.set_params params conn in
                  (* Call handler with both conn and req *)
                  handler conn req
              | None -> try_routes rest
            else
              try_routes rest
      in
      try_routes flat_routes
