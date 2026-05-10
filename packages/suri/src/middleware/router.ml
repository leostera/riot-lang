open Std

type handler = Conn.t -> Web_server.Request.t -> Conn.t

type route_method =
  | SpecificMethod of Net.Http.Method.t
  | AnyMethod

type route =
  | Route of {
      meth: route_method;
      path: string;
      handler: handler;
    }
  | Scope of {
      prefix: string;
      routes: route list;
    }
  | Forward of {
      prefix: string;
      routes: route list;
    }

type flat_route =
  | FlatRoute of {
      meth: route_method;
      path: string;
      handler: handler;
    }
  | FlatForward of {
      prefix: string;
      routes: flat_route list;
    }

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

let forward = fun prefix routes -> Forward { prefix; routes }

let websocket = fun
  (type a s)
  path
  (module H : Channel.Handler.Intf with type args = a and type state = s)
  (args: a) ->
  get
    path
    (fun conn _req ->
      let handler = Channel.Handler.make (module H) args in
      Conn.upgrade_websocket Channel.Handler.{ do_upgrade = true } handler conn)

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
    | Literal of string
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
      | ([], []) -> Some (List.rev acc)
      | ((Literal p) :: ps, h :: hs) when p = h -> go acc ps hs
      | ((Param name) :: ps, value :: hs) -> go ((name, value) :: acc) ps hs
      | (_, _) -> None
    in
    go [] pattern_segs path_segs

  let match_path = fun pattern path ->
    let pattern_segs = parse_path pattern in
    let path_parts =
      String.split_on_char '/' path
      |> List.filter ~fn:(fun s -> not (s = ""))
    in
    match_segments pattern_segs path_parts
end

let match_path = Matcher.match_path

let route_method_matches = fun route_meth meth ->
  match route_meth with
  | SpecificMethod m -> Net.Http.Method.equal m meth
  | AnyMethod -> true

let add_allowed_method = fun meth methods ->
  if List.exists (fun existing -> Net.Http.Method.equal existing meth) methods then
    methods
  else
    meth :: methods

let route_method_allowed = fun route_meth allowed ->
  match route_meth with
  | SpecificMethod meth -> add_allowed_method meth allowed
  | AnyMethod -> allowed

let render_allow_header = fun methods ->
  methods
  |> List.sort ~compare:Net.Http.Method.compare
  |> List.map ~fn:Net.Http.Method.to_string
  |> String.concat ", "

let method_not_allowed = fun allowed conn ->
  conn
  |> Conn.respond ~status:Net.Http.Status.MethodNotAllowed ~body:"Method Not Allowed"
  |> Conn.set_header "allow" (render_allow_header allowed)
  |> Conn.send

let not_found = fun conn ->
  conn
  |> Conn.respond ~status:Net.Http.Status.NotFound ~body:"Not Found"
  |> Conn.set_header "content-type" "text/plain; charset=utf-8"
  |> Conn.send

let path_matches_prefix = fun ~prefix path ->
  let prefix = normalize_path prefix in
  let path = normalize_path path in
  String.equal prefix "/"
  || String.equal path prefix
  || String.starts_with ~prefix:(prefix ^ "/") path

let rec flatten_routes = fun prefix routes ->
  List.fold_right
    routes
    ~init:[]
    ~fn:(fun route acc ->
      let flattened =
        match route with
        | Route { meth; path; handler } ->
            let full_path = normalize_path (prefix ^ path) in
            [ FlatRoute { meth; path = full_path; handler }; ]
        | Scope { prefix = scope_prefix; routes = scope_routes } ->
            let new_prefix = prefix ^ scope_prefix in
            flatten_routes new_prefix scope_routes
        | Forward { prefix = forward_prefix; routes = forward_routes } ->
            let mount_prefix = normalize_path (prefix ^ forward_prefix) in
            [
              FlatForward {
                prefix = mount_prefix;
                routes = flatten_routes mount_prefix forward_routes;
              };
            ]
      in
      flattened @ acc)

let rec dispatch = fun ~on_not_found flat_routes conn ->
  let meth = Conn.method_ conn in
  let path = normalize_path (Conn.path conn) in
  let req = Conn.request conn in
  let rec try_routes allowed_methods = fun remaining_routes ->
    match remaining_routes with
    | [] ->
        if List.is_empty allowed_methods then
          on_not_found conn
        else
          method_not_allowed allowed_methods conn
    | FlatRoute { meth = route_meth; path = route_path; handler } :: rest -> (
        match Matcher.match_path route_path path with
        | Some params ->
            if route_method_matches route_meth meth then
              let conn = Conn.set_params params conn in
              handler conn req
            else
              let allowed_methods = route_method_allowed route_meth allowed_methods in
              try_routes allowed_methods rest
        | None -> try_routes allowed_methods rest
      )
    | FlatForward { prefix; routes } :: rest ->
        if path_matches_prefix ~prefix path then
          dispatch ~on_not_found:not_found routes conn
        else
          try_routes allowed_methods rest
  in
  try_routes [] flat_routes

let middleware = fun routes ->
  let flat_routes = flatten_routes "" routes in
  fun ~conn ~next ->
    if Conn.sent conn || Conn.halted conn then
      conn
    else
      dispatch ~on_not_found:next flat_routes conn
