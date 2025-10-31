open Std

module Html = Html

module type Component = sig
  type state
  type msg

  val init : Middleware.Conn.t -> state
  val update : msg -> state -> state
  val render : state:state -> unit -> msg Html.t
end

type event = Mount | Event of string * string | Patch of string

let serialize_event event =
  match event with
  | Mount -> Ok {|"Mount"|}
  | Event (id, data) -> Ok (Format.sprintf {|{"Event":[%S,%S]}|} id data)
  | Patch html -> Ok (Format.sprintf {|{"Patch":[%S]}|} html)

let deserialize_event str =
  try
    if str = {|"Mount"|} || str = {| "Mount" |} then Ok Mount
    else if String.contains str '{' then
      match Data.Json.of_string str with
      | Error _ -> Error "Failed to parse JSON"
      | Ok json -> (
          match Data.Json.get_field "Event" json with
          | Some
              (Data.Json.Array [ Data.Json.String id; Data.Json.String data ])
            ->
              Ok (Event (id, data))
          | _ -> (
              match Data.Json.get_field "Patch" json with
              | Some (Data.Json.Array [ Data.Json.String html ]) ->
                  Ok (Patch html)
              | _ -> Error "Unknown event format"))
    else Error "Invalid event format"
  with _ -> Error "Failed to parse event"

type Message.t += Render of string

module ComponentProcess = struct
  type ('state, 'msg) t = {
    state : 'state;
    update : 'msg -> 'state -> 'state;
    render : state:'state -> unit -> 'msg Html.t;
    handlers : (string, string -> 'msg) Collections.HashMap.t;
    renderer : Pid.t;
  }

  type Message.t += Mount | Event of { id : string; event : string }

  let render t html =
    let str = Html.to_string html in
    send t.renderer (Render str)

  let rec update_handlers ?(idx = 0) t (html : 'msg Html.t) =
    match html with
    | Html.Text _ -> html
    | Html.Splat els ->
        Html.Splat (List.mapi (fun idx el -> update_handlers ~idx t el) els)
    | Html.El { tag; attrs; children } ->
        let attrs =
          match Html.event_handlers attrs with
          | [] -> attrs
          | handlers ->
              let attrs = ref attrs in
              List.iteri
                (fun n (name, handler) ->
                  let id =
                    "liveview-handler-" ^ Int.to_string idx ^ "-"
                    ^ Int.to_string n
                  in
                  let _ = Collections.HashMap.insert t.handlers id handler in
                  attrs :=
                    Html.
                      [ attr "data-lv-event" name; attr "data-liveview-id" id ]
                    @ !attrs)
                handlers;
              !attrs
        in
        let children = List.mapi (fun idx -> update_handlers ~idx t) children in
        Html.El { tag; attrs; children }

  let rec loop (t : ('state, 'msg) t) =
    match receive_any () with
    | Mount -> handle_mount t
    | Event { id; event } -> handle_event t id event
    | _ -> loop t

  and handle_mount t =
    let html = t.render ~state:t.state () in
    let html = update_handlers t html in
    render t html;
    loop t

  and handle_event t id event =
    match Collections.HashMap.get t.handlers id with
    | None -> loop t
    | Some handler ->
        let msg = handler event in
        let state = t.update msg t.state in
        let html = t.render ~state () in
        let html = update_handlers t html in
        render t html;
        loop { t with state }

  let start_link renderer (type s m)
      (module C : Component with type state = s and type msg = m) conn =
    (* TODO(@leostera): use spawn_link when process links are implemented in Miniriot *)
    spawn (fun () ->
        loop
          {
            renderer;
            state = C.init conn;
            update = C.update;
            render = C.render;
            handlers = Collections.HashMap.create ();
          };
        Ok ())
end

module MountHandler (C : Component) = struct
  include Channel.Handler.Default

  type args = Middleware.Conn.t
  type state = { component : Pid.t }

  let init conn =
    let this = self () in
    let component = ComponentProcess.start_link this (module C) conn in
    `ok { component }

  let handle_frame (frame : Http.Ws.Frame.t) _conn state =
    match frame.opcode with
    | Http.Ws.Frame.Text -> (
        match deserialize_event frame.payload with
        | Ok (Event (id, event)) ->
            send state.component (ComponentProcess.Event { id; event });
            `ok state
        | Ok Mount ->
            send state.component ComponentProcess.Mount;
            `ok state
        | Ok _ -> `ok state
        | Error _ -> `ok state)
    | Http.Ws.Frame.Ping -> `push ([ Http.Ws.Frame.pong () ], state)
    | _ -> `ok state

  let handle_message msg state =
    match msg with
    | Render html -> (
        match serialize_event (Patch html) with
        | Ok event ->
            let frame = Http.Ws.Frame.text event in
            `push ([ frame ], state)
        | Error _ -> `ok state)
    | _ -> `ok state
end

let mount (type s m) (module C : Component with type state = s and type msg = m)
    =
  let module M = MountHandler (C) in
  let opts = Channel.Handler.{ do_upgrade = true } in
  (opts, Channel.Handler.make (module M) (Obj.magic 0))
