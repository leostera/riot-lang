open Global

(** Agent - Lightweight parametric state server
    
    Simple API without functors:
    ```ocaml
    let counter = Agent.start (fun () -> 0) in
    Agent.update counter (fun n -> n + 1);
    let value = Agent.get counter (fun n -> n) in
    (* value = 1 *)
    ```
*)
type 'state t = {
  pid: Pid.t;
  state_ref: 'state Ref.t;
}

(* Message types *)

type agent_request =
  | Get: {
      reply_to: Pid.t;
      fn: 'state -> 'reply;
      state_ref: 'state Ref.t;
      reply_ref: 'reply Ref.t;
    } -> agent_request
  | Update: {
      reply_to: Pid.t;
      fn: 'state -> 'state;
      state_ref: 'state Ref.t;
    } -> agent_request
  | GetAndUpdate: {
      reply_to: Pid.t;
      fn: 'state -> 'reply * 'state;
      state_ref: 'state Ref.t;
      reply_ref: 'reply Ref.t;
    } -> agent_request
  | Cast: {
      fn: 'state -> 'state;
      state_ref: 'state Ref.t;
    } -> agent_request
  | Stop: {
      reply_to: Pid.t;
    } -> agent_request

type agent_response =
  | GetReply: 'reply * 'reply Ref.t -> agent_response
  | UpdateReply
  | GetAndUpdateReply: 'reply * 'reply Ref.t -> agent_response
  | StopReply

type Message.t +=
  | AgentRequest of agent_request
  | AgentResponse of agent_response

let rec loop: type state. state Ref.t -> state -> (unit, exn) result = fun state_ref state ->
  let selector msg =
    match msg with
    | AgentRequest req -> `select req
    | _ -> `skip
  in
  match receive ~selector () with
  | Get { reply_to; fn; state_ref=sr; reply_ref } -> (
      match Ref.type_equal state_ref sr with
      | Some Type.Equal ->
          let result = fn state in
          send reply_to (AgentResponse (GetReply (result, reply_ref)));
          loop state_ref state
      | None ->
          (* Message for different agent type, ignore *)
          loop state_ref state
    )
  | Update { reply_to; fn; state_ref=sr } -> (
      match Ref.type_equal state_ref sr with
      | Some Type.Equal ->
          let new_state = fn state in
          send reply_to (AgentResponse UpdateReply);
          loop state_ref new_state
      | None -> loop state_ref state
    )
  | GetAndUpdate { reply_to; fn; state_ref=sr; reply_ref } -> (
      match Ref.type_equal state_ref sr with
      | Some Type.Equal ->
          let (result, new_state) = fn state in
          send reply_to (AgentResponse (GetAndUpdateReply (result, reply_ref)));
          loop state_ref new_state
      | None -> loop state_ref state
    )
  | Cast { fn; state_ref=sr } -> (
      match Ref.type_equal state_ref sr with
      | Some Type.Equal ->
          let new_state = fn state in
          loop state_ref new_state
      | None -> loop state_ref state
    )
  | Stop { reply_to } ->
      send reply_to (AgentResponse StopReply);
      Ok ()

let start: type state. fn:(unit -> state) -> state t = fun ~fn ->
  let state_ref: state Ref.t = Ref.make () in
  let pid =
    spawn (fun () -> loop state_ref (fn ()))
  in
  { pid; state_ref }

let start_link: type state. fn:(unit -> state) -> state t = fun ~fn ->
  let state_ref: state Ref.t = Ref.make () in
  let pid =
    spawn_link (fun () -> loop state_ref (fn ()))
  in
  { pid; state_ref }

let get: type state reply. state t -> fn:(state -> reply) -> reply = fun agent ~fn ->
  let reply_ref: reply Ref.t = Ref.make () in
  send
    agent.pid
    (AgentRequest (Get { reply_to = self (); fn; state_ref = agent.state_ref; reply_ref }));
  let selector msg =
    match msg with
    | AgentResponse res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | GetReply (result, rr) when Ref.equal reply_ref rr -> (
      match Ref.type_equal reply_ref rr with
      | Some Type.Equal -> result
      | None -> panic "impossible: reply ref mismatch"
    )
  | _ -> panic "unexpected agent response"

let update: type state. state t -> fn:(state -> state) -> unit = fun agent ~fn ->
  send agent.pid (AgentRequest (Update { reply_to = self (); fn; state_ref = agent.state_ref }));
  let selector msg =
    match msg with
    | AgentResponse res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | UpdateReply -> ()
  | _ -> panic "unexpected agent response"

let get_and_update: type state reply. state t -> fn:(state -> reply * state) -> reply = fun agent ~fn ->
  let reply_ref: reply Ref.t = Ref.make () in
  send
    agent.pid
    (AgentRequest (GetAndUpdate { reply_to = self (); fn; state_ref = agent.state_ref; reply_ref }));
  let selector msg =
    match msg with
    | AgentResponse res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | GetAndUpdateReply (result, rr) when Ref.equal reply_ref rr -> (
      match Ref.type_equal reply_ref rr with
      | Some Type.Equal -> result
      | None -> panic "impossible: reply ref mismatch"
    )
  | _ -> panic "unexpected agent response"

let cast: type state. state t -> fn:(state -> state) -> unit = fun agent ~fn ->
  send agent.pid (AgentRequest (Cast { fn; state_ref = agent.state_ref }))

let stop: type state. state t -> unit = fun agent ->
  send agent.pid (AgentRequest (Stop { reply_to = self () }));
  let selector msg =
    match msg with
    | AgentResponse res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | StopReply -> ()
  | _ -> panic "unexpected agent response"
