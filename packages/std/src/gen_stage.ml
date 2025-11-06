open Global
open Sync

(* # GenStage - Demand-driven data processing with back-pressure

   ## Design Notes

   GenStage implements a demand-driven pipeline where:
   1. Consumers ASK for events (send demand upstream)
   2. Producers SEND events (never more than requested)
   3. This creates automatic back-pressure

   ## Key Concepts

   **Demand Tracking**:
   - Each subscription tracks: (pending_demand, min_demand, max_demand)
   - When pending_demand < min_demand, ask for (max_demand - pending_demand) more
   - This ensures smooth flow without over-requesting

   **Messages**:
   - `GenStage_subscribe`: Consumer -> Producer (establish subscription)
   - `GenStage_ask`: Consumer -> Producer (request N events)
   - `GenStage_events`: Producer -> Consumer (deliver events)
   - `GenStage_cancel`: Consumer -> Producer (cancel subscription)

   **State Management**:
   - Producer: tracks (downstream_consumers, pending_events_queue)
   - Consumer: tracks (upstream_producers, pending_demand_per_producer)
   - ProducerConsumer: tracks both upstream and downstream

   ## Future Enhancements

   - Dispatchers (broadcast, partition, round-robin)
   - Buffering strategies
   - Flow control (rate limiting)
   - ConsumerSupervisor (start child per event)
*)

(** {1 Common Types} *)

type from = {
  pid : Pid.t;
  subscription_ref : unit Ref.t;
}

type subscription_options = {
  min_demand : int;
  max_demand : int;
  partition : string option;
}

let default_subscription_options = {
  min_demand = 5;
  max_demand = 1000;
  partition = None;
}

(** {1 GenStage Protocol Messages} *)

type Message.t +=
  | GenStage_subscribe of {
      consumer : Pid.t;
      subscription_ref : unit Ref.t;
      options : subscription_options;
      reply_to : Pid.t;
    }
  | GenStage_subscribe_reply of {
      result : (unit, string) result;
      subscription_ref : unit Ref.t;
    }
  | GenStage_ask of {
      subscription_ref : unit Ref.t;
      count : int;
    }
  | GenStage_events : {
      subscription_ref : unit Ref.t;
      events : Message.t list;  (* Polymorphic - actual events *)
    } -> Message.t
  | GenStage_cancel of {
      subscription_ref : unit Ref.t;
    }

(** {1 Producer Stage} *)

module Producer = struct

  type ('event, 'state) demand_result =
    | Reply of 'event list * 'state
    | Noreply of 'state
    | Stop of exn * 'state

  type 'state cast_result =
    | Noreply of 'state
    | Stop of exn * 'state

  module type Spec = sig
    type args
    type state
    type event

    val init : args -> (state, exn) result

    val handle_demand : int -> state -> (event list, state) demand_result

    val handle_cast : Message.t -> state -> state cast_result

    val terminate : exn -> state -> unit
  end

  module type S = sig
    type args
    type state
    type event
    type t

    val start_link : args -> t
    val start : args -> t
    val cast : t -> Message.t -> unit
    val stop : t -> unit
  end

  module Make (Impl : Spec) : S
    with type args = Impl.args
     and type state = Impl.state
     and type event = Impl.event
  = struct
    type args = Impl.args
    type state = Impl.state
    type event = Impl.event
    type t = Pid.t

    (* Producer tracks downstream consumers *)
    type consumer_state = {
      pid : Pid.t;
      subscription_ref : unit Ref.t;
      pending_demand : int;
      options : subscription_options;
    }

    type internal_state = {
      user_state : state;
      consumers : consumer_state list Cell.t;
      buffer : event list Cell.t;  (* Buffered events waiting to be sent *)
    }

    (* TODO: Implement producer loop with:
       - handle GenStage_subscribe: add consumer to list
       - handle GenStage_ask: accumulate demand, call handle_demand
       - handle GenStage_cancel: remove consumer
       - dispatch events to consumers based on demand
    *)

    let start_link _args = unimplemented ()
    let start _args = unimplemented ()
    let cast _t _msg = unimplemented ()
    let stop _t = unimplemented ()
  end
end

(** {1 Consumer Stage} *)

module Consumer = struct
  type 'state events_result =
    | Noreply of 'state
    | Stop of exn * 'state

  type 'state cast_result =
    | Noreply of 'state
    | Stop of exn * 'state

  module type Spec = sig
    type args
    type state
    type event

    val init : args -> (state, exn) result
    val handle_events : event list -> from -> state -> state events_result
    val handle_cast : Message.t -> state -> state cast_result
    val terminate : exn -> state -> unit
  end

  module type S = sig
    type args
    type state
    type event
    type t

    val start_link : args -> t
    val start : args -> t
    val subscribe :
      t ->
      to_stage:Pid.t ->
      ?options:subscription_options ->
      unit ->
      (unit, string) result
    val cast : t -> Message.t -> unit
    val stop : t -> unit
  end

  module Make (Impl : Spec) : S
    with type args = Impl.args
     and type state = Impl.state
     and type event = Impl.event
  = struct
    type args = Impl.args
    type state = Impl.state
    type event = Impl.event
    type t = Pid.t

    (* Consumer tracks upstream producers *)
    type producer_state = {
      pid : Pid.t;
      subscription_ref : unit Ref.t;
      pending_demand : int;  (* How much demand we've sent but not received *)
      options : subscription_options;
    }

    type internal_state = {
      user_state : state;
      producers : producer_state list Cell.t;
    }

    (* TODO: Implement consumer loop with:
       - handle GenStage_events: deliver to handle_events, send more demand
       - track demand per producer
       - auto-demand when pending < min_demand
    *)

    let start_link _args = unimplemented ()
    let start _args = unimplemented ()
    let subscribe _t ~to_stage:_ ?options:_ () = unimplemented ()
    let cast _t _msg = unimplemented ()
    let stop _t = unimplemented ()
  end
end

(** {1 ProducerConsumer Stage} *)

module ProducerConsumer = struct
  type ('out_event, 'state) events_result =
    | Reply of 'out_event list * 'state
    | Noreply of 'state
    | Stop of exn * 'state

  type ('out_event, 'state) demand_result =
    | Reply of 'out_event list * 'state
    | Noreply of 'state
    | Stop of exn * 'state

  type 'state cast_result =
    | Noreply of 'state
    | Stop of exn * 'state

  module type Spec = sig
    type args
    type state
    type in_event
    type out_event

    val init : args -> (state, exn) result
    val handle_events :
      in_event list -> from -> state -> (out_event list, state) events_result
    val handle_demand : int -> state -> (out_event list, state) demand_result
    val handle_cast : Message.t -> state -> state cast_result
    val terminate : exn -> state -> unit
  end

  module type S = sig
    type args
    type state
    type in_event
    type out_event
    type t

    val start_link : args -> t
    val start : args -> t
    val subscribe :
      t ->
      to_stage:Pid.t ->
      ?options:subscription_options ->
      unit ->
      (unit, string) result
    val cast : t -> Message.t -> unit
    val stop : t -> unit
  end

  module Make (Impl : Spec) : S
    with type args = Impl.args
     and type state = Impl.state
     and type in_event = Impl.in_event
     and type out_event = Impl.out_event
  = struct
    type args = Impl.args
    type state = Impl.state
    type in_event = Impl.in_event
    type out_event = Impl.out_event
    type t = Pid.t

    (* ProducerConsumer tracks both upstream and downstream *)
    type internal_state = {
      user_state : state;
      producers : unit;  (* TODO: same as Consumer *)
      consumers : unit;  (* TODO: same as Producer *)
      buffer : out_event list Cell.t;
    }

    (* TODO: Implement producer-consumer loop:
       - Combines Producer and Consumer behavior
       - Receives events from upstream, transforms, sends downstream
       - Forwards demand upstream automatically
    *)

    let start_link _args = unimplemented ()
    let start _args = unimplemented ()
    let subscribe _t ~to_stage:_ ?options:_ () = unimplemented ()
    let cast _t _msg = unimplemented ()
    let stop _t = unimplemented ()
  end
end

(** {1 Dispatchers} *)

module Dispatcher = struct
  type t =
    | Broadcast
    | Partition of (Message.t -> string)
    | RoundRobin
    | FirstAvailable
end

(** {1 Utilities} *)

let ask _pid ~count:_ = unimplemented ()

let cancel _pid ~subscription_ref:_ = unimplemented ()

let sync_subscribe ~consumer:_ ~to_producer:_ ?options:_ () = unimplemented ()

let async_subscribe ~consumer:_ ~to_producer:_ ?options:_ () = unimplemented ()
