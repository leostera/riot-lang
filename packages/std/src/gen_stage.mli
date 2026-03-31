(** # GenStage - Demand-driven data processing with back-pressure

   GenStage provides a specification for exchanging events between producers
   and consumers with automatic back-pressure control.

   Unlike traditional push-based systems, GenStage is **pull-based**: consumers
   ask for events (demand), and producers only send what was requested.

   ## Stage Types

   - **Producer**: Only emits events when demand arrives
   - **Consumer**: Only receives events, sends demand upstream
   - **ProducerConsumer**: Receives events, transforms them, emits new events

   ## Example

   ```ocaml
   open Std

   (* Producer: Counter that emits numbers *)
   module Counter = struct
     type args = int  (* starting number *)
     type state = int (* current counter *)
     type event = int

     let init counter = Ok counter

     let handle_demand demand counter =
       let events = List.init demand (fun i -> counter + i) in
       Reply (events, counter + demand)

     let terminate _reason _state = ()
   end

   module CounterStage = GenStage.Producer.Make(Counter)

   (* ProducerConsumer: Doubles numbers *)
   module Doubler = struct
     type args = unit
     type state = unit
     type in_event = int
     type out_event = int

     let init () = Ok ()

     let handle_events events _from state =
       let doubled = List.map (fun x -> x * 2) events in
       Reply (doubled, state)

     let terminate _reason _state = ()
   end

   module DoublerStage = GenStage.ProducerConsumer.Make(Doubler)

   (* Consumer: Prints numbers *)
   module Printer = struct
     type args = unit
     type state = unit
     type event = int

     let init () = Ok ()

     let handle_events events _from state =
       List.iter (fun x -> println "Got: %d" x) events;
       Noreply state

     let terminate _reason _state = ()
   end

   module PrinterStage = GenStage.Consumer.Make(Printer)

   (* Start the pipeline *)
   let counter = CounterStage.start_link 0 in
   let doubler = DoublerStage.start_link () in
   let printer = PrinterStage.start_link () in

   (* Wire them up (subscribe bottom-to-top) *)
   PrinterStage.subscribe printer ~to_stage:doubler ();
   DoublerStage.subscribe doubler ~to_stage:counter ()
   ```

   ## Back-Pressure

   When the consumer is slow, it will request fewer events. The producer
   will naturally slow down to match, preventing mailbox overflow.

   ```
   Producer <--[demand: 10]-- Consumer
   Producer --[events: 10]--> Consumer
   
   (Consumer processes slowly)
   
   Producer <--[demand: 1]--- Consumer
   Producer --[events: 1]---> Consumer
   ```
*)

(** {1 Stage Types} *)

open Global

type from = {
  pid: Pid.t;
  subscription_ref: unit Ref.t;
}
(** Information about where events came from *)
type subscription_options = {
  min_demand: int;
  (** Minimum demand to send upstream (default: 5) *)
  max_demand: int;
  (** Maximum demand to send upstream (default: 1000) *)
  partition: string option;
  (** Partition key for dispatching (optional) *)
}
(** Options for subscribing to a stage *)
val default_subscription_options: subscription_options

(** Default subscription options: min_demand=5, max_demand=1000 *)
(** {1 Producer Stage} *)

module Producer: sig
  (** Producer stages generate events in response to demand *)
  type ('event, 'state) demand_result =
    | Reply of 'event list * 'state
    (** Send events and continue *)
    | Noreply of 'state
    (** Don't send events yet, continue *)
    | Stop of exn * 'state
  (** Stop the producer *)
  type 'state cast_result =
    | Noreply of 'state
    | Stop of exn * 'state
  module type Spec = sig
    type args
    (** Arguments passed to init *)
    type state
    (** Internal producer state *)
    type event
    (** Type of events emitted *)
    val init: args -> (state, exn) result

    (** Initialize the producer state *)
    val handle_demand: int -> state -> (event list, state) demand_result

    (** Handle demand from downstream.
    
        The integer is how many events are being requested.
        Return a list of events (up to the requested amount) and new state.
    *)
    val handle_cast: Message.t -> state -> state cast_result

    (** Handle cast messages (optional, for external control) *)
    val terminate: exn -> state -> unit

    (** Cleanup on termination *)
  end

  module type S = sig
    type args
    type state
    type event
    type t
    val start_link: args -> t

    val start: args -> t

    val cast: t -> Message.t -> unit

    val stop: t -> unit
  end

  module Make (Impl : Spec): S with type args = Impl.args and type state = Impl.state and type event = Impl.event

  (** Create a Producer stage from a specification *)
end

(** {1 Consumer Stage} *)

module Consumer: sig
  (** Consumer stages receive events and send demand upstream *)
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
    val init: args -> (state, exn) result

    val handle_events: event list -> from -> state -> state events_result

    (** Handle a batch of events from upstream.
    
        Events are delivered in order.
        The `from` parameter tells you which producer sent them.
        
        After processing, demand is automatically sent upstream
        based on subscription options (min_demand/max_demand).
    *)
    val handle_cast: Message.t -> state -> state cast_result

    val terminate: exn -> state -> unit
  end

  module type S = sig
    type args
    type state
    type event
    type t
    val start_link: args -> t

    val start: args -> t

    val subscribe: t -> to_stage:Pid.t -> ?options:subscription_options -> unit -> (unit, string) result

    (** Subscribe to a producer or producer-consumer.
    
        This starts the flow of events. The consumer will send initial demand
        based on max_demand.
    *)
    val cast: t -> Message.t -> unit

    val stop: t -> unit
  end

  module Make (Impl : Spec): S with type args = Impl.args and type state = Impl.state and type event = Impl.event
end

(** {1 ProducerConsumer Stage} *)

module ProducerConsumer: sig
  (** ProducerConsumer stages receive events, transform them, and emit new events *)
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
    (** Type of events received from upstream *)
    type out_event
    (** Type of events sent downstream *)
    val init: args -> (state, exn) result

    val handle_events: in_event list -> from -> state -> (out_event list, state) events_result

    (** Receive events, process them, emit transformed events.
    
        The events you return will be sent downstream.
        Demand is automatically forwarded upstream.
    *)
    val handle_demand: int -> state -> (out_event list, state) demand_result

    (** Handle demand from downstream (optional).
    
        Most producer-consumers just forward demand upstream automatically.
        Implement this if you need custom demand handling (e.g., buffering).
    *)
    val handle_cast: Message.t -> state -> state cast_result

    val terminate: exn -> state -> unit
  end

  module type S = sig
    type args
    type state
    type in_event
    type out_event
    type t
    val start_link: args -> t

    val start: args -> t

    val subscribe: t -> to_stage:Pid.t -> ?options:subscription_options -> unit -> (unit, string) result

    (** Subscribe to an upstream producer *)
    val cast: t -> Message.t -> unit

    val stop: t -> unit
  end

  module Make (Impl : Spec): S with type args = Impl.args and type state = Impl.state and type in_event = Impl.in_event and type out_event = Impl.out_event
end

(** {1 Dispatchers} *)

module Dispatcher: sig
  (** Dispatchers control how events are sent to multiple consumers *)
  type t =
    | Broadcast
    (** Send all events to all consumers *)
    | Partition of (Message.t -> string)
    (** Route events by partition key (consistent hashing) *)
    | RoundRobin
    (** Distribute events evenly across consumers *)
    | FirstAvailable
  (** Send to the first consumer with available demand *)

  (** Example: Partition by user ID
  
      ```ocaml
      let dispatcher = Dispatcher.Partition (fun event ->
        match event with
        | UserEvent { user_id; _ } -> format "user_%d" user_id
        | _ -> "default"
      )
      ```
  *)
end

(** {1 Advanced: Manual Subscription} *)
val ask: Pid.t -> count:int -> unit

(** Manually send demand to a producer.

    Usually not needed - subscription options handle this automatically.
    Use this for fine-grained demand control.
*)
val cancel: Pid.t -> subscription_ref:unit Ref.t -> unit

(** Cancel a subscription.

    The consumer will stop receiving events from this producer.
*)
(** {1 Utilities} *)

val sync_subscribe:
  consumer:Pid.t -> to_producer:Pid.t -> ?options:subscription_options -> unit -> (unit, string) result

(** Subscribe and wait for confirmation.

    Useful for setting up pipelines during startup.
*)
val async_subscribe:
  consumer:Pid.t -> to_producer:Pid.t -> ?options:subscription_options -> unit -> unit

(** Subscribe without waiting for confirmation *)
