open Std

(** # SSE - Server-Sent Events Parser
    
    Parses Server-Sent Events (SSE) from HTTP responses into a stream of events.
    
    ## Examples
    
    Process events as they stream:
    ```ocaml
    Blink.SSE.await conn
    |> Iter.MutIterator.for_each (fun event ->
        Log.info event.data)
    ```
    
    Parse JSON and collect:
    ```ocaml
    let results = 
      Blink.SSE.await conn
      |> Iter.MutIterator.filter_map ~fn:(fun event ->
          Data.Json.of_string event.data |> Result.to_option)
      |> Iter.MutIterator.to_list
    ```
    
    Collect all events:
    ```ocaml
    let events = Blink.SSE.await conn |> Iter.MutIterator.to_list
    ```
*)

(** SSE event type *)
type event = {
  data: string;  (** Event payload *)
  event_type: string option;  (** Optional event type field *)
  id: string option;  (** Optional event ID field *)
}
val await: Connection.t -> event Iter.MutIterator.t

(** Returns a mutable iterator over SSE events from the connection.
    
    The iterator:
    - Parses SSE format (data:, event:, id: fields)
    - Yields events lazily by calling Connection.stream
    - Stops when [DONE] marker is seen or connection closes
    - Handles multi-line data fields
    - Blocks on next() when waiting for data (Riot-style blocking I/O)
    
    ## Examples
    
    ```ocaml
    (* Stream and process events *)
    Blink.SSE.await conn
    |> Iter.MutIterator.for_each (fun event ->
        match Data.Json.of_string event.data with
        | Ok json -> process_json json
        | Error _ -> ())
    ```
*)
