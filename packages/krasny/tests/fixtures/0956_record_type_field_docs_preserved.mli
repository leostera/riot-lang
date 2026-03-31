type event = {
  (** Event payload *)
  mutable data : string;
  (** Optional event type field *)
  event_type : string option;
  (** Optional event ID field *)
  id : string;
}
