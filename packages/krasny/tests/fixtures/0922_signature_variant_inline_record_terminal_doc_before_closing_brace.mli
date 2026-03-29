type event =
  | Message of {
      data : string;
      id : string option;
      (** Optional event ID field *)
    }
