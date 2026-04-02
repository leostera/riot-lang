type config = {
  handlers: (string -> unit) list;
  timeout: int option;
  retry: bool;
}
