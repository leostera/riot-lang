(** Process effects for communication and I/O *)

type timeout = [ `infinity | `after of float ]
(** Timeout specification for operations *)

type _ Effect.t +=
  | Receive : {
      selector : Message.t -> [ `select of 'msg | `skip ];
    }
      -> 'msg Effect.t
        (** Effect for receiving messages with a selector *)

type _ Effect.t +=
  | Yield : unit Effect.t  (** Effect for yielding control to the scheduler *)

type _ Effect.t +=
  | Syscall : {
      name : string;
      interest : Kernel.IO.Interest.t;
      source : Kernel.IO.Source.t;
      timeout : timeout;
    }
      -> unit Effect.t
        (** Effect for system calls with I/O polling *)
