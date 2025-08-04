type _ Effect.t +=
  | Receive : {
      selector : Message.t -> [ `select of 'msg | `skip ];
    }
      -> 'msg Effect.t

type _ Effect.t += Yield : unit Effect.t

(* Timeout type for syscalls *)
type timeout = [ `infinity | `after of float ]

(* I/O Effects *)
type _ Effect.t +=
  | Syscall : {
      name : string;
      interest : Gluon.Interest.t;
      source : Gluon.Source.t;
      timeout : timeout;
    }
      -> unit Effect.t