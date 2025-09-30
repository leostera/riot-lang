(** Process management and lifecycle *)

type exit_reason = exn
(** Reasons why a process might exit *)

type state = private
  | Uninitialized
  | Runnable
  | Waiting_message
  | Waiting_io of {
      name : string;
      token : Kernel.Async.Token.t;
      source : Kernel.Async.Source.t;
    }
  | Running
  | Exited of (unit, exit_reason) result
  | Finalized  (** Process state - tracks current status in scheduler *)

type t
(** Opaque process type *)

val make : (unit -> (unit, exit_reason) result) -> t
(** Create a new process from a function *)

val init : t -> unit
(** Initialize a process, making it ready to run *)

val pid : t -> Pid.t
(** Get the process ID *)

val state : t -> state
(** Get the current process state *)

val is_alive : t -> bool
(** Check if process is alive (not exited or finalized) *)

val is_exited : t -> bool
(** Check if process has exited *)

val is_waiting : t -> bool
(** Check if process is waiting for messages *)

val is_waiting_io : t -> bool
(** Check if process is waiting for I/O *)

val is_runnable : t -> bool
(** Check if process is runnable *)

val is_running : t -> bool
(** Check if process is currently running *)

val is_main : t -> bool
(** Check if this is the main process *)

val has_empty_mailbox : t -> bool
(** Check if process mailbox is empty *)

val has_messages : t -> bool
(** Check if process has pending messages *)

val message_count : t -> int
(** Get total number of messages in mailbox *)

val mark_as_running : t -> unit
(** Mark process as currently running *)

val mark_as_runnable : t -> unit
(** Mark process as runnable (if alive) *)

val mark_as_awaiting_message : t -> unit
(** Mark process as waiting for messages *)

val mark_as_exited : t -> (unit, exit_reason) result -> unit
(** Mark process as exited with given reason *)

val mark_as_finalized : t -> unit
(** Mark process as finalized *)

val cont : t -> (unit, exit_reason) result Proc_state.t
(** Get process continuation *)

val set_cont : t -> (unit, exit_reason) result Proc_state.t -> unit
(** Set process continuation *)

val next_message : t -> Message.envelope option
(** Get next message from process mailbox *)

val add_to_save_queue : t -> Message.envelope -> unit
(** Add message to save queue *)

val read_save_queue : t -> unit
(** Start reading from save queue *)

val send_message : t -> Message.t -> unit
(** Send a message to the process *)

val mark_as_awaiting_io :
  t -> name:string -> Kernel.Async.Token.t -> Kernel.Async.Source.t -> unit
(** Mark process as waiting for I/O operation *)

val add_ready_token : t -> Kernel.Async.Token.t -> Kernel.Async.Source.t -> unit
(** Add a ready I/O token to the process *)

val get_ready_token : t -> (Kernel.Async.Token.t * Kernel.Async.Source.t) option
(** Get next ready I/O token *)

val consume_ready_tokens :
  t -> (Kernel.Async.Token.t * Kernel.Async.Source.t -> unit) -> unit
(** Consume all ready I/O tokens with a function *)

val pp : Format.formatter -> t -> unit
(** Pretty-print process information *)
