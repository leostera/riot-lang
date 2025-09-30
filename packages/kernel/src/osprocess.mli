(** OS Process operations for Kernel *)

val sigterm : int
(** Signal value for SIGTERM *)

val environment : unit -> string array
(** Return the process environment *)

val getpid : unit -> int
(** Return the process ID of the current process *)

val kill : int -> int -> unit
(** Send a signal to a process *)

val system : string -> Unix.process_status
(** Execute a shell command *)

val execv : string -> string array -> 'a
(** Execute a program, replacing the current process *)

val create_process :
  string ->
  string array ->
  Unix.file_descr ->
  Unix.file_descr ->
  Unix.file_descr ->
  int
(** Create a new process *)

val open_process_in : string -> in_channel
(** Open a process for reading *)

val close_process_in : in_channel -> Unix.process_status
(** Close a process opened for reading *)

val open_process_full :
  string -> string array -> in_channel * out_channel * in_channel
(** Open a process with full control *)

val close_process_full :
  in_channel * out_channel * in_channel -> Unix.process_status
(** Close a process opened with full control *)
