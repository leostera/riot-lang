(** Implementation of the Gluon kqueue-based I/O notification library *)

let ( let* ) = Result.bind

(** {1 Core Types} *)

type ('a, 'e) io_result = ('a, ([> `Noop ] as 'e)) result

module Fd = struct
  type t = Unix.file_descr
  
  let to_int fd = Obj.magic fd
  let pp fmt fd = Format.fprintf fmt "Fd(%d)" (to_int fd)
end

module Token = struct
  type t = Obj.t
  
  let make x = Obj.repr x
  let unsafe_to_value t = Obj.obj t
  
  let equal ?eq a b =
    match eq with
    | Some f -> f (unsafe_to_value a) (unsafe_to_value b)
    | None -> a == b
    
  let pp fmt t = 
    Format.fprintf fmt "Token(%d)" (Obj.magic t : int)
end

module Interest = struct
  type t = int
  
  let readable = 0b01
  let writable = 0b10
  
  let ( + ) a b = a lor b
  let ( - ) a b = 
    let result = a land (lnot b) in
    if result = 0 then None else Some result
    
  let is_readable t = (t land readable) <> 0
  let is_writable t = (t land writable) <> 0
  
  let pp fmt t =
    let parts = [] in
    let parts = if is_readable t then "readable" :: parts else parts in
    let parts = if is_writable t then "writable" :: parts else parts in
    Format.fprintf fmt "Interest(%s)" (String.concat "|" parts)
end

(** {1 Kqueue Constants} *)

module Kqueue_const = struct
  (* Event filters *)
  let evfilt_read   = -1
  let evfilt_write  = -2
  
  (* Event flags *)
  let ev_add     = 0x0001
  let ev_enable  = 0x0004
  let _ev_disable = 0x0008
  let ev_delete  = 0x0002
  let _ev_oneshot = 0x0010
  let _ev_clear   = 0x0020
  let ev_eof     = 0x8000
  let ev_error   = 0x4000
end

(** {1 Event Type} *)

type kevent = {
  ident: int;    (* identifier for this event (fd) *)
  filter: int;   (* filter for event *)
  flags: int;    (* action flags for kqueue *)
  fflags: int [@warning "-unused-field"];   (* filter flag value *)
  data: int [@warning "-unused-field"];     (* filter data value *)
  udata: Token.t; (* user data *)
}

module Event = struct
  type t = kevent
  
  let token t = t.udata
  let is_readable t = t.filter = Kqueue_const.evfilt_read
  let is_writable t = t.filter = Kqueue_const.evfilt_write
  let is_error t = (t.flags land Kqueue_const.ev_error) <> 0
  let is_eof t = (t.flags land Kqueue_const.ev_eof) <> 0
  
  let pp fmt t =
    let filter_str = 
      if t.filter = Kqueue_const.evfilt_read then "READ"
      else if t.filter = Kqueue_const.evfilt_write then "WRITE"
      else Printf.sprintf "FILTER(%d)" t.filter
    in
    let flags = [] in
    let flags = if is_error t then "ERROR" :: flags else flags in
    let flags = if is_eof t then "EOF" :: flags else flags in
    let flags_str = if flags = [] then "" else " " ^ String.concat "|" flags in
    Format.fprintf fmt "Event(fd=%d, %s%s)" t.ident filter_str flags_str
end

(** {1 Source abstraction} *)

module Source = struct
  type t = Fd.t
  let fd t = t
end

(** {1 I/O Vectors} *)

module Iovec = struct
  type t = bytes * int * int
  
  let create bytes ~pos ~len = (bytes, pos, len)
  let create_array arr = arr
end

(** {1 FFI External Functions} *)

external gluon_kqueue : unit -> Unix.file_descr = "gluon_kqueue"
external gluon_kevent : 
  Unix.file_descr -> kevent array -> int -> kevent array -> int -> int64 -> int = 
  "gluon_kevent_bytecode" "gluon_kevent"
external gluon_set_nonblocking : Unix.file_descr -> unit = "gluon_set_nonblocking"
external gluon_read : Unix.file_descr -> bytes -> int -> int -> int = "gluon_read"
external gluon_write : Unix.file_descr -> bytes -> int -> int -> int = "gluon_write"
external gluon_readv : Unix.file_descr -> Iovec.t array -> int = "gluon_readv"
external gluon_writev : Unix.file_descr -> Iovec.t array -> int = "gluon_writev"
external gluon_sendfile : Unix.file_descr -> Unix.file_descr -> int -> int -> int = "gluon_sendfile"

(** {1 Syscall wrapper} *)

let rec syscall fn =
  try Ok (fn ())
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> 
      (* Retry on EINTR *)
      syscall fn
  | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      Error `Would_block
  | Unix.Unix_error (Unix.EINPROGRESS, _, _) ->
      Error `In_progress
  | Unix.Unix_error (error, _, _) ->
      Error (`System_error (Unix.error_message error))
  | exn ->
      Error (`System_error (Printexc.to_string exn))

(** {1 File I/O} *)

module File = struct
  type t = Fd.t
  
  let pp = Fd.pp
  
  let close fd = 
    try Unix.close fd
    with Unix.Unix_error _ -> ()
  
  let read fd ?(pos = 0) ?len bytes =
    let len = match len with
      | None -> Bytes.length bytes - pos
      | Some l -> l
    in
    match syscall (fun () -> gluon_read fd bytes pos len) with
    | Ok n -> Ok n
    | Error _ -> Error `Noop
  
  let write fd ?(pos = 0) ?len bytes =
    let len = match len with
      | None -> Bytes.length bytes - pos
      | Some l -> l
    in
    match syscall (fun () -> gluon_write fd bytes pos len) with
    | Ok n -> Ok n
    | Error _ -> Error `Noop
  
  let read_vectored fd iovec =
    match syscall (fun () -> gluon_readv fd [|iovec|]) with
    | Ok n -> Ok n
    | Error _ -> Error `Noop
  
  let write_vectored fd iovec =
    match syscall (fun () -> gluon_writev fd [|iovec|]) with
    | Ok n -> Ok n
    | Error _ -> Error `Noop
  
  let to_source fd = fd
  
  let open_read path =
    match syscall (fun () ->
      Unix.openfile path [Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC] 0o644
    ) with
    | Ok fd -> Ok fd
    | Error _ -> Error `Noop
  
  let open_write ?(create = true) ?(truncate = false) path =
    let flags = [Unix.O_WRONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC] in
    let flags = if create then Unix.O_CREAT :: flags else flags in
    let flags = if truncate then Unix.O_TRUNC :: flags else flags in
    match syscall (fun () -> Unix.openfile path flags 0o644) with
    | Ok fd -> Ok fd
    | Error _ -> Error `Noop
end

(** {1 Network I/O} *)

module Net = struct
  module Addr = struct
    type 't raw_addr = string
    type tcp_addr = [ `v4 | `v6 ] raw_addr
    
    type stream_addr = {
      family: Unix.socket_domain;
      addr: Unix.inet_addr;
      port: int;
    }
    
    let loopback : tcp_addr = Unix.inet_addr_loopback |> Unix.string_of_inet_addr
    
    let to_string : tcp_addr -> string = fun addr -> addr
    
    let tcp (addr : tcp_addr) port =
      let inet_addr = Unix.inet_addr_of_string (addr :> string) in
      let family = 
        if String.contains (addr :> string) ':' then Unix.PF_INET6 
        else Unix.PF_INET 
      in
      { family; addr = inet_addr; port }
    
    let ip t = Unix.string_of_inet_addr t.addr
    let port t = t.port
    
    let pp fmt t =
      Format.fprintf fmt "%s:%d" (ip t) (port t)
    
    let to_domain t = t.family
    
    let to_unix t =
      Unix.SOCK_STREAM, Unix.ADDR_INET (t.addr, t.port)
    
    let of_unix = function
      | Unix.ADDR_INET (addr, port) ->
          let family = 
            if String.contains (Unix.string_of_inet_addr addr) ':' 
            then Unix.PF_INET6 else Unix.PF_INET 
          in
          { family; addr; port }
      | _ -> failwith "Unsupported address type"
    
    let of_addr_info info =
      match info.Unix.ai_addr with
      | Unix.ADDR_INET (addr, port) ->
          Some { family = info.ai_family; addr; port }
      | _ -> None
    
    let parse str =
      try
        match String.rindex_opt str ':' with
        | None -> Error `Noop
        | Some idx ->
            let host = String.sub str 0 idx in
            let port_str = String.sub str (idx + 1) (String.length str - idx - 1) in
            let port = int_of_string port_str in
            let addr = Unix.inet_addr_of_string host in
            let family = if String.contains host ':' then Unix.PF_INET6 else Unix.PF_INET in
            Ok { family; addr; port }
      with _ -> Error `Noop
    
    
    let get_info addr =
      try
        let _sock_type, _sock_addr = to_unix addr in
        let host = ip addr in
        let service = string_of_int (port addr) in
        let info_list = Unix.getaddrinfo host service [] in
        let addrs = List.filter_map of_addr_info info_list in
        Ok addrs
      with _ -> Error `Noop
  end
  
  module Socket = struct
    type 'kind socket = Fd.t
    type listen_socket = [ `listen ] socket
    type stream_socket = [ `stream ] socket
    
    let pp = Fd.pp
    let close fd = try Unix.close fd with _ -> ()
  end
  
  module TcpStream = struct
    type t = Socket.stream_socket
    
    let pp = Socket.pp
    let close = Socket.close
    
    let connect addr =
      let sock_type, sock_addr = Addr.to_unix addr in
      match syscall (fun () ->
        let sock = Unix.socket (Addr.to_domain addr) sock_type 0 in
        gluon_set_nonblocking sock;
        sock
      ) with
      | Error _ -> Error `Noop
      | Ok sock ->
          match syscall (fun () ->
            Unix.connect sock sock_addr;
            0
          ) with
          | Ok _ -> Ok (`Connected sock)
          | Error `In_progress -> Ok (`In_progress sock)
          | Error _ -> 
              Unix.close sock;
              Error `Noop
    
    let read t ?pos ?len bytes = File.read t ?pos ?len bytes
    let write t ?pos ?len bytes = File.write t ?pos ?len bytes
    let read_vectored t iovec = File.read_vectored t iovec
    let write_vectored t iovec = File.write_vectored t iovec
    
    let sendfile t ~file ~off ~len =
      match syscall (fun () -> gluon_sendfile t file off len) with
      | Ok n -> Ok n
      | Error _ -> Error `Noop
    
    let to_source t = t
  end
  
  module TcpListener = struct
    type t = Socket.listen_socket
    
    let pp = Socket.pp
    let close = Socket.close
    
    let bind ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr =
      let sock_type, sock_addr = Addr.to_unix addr in
      match syscall (fun () ->
        let sock = Unix.socket (Addr.to_domain addr) sock_type 0 in
        gluon_set_nonblocking sock;
        sock
      ) with
      | Error _ -> Error `Noop
      | Ok sock ->
          (* Set socket options *)
          (try
            if reuse_addr then
              Unix.setsockopt sock Unix.SO_REUSEADDR true;
            if reuse_port then
              Unix.setsockopt sock Unix.SO_REUSEPORT true
          with _ -> ());
          
          (* Bind and listen *)
          match syscall (fun () ->
            Unix.bind sock sock_addr;
            Unix.listen sock backlog
          ) with
          | Ok () -> Ok sock
          | Error _ ->
              Unix.close sock;
              Error `Noop
    
    let accept t =
      match syscall (fun () -> Unix.accept t) with
      | Ok (client_sock, client_addr) ->
          let _ = syscall (fun () -> gluon_set_nonblocking client_sock) in
          Ok (client_sock, Addr.of_unix client_addr)
      | Error _ -> Error `Noop
    
    let to_source t = t
  end
end

(** {1 Poll Type and Operations} *)

type t = {
  kq: Unix.file_descr;
  mutable registered_fds: (int, Token.t * Interest.t) Hashtbl.t [@warning "-unused-field"];
}

let create () =
  match syscall (fun () ->
    let kq = gluon_kqueue () in
    { kq; registered_fds = Hashtbl.create 128 }
  ) with
  | Ok t -> Ok t
  | Error (`System_error msg) -> Error (`System_error msg)
  | Error _ -> Error (`System_error "Failed to create kqueue")

let make_changelist fd token interests =
  let changes = ref [] in
  
  (* Add read event if readable interest *)
  if Interest.is_readable interests then
    changes := {
      ident = Fd.to_int fd;
      filter = Kqueue_const.evfilt_read;
      flags = Kqueue_const.(ev_add lor ev_enable);
      fflags = 0;
      data = 0;
      udata = token;
    } :: !changes;
  
  (* Add write event if writable interest *)
  if Interest.is_writable interests then
    changes := {
      ident = Fd.to_int fd;
      filter = Kqueue_const.evfilt_write;
      flags = Kqueue_const.(ev_add lor ev_enable);
      fflags = 0;
      data = 0;
      udata = token;
    } :: !changes;
  
  Array.of_list !changes

let make_delete_changelist fd interests =
  let changes = ref [] in
  
  (* Only delete events that were registered *)
  if Interest.is_readable interests then
    changes := {
      ident = Fd.to_int fd;
      filter = Kqueue_const.evfilt_read;
      flags = Kqueue_const.ev_delete;
      fflags = 0;
      data = 0;
      udata = Obj.magic 0;  (* NULL for delete operations *)
    } :: !changes;
  
  if Interest.is_writable interests then
    changes := {
      ident = Fd.to_int fd;
      filter = Kqueue_const.evfilt_write;
      flags = Kqueue_const.ev_delete;
      fflags = 0;
      data = 0;
      udata = Obj.magic 0;  (* NULL for delete operations *)
    } :: !changes;
  
  Array.of_list !changes

let poll ?timeout ?(max_events = 1024) t =
  let timeout_ns = 
    match timeout with
    | None -> -1L  (* Block indefinitely *)
    | Some ms -> Int64.(mul (of_int ms) 1_000_000L)  (* Convert ms to ns *)
  in
  
  match syscall (fun () ->
    let events = Array.make max_events (Obj.magic 0 : kevent) in
    let n = gluon_kevent t.kq [||] 0 events max_events timeout_ns in
    if n = 0 then [||]
    else Array.sub events 0 n
  ) with
  | Ok events -> Ok events
  | Error (`System_error msg) -> Error (`System_error msg)
  | Error _ -> Error (`System_error "Poll failed")

let register t ~fd ~token ~interests =
  let fd_int = Fd.to_int fd in
  
  (* Check if already registered *)
  if Hashtbl.mem t.registered_fds fd_int then
    Error (`System_error "File descriptor already registered")
  else
    let changelist = make_changelist fd token interests in
    match syscall (fun () ->
      let _ = gluon_kevent t.kq changelist (Array.length changelist) [||] 0 0L in
      Hashtbl.add t.registered_fds fd_int (token, interests)
    ) with
    | Ok () -> Ok ()
    | Error (`System_error msg) -> Error (`System_error msg)
    | Error _ -> Error (`System_error "Failed to register file descriptor")

let reregister t ~fd ~token ~interests =
  let fd_int = Fd.to_int fd in
  
  (* Must be already registered *)
  match Hashtbl.find_opt t.registered_fds fd_int with
  | None -> Error (`System_error "File descriptor not registered")
  | Some (_old_token, old_interests) ->
      (* First remove old events *)
      let delete_list = make_delete_changelist fd old_interests in
      let* () = match syscall (fun () ->
        let _ = gluon_kevent t.kq delete_list (Array.length delete_list) [||] 0 0L in
        ()
      ) with
      | Ok () -> Ok ()
      | Error (`System_error msg) -> Error (`System_error msg)
      | Error _ -> Error (`System_error "Failed to delete old events")
      in
      
      (* Then add new events *)
      let changelist = make_changelist fd token interests in
      match syscall (fun () ->
        let _ = gluon_kevent t.kq changelist (Array.length changelist) [||] 0 0L in
        Hashtbl.replace t.registered_fds fd_int (token, interests)
      ) with
      | Ok () -> Ok ()
      | Error (`System_error msg) -> Error (`System_error msg)
      | Error _ -> Error (`System_error "Failed to add new events")

let deregister t ~fd =
  let fd_int = Fd.to_int fd in
  
  match Hashtbl.find_opt t.registered_fds fd_int with
  | None -> Error (`System_error "File descriptor not registered")
  | Some (_token, interests) ->
      let changelist = make_delete_changelist fd interests in
      match syscall (fun () ->
        let _ = gluon_kevent t.kq changelist (Array.length changelist) [||] 0 0L in
        Hashtbl.remove t.registered_fds fd_int
      ) with
      | Ok () -> Ok ()
      | Error (`System_error msg) -> Error (`System_error msg)
      | Error _ -> Error (`System_error "Failed to deregister")

let set_nonblocking fd =
  match syscall (fun () -> gluon_set_nonblocking fd) with
  | Ok () -> Ok ()
  | Error (`System_error msg) -> Error (`System_error msg)
  | Error _ -> Error (`System_error "Failed to set non-blocking")

let pipe () =
  match syscall (fun () -> Unix.pipe ()) with
  | Error (`System_error msg) -> Error (`System_error msg) 
  | Error _ -> Error (`System_error "Failed to create pipe")
  | Ok (read_fd, write_fd) ->
      match set_nonblocking read_fd with
      | Error e -> 
          Unix.close read_fd;
          Unix.close write_fd;
          Error e
      | Ok () ->
          match set_nonblocking write_fd with
          | Error e ->
              Unix.close read_fd;
              Unix.close write_fd;
              Error e
          | Ok () -> Ok (read_fd, write_fd)

let pp fmt t =
  Format.fprintf fmt "Gluon.Poll(kqueue=%a, registered=%d)" 
    Fd.pp t.kq 
    (Hashtbl.length t.registered_fds)