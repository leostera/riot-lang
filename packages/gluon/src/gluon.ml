let ( let* ) = Result.bind
let log = Format.printf

type io_error =
  [ `Connection_closed
  | `Exn of exn
  | `No_info
  | `Unix_error of Unix.error [@config not (target_arch = "js")]
  | `Noop
  | `Eof
  | `Closed
  | `Process_down
  | `Timeout
  | `Would_block ]

type ('ok, 'err) io_result = ('ok, ([> io_error ] as 'err)) Stdlib.result

let pp_err fmt = function
  | `Noop -> Format.fprintf fmt "Noop"
  | `Eof -> Format.fprintf fmt "End of file"
  | `Timeout -> Format.fprintf fmt "Timeout"
  | `Process_down -> Format.fprintf fmt "Process_down"
  | `System_limit -> Format.fprintf fmt "System_limit"
  | `Closed -> Format.fprintf fmt "Closed"
  | `Connection_closed -> Format.fprintf fmt "Connection closed"
  | `Exn exn ->
      Format.fprintf fmt "Unexpected exceptoin: %s" (Printexc.to_string exn)
  | `No_info -> Format.fprintf fmt "No info"
  | `Would_block -> Format.fprintf fmt "Would block"
  | `Unix_error err ->
      Format.fprintf fmt "Unix_error(%s)" (Unix.error_message err)

module Libc = struct
  let enoent = 2
  let epipe = 32
  let ev_add = 0x1
  let ev_clear = 0x20
  let ev_delete = 0x2
  let ev_eof = 0x8000
  let ev_error = 0x4000
  let ev_receipt = 0x40
  let evfilt_read = -1
  let evfilt_write = -2
  let f_dupfd_cloexec = 67
  let f_setfd = 2
end

module Iovec = struct
  type iov = { ba : bytes; off : int; len : int }
  type t = iov array

  (** creates an iovector array with [size] equally distributed in [count]s *)
  let create ?(count = 1) ~size () =
    assert (count > 0);
    assert (size > 0);
    let size = size / count in
    Array.init count (fun _id ->
        { ba = Bytes.create size; off = 0; len = size })

  let with_capacity size = create ~size ()

  let sub ?(pos = 0) ~len t =
    let curr = ref 0 in
    t |> Array.to_list
    |> List.filter_map (fun iov ->
        if !curr + iov.len < pos then (
          curr := !curr + iov.len;
          None)
        else
          let next_curr = iov.len + !curr in
          let diff = len - !curr in
          if next_curr < len then (
            curr := next_curr;
            Some iov)
          else if diff > 0 then (
            curr := len;
            Some { iov with len = diff })
          else None)
    |> Array.of_list

  let length t = Array.fold_left (fun acc iov -> acc + (iov.len - iov.off)) 0 t
  let iter (t : t) fn = Array.iter fn t
  let of_bytes ba = [| { ba; off = 0; len = Bytes.length ba } |]
  let from_string str = of_bytes (Bytes.of_string str)
  let from_buffer buf = of_bytes (Buffer.to_bytes buf)

  let into_string t =
    let buf = Buffer.create (length t) in
    iter t (fun iov -> Buffer.add_bytes buf (Bytes.sub iov.ba iov.off iov.len));
    Buffer.contents buf
end

module Token = struct
  type t

  let unsafe_to_value (x : t) = Obj.magic x
  let unsafe_to_int (t : t) : int = unsafe_to_value t
  let hash t = Int.hash (unsafe_to_int t)

  let equal ?eq a b =
    match eq with
    | Some f -> f (unsafe_to_value a) (unsafe_to_value b)
    | None -> Int.equal (unsafe_to_int a) (unsafe_to_int b)

  let pp fmt t = Format.fprintf fmt "Token(%d)" (unsafe_to_int t)
  let make (x : 'whatever) : t = Obj.magic x
end

let rec syscall fn =
  match fn () with
  | ok -> ok
  | exception Unix.(Unix_error (EINTR, _, _)) -> syscall fn
  | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
      (* log "syscall is try again\n"; *)
      Error `Would_block
  | exception Unix.(Unix_error (reason, _, _)) -> Error (`Unix_error reason)

module Fd = struct
  type t = Unix.file_descr

  let to_int fd = Obj.magic fd
  let make fd = fd
  let pp fmt t = Format.fprintf fmt "Fd(%d)" (Obj.magic t)
  let close t = Unix.close t
  let seek = Unix.lseek
  let equal a b = Int.equal (to_int a) (to_int b)
end

module Non_zero_int = struct
  type t = int

  let make a = if a > 0 then Some a else None
end

module Interest : sig
  type t

  val readable : t
  val writable : t
  val add : t -> t -> t
  val remove : t -> t -> t option
  val is_readable : t -> bool
  val is_writable : t -> bool
end = struct
  type t = Non_zero_int.t

  let readable = 0b0001
  let writable = 0b0010
  let add a b = a lor b
  let remove a b = Non_zero_int.make (a land lnot b)
  let is_readable t = t land readable != 0
  let is_writable t = t land writable != 0
end

module Event = struct
  module type Intf = sig
    type t

    val is_error : t -> bool
    val is_priority : t -> bool
    val is_read_closed : t -> bool
    val is_readable : t -> bool
    val is_writable : t -> bool
    val is_write_closed : t -> bool
    val token : t -> Token.t
  end

  type t = E : (module Intf with type t = 'state) * 'state -> t

  let make m e = E (m, e)
  let token (E ((module Ev), state)) = Ev.token state
  let is_readable (E ((module Ev), state)) = Ev.is_readable state
  let is_writable (E ((module Ev), state)) = Ev.is_writable state
  let is_error (E ((module Ev), state)) = Ev.is_error state
  let is_read_closed (E ((module Ev), state)) = Ev.is_read_closed state
  let is_write_closed (E ((module Ev), state)) = Ev.is_write_closed state
  let is_priority (E ((module Ev), state)) = Ev.is_priority state
end

module Adapter = struct
  type kevent
  type kqueue = Fd.t
  type event = { fd : Fd.t; filter : int; flags : int; token : int }

  module FFI = struct
    external gluon_unix_kevent :
      max_events:int -> timeout:int64 -> kqueue -> event array
      = "gluon_unix_kevent"

    let kevent ~max_events ~timeout kq =
      syscall @@ fun () -> Ok (gluon_unix_kevent ~max_events ~timeout kq)

    external gluon_unix_kqueue : unit -> kqueue = "gluon_unix_kqueue"

    let kqueue () = syscall @@ fun () -> Ok (gluon_unix_kqueue ())

    external gluon_unix_fcntl : Fd.t -> cmd:int -> arg:int -> int
      = "gluon_unix_fcntl"

    let fcntl fd cmd arg =
      syscall @@ fun () -> Ok (gluon_unix_fcntl fd ~cmd ~arg)

    external gluon_unix_kevent_register :
      kqueue -> event array -> int array -> unit = "gluon_unix_kevent_register"

    let kevent_register fd changes ignored_errors =
      syscall @@ fun () ->
      Ok (gluon_unix_kevent_register fd changes ignored_errors)
  end

  module Kevent = struct
    type t = event

    let make fd ~filter ~flags ~token = { fd; filter; flags; token }
    let filter t = t.filter
    let flags t = t.flags
    let token t = Token.make t.token
    let is_readable t = filter t = Libc.evfilt_read
    let is_writable t = filter t = Libc.evfilt_write
    let is_error t = flags t land Libc.ev_error != 0
    let is_read_closed t = is_readable t && flags t land Libc.ev_eof != 0
    let is_write_closed t = is_writable t && flags t land Libc.ev_eof != 0
    let is_priority _t = false
  end

  module Selector = struct
    let name = "kqueue"

    type t = { kq : kqueue }

    let make () =
      let* kq = FFI.kqueue () in
      let* _ = FFI.(fcntl kq Libc.f_setfd Libc.f_dupfd_cloexec) in
      Ok { kq }

    let select ?(timeout = 500_000_000L) ?(max_events = 1_000) t =
      let* events = FFI.kevent ~timeout ~max_events t.kq in
      let events = Array.to_list events in
      let events = List.map (Event.make (module Kevent)) events in
      Ok events

    let register t ~fd ~token ~interest =
      let token = Token.unsafe_to_int token in
      let flags = Libc.(ev_clear lor ev_receipt lor ev_add) in
      let changes = ref [] in

      (if Interest.is_writable interest then
         (* log "%a registering writeable interest for %a\r\n%!" Token.pp tok Fd.pp fd; *)
         let kevent = Kevent.make fd ~filter:Libc.evfilt_write ~flags ~token in
         changes := kevent :: !changes);

      (if Interest.is_readable interest then
         (* log "%a registering readable interest for %a\r\n%!" Token.pp tok Fd.pp fd; *)
         let kevent = Kevent.make fd ~filter:Libc.evfilt_read ~flags ~token in
         changes := kevent :: !changes);

      let changes = Array.of_list !changes in
      (* log "%a registering %a\r\n%!" Token.pp tok Fd.pp fd; *)
      FFI.kevent_register t.kq changes [| Libc.epipe |]

    let reregister t ~fd ~token ~interest =
      let token = Token.unsafe_to_int token in
      let flags = Libc.(ev_clear lor ev_receipt) in

      let write_flags =
        if Interest.is_writable interest then Libc.(flags lor ev_add)
        else Libc.(flags lor ev_delete)
      in

      let read_flags =
        if Interest.is_readable interest then Libc.(flags lor ev_add)
        else Libc.(flags lor ev_delete)
      in

      let changes =
        [|
          Kevent.make fd ~filter:Libc.evfilt_write ~flags:write_flags ~token;
          Kevent.make fd ~filter:Libc.evfilt_read ~flags:read_flags ~token;
        |]
      in

      (* log "reregistering %a\r\n%!" Fd.pp fd; *)
      FFI.kevent_register t.kq changes Libc.[| epipe; enoent |]

    let deregister t ~fd =
      let flags = Libc.(ev_delete lor ev_receipt) in
      let changes =
        [|
          Kevent.make fd ~filter:Libc.evfilt_write ~flags ~token:0;
          Kevent.make fd ~filter:Libc.evfilt_read ~flags ~token:0;
        |]
      in
      (* log "deregistering %a\r\n%!" Fd.pp fd; *)
      FFI.kevent_register t.kq changes Libc.[| enoent |]
  end

  module Event = Kevent
end

module Source = struct
  module type Intf = sig
    type t

    val deregister : t -> Adapter.Selector.t -> (unit, [> `Noop ]) io_result

    val register :
      t ->
      Adapter.Selector.t ->
      Token.t ->
      Interest.t ->
      (unit, [> `Noop ]) io_result

    val reregister :
      t ->
      Adapter.Selector.t ->
      Token.t ->
      Interest.t ->
      (unit, [> `Noop ]) io_result
  end

  type t = S : ((module Intf with type t = 'state) * 'state) -> t

  let make src state = S (src, state)
  let register (S ((module Src), state)) = Src.register state
  let reregister (S ((module Src), state)) = Src.reregister state
  let deregister (S ((module Src), state)) = Src.deregister state
end

module File = struct
  type t = Fd.t

  let pp = Fd.pp
  let close = Fd.close

  let read fd ?(pos = 0) ?len buf =
    let len = Option.value len ~default:(Bytes.length buf - 1) in
    syscall @@ fun () -> Ok (UnixLabels.read fd ~buf ~pos ~len)

  let write fd ?(pos = 0) ?len buf =
    let len = Option.value len ~default:(Bytes.length buf - 1) in
    syscall @@ fun () -> Ok (UnixLabels.write fd ~buf ~pos ~len)

  external gluon_readv : Unix.file_descr -> Iovec.t -> int = "gluon_unix_readv"

  let read_vectored fd iov = syscall @@ fun () -> Ok (gluon_readv fd iov)

  external gluon_writev : Unix.file_descr -> Iovec.t -> int
    = "gluon_unix_writev"

  let write_vectored fd iov = syscall @@ fun () -> Ok (gluon_writev fd iov)

  external gluon_sendfile :
    Unix.file_descr -> Unix.file_descr -> int -> int -> int
    = "gluon_unix_sendfile"

  let sendfile fd ~file ~off ~len =
    syscall @@ fun () -> Ok (gluon_sendfile file fd off len)

  let readdir path =
    syscall @@ fun () ->
    try Ok (Array.to_list (Sys.readdir path))
    with e -> Error (`Unix_error (Unix.ENOENT))

  let mkdir path perm =
    syscall @@ fun () ->
    try
      Unix.mkdir path perm;
      Ok ()
    with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

  let mkdirp path perm =
    syscall @@ fun () ->
    (* Split path into components, handling absolute paths *)
    let components = 
      let parts = String.split_on_char '/' path in
      match parts with
      | "" :: rest -> "/" :: List.filter (fun s -> s <> "") rest
      | parts -> List.filter (fun s -> s <> "") parts
    in
    (* Create each directory component incrementally using fold *)
    let create_dir acc_result component =
      match acc_result with
      | Error e -> Error e
      | Ok current_path ->
          let new_path = 
            match current_path, component with
            | "", "/" -> "/"
            | "", c -> c
            | "/", c -> "/" ^ c
            | p, c -> p ^ "/" ^ c
          in
          try
            Unix.mkdir new_path perm;
            Ok new_path
          with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok new_path
          | Unix.Unix_error (e, _, _) -> Error (`Unix_error e)
    in
    match List.fold_left create_dir (Ok "") components with
    | Ok _ -> Ok ()
    | Error e -> Error e

  let copy_file src dst =
    syscall @@ fun () ->
    try
      let ic = open_in_bin src in
      let oc = open_out_bin dst in
      let buf_size = 65536 in (* 64KB buffer *)
      let buf = Bytes.create buf_size in
      let rec copy () =
        match input ic buf 0 buf_size with
        | 0 -> ()
        | n ->
            output oc buf 0 n;
            copy ()
      in
      Fun.protect
        ~finally:(fun () ->
          close_in_noerr ic;
          close_out_noerr oc)
        (fun () ->
          copy ();
          Ok ())
    with e -> Error (`Exn e)

  let is_directory path =
    syscall @@ fun () ->
    try Ok (Sys.is_directory path)
    with e -> Error (`Exn e)

  let file_exists path =
    syscall @@ fun () ->
    try Ok (Sys.file_exists path)
    with e -> Error (`Exn e)

  let stat path =
    syscall @@ fun () ->
    try Ok (Unix.stat path)
    with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

  let chmod path perm =
    syscall @@ fun () ->
    try
      Unix.chmod path perm;
      Ok ()
    with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

  let symlink src dst =
    syscall @@ fun () ->
    try
      Unix.symlink src dst;
      Ok ()
    with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

  let rmdir path =
    syscall @@ fun () ->
    try
      Unix.rmdir path;
      Ok ()
    with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

  let remove path =
    syscall @@ fun () ->
    try
      Sys.remove path;
      Ok ()
    with e -> Error (`Exn e)

  let getcwd () =
    syscall @@ fun () ->
    try Ok (Sys.getcwd ())
    with e -> Error (`Exn e)

  let chdir path =
    syscall @@ fun () ->
    try
      Sys.chdir path;
      Ok ()
    with e -> Error (`Exn e)

  let opendir path =
    syscall @@ fun () ->
    try Ok (Unix.opendir path)
    with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

  let readdir_handle handle =
    syscall @@ fun () ->
    try Ok (Unix.readdir handle)
    with 
    | End_of_file -> Error `Eof
    | Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

  let closedir handle =
    syscall @@ fun () ->
    try
      Unix.closedir handle;
      Ok ()
    with Unix.Unix_error (e, _, _) -> Error (`Unix_error e)

  let to_source t =
    let module Src = struct
      type nonrec t = t

      let register t selector token interest =
        Adapter.Selector.register selector ~fd:t ~token ~interest

      let reregister t selector token interest =
        Adapter.Selector.reregister selector ~fd:t ~token ~interest

      let deregister t selector = Adapter.Selector.deregister selector ~fd:t
    end in
    Source.make (module Src) t
end

module Net = struct
  module Addr = struct
    type 't raw_addr = string
    type tcp_addr = [ `v4 | `v6 ] raw_addr
    type stream_addr = [ `Tcp of tcp_addr * int ]

    module Ipaddr = struct
      let to_unix : tcp_addr -> Unix.inet_addr = Unix.inet_addr_of_string
      let of_unix : Unix.inet_addr -> tcp_addr = Unix.string_of_inet_addr
    end

    let loopback : tcp_addr = "0.0.0.0"

    let tcp host port =
      assert (String.length host > 0);
      `Tcp (host, port)

    let to_unix addr =
      match addr with
      | `Tcp (host, port) ->
          (Unix.SOCK_STREAM, Unix.ADDR_INET (Ipaddr.to_unix host, port))

    let to_domain addr = match addr with `Tcp (_host, _) -> Unix.PF_INET

    let of_unix sockaddr =
      match sockaddr with
      | Unix.ADDR_INET (host, port) -> tcp (Ipaddr.of_unix host) port
      | Unix.ADDR_UNIX addr -> failwith ("unsupported unix addresses: " ^ addr)

    let pp ppf (addr : stream_addr) =
      match addr with
      | `Tcp (host, port) -> Format.fprintf ppf "%s:%d" host port

    let to_string t = t

    let of_addr_info Unix.{ ai_family; ai_addr; ai_socktype; ai_protocol; _ } =
      match (ai_family, ai_socktype, ai_addr) with
      | ( (Unix.PF_INET | Unix.PF_INET6),
          (Unix.SOCK_DGRAM | Unix.SOCK_STREAM),
          Unix.ADDR_INET (addr, port) ) -> (
          match ai_protocol with
          | 6 -> Some (tcp (Unix.string_of_inet_addr addr) port)
          | _ -> None)
      | _ -> None

    let get_info host service =
      syscall @@ fun () ->
      let info = Unix.getaddrinfo host service [] in
      Ok (List.filter_map of_addr_info info)

    let of_host_and_port ~host ~port =
      match get_info host (Int.to_string port) with
      | Ok (ip :: _) -> Ok ip
      | Ok [] -> Error `No_info
      | Error err -> Error err

    let get_info (`Tcp (host, port)) = get_info host (Int.to_string port)
    let ip (`Tcp (ip, _)) = ip
    let port (`Tcp (_, port)) = port
  end

  module Socket = struct
    type 'kind socket = Fd.t
    type listen_socket = [ `listen ] socket
    type stream_socket = [ `stream ] socket

    let pp fmt t = Fd.pp fmt t
    let close t = Unix.close t

    let make sock_domain sock_type =
      let fd = Unix.socket ~cloexec:true sock_domain sock_type 0 in
      Unix.set_nonblock fd;
      Fd.make fd
  end

  module TcpListener = struct
    type t = Socket.listen_socket

    let pp = Socket.pp
    let close = Socket.close

    let bind ?(reuse_addr = true) ?(reuse_port = true) ?(backlog = 128) addr =
      syscall @@ fun () ->
      let sock_domain = Addr.to_domain addr in
      let sock_type, sock_addr = Addr.to_unix addr in
      let fd = Socket.make sock_domain sock_type in
      Unix.setsockopt fd Unix.SO_REUSEADDR reuse_addr;
      Unix.setsockopt fd Unix.SO_REUSEPORT reuse_port;
      Unix.bind fd sock_addr;
      Unix.listen fd backlog;
      Ok fd

    let accept fd =
      syscall @@ fun () ->
      let raw_fd, client_addr = Unix.accept ~cloexec:true fd in
      Unix.set_nonblock raw_fd;
      let addr = Addr.of_unix client_addr in
      let fd = Fd.make raw_fd in
      Ok (fd, addr)

    let to_source t =
      let module Src = struct
        type nonrec t = t

        let register t selector token interest =
          Adapter.Selector.register selector ~fd:t ~token ~interest

        let reregister t selector token interest =
          Adapter.Selector.reregister selector ~fd:t ~token ~interest

        let deregister t selector = Adapter.Selector.deregister selector ~fd:t
      end in
      Source.make (module Src) t
  end

  module TcpStream = struct
    type t = Socket.stream_socket

    let pp = Socket.pp
    let close = Socket.close

    let connect addr =
      let sock_domain = Addr.to_domain addr in
      let sock_type, sock_addr = Addr.to_unix addr in
      let fd = Socket.make sock_domain sock_type in
      syscall @@ fun () ->
      try
        Unix.connect fd sock_addr;
        Ok (`Connected fd)
      with Unix.(Unix_error (EINPROGRESS, _, _)) -> Ok (`In_progress fd)

    let read fd ?(pos = 0) ?len buf =
      let len = Option.value len ~default:(Bytes.length buf - 1) in
      syscall @@ fun () -> Ok (UnixLabels.read fd ~buf ~pos ~len)

    let write fd ?(pos = 0) ?len buf =
      let len = Option.value len ~default:(Bytes.length buf - 1) in
      syscall @@ fun () -> Ok (UnixLabels.write fd ~buf ~pos ~len)

    external gluon_readv : Unix.file_descr -> Iovec.t -> int
      = "gluon_unix_readv"

    let read_vectored fd iov = syscall @@ fun () -> Ok (gluon_readv fd iov)

    external gluon_writev : Unix.file_descr -> Iovec.t -> int
      = "gluon_unix_writev"

    let write_vectored fd iov = syscall @@ fun () -> Ok (gluon_writev fd iov)

    external gluon_sendfile :
      Unix.file_descr -> Unix.file_descr -> int -> int -> int
      = "gluon_unix_sendfile"

    let sendfile fd ~file ~off ~len =
      syscall @@ fun () -> Ok (gluon_sendfile file fd off len)

    let to_source t =
      let module Src = struct
        type nonrec t = t

        let register t selector token interest =
          Adapter.Selector.register selector ~fd:t ~token ~interest

        let reregister t selector token interest =
          Adapter.Selector.reregister selector ~fd:t ~token ~interest

        let deregister t selector = Adapter.Selector.deregister selector ~fd:t
      end in
      Source.make (module Src) t
  end
end

module Poll = struct
  type t = { selector : Adapter.Selector.t }

  let make () =
    let* selector = Adapter.Selector.make () in
    Ok { selector }

  let poll ?max_events ?timeout t =
    Adapter.Selector.select ?timeout ?max_events t.selector

  let register (t : t) token interests source =
    Source.register source t.selector token interests

  let reregister (t : t) token interests source =
    Source.reregister source t.selector token interests

  let deregister (t : t) source = Source.deregister source t.selector
end
(* Test change *)
