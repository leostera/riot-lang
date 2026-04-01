open Global0
open Collections

type t = Unix.file_descr

type pipe = {
  read_fd: t;
  write_fd: t;
}

module OpenFlags = struct
  type t =
    | ReadOnly
    | WriteOnly
    | ReadWrite
    | Create
    | Truncate
    | Append
    | Exclusive

  let to_unix = fun flags ->
    List.map
      (
        function
        | ReadOnly -> Unix.O_RDONLY
        | WriteOnly -> Unix.O_WRONLY
        | ReadWrite -> Unix.O_RDWR
        | Create -> Unix.O_CREAT
        | Truncate -> Unix.O_TRUNC
        | Append -> Unix.O_APPEND
        | Exclusive -> Unix.O_EXCL
      )
      flags
end

let to_int = fun fd -> Obj.magic fd

let to_unix = fun fd -> fd

let of_unix = fun fd ->
  Unix.set_nonblock fd;
  fd

let make_blocking = fun fd -> fd

let set_blocking = fun fd -> Unix.clear_nonblock fd

let close = fun t -> Unix.close t

let equal = fun a b ->
  Int.equal (Obj.magic a) (Obj.magic b)

let open_file = fun path flags perm ->
  let fd = Unix.openfile path (OpenFlags.to_unix flags) perm in
  of_unix fd

let pipe = fun () ->
  let read_fd, write_fd = Unix.pipe () in
  {read_fd = of_unix read_fd;write_fd = of_unix write_fd;}

let is_tty = fun fd -> Unix.isatty fd
