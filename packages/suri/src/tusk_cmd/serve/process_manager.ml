open Std

type t = {
  pid : int;
  cmd : string;
  args : string list;
}

type status = 
  | Running 
  | Exited of int 
  | Signaled of int

let spawn ~cmd ~args =
  (* Spawn child process, inheriting stdout/stderr *)
  let pid = Unix.fork () in
  if pid = 0 then begin
    (* Child process *)
    (* Just pass the command with args as a list *)
    (* We'll use a simpler syscall *)
    let full_cmd = cmd ^ " " ^ (String.concat " " args) in
    Unix.execv "/bin/sh" [|"/bin/sh"; "-c"; full_cmd|]
  end else begin
    (* Parent process *)
    { pid; cmd; args }
  end

let status t =
  match Unix.waitpid [Unix.WNOHANG] t.pid with
  | 0, _ -> Running
  | _, Unix.WEXITED code -> Exited code
  | _, Unix.WSIGNALED signal -> Signaled signal
  | _, Unix.WSTOPPED _ -> Running

let kill t ~signal =
  try Unix.kill t.pid signal
  with Unix.Unix_error _ -> ()

let wait_for_exit t ~timeout =
  let start = Time.Instant.now () in
  let rec loop () =
    match status t with
    | Running ->
        if Time.Instant.elapsed start > timeout then
          false  (* Timeout *)
        else begin
          sleep (Time.Duration.from_millis 100);
          loop ()
        end
    | Exited _ | Signaled _ -> true
  in
  loop ()

let graceful_shutdown t =
  (* Try SIGTERM first (signal 15) *)
  kill t ~signal:15;
  
  (* Wait up to 5 seconds *)
  if wait_for_exit t ~timeout:(Time.Duration.from_secs 5) then
    ()
  else begin
    (* Force kill - SIGKILL (signal 9) *)
    Log.warn "Process didn't exit gracefully, force killing";
    kill t ~signal:9;
    ignore (wait_for_exit t ~timeout:(Time.Duration.from_secs 1))
  end
