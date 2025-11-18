(** LSM Database Lock Implementation *)

open Std

type lock_mode =
  | Shared
  | Exclusive

type t = {
  mutable file : Fs.File.t option;
  data_dir : Path.t;
  mode : lock_mode;
}

let lock_path data_dir = 
  Path.join data_dir (Path.v "LOCK")

let acquire ~data_dir ~mode ~timeout =
  let lock_file_path = lock_path data_dir in
  
  (* Ensure data directory exists *)
  (match Fs.create_dir_all data_dir with
  | Error e -> Error ("Failed to create data dir: " ^ IO.error_message e)
  | Ok () ->
      (* Create lock file if it doesn't exist, then open for read/write *)
      let _ = match Fs.File.create_new lock_file_path with
        | Ok f -> Fs.File.close f |> ignore
        | Error _ -> () (* File already exists, that's fine *)
      in
      
      (* Now open for read/write (don't truncate!) *)
      match Fs.File.open_read_write lock_file_path with
      | Error e -> Error ("Failed to open lock file: " ^ IO.error_message e)
      | Ok file ->
          (* Check if infinite wait requested *)
          let timeout_secs = Time.Duration.to_secs timeout in
          let is_infinite = timeout_secs < 0 in
          
          (* Choose lock function based on mode *)
          let (lock_fn, try_lock_fn, lock_name) = match mode with
            | Shared -> (Fs.File.lock_shared, Fs.File.try_lock_shared, "shared")
            | Exclusive -> (Fs.File.lock_exclusive, Fs.File.try_lock_exclusive, "exclusive")
          in
          
          (* Try to acquire lock with timeout *)
          let rec try_lock_with_timeout remaining =
            match try_lock_fn file with
            | Error e -> 
                Fs.File.close file |> ignore;
                Error ("Lock error: " ^ IO.error_message e)
            | Ok true ->
                (* Got lock! *)
                Ok { file = Some file; data_dir; mode }
            | Ok false ->
                (* Lock held by another process *)
                if Time.Duration.is_zero timeout then begin
                  Fs.File.close file |> ignore;
                  Error ("Database locked by another process (" ^ lock_name ^ " lock unavailable)")
                end else if Time.Duration.to_secs remaining < 0 && not is_infinite then begin
                  Fs.File.close file |> ignore;
                  Error (
                    "Database locked by another process (timeout after " ^
                    string_of_int timeout_secs ^ "s).\n" ^
                    "Another 'poneglyph' command may be running.\n" ^
                    "Try again in a moment or check for stuck processes."
                  )
                end else begin
                  (* Wait and retry *)
                  let sleep_duration = Time.Duration.from_millis 100 in
                  sleep sleep_duration;
                  let new_remaining = Time.Duration.sub remaining sleep_duration in
                  try_lock_with_timeout new_remaining
                end
          in
          
          if is_infinite then
            (* Blocking wait (infinite timeout) *)
            match lock_fn file with
            | Error e ->
                Fs.File.close file |> ignore;
                Error ("Lock error: " ^ IO.error_message e)
            | Ok () -> Ok { file = Some file; data_dir; mode }
          else
            (* Timeout-based retry *)
            try_lock_with_timeout timeout
  )

let try_acquire ~data_dir ~mode =
  match acquire ~data_dir ~mode ~timeout:Time.Duration.zero with
  | Ok lock -> Ok (Some lock)
  | Error msg when String.starts_with ~prefix:"Database locked" msg -> 
      Ok None
  | Error e -> Error e

let release lock =
  match lock.file with
  | None -> Ok ()  (* Already released *)
  | Some file ->
      lock.file <- None;  (* Mark as released *)
      (* Unlock and close *)
      let unlock_result = Fs.File.unlock file in
      let close_result = Fs.File.close file in
      
      match (unlock_result, close_result) with
      | Ok (), Ok () -> Ok ()
      | Error e, _ -> Error ("Unlock failed: " ^ IO.error_message e)
      | _, Error e -> Error ("Close failed: " ^ IO.error_message e)

let is_locked ~data_dir =
  (* Try to acquire exclusive lock to check if database is locked *)
  match try_acquire ~data_dir ~mode:Exclusive with
  | Ok (Some lock) ->
      (* Not locked - we got it, release immediately *)
      release lock |> ignore;
      false
  | Ok None -> true  (* Locked by another process *)
  | Error _ -> false  (* Error means can't determine, assume not locked *)
