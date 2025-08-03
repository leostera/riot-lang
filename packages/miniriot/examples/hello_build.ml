open Miniriot

(* Define some message types for our build system *)
type Message.t += 
  | Compile of string
  | Compiled of string * bool
  | Done

let worker name coordinator_pid () =
  let rec loop () =
    match receive () with
    | Compile file ->
        Printf.printf "[%s] Compiling %s...\n" name file;
        (* Simulate compilation *)
        yield ();
        send coordinator_pid (Compiled (file, true));
        loop ()
    | Exit ->
        Printf.printf "[%s] Shutting down\n" name;
        exit ()
    | _ -> loop ()
  in
  loop ()

let coordinator main_pid files () =
  let my_pid = self () in
  (* Spawn workers *)
  let worker1 = spawn (worker "Worker1" my_pid) in
  let worker2 = spawn (worker "Worker2" my_pid) in
  
  Printf.printf "[Coordinator] Starting build of %d files\n" (List.length files);
  
  (* Distribute work *)
  List.iteri (fun i file ->
    let target = if i mod 2 = 0 then worker1 else worker2 in
    send target (Compile file)
  ) files;
  
  (* Collect results *)
  let rec collect_results n acc =
    if n = 0 then acc
    else
      match receive () with
      | Compiled (file, success) ->
          Printf.printf "[Coordinator] %s: %s\n" file 
            (if success then "✓" else "✗");
          collect_results (n - 1) ((file, success) :: acc)
      | _ -> collect_results n acc
  in
  
  let results = collect_results (List.length files) [] in
  
  (* Shutdown workers *)
  send worker1 Exit;
  send worker2 Exit;
  
  (* Report *)
  let successful = List.filter (fun (_, s) -> s) results |> List.length in
  Printf.printf "\n[Coordinator] Build complete: %d/%d successful\n" 
    successful (List.length files);
  
  (* Tell main we're done *)
  send main_pid Done;
  
  if successful = List.length files then
    exit ()
  else
    Miniriot.Process.Exception (Failure "Build failed")

let main () =
  Printf.printf "[Main] Starting...\n%!";
  let files = [
    "src/foo.ml";
    "src/bar.ml";
    "src/baz.ml";
    "lib/utils.ml";
    "lib/helpers.ml";
  ] in
  
  let my_pid = self () in
  let _coordinator_pid = spawn (coordinator my_pid files) in
  Printf.printf "[Main] Spawned coordinator\n%!";
  
  (* Wait for Done message *)
  match receive () with
  | Done -> 
      Printf.printf "[Main] Build finished!\n%!";
      exit ()
  | _ -> exit ()

let () =
  (* enable_trace (); *)
  let status = run ~main in
  Stdlib.exit status