(**
   Asynchronous task execution.

   This module provides simple async/await functionality for running operations
   concurrently. Tasks run in separate system threads and can be awaited for
   their results.

   ## Examples

   Basic async operations:

   ```ocaml open Std

   (* Run async computation *) let task = Task.async (fun () -> (* This runs in
   parallel *) expensive_computation () ) in

   (* Do other work... *)

   (* Wait for result *) match Task.await task with | Ok result -> println
   "Got: %d" result | Error exn -> println "Failed: %s" (Printexc.to_string
   exn)

   (* Run multiple tasks concurrently *) let tasks = List.map (fun url ->
   Task.async (fun () -> fetch_url url) ) urls in

   let results = Task.await_all tasks ```

   ## Concurrency Model

   Tasks run in system threads (domains in OCaml 5), allowing true parallelism
   on multi-core systems. Each task runs independently and can be awaited for
   its result.

   ## Error Handling

   Tasks that raise exceptions return [`Error exn`] when awaited. The exception
   is captured and propagated to the awaiting thread.

   ## Use Cases

   - I/O-bound operations (network requests, file operations)
   - CPU-bound parallel computations
   - Running multiple independent operations concurrently
*)
open Global

(** The type of an asynchronous task that will produce a value of type `'a` *)
type 'a t

(**
   Starts an asynchronous task.

   The function runs in a separate thread and begins executing immediately. The
   returned task handle can be awaited to get the result.

   ## Examples

   ```ocaml (* Simple async operation *) let task = Task.async (fun () ->
   Thread.sleep 1.0; 42 ) in

   (* I/O operations *) let read_task = Task.async (fun () -> Fs.read (Path.v
   "large_file.txt") |> Result.expect ~msg:"Cannot read file" ) in

   (* Parallel computation *) let compute_task = Task.async (fun () ->
   List.init 1000000 (fun i -> i * i) |> List.fold_left (+) 0 ) ```

   ## Exceptions

   If the function raises an exception, it's captured and returned as [`Error`]
   when the task is awaited.

   ```ocaml let failing_task = Task.async (fun () -> panic "Something went
   wrong" ) in

   match Task.await failing_task with | Ok _ -> assert false | Error exn ->
   println "Caught: %s" (Printexc.to_string exn) ```
*)
val async: (unit -> 'a) -> 'a t

(**
   Waits for a task to complete and returns its result.

   Blocks the current thread until the task completes. Returns [`Ok value`] if
   the task succeeded, or [`Error exn`] if it raised an exception.

   ## Examples

   ```ocaml (* Wait for single task *) let task = Task.async (fun () ->
   compute_result ()) in match Task.await task with | Ok result -> println
   "Computation done: %d" result | Error exn -> println "Computation failed:
   %s" (Printexc.to_string exn)

   (* Chain operations *) let result = Task.async (fun () -> fetch_data ()) |>
   Task.await |> Result.map process_data |> Result.map_err (fun e ->
   Printf.sprintf "Task failed: %s" (Printexc.to_string e)) ```

   ## Blocking Behavior

   This function blocks the calling thread. Don't call it from within another
   task if you need to maintain parallelism.
*)
val await: 'a t -> ('a, exn) result

(**
   Waits for multiple tasks to complete.

   More efficient than `List.map await` for large task lists, as it collects
   results as they arrive rather than waiting for each task sequentially.

   ## Examples

   ```ocaml (* Parallel HTTP requests *) let fetch_all urls = urls |> List.map
   (fun url -> Task.async (fun () -> http_get url)) |> Task.await_all |>
   List.filter_map Result.to_option

   (* Parallel file processing *) let process_files paths = paths |> List.map
   (fun path -> Task.async (fun () -> Fs.read path |> Result.map parse_file |>
   Result.expect ~msg:"Failed to process")) |> Task.await_all

   (* Mixed success/failure handling *) let results = Task.await_all tasks in
   let successes = List.filter_map Result.to_option results in let failures =
   List.filter_map (function Error e -> Some e | _ -> None) results in
   Printf.printf "Succeeded: %d, Failed: %d\n" (List.length successes)
   (List.length failures) ```

   ## Performance

   This function uses efficient polling to collect results as tasks complete,
   rather than waiting for them in order. This means faster overall completion
   when tasks have varying execution times.

   ## Order Preservation

   Results are returned in the same order as the input task list, regardless of
   completion order.
*)
val await_all: 'a t list -> ('a, exn) result list
