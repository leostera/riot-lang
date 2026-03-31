open Kernel

let test_basic_watching = fun () ->
    (* Create a watcher *)
    let watcher = Fs.Events.create () |> Result.expect ~msg:"Failed to create watcher" in
    (* Watch /tmp directory *)
    let _ = Fs.Events.watch watcher ~path:"/tmp" ~latency:0.1 |> Result.expect ~msg:"Failed to watch /tmp" in
    println "✓ FSEvents watcher created and watching /tmp";
    (* Stop watcher *)
    Fs.Events.stop watcher |> Result.expect ~msg:"Failed to stop watcher";
    println "✓ FSEvents watcher stopped successfully"

let () = test_basic_watching ()
