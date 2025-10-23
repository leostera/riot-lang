open Std
open Tusk_model

(** Format an event for cargo-style output *)
let format (event : Event.t) =
  match event.kind with
  | PackageStarted { package = _ } ->
      "" (* Don't show on start - wait for cache status *)
  | PackageComplete { package; success; errors; _ } ->
      if success then "" (* Already shown as "Compiling" *)
      else if errors = [] then ""
        (* Skipped due to dependency failure, don't show *)
      else format "   \027[1;31mFailed\027[0m %s" package
  | PackageSkipped _ -> "" (* Don't show skipped packages *)
  | CacheHit { package; _ } ->
      format "   \027[1;32mCompiling\027[0m %s \027[1;90m(cached)\027[0m"
        package
  | CacheMiss { package; _ } ->
      format "   \027[1;32mCompiling\027[0m %s" package
  | CompileError { package = _; error } ->
      (* Just display the raw compiler output for best fidelity *)
      error.raw
  | BuildComplete { duration_ms; succeeded; failed; _ } ->
      if List.length failed = 0 then
        format "   \027[1;32mFinished\027[0m in %.2fs"
          (float_of_int duration_ms /. 1000.0)
      else
        format "   \027[1;31mFailed\027[0m with %d errors" (List.length failed)
  | CycleDetected { packages } ->
      let cycle_str = String.concat " → " packages in
      format
        "\n\
         \027[1;31merror:\027[0m circular dependency detected\n\n\
         The following packages form a dependency cycle:\n\
        \  %s → %s\n\n\
         Please remove one of these dependencies to break the cycle.\n"
        cycle_str (List.hd packages)
  | BuildGraphCreated _ -> ""
  | BuildGraphCreating -> ""
  | BuildStarted _ -> ""
  | CacheStored _ -> ""
  | CompilingImplementation _ -> ""
  | CompilingInterface _ -> ""
  | ComputingHash _ -> ""
  | CopyingFile _ -> ""
  | CreatingDirectory _ -> ""
  | DependencyMissing _ -> ""
  | DependencySatisfied _ -> ""
  | HashComputed _ -> ""
  | LinkingExecutable _ -> ""
  | LinkingLibrary _ -> ""
  | McpToolCall _ -> ""
  | QueuePackage _ -> ""
  | QueueStats _ -> ""
  | RpcRequestReceived _ -> ""
  | RpcResponseSent _ -> ""
  | ServerRestarted _ -> ""
  | ServerScanning _ -> ""
  | ServerShutdown -> ""
  | ServerStarted _ -> ""
  | WorkerAssigned _ -> ""
  | WorkerIdle _ -> ""
  | WorkerPoolStarted _ -> ""
  | WorkerStarted _ -> ""
  | StoreCreating -> ""
  | StoreCreated _ -> ""
  | WorkerPoolCreating _ -> ""
  | WorkerPoolCreated _ -> ""
  | WorkspaceEmpty -> ""
  | WorkspaceScanning -> ""
  | WorkspaceScanned _ -> ""
  | WritingFile _ -> ""
