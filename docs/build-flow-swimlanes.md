# Build Flow Swimlanes Diagram

## Third Build (Hanging Case) - When minitusk is cached

```
Client          Server                   Queue               Worker Pool         Worker            Build Results
  |                |                        |                      |                |                    |
  |--BuildPackage->|                        |                      |                |                    |
  |                |                        |                      |                |                    |
  |<-BuildStarted--|                        |                      |                |                    |
  |                |                        |                      |                |                    |
  |                |--queue_initial_tasks-->|                      |                |                    |
  |                |  (minitusk only)       |                      |                |                    |
  |                |                        | minitusk -> ready    |                |                    |
  |                |                        |                      |                |                    |
  |                |--try_assign_work------>|                      |                |                    |
  |                |                        |                      |                |                    |
  |                |<-get_next_buildable----|                      |                |                    |
  |                |  (returns minitusk)    |                      |                |                    |
  |                |                        |                      |                |                    |
  |                |---send_task----------->|---send_to_worker---->|                |                    |
  |                |                        |                      |                |                    |
  |                |                        | minitusk -> busy     |                |                    |
  |                |                        |                      |                |                    |
  |                |                        |                      |                |--mark_building----->|
  |                |                        |                      |                |  (minitusk)         |
  |                |                        |                      |                |                    |
  |                |                        |                      |<-Task(minitusk)-|                    |
  |                |                        |                      |                |                    |
  |                |                        |                      |                |--check_cache        |
  |                |                        |                      |                |  (CACHE HIT)        |
  |                |                        |                      |                |                    |
  |<-Log.CacheHit--|<-Log.CacheHit----------|<-Log.CacheHit--------|<-Log.CacheHit--|                    |
  |                |                        |                      |                |                    |
  |                |                        |                      |                |--promote_from_store |
  |                |                        |                      |                |  (success)          |
  |                |                        |                      |                |                    |
  |                |                        |                      |<-TaskCompleted--|                    |
  |                |                        |                      |  (minitusk)     |                    |
  |                |                        |                      |                |                    |
  |                |<-TaskCompleted----------|<-TaskCompleted------|                |                    |
  |                |  (minitusk)             |                      |                |                    |
  |                |                        |                      |                |                    |
  |                |--handle_task_complete-->|                      |                |                    |
  |                |                        | mark_completed       |                |                    |
  |                |                        | (minitusk)          |                |                    |
  |                |                        |                      |                |                    |
  |                |                        |                      |                |--mark_built-------->|
  |                |                        |                      |                |  (minitusk)         |
  |                |                        |                      |                |                    |
  |<-Log.PackageComplete                    |                      |                |                    |
  |                |                        |                      |                |                    |
  |                |--check_build_complete-->|                      |                |--all_done?--------->|
  |                |                        |                      |                |  returns TRUE       |
  |                |                        |                      |                |                    |
  |<-Log.BuildComplete                      |                      |                |                    |
  |                |                        |                      |                |                    |
  |<-BuildComplete--|                        |                      |                |                    |
  |                |                        |                      |                |                    |
  ✓                ✓                        ✓                      ✓                ✓                    ✓
```

## Expected Additional Steps After Fix

After the `handle_task_complete`, the server should:
1. Call `check_build_complete` which detects all packages are done
2. Send Log.BuildComplete event to client  
3. Send Rpc.BuildComplete to client
4. Client receives completion and exits

## The Bug

Looking at the flow, after `handle_task_complete`:
1. ✅ We call `check_build_complete` 
2. ✅ It returns `true` (all done)
3. ✅ We send Log.BuildComplete to client
4. ❌ **BUT** we never send Rpc.BuildComplete to the client!

The server loop continues with `if is_complete then server_loop new_state` but never sends the final RPC response.

## Problem Location

In `tusk_server.ml` around line 900, when handling `task_completed`:
```ocaml
| `task_completed (pkg_name, hash) ->
    handle_task_complete state pkg_name true hash;
    let is_complete, new_state = check_build_complete state in
    if is_complete then server_loop new_state   // <-- MISSING Rpc.BuildComplete!
    else (
      try_assign_work state;
      server_loop state
    )
```

The `check_build_complete` function sends Log.BuildComplete but NOT Rpc.BuildComplete. It only returns a boolean and new state.