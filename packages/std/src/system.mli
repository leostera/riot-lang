(** # System - System information and operations
    
    System-level operations and information queries. This is a re-export
    of [Kernel.System] with cross-platform abstractions for common system tasks.
    
    ## Examples
    
    Getting system information:
    
    ```ocaml
    open Std
    
    (* Get environment information *)
    let os = System.os_type in
    let arch = System.arch in
    
    Log.info "Running on %s (%s)" os arch
    ```
    
    Working with exit codes:
    
    ```ocaml
    (* Exit with error code *)
    if not valid_config then
      System.exit 1
    
    (* Successful exit *)
    System.exit 0
    ```
    
    ## See Also
    
    - [Env] for environment variables
    - [Command] for running external processes
    - Full Kernel.System documentation for all available functions
*)

include module type of Kernel.System
