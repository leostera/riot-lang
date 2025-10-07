(** # Exception - Exception handling utilities
    
    Exception handling utilities and helpers. This is a re-export of
    [Kernel.Exception] providing common exception operations.
    
    ## Examples
    
    Handling exceptions:
    
    ```ocaml
    open Std
    
    try
      risky_operation ()
    with
    | Exception.Error msg ->
        Log.error "Operation failed: %s" msg
    | exn ->
        Log.error "Unexpected error: %s" (Exception.to_string exn)
    ```
    
    ## Note
    
    In Std, prefer using [Result] types over exceptions for error handling.
    Exceptions should be reserved for truly exceptional circumstances.
    
    ## See Also
    
    - [Result] for functional error handling
    - Full Kernel.Exception documentation for all available functions
*)

include module type of Kernel.Exception
