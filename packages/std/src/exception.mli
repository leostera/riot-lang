(** # Exception - Exception handling utilities
    
    Exception handling utilities for working with OCaml exceptions.
    
    ## Examples
    
    Handling exceptions:
    
    ```ocaml
    open Std
    
    try
      risky_operation ()
    with
    | exn ->
        Log.error "Unexpected error: %s" (Exception.to_string exn)
    ```
    
    ## Note
    
    In Std, prefer using [Result] types over exceptions for error handling.
    Exceptions should be reserved for truly exceptional circumstances like
    programmer errors or unrecoverable failures.
    
    ## See Also
    
    - [Result] for functional error handling
    - [Option] for representing absence of values
*)

include module type of Kernel.Exception
