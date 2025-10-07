(** # Global - Commonly used utility functions
    
    Global utility functions available throughout the Std library.
    Includes formatting, printing, and development helpers.
    
    ## Examples
    
    Printing output:
    
    ```ocaml
    open Std
    
    print "Hello, %s!" "world"
    (* Prints: Hello, world! *)
    
    println "Value: %d" 42
    (* Prints: Value: 42\n *)
    ```
    
    String formatting:
    
    ```ocaml
    let msg = format "Error at line %d: %s" 42 "syntax error" in
    (* msg = "Error at line 42: syntax error" *)
    ```
    
    Development helpers:
    
    ```ocaml
    let implement_later () =
      todo "Need to implement caching"
    
    let not_ready () =
      unimplemented ()
    ```
    
    Mutable cells:
    
    ```ocaml
    let counter = cell 0 in
    Cell.set counter 1;
    Cell.get counter  (* 1 *)
    ```
*)

val panic : string -> 'a
(** Raises a panic exception with the given message. Program terminates
    unless caught.
    
    ## Examples
    
    ```ocaml
    if config_missing then
      panic "Configuration file not found"
    ```
    
    ## Use Cases
    
    - Irrecoverable errors
    - Invariant violations
    - Programmer errors (use assertions instead when possible)
*)

val cell : 'a -> 'a Cell.t
(** Creates a mutable cell containing the given value.
    
    ## Examples
    
    ```ocaml
    let counter = cell 0 in
    Cell.update counter (fun n -> n + 1);
    Cell.get counter  (* 1 *)
    ```
    
    ## See Also
    
    - [Cell] for full cell API
*)

val format : ('a, unit, string, string) format4 -> 'a
(** Formats a string using Printf-style formatting.
    
    ## Examples
    
    ```ocaml
    let msg = format "Hello, %s! You have %d messages." "Alice" 5 in
    (* "Hello, Alice! You have 5 messages." *)
    ```
*)

val print : ('a, unit, string, unit) format4 -> 'a
(** Prints to stdout with immediate flush (no newline).
    
    ## Examples
    
    ```ocaml
    print "Processing..."
    (* Output: Processing... *)
    ```
*)

val println : ('a, unit, string, unit) format4 -> 'a
(** Prints to stdout with newline and immediate flush.
    
    ## Examples
    
    ```ocaml
    println "Operation complete"
    (* Output: Operation complete\n *)
    ```
*)

val todo : string -> 'a
(** Marks code as TODO, panicking with the given message when called.
    
    ## Examples
    
    ```ocaml
    let cache_lookup key =
      todo "Implement Redis caching"
    ```
    
    ## Use Cases
    
    - Placeholder for future implementation
    - Self-documenting incomplete code
    - Fails fast if accidentally called
*)

val unimplemented : unit -> 'a
(** Marks code as unimplemented, panicking when called.
    
    ## Examples
    
    ```ocaml
    let complex_algorithm () =
      unimplemented ()
    ```
*)
