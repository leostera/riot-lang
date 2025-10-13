(** Non-blocking UTF-8 input reading from stdin.

    This module provides functionality to read UTF-8 encoded input from stdin in
    a non-blocking manner. It handles terminal mode configuration for raw input
    and proper UTF-8 character boundary detection.

    {1 Example: Reading User Input}

    {[
      open Std
      open Tty

      let rec read_loop () =
        match Stdin.read_utf8 () with
        | `Read s ->
            println (format "Read: %S" s);
            if s = "q" then println "Goodbye!" else read_loop ()
        | `End -> println "End of input"
        | `Malformed reason ->
            println (format "Malformed UTF-8: %s" reason);
            read_loop ()
        | `Retry ->
            (* No data available yet, yield and try again *)
            read_loop ()

      let () =
        let old_settings = Stdin.setup () in
        read_loop ();
        Stdin.shutdown old_settings
    ]}

    {1 Example: Interactive Prompt}

    {[
      let wait_for_key () =
        let old_settings = Stdin.setup () in
        print "Press any key to continue...";

        let rec wait () =
          match Stdin.read_utf8 () with
          | `Read _ -> ()
          | `Retry -> wait ()
          | _ -> ()
        in
        wait ();
        Stdin.shutdown old_settings
    ]} *)

val read_utf8 :
  unit -> [> `Retry | `End | `Malformed of string | `Read of string ]
(** [read_utf8 ()] performs a non-blocking read from stdin and returns one of:
    - [`Read s] - Successfully read a UTF-8 string [s]
    - [`Retry] - No data available, try again later
    - [`End] - End of input stream
    - [`Malformed reason] - Invalid UTF-8 sequence with error description

    This function should be called after {!setup} has configured stdin for raw
    non-blocking mode. It properly handles UTF-8 multi-byte sequences by
    detecting character boundaries. *)

val setup : unit -> Unix.terminal_io
(** [setup ()] configures stdin for non-blocking raw input mode.

    Returns the original terminal settings which must be passed to {!shutdown}
    to restore normal terminal behavior. In raw mode:
    - Input is available immediately without waiting for newline
    - Control characters are not interpreted (no Ctrl+C handling, etc.)
    - Echo is disabled

    Always call {!shutdown} before program exit to restore the terminal. *)

val shutdown : Unix.terminal_io -> unit
(** [shutdown settings] restores the terminal to its original configuration.

    [settings] must be the value returned by {!setup}. This should always be
    called before program exit, even in error cases, to prevent leaving the
    terminal in an unusable state. *)
