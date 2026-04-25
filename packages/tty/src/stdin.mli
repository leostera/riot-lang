(**
   Non-blocking UTF-8 input reading from stdin.

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
   ]} 
*)
val read_utf8: unit -> [> `Retry | `End | `Malformed of string | `Read of string]

(**
   [read_utf8 ()] performs a non-blocking read from stdin and returns one of:
   - [`Read s] - Successfully read a UTF-8 string [s]
   - [`Retry] - No data available, try again later
   - [`End] - End of input stream
   - [`Malformed reason] - Invalid UTF-8 sequence with error description

   This function should be called after {!make_raw} has configured stdin for raw
   non-blocking mode. It properly handles UTF-8 multi-byte sequences by
   detecting character boundaries. 
*)
val make_raw: unit -> Terminal.t

(**
   [make_raw ()] configures the terminal (via /dev/tty) for immediate raw input.

   Returns a tuple of (tty_fd, original_settings) which must be passed to {!restore}
   to restore normal terminal behavior. In raw mode:
   - Input is available immediately without waiting for newline (no canonical mode)
   - Echo is disabled (typed characters don't appear on screen)
   - CR/NL mapping is disabled (carriage return keys work correctly)

   This uses a minimal termios configuration (only 3 flags changed) proven to work
   reliably across all terminals. The implementation follows notcurses' approach,
   preserving the terminal's existing configuration for output processing, which
   is critical for ANSI escape sequence rendering.

   The function opens /dev/tty directly to ensure terminal settings apply to both
   stdin and stdout. Always call {!restore} before program exit to restore the terminal.

   {b Implementation note:} Despite the name "raw mode", this is technically "cbreak mode".
   It disables canonical input and echo but preserves output processing and the
   terminal's character configuration, which is what TUI applications need. 
*)
val restore: Terminal.t -> unit(**
   [restore (tty_fd, settings)] restores the terminal to its original configuration
   and closes the tty file descriptor.

   The argument must be the tuple returned by {!make_raw}. This should always be
   called before program exit, even in error cases, to prevent leaving the
   terminal in an unusable state. 
*)
