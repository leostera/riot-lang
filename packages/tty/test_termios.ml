open Unix

let () =
  if isatty stdin then (
    let t = tcgetattr stdin in
    Printf.printf "ALL TERMIOS FIELDS:\n";
    Printf.printf "c_opost: %b (output processing)\n" t.c_opost;
    Printf.printf "c_icrnl: %b (map CR to NL on input)\n" t.c_icrnl;
    Printf.printf "c_ixon: %b (enable XON/XOFF flow control)\n" t.c_ixon;
  )
