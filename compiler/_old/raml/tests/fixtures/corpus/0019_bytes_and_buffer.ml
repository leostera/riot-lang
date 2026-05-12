(* Mutable bytes and buffer accumulation. *)
let () =
  let b = Bytes.from_string "abcde" in
  Bytes.set b 1 'X';
  Bytes.set b 2 'Y';
  Bytes.set b 3 'Z';
  let buf = Buffer.create 16 in
  Buffer.add_string buf (Bytes.to_string b);
  Buffer.add_char buf '!';
  print_endline (Buffer.contents buf)
