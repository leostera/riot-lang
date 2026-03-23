let test1 () =
  print "a";
  print "b";
  print "c"

let test2 x =
  match x with
  | Ok v ->
      set cell v;
      Ok v
  | Error e -> e
