open Std

let () =
  Log.set_level Log.Info;
  Log.info "=== UUID v7 (timestamp-ordered) ===";
  let id1 = UUID.v7 () in
  let id2 = UUID.v7 () in
  Log.info ("ID1: " ^ UUID.to_string id1);
  Log.info ("ID2: " ^ UUID.to_string id2);
  let ordering =
    match UUID.compare id1 id2 with
    | Order.LT -> "LT"
    | Order.EQ -> "EQ"
    | Order.GT -> "GT"
  in
  Log.info ("Compare: " ^ ordering);
  Log.info "";
  Log.info "=== UUID v4 (random) ===";
  let rand1 = UUID.v4 () in
  let rand2 = UUID.v4 () in
  Log.info ("Random 1: " ^ UUID.to_string rand1);
  Log.info ("Random 2: " ^ UUID.to_string rand2);
  Log.info "";
  Log.info "✓ UUID generation works!"
