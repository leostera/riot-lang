open Std

let test_order_predicates = fun _ctx ->
  if Order.is_lt Order.LT
  && not (Order.is_lt Order.EQ)
  && not (Order.is_lt Order.GT)
  && Order.is_lte Order.LT
  && Order.is_lte Order.EQ
  && not (Order.is_lte Order.GT)
  && not (Order.is_eq Order.LT)
  && Order.is_eq Order.EQ
  && not (Order.is_eq Order.GT)
  && not (Order.is_gte Order.LT)
  && Order.is_gte Order.EQ
  && Order.is_gte Order.GT
  && not (Order.is_gt Order.LT)
  && not (Order.is_gt Order.EQ)
  && Order.is_gt Order.GT then
    Ok ()
  else
    Error "expected Order predicate helpers to match LT/EQ/GT ordering"

let test_order_predicates_compose_with_compare = fun _ctx ->
  if
    Order.is_lt (Int.compare 1 2)
    && Order.is_lte (Int.compare 2 2)
    && Order.is_eq (Int.compare 2 2)
    && Order.is_gte (Int.compare 3 2)
    && Order.is_gt (Int.compare 3 2)
  then
    Ok ()
  else
    Error "expected Order predicate helpers to compose with compare results"

let tests =
  Test.[
    case "Order predicates classify LT/EQ/GT" test_order_predicates;
    case "Order predicates compose with compare results" test_order_predicates_compose_with_compare;
  ]

let main ~args = Test.Cli.main ~name:"std_order_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
