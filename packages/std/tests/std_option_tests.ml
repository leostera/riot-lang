open Std

let test_some = fun _ctx ->
  match Option.some 5 with
  | Some 5 -> Ok ()
  | _ -> Error "expected Option.some 5 = Some 5"

let test_none = fun _ctx ->
  match (Option.none: int option) with
  | None -> Ok ()
  | Some _ -> Error "expected Option.none = None"

let test_equal = fun _ctx ->
  if Option.equal (Some 7) (Some 7) ~fn:Int.equal && not (Option.equal (Some 7) None ~fn:Int.equal) then Ok ()
  else Error "expected Option.equal to compare Some/None correctly"

let test_is_some_and_is_none = fun _ctx ->
  if Option.is_some (Some 1) && Option.is_none None then Ok ()
  else Error "expected is_some/is_none to reflect constructors"

let test_is_some_and = fun _ctx ->
  if Option.is_some_and (Some 4) ~fn:(fun value -> value > 0) then Ok ()
  else Error "expected is_some_and to run predicate on Some"

let test_map = fun _ctx ->
  match Option.map (Some 5) ~fn:(fun value -> value * 2) with
  | Some 10 -> Ok ()
  | _ -> Error "expected map Some 5 -> Some 10"

let test_map_or = fun _ctx ->
  if Int.equal (Option.map_or None ~default:42 ~fn:(fun value -> value * 2)) 42 then Ok ()
  else Error "expected map_or on None to return default"

let test_map_or_else = fun _ctx ->
  if Int.equal (Option.map_or_else (Some 5) ~default:(fun () -> 0) ~fn:(fun value -> value + 1)) 6 then Ok ()
  else Error "expected map_or_else on Some to run fn"

let test_and_then = fun _ctx ->
  match Option.and_then (Some 3) ~fn:(fun value -> Some (value * value)) with
  | Some 9 -> Ok ()
  | _ -> Error "expected and_then to chain Some values"

let test_or_else = fun _ctx ->
  match Option.or_else None ~fn:(fun () -> Some "fallback") with
  | Some "fallback" -> Ok ()
  | _ -> Error "expected or_else None to use fallback"

let test_xor = fun _ctx ->
  match Option.xor (Some 1) None with
  | Some 1 -> Ok ()
  | _ -> Error "expected xor Some None = Some"

let test_unwrap_or = fun _ctx ->
  if Int.equal (Option.unwrap_or None ~default:9) 9 then Ok ()
  else Error "expected unwrap_or None to return default"

let test_ok_or = fun _ctx ->
  match Option.ok_or None ~error:"missing" with
  | Error "missing" -> Ok ()
  | _ -> Error "expected ok_or None to produce Error"

let test_to_list = fun _ctx ->
  if Option.to_list (Some 4) = [ 4 ] && Option.to_list None = [] then Ok ()
  else Error "expected to_list to convert Some/None to singleton/empty lists"

let test_transpose = fun _ctx ->
  match Option.transpose (Some (Ok 7)) with
  | Ok (Some 7) -> Ok ()
  | _ -> Error "expected transpose Some (Ok x) = Ok (Some x)"

let test_filter = fun _ctx ->
  match Option.filter (Some 8) ~fn:(fun value -> value mod 2 = 0) with
  | Some 8 -> Ok ()
  | _ -> Error "expected filter to keep matching Some values"

let test_zip = fun _ctx ->
  match Option.zip (Some "a") (Some 1) with
  | Some ("a", 1) -> Ok ()
  | _ -> Error "expected zip on two Somes to produce Some pair"

let test_all = fun _ctx ->
  match Option.all [ Some 1; Some 2; Some 3 ] with
  | Some [ 1; 2; 3 ] -> Ok ()
  | _ -> Error "expected Option.all to collect all Somes"

let tests =
  Test.[
    case "Option.some wraps values in Some" test_some;
    case "Option.none is None" test_none;
    case "Option.equal compares Some and None" test_equal;
    case "Option.is_some and Option.is_none reflect constructors" test_is_some_and_is_none;
    case "Option.is_some_and runs predicate on Some" test_is_some_and;
    case "Option.map transforms Some values" test_map;
    case "Option.map_or returns default on None" test_map_or;
    case "Option.map_or_else runs mapping function on Some" test_map_or_else;
    case "Option.and_then chains optional computations" test_and_then;
    case "Option.or_else uses fallback for None" test_or_else;
    case "Option.xor keeps exactly one Some" test_xor;
    case "Option.unwrap_or returns provided default" test_unwrap_or;
    case "Option.ok_or converts None to Error" test_ok_or;
    case "Option.to_list converts Some and None to lists" test_to_list;
    case "Option.transpose flips Some Result to Result Option" test_transpose;
    case "Option.filter keeps matching values" test_filter;
    case "Option.zip combines two Somes" test_zip;
    case "Option.all collects all present values" test_all;
  ]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"option" ~tests ~args) ~args:Env.args ()
