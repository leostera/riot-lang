open Std

let test_is_ok_and_is_err = fun _ctx ->
  if Result.is_ok (Ok 1) && Result.is_err (Error "boom") then
    Ok ()
  else Error "expected Result.is_ok/is_err to reflect constructors"

let test_map = fun _ctx ->
  match Result.map (Ok 5) ~fn:(
    fun value -> value * 2
  ) with
  | Ok 10 -> Ok ()
  | _ -> Error "expected Result.map (Ok 5) -> Ok 10"

let test_map_err = fun _ctx ->
  match Result.map_err (Error 5) ~fn:(
    fun value -> value + 1
  ) with
  | Error 6 -> Ok ()
  | _ -> Error "expected Result.map_err to transform the error branch"

let test_map_or = fun _ctx ->
  if Int.equal (Result.map_or (Error "x") ~default:7 ~fn:(
    fun value -> value * 2
  )) 7 then
    Ok ()
  else Error "expected Result.map_or Error to return default"

let test_map_or_else = fun _ctx ->
  if Int.equal (Result.map_or_else (Error "boom") ~default:String.length ~fn:(
    fun value -> value * 2
  )) 4 then
    Ok ()
  else Error "expected Result.map_or_else Error to use default"

let test_and_then = fun _ctx ->
  match Result.and_then (Ok 5) ~fn:(
    fun value -> Ok (value + 3)
  ) with
  | Ok 8 -> Ok ()
  | _ -> Error "expected Result.and_then to chain Ok values"

let test_or_else = fun _ctx ->
  match Result.or_else (Error "boom") ~fn:(
    fun err -> Ok (String.length err)
  ) with
  | Ok 4 -> Ok ()
  | _ -> Error "expected Result.or_else Error to recover"

let test_unwrap_or = fun _ctx ->
  if Int.equal (Result.unwrap_or (Error "boom") ~default:12) 12 then
    Ok ()
  else Error "expected Result.unwrap_or Error to return default"

let test_ok_value = fun _ctx ->
  match Result.ok_value (Ok "value") with
  | Some "value" -> Ok ()
  | _ -> Error "expected ok_value Ok x = Some x"

let test_err_value = fun _ctx ->
  match Result.err_value (Error "boom") with
  | Some "boom" -> Ok ()
  | _ -> Error "expected err_value Error x = Some x"

let test_to_option = fun _ctx ->
  match Result.to_option (Ok 99) with
  | Some 99 -> Ok ()
  | _ -> Error "expected Result.to_option Ok x = Some x"

let test_transpose = fun _ctx ->
  match Result.transpose (Ok (Some 3)) with
  | Some (Ok 3) -> Ok ()
  | _ -> Error "expected Result.transpose Ok (Some x) = Some (Ok x)"

let test_inspect = fun _ctx ->
  let seen = Sync.Atomic.make 0 in
  let actual =
    Result.inspect
      (
        fun value -> Sync.Atomic.set seen value
      )
      (Ok 14)
  in
  if actual = Ok 14 && Int.equal (Sync.Atomic.get seen) 14 then
    Ok ()
  else Error "expected inspect to run callback and preserve original result"

let test_iter_err = fun _ctx ->
  let seen = Sync.Atomic.make false in
  Result.iter_err (Error "boom") ~fn:(
    fun _ -> Sync.Atomic.set seen true
  );
  if Sync.Atomic.get seen then
    Ok ()
  else Error "expected iter_err to run on Error"

let tests = Test.[
  case "Result.is_ok and Result.is_err reflect constructors" test_is_ok_and_is_err;
  case "Result.map transforms Ok" test_map;
  case "Result.map_err transforms Error" test_map_err;
  case "Result.map_or returns default for Error" test_map_or;
  case "Result.map_or_else uses the default branch for Error" test_map_or_else;
  case "Result.and_then chains Ok computations" test_and_then;
  case "Result.or_else can recover from Error" test_or_else;
  case "Result.unwrap_or returns provided default" test_unwrap_or;
  case "Result.ok_value extracts Ok branch" test_ok_value;
  case "Result.err_value extracts Error branch" test_err_value;
  case "Result.to_option converts Ok to Some" test_to_option;
  case "Result.transpose flips Result Option" test_transpose;
  case "Result.inspect runs callback on Ok" test_inspect;
  case "Result.iter_err runs callback on Error" test_iter_err;
]

let main ~args = Test.Cli.main ~name:"result" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
