let first = 1
let second = 2

let if_then_else_simple =
  if ready then 1 else 0

let if_no_else =
  if ready then log "ready"

let if_nested =
  if outer then if inner then one else two else three

let let_in_simple =
  let x = 1 in
  x + 2

let let_in_nested =
  let x = 1 in
  let y = x + 1 in
  x + y

let let_and_bindings =
  let left = compute_left ()
  and right = compute_right () in
  left + right

let let_rec_in =
  let rec loop n =
    if n <= 0 then 0 else n + loop (n - 1)
  in
  loop count

let sequence_two =
  log "start";
  log "done"

let sequence_with_let =
  log "before";
  let value = compute () in
  value

let sequence_with_if =
  log "before";
  if ready then use value else fallback ()

let sequence_with_match =
  log "before";
  match value with
  | Some x -> x
  | None -> 0

let sequence_in_fun =
  fun x ->
    log_value x;
    x + 1

let begin_sequence =
  begin
    log "begin";
    log "end"
  end
