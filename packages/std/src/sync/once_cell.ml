open Kernel

type 'a t = 'a option Cell.t

let create = fun () -> Cell.create None

let get = fun cell -> Cell.get cell

let take = fun cell ->
  let value = Cell.get cell in
  Cell.set cell None;
  value

let get_or_init = fun cell f ->
  match Cell.get cell with
  | Some value -> value
  | None ->
      let value = f () in
      Cell.set cell (Some value);
      value

let get_or_try_init = fun cell f ->
  match Cell.get cell with
  | Some value -> Ok value
  | None -> (
    match f () with
    | Ok value ->
        Cell.set cell (Some value);
        Ok value
    | Error _ as error -> error
  )

let set = fun cell value ->
  match Cell.get cell with
  | None ->
      Cell.set cell (Some value);
      Ok ()
  | Some _ -> Error `AlreadyInitialized

let is_initialized = fun cell ->
  match Cell.get cell with
  | Some _ -> true
  | None -> false
