(** A cell that can only be set once *)
open Global0

type 'a t = 'a option Cell.t

let create = fun () -> Cell.create None

let get = fun cell -> Cell.get cell

let take = fun cell ->
    let v = Cell.get cell in
    Cell.set cell None;
    v

let get_or_init = fun cell f ->
    match Cell.get cell with
    | Some v -> v
    | None ->
        let v = f () in
        Cell.set cell (Some v);
        v

let get_or_try_init = fun cell f ->
    match Cell.get cell with
    | Some v -> Ok v
    | None -> (
        match f () with
        | Ok v ->
            Cell.set cell (Some v);
            Ok v
        | Error _ as e -> e
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
