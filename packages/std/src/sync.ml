open Kernel
module Runtime_atomic = Atomic
module Runtime_mutex = Mutex
module Runtime_condition = Condition

module Atomic = struct
  include Runtime_atomic
end

module Cell = struct
  type 'a t = {
    mutable value: 'a;
  }

  let create = fun value -> { value }

  let get = fun cell -> cell.value

  let ( ! ) = get

  let set = fun cell value -> cell.value <- value

  let ( := ) = set

  let update = fun cell f -> cell.value <- f cell.value

  let incr = fun cell -> cell.value <- cell.value + 1

  let decr = fun cell -> cell.value <- cell.value - 1

  let replace = fun cell new_value ->
    let old_value = cell.value in
    cell.value <- new_value;
    old_value

  let take = fun cell ~default ->
    let old_value = cell.value in
    cell.value <- default;
    old_value

  let swap = fun left right ->
    let temp = left.value in
    left.value <- right.value;
    right.value <- temp

  let compare_and_swap = fun cell expected new_value ->
    if cell.value = expected then
      (
        cell.value <- new_value;
        true
      )
    else
      false

  let equal = fun left right -> left.value = right.value
end

module Mutex = struct
  include Runtime_mutex
end

module Condition = struct
  type t = Runtime_condition.t

  let create = Runtime_condition.create

  let wait = Runtime_condition.wait

  let signal = Runtime_condition.signal

  let broadcast = Runtime_condition.broadcast
end

module OnceCell = struct
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
end

module LazyCell = struct
  type 'a t = {
    storage: 'a option Cell.t;
    init: unit -> 'a;
  }

  let create = fun init -> { storage = Cell.create None; init }

  let force = fun lazy_cell ->
    match Cell.get lazy_cell.storage with
    | Some value -> value
    | None ->
        let value = lazy_cell.init () in
        Cell.set lazy_cell.storage (Some value);
        value

  let is_initialized = fun lazy_cell ->
    match Cell.get lazy_cell.storage with
    | Some _ -> true
    | None -> false

  let take = fun lazy_cell ->
    let value = Cell.get lazy_cell.storage in
    Cell.set lazy_cell.storage None;
    value

  let get = force
end

module RefCell = struct
  type borrow_state =
    | Available
    | Borrowed of int
    | BorrowedMut

  type 'a t = {
    mutable value: 'a;
    mutable state: borrow_state;
  }

  exception BorrowError of string

  exception BorrowMutError of string

  let create = fun value -> { value; state = Available }

  type 'a borrow = 'a t * 'a

  let borrow = fun cell ->
    match cell.state with
    | Available ->
        cell.state <- Borrowed 1;
        (cell, cell.value)
    | Borrowed count ->
        cell.state <- Borrowed (count + 1);
        (cell, cell.value)
    | BorrowedMut ->
        raise (BorrowError "Cannot borrow while mutably borrowed")

  let release_borrow = fun (cell, _) ->
    match cell.state with
    | Borrowed 1 -> cell.state <- Available
    | Borrowed count -> cell.state <- Borrowed (count - 1)
    | _ -> ()

  type 'a borrow_mut = 'a t

  let borrow_mut = fun cell ->
    match cell.state with
    | Available ->
        cell.state <- BorrowedMut;
        cell
    | Borrowed _ ->
        raise (BorrowMutError "Cannot mutably borrow while borrowed")
    | BorrowedMut ->
        raise (BorrowMutError "Already mutably borrowed")

  let get_mut = fun cell ->
    match cell.state with
    | BorrowedMut -> cell.value
    | _ -> raise (BorrowMutError "Not mutably borrowed")

  let set_mut = fun cell value ->
    match cell.state with
    | BorrowedMut -> cell.value <- value
    | _ -> raise (BorrowMutError "Not mutably borrowed")

  let release_borrow_mut = fun cell ->
    match cell.state with
    | BorrowedMut -> cell.state <- Available
    | _ -> ()

  let with_borrow = fun cell f ->
    let borrow = borrow cell in
    let _, value = borrow in
    let result = f value in
    release_borrow borrow;
    result

  let with_borrow_mut = fun cell f ->
    let borrow = borrow_mut cell in
    let result =
      f (fun () -> get_mut borrow) (fun value -> set_mut borrow value)
    in
    release_borrow_mut borrow;
    result

  let try_borrow = fun cell ->
    try Ok (borrow cell) with
    | BorrowError message -> Error message

  let try_borrow_mut = fun cell ->
    try Ok (borrow_mut cell) with
    | BorrowMutError message -> Error message

  let get_unchecked = fun cell -> cell.value

  let set_unchecked = fun cell value -> cell.value <- value

  let is_borrowed = fun cell ->
    match cell.state with
    | Available -> false
    | _ -> true

  let borrow_count = fun cell ->
    match cell.state with
    | Available -> 0
    | Borrowed count -> count
    | BorrowedMut -> 1
end
