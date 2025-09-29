(** Mutable cell types for interior mutability *)

type 'a cell = { mutable value: 'a }
type 'a t = 'a cell

(* Creation *)
let create value = { value }

(* Reading *)
let get cell = cell.value

(* Writing *)
let set cell x = cell.value <- x

(* Updating *)
let update cell f = cell.value <- f cell.value

let replace cell new_value =
  let old_value = cell.value in
  cell.value <- new_value;
  old_value

(* Taking - useful for option/result types *)
let take cell ~default =
  let old_value = cell.value in
  cell.value <- default;
  old_value

(* Swapping *)
let swap cell1 cell2 =
  let temp = cell1.value in
  cell1.value <- cell2.value;
  cell2.value <- temp

(* Comparison *)
let compare_and_swap cell expected new_value =
  if cell.value = expected then (
    cell.value <- new_value;
    true
  ) else
    false

let equal cell1 cell2 = cell1.value = cell2.value

(** OnceCell - a cell that can only be set once *)
module OnceCell = struct
  type 'a t = 'a option cell

  let create () = create None

  let get cell = get cell

  let take cell =
    let v = get cell in
    set cell None;
    v

  let get_or_init cell f =
    match get cell with
    | Some v -> v
    | None ->
        let v = f () in
        set cell (Some v);
        v

  let get_or_try_init cell f =
    match get cell with
    | Some v -> Ok v
    | None ->
        (match f () with
         | Ok v ->
             set cell (Some v);
             Ok v
         | Error _ as e -> e)

  let set cell value =
    match get cell with
    | None ->
        set cell (Some value);
        Ok ()
    | Some _ ->
        Error `AlreadyInitialized

  let is_initialized cell =
    match get cell with
    | Some _ -> true
    | None -> false

end

(** LazyCell - a cell that lazily initializes on first access *)
module LazyCell = struct
  type 'a t = {
    storage: 'a option cell;
    init: unit -> 'a;
  }

  let create init =
    { storage = create None; init }

  let force lazy_cell =
    match get lazy_cell.storage with
    | Some v -> v
    | None ->
        let v = lazy_cell.init () in
        set lazy_cell.storage (Some v);
        v

  let is_initialized lazy_cell =
    match get lazy_cell.storage with
    | Some _ -> true
    | None -> false

  let take lazy_cell =
    let v = get lazy_cell.storage in
    set lazy_cell.storage None;
    v

  let get = force

end

(** RefCell - a cell with runtime borrow checking *)
module RefCell = struct
  type borrow_state =
    | Available
    | Borrowed of int  (* count of immutable borrows *)
    | BorrowedMut      (* exclusive mutable borrow *)

  type 'a t = {
    mutable value: 'a;
    mutable state: borrow_state;
  }

  exception BorrowError of string
  exception BorrowMutError of string

  let create value =
    { value; state = Available }

  (* Immutable borrow *)
  type 'a borrow = 'a t * 'a

  let borrow cell =
    match cell.state with
    | Available ->
        cell.state <- Borrowed 1;
        (cell, cell.value)
    | Borrowed n ->
        cell.state <- Borrowed (n + 1);
        (cell, cell.value)
    | BorrowedMut ->
        raise (BorrowError "Cannot borrow while mutably borrowed")

  let release_borrow (cell, _) =
    match cell.state with
    | Borrowed 1 -> cell.state <- Available
    | Borrowed n -> cell.state <- Borrowed (n - 1)
    | _ -> () (* Should not happen *)

  (* Mutable borrow *)
  type 'a borrow_mut = 'a t

  let borrow_mut cell =
    match cell.state with
    | Available ->
        cell.state <- BorrowedMut;
        cell
    | Borrowed _ ->
        raise (BorrowMutError "Cannot mutably borrow while borrowed")
    | BorrowedMut ->
        raise (BorrowMutError "Already mutably borrowed")

  let get_mut cell =
    if cell.state <> BorrowedMut then
      raise (BorrowMutError "Not mutably borrowed");
    cell.value

  let set_mut cell value =
    if cell.state <> BorrowedMut then
      raise (BorrowMutError "Not mutably borrowed");
    cell.value <- value

  let release_borrow_mut cell =
    if cell.state = BorrowedMut then
      cell.state <- Available

  (* Safe accessors with automatic borrow management *)
  let with_borrow cell f =
    let b = borrow cell in
    let result = f (snd b) in
    release_borrow b;
    result

  let with_borrow_mut cell f =
    let b = borrow_mut cell in
    let result = f (fun () -> get_mut b) (fun v -> set_mut b v) in
    release_borrow_mut b;
    result

  (* Try variants that return Result instead of raising *)
  let try_borrow cell =
    try Ok (borrow cell)
    with BorrowError msg -> Error msg

  let try_borrow_mut cell =
    try Ok (borrow_mut cell)
    with BorrowMutError msg -> Error msg

  (* Direct access (unsafe - bypasses borrow checking) *)
  let get_unchecked cell = cell.value
  let set_unchecked cell value = cell.value <- value

  (* Query borrow state *)
  let is_borrowed cell =
    match cell.state with
    | Available -> false
    | _ -> true

  let borrow_count cell =
    match cell.state with
    | Available -> 0
    | Borrowed n -> n
    | BorrowedMut -> 1
end
