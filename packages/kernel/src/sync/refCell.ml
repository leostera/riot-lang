(** A cell with runtime borrow checking *)

type borrow_state =
  | Available
  | Borrowed of int (* count of immutable borrows *)
  | BorrowedMut (* exclusive mutable borrow *)

type 'a t = { mutable value : 'a; mutable state : borrow_state }

exception BorrowError of string
exception BorrowMutError of string

let create value = { value; state = Available }

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
  | BorrowedMut -> raise (BorrowError "Cannot borrow while mutably borrowed")

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
  | BorrowedMut -> raise (BorrowMutError "Already mutably borrowed")

let get_mut cell =
  if cell.state <> BorrowedMut then
    raise (BorrowMutError "Not mutably borrowed");
  cell.value

let set_mut cell value =
  if cell.state <> BorrowedMut then
    raise (BorrowMutError "Not mutably borrowed");
  cell.value <- value

let release_borrow_mut cell =
  if cell.state = BorrowedMut then cell.state <- Available

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
let try_borrow cell = try Ok (borrow cell) with BorrowError msg -> Error msg

let try_borrow_mut cell =
  try Ok (borrow_mut cell) with BorrowMutError msg -> Error msg

(* Direct access (unsafe - bypasses borrow checking) *)
let get_unchecked cell = cell.value
let set_unchecked cell value = cell.value <- value

(* Query borrow state *)
let is_borrowed cell = match cell.state with Available -> false | _ -> true

let borrow_count cell =
  match cell.state with Available -> 0 | Borrowed n -> n | BorrowedMut -> 1