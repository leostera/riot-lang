open Kernel

let protect = fun ~finally f ->
  match f () with
  | value ->
      finally ();
      value
  | exception error ->
      finally ();
      raise error

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
      cell.state <- Borrowed (Int.succ count);
      (cell, cell.value)
  | BorrowedMut ->
      raise (BorrowError "Cannot borrow while mutably borrowed")

let release_borrow = fun (cell, _) ->
  match cell.state with
  | Borrowed 1 -> cell.state <- Available
  | Borrowed count -> cell.state <- Borrowed (Int.pred count)
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
  protect ~finally:(fun () -> release_borrow borrow)
    (fun () ->
      let _, value = borrow in
      f value)

let with_borrow_mut = fun cell f ->
  let borrow = borrow_mut cell in
  protect
    ~finally:(fun () -> release_borrow_mut borrow)
    (fun () -> f (fun () -> get_mut borrow) (fun value -> set_mut borrow value))

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
