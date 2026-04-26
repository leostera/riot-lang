open Kernel

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
