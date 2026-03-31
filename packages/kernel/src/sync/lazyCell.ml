(** A cell that lazily initializes on first access *)
type 'a t = {
  storage: 'a option Cell.t;
  init: unit -> 'a;
}

let create = fun init -> {storage = Cell.create None; init}

let force = fun lazy_cell ->
  match Cell.get lazy_cell.storage with
  | Some v -> v
  | None ->
      let v = lazy_cell.init () in
      Cell.set lazy_cell.storage (Some v);
      v

let is_initialized = fun lazy_cell ->
  match Cell.get lazy_cell.storage with
  | Some _ -> true
  | None -> false

let take = fun lazy_cell ->
  let v = Cell.get lazy_cell.storage in
  Cell.set lazy_cell.storage None;
  v

let get = force
