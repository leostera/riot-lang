type ('k, 'v) t = ('k, 'v) Hashtbl.t
type ('k, 'v) entry = Occupied of 'v ref | Vacant

let create () = Hashtbl.create 16
let with_capacity capacity = Hashtbl.create capacity

let of_list pairs =
  let map = Hashtbl.create (List.length pairs) in
  List.iter (fun (k, v) -> Hashtbl.replace map k v) pairs;
  map

let insert map key value =
  let previous = try Some (Hashtbl.find map key) with Not_found -> None in
  Hashtbl.replace map key value;
  previous

let get map key = try Some (Hashtbl.find map key) with Not_found -> None

let remove map key =
  let previous = get map key in
  Hashtbl.remove map key;
  previous

let contains_key map key = Hashtbl.mem map key
let len map = Hashtbl.length map
let is_empty map = Hashtbl.length map = 0
let clear map = Hashtbl.clear map
let keys map = Hashtbl.fold (fun k _ acc -> k :: acc) map []
let values map = Hashtbl.fold (fun _ v acc -> v :: acc) map []
let iter f map = Hashtbl.iter f map
let fold f map acc = Hashtbl.fold f map acc
let to_list map = Hashtbl.fold (fun k v acc -> (k, v) :: acc) map []

let entry map key =
  try
    let value = Hashtbl.find map key in
    Occupied (ref value)
  with Not_found -> Vacant

let or_insert map key default =
  match get map key with
  | Some value -> value
  | None ->
      Hashtbl.replace map key default;
      default

let and_modify map key f =
  match get map key with
  | Some value ->
      let new_value = f value in
      Hashtbl.replace map key new_value
  | None -> ()
