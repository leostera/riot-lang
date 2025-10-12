type 'a option = None | Some of 'a

let unwrap_or default opt = match opt with None -> default | Some x -> x
