(* client.ml - depends on server and uses List, String *)
let process_hostname () =
  let host = Server.get_hostname () in
  String.uppercase_ascii host

let process_list items =
  List.map (fun x -> x * 2) items