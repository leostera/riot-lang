open Std

let main ~args:_ =
  let test_uris = [
    "https://api.ipify.org?format=json";
    "https://leostera.com";
    "https://httpbin.org/get";
  ]
  in
  List.for_each
    test_uris
    ~fn:(fun uri_str ->
      println ("Testing: " ^ uri_str);
      match Net.Uri.from_string uri_str with
      | Ok uri ->
          println "  OK - Parsed successfully";
          println ("  scheme: " ^ (Option.unwrap_or ~default:"none" (Net.Uri.scheme uri)));
          println ("  host: " ^ (Option.unwrap_or ~default:"none" (Net.Uri.host uri)));
          println ("  path: " ^ (Net.Uri.path uri));
          println ("  query: " ^ (Option.unwrap_or ~default:"none" (Net.Uri.query uri)))
      | Error _ -> println "  ERROR - Parse failed");
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
