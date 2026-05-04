open Std

let main ~args:_ =
  match Net.Uri.from_string "https://example.com/search?q=hello+world&filter=name%3DJohn" with
  | Ok uri ->
      (
        match Net.Uri.query uri with
        | Some query_str ->
            println ("Raw query string from Uri.query: '" ^ query_str ^ "'");
            let params = Net.Uri.Query.parse query_str in
            println ("Parsed params count: " ^ Int.to_string (List.length params));
            List.for_each params ~fn:(fun (k, v) -> println ("  " ^ k ^ " = '" ^ v ^ "'"))
        | None -> println "No query"
      );
      Ok ()
  | Error _ ->
      println "Failed to parse URI";
      Ok ()

let () = Runtime.run ~main ~args:Env.args ()
