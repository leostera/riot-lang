open Std
open Std.Data

let read_file path =
  match Fs.read (Path.v path) with
  | Ok content -> content
  | Error _ -> failwith (format "Failed to read file: %s" path)

type spec_test = {
  markdown : string;
  html : string;
  example : int;
  section : string;
}

let load_spec_tests () =
  let content = read_file "packages/markdown/src/commonmark_v0.31.2.json" in
  match Json.of_string content with
  | Ok (Json.Array tests) ->
      List.filter_map
        (fun test ->
          match test with
          | Json.Object fields -> (
              let get_string key =
                Option.and_then (List.assoc_opt key fields) (function
                  | Json.String s -> Some s
                  | _ -> None)
              in
              let get_int key =
                Option.and_then (List.assoc_opt key fields) (function
                  | Json.Int n -> Some n
                  | Json.Float n -> Some (Int.of_float n)
                  | _ -> None)
              in
              match
                ( get_string "markdown",
                  get_string "html",
                  get_int "example",
                  get_string "section" )
              with
              | Some markdown, Some html, Some example, Some section ->
                  Some { markdown; html; example; section }
              | _ -> None)
          | _ -> None)
        tests
  | _ -> []

let spec_test test =
  Test.case (format "Example %d (%s)" test.example test.section) (fun () ->
      let tree = Markdown.parse test.markdown in
      let html_node = Markdown.compile tree in
      let actual =
        match html_node with
        | Html.Element { children; _ } ->
            String.concat "" (List.map Html.to_string children)
        | _ -> Html.to_string html_node
      in

      if actual = test.html then Ok ()
      else
        Error
          (format
             "Example %d:\n\
              Markdown:\n\
              %s\n\
              Expected HTML (len=%d):\n\
              %s\n\
              Actual HTML (len=%d):\n\
              %s\n"
             test.example test.markdown (String.length test.html) test.html
             (String.length actual) actual))

let () =
  Miniriot.run
    ~main:(fun ~args ->
      let spec_tests = load_spec_tests () in
      let tests = List.map spec_test spec_tests in
      Test.Cli.main ~name:"markdown" ~tests ~args ())
    ~args:Env.args
  |> exit
