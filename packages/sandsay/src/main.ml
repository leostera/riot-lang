open Std

let sand_creature = {|
    \
     \
      \
         __
       .'  '.
      /      \
     |  o  o  |
     |   <>   |
      \ '__' /
       '.__.'
      ___|___
     / . | . \
    /___|___|_\
|}

let make_bubble text =
  let lines = String.split_on_char '\n' text in
  let max_len = List.fold_left (fun acc line ->
    max acc (String.length line)
  ) 0 lines in

  let top = " " ^ String.make (max_len + 2) '_' in
  let bottom = " " ^ String.make (max_len + 2) '-' in

  let format_line line =
    let padding = max_len - String.length line in
    format "| %s%s |" line (String.make padding ' ')
  in

  let middle = List.map format_line lines in

  String.concat "\n" ([top] @ middle @ [bottom])

let print_sandsay message =
  let bubble = make_bubble message in
  print_endline bubble;
  print_endline sand_creature

let () =
  let args = Array.to_list Sys.argv in
  match List.tl args with
  | [] -> print_sandsay "Hello from the sand!"
  | message_parts ->
      let message = String.concat " " message_parts in
      print_sandsay message
