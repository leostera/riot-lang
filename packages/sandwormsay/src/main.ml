open Std

let sandworm =
  {|
    __________________
   /                  \
  /  O    O    O    O  \
 |  ____________________  |
 | |                    | |
 | |  THE SPICE MUST   | |
 | |      FLOW         | |
 | |____________________| |
  \________________________/
       |||||||||||||||
      |||||||||||||||
     |||||||||||||||
    |||||||||||||||
   |||||||||||||||
  |||||||||||||||
 |||||||||||||||
|||||||||||||||
 |||||||||||||||
  |||||||||||||||
   |||||||||||||||
    |||||||||||||||
     |||||||||||||||
      ~~~~~~~~~~~~~
|}

let wrap_message msg =
  let lines = String.split_on_char '\n' msg in
  let max_len =
    List.fold_left (fun acc line -> max acc (String.length line)) 0 lines
  in
  let border_top = " " ^ String.make (max_len + 2) '_' in
  let border_bottom = " " ^ String.make (max_len + 2) '-' in

  let format_line line =
    let padding = max_len - String.length line in
    "| " ^ line ^ String.make padding ' ' ^ " |"
  in

  let formatted_lines = List.map format_line lines in
  String.concat "\n" ((border_top :: formatted_lines) @ [ border_bottom ])

let sandworm_with_message msg =
  let bubble = wrap_message msg in
  let worm_lines = String.split_on_char '\n' sandworm in

  bubble ^ "\n" ^ String.concat "\n" ("      \\" :: List.tl worm_lines)

let main () =
  let args = Sys.argv in
  let message =
    if Array.length args > 1 then
      String.concat " " (List.tl (Array.to_list args))
    else "Bless the Maker and His water. Bless the coming and going of Him."
  in

  print_endline (sandworm_with_message message)

let () = main ()
