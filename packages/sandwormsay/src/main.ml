open Std

let sandworm = {|
                    /\_/\
                   ( o.o )
                    > ^ <
         ___________________
        /                   \
       /  BLESS THE MAKER   \
      /   AND HIS WATER      \
     /    BLESS THE COMING   \
    /     AND GOING OF HIM    \
   /   MAY HIS PASSAGE CLEANSE \
  /         THE WORLD          \
 /_____________________________\
        |               |
        |   ~~~~~~~~    |
        |  ~~~~~~~~~~   |
        | ~~~~~~~~~~~~  |
        |~~~~~~~~~~~~~~|
        |~~~~~~~~~~~~~~|
       /~~~~~~~~~~~~~~~~\
      /~~~~~~~~~~~~~~~~~~\
     |~~~~~~~~~~~~~~~~~~~~|
     |~~~~~~~~~~~~~~~~~~~~|
      \~~~~~~~~~~~~~~~~~~/
       \~~~~~~~~~~~~~~~~/
        |~~~~~~~~~~~~~~|
        |~~~~~~~~~~~~~~|
        |~~~~~~~~~~~~~~|
         \~~~~~~~~~~~~/
          \~~~~~~~~~~/
           \~~~~~~~~/
            |~~~~~~|
            |~~~~~~|
            |~~~~~~|
             \~~~~/
              \~~/
               \/
|}

let make_bubble text max_width =
  let lines = String.split_on_char '\n' text in
  let lines = List.filter (fun s -> String.length s > 0) lines in

  let wrap_line line max_width =
    let words = String.split_on_char ' ' line in
    let rec build_lines current_line current_len acc = function
      | [] ->
          let acc = if String.length current_line > 0 then current_line :: acc else acc in
          List.rev acc
      | word :: rest ->
          let word_len = String.length word in
          if current_len = 0 then
            build_lines word word_len acc rest
          else if current_len + 1 + word_len <= max_width then
            build_lines (current_line ^ " " ^ word) (current_len + 1 + word_len) acc rest
          else
            build_lines word word_len (current_line :: acc) rest
    in
    build_lines "" 0 [] words
  in

  let wrapped_lines = List.concat_map (fun line -> wrap_line line max_width) lines in
  let max_len = List.fold_left (fun m line -> max m (String.length line)) 0 wrapped_lines in

  let top = " " ^ String.make (max_len + 2) '_' in
  let bottom = " " ^ String.make (max_len + 2) '-' in

  let format_line line =
    let padding = max_len - String.length line in
    "| " ^ line ^ String.make padding ' ' ^ " |"
  in

  let middle = List.map format_line wrapped_lines in

  String.concat "\n" (top :: middle @ [bottom])

let () =
  let args = System.get_args () in
  let message =
    match args with
    | [] -> "Shai-Hulud!"
    | _ -> String.concat " " args
  in

  let max_width = 40 in
  let bubble = make_bubble message max_width in

  print_endline bubble;
  print_endline sandworm
