open Std

let sandworm = {|
     ____________________
    /                    \
   |  %s
    \____________________/
            ||
            ||
         ___||___
        /~~~~~~~~~\
       |  O     O  |
       |     ^     |
        \  \___/  /
         \_______/
            | |
           /| |\
          | | | |
         /  | |  \
        |   | |   |
       /|   | |   |\
      | |   | |   | |
     /  |   | |   |  \
    |   |   | |   |   |
   /|   |   | |   |   |\
  | |   |   | |   |   | |
 /  |   |   | |   |   |  \
|   |   |   | |   |   |   |
|   |   |   | |   |   |   |
 \  |   |   | |   |   |  /
  \ |   |   | |   |   | /
   \|   |   | |   |   |/
    |   |   | |   |   |
     \  |   | |   |  /
      \ |   | |   | /
       \|   | |   |/
        |   | |   |
         \  | |  /
          \ | | /
           \| |/
            | |
            | |
         ___| |___
        /~~~~~~~~~\
       |  O     O  |
       |     ^     |
        \  \___/  /
         \_______/
|}

let print_message msg =
  let formatted = String.split_on_char '\n' msg
    |> List.map (fun line ->
        let padded = line ^ String.make (max 0 (50 - String.length line)) ' ' in
        String.sub padded 0 (min 50 (String.length padded)))
    |> String.concat "\n   |  "
  in
  format sandworm formatted |> print_endline

let () =
  let args = Sys.argv |> Array.to_list |> List.tl in
  let message =
    if List.length args = 0 then
      "Bless the Maker and His water.\nBless the coming and going of Him."
    else
      String.concat " " args
  in
  print_message message
