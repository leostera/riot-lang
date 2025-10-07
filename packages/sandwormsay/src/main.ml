open Std

let sandworm = {|
                                    ___
                                 .-'   `'.
                                /         \
                                |         ;
                                |         |           ___.--,
                       _.._     |0) ~ (0) |    _.---'`__.-( (_.
                __.--'`_.. '.__.\    '--. \_.-' ,.--'`     `""`
               ( ,.--'`   ',__ /./;   ;, '.__.'`    __
               _`) )  .---.__.' / |   |\   \__..--""  """--.,_
              `---' .'.''-._.-'`_./  /\ '.  \ _.-~~~````~~~-._`-.__.'
                    | |  .' _.-' |  |  \  \  '.               `~---`
                     \ \/ .'     \  \   '. '-._)
                      \/ /        \  \    `=.__`~-.
                      / /\         `) )    / / `"".`\
                ,. ,.-'.'  \        / /    ( (     / /
                 `'`-'      \_    .(` )     \ \_.-' /
                             `---` \ \/       `-'  /
                                    ) )            (
                                   (_/             `
|}

let print_message msg =
  let lines = String.split_on_char '\n' msg in
  let max_len = List.fold_left (fun acc line ->
    Int.max acc (String.length line)
  ) 0 lines in

  (* Top border *)
  Format.printf " %s\n" (String.make (max_len + 2) '_');

  (* Message with side borders *)
  List.iter (fun line ->
    let padding = String.make (max_len - String.length line) ' ' in
    Format.printf "< %s%s >\n" line padding
  ) lines;

  (* Bottom border *)
  Format.printf " %s\n" (String.make (max_len + 2) '-');
  ()

let () =
  let args = Sys.argv in
  let message =
    if Array.length args > 1 then
      String.concat " " (List.tl (Array.to_list args))
    else
      "Bless the Maker and His water. Bless the coming and going of Him."
  in

  print_message message;
  Format.printf "%s" sandworm;
  Format.printf "\n"
