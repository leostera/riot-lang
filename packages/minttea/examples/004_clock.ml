(**
 * Example: Digital Clock
 * 
 * This example demonstrates:
 * - Timer-based updates every second
 * - Large digit display using ASCII art
 * - Time formatting
 * 
 * Key concepts:
 * - Using SetTimer command for periodic updates
 * - Rendering large text with custom ASCII art
 * 
 * Controls:
 * - q/Q/Escape - Quit the application
 *)
open Std
open Std.Collections
open Std.Iter
open Minttea

(* Model: Current time and timer reference *)

type model = {
  time: DateTime.t;
  timer_ref: Timer.id Ref.t;
}

(* ASCII art for digits 0-9 and colon *)

let digits = [|
  [|
    " ██████ ";
    "██    ██";
    "██    ██";
    "██    ██";
    " ██████ ";
  |];
  [|"   ██   "; " ████   "; "   ██   "; "   ██   "; " ██████ ";|];
  [|
    " ██████ ";
    "██    ██";
    "    ██  ";
    "  ██    ";
    "████████";
  |];
  [|
    " ██████ ";
    "██    ██";
    "   ███  ";
    "██    ██";
    " ██████ ";
  |];
  [|
    "██    ██";
    "██    ██";
    "████████";
    "      ██";
    "      ██";
  |];
  [|
    "████████";
    "██      ";
    "███████ ";
    "      ██";
    "███████ ";
  |];
  [|
    " ██████ ";
    "██      ";
    "███████ ";
    "██    ██";
    " ██████ ";
  |];
  [|"████████"; "      ██"; "    ██  "; "  ██    "; "██      ";|];
  [|
    " ██████ ";
    "██    ██";
    " ██████ ";
    "██    ██";
    " ██████ ";
  |];
  [|
    " ██████ ";
    "██    ██";
    " ███████";
    "      ██";
    " ██████ ";
  |];
|]

let colon = [|"        "; "   ██   "; "        "; "   ██   "; "        ";|]

let space = [|"  "; "  "; "  "; "  "; "  ";|]

(* Initialize: Set up initial time and start timer *)

let init = fun model ->
  let timer_ref, timer_cmd = Command.timer ~after:(Time.Duration.from_secs 1) in
  ({ model with timer_ref }, timer_cmd)

(* Update: Handle events *)

let update = fun event model ->
  match event with
  | Event.KeyDown (Event.Key "q", _)
  | Event.KeyDown (Event.Key "Q", _)
  | Event.KeyDown (Event.Escape, _) ->
      (model, Command.Quit)
  | Event.Timer ref when Ref.equal ref model.timer_ref ->
      (* Update time and reset timer *)
      let timer_ref, timer_cmd = Command.timer ~after:(Time.Duration.from_secs 1) in
      let new_model = { time = DateTime.now (); timer_ref } in
      (new_model, timer_cmd)
  | _ ->
      (model, Command.Noop)

(* Render a single digit as ASCII art *)

let render_digit = fun d ->
  if d >= 0 && d <= 9 then
    Array.get_unchecked digits ~at:d
  else
    space

(* Render time as large ASCII art *)

let render_time_ascii = fun time_str ->
  let chars = String.fold_left ~fn:(fun acc ch -> ch :: acc) ~init:[] time_str |> List.rev in
  (* Convert each character to its ASCII art representation *)
  let char_to_art = function
    | '0' .. '9' as c -> render_digit (Char.code c - Char.code '0')
    | ':' -> colon
    | _ -> space
  in
  let art_chars = List.map ~fn:char_to_art chars in
  (* Combine ASCII art horizontally, line by line *)
  let lines = ref [] in
  for row = 0 to 4 do
    let line_parts =
      List.map ~fn:(fun art -> Array.get_unchecked art ~at:row) art_chars
    in
    let line = String.concat "" line_parts in
    lines := line :: !lines
  done;
  List.rev !lines

(* View: Render the clock *)

let view = fun model ->
  let open Element in
    let pad_two n =
      let s = Int.to_string n in
      if String.length s < 2 then
        "0" ^ s
      else
        s
    in
    let time_str = pad_two model.time.hour
    ^ ":"
    ^ pad_two model.time.minute
    ^ ":"
    ^ pad_two model.time.second in
    (* Render ASCII art *)
    let ascii_lines = render_time_ascii time_str in
    (* Create elements for each line of ASCII art *)
    let ascii_elements =
      List.map ~fn:(fun line -> text ~style:Style.(empty |> fg (`rgb (0, 255, 127)) |> bold) line) ascii_lines
    in
    (* Center everything *)
    column
      ~style:Style.(empty |> align ~x:Center ~y:Middle |> padding (Style.Padding.all 2))
      [
        column ascii_elements;
        text "";
        text ~style:Style.(empty |> fg (`rgb (100, 100, 100))) "Press 'q' to quit";
      ]

(* Create and run the app *)

let app = App.make ~init ~update ~view ()

(* Run it *)

let () =
  let initial_model = { time = DateTime.now (); timer_ref = Ref.make () } in
  let config = Minttea.config () in
  Minttea.start ~config app initial_model
