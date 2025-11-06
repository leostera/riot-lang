open Std

(** ANSI AST - Compositional representation of ANSI escape sequences
    
    This nested AST makes it easy to:
    - Write tests with pattern matching
    - Compose styles naturally
    - Optimize redundant operations
    - Generate minimal ANSI sequences
*)

type color = Tty.Color.t

type t =
  | Seq of t list            (* Sequence of operations *)
  | Fg of color * t list     (* Foreground color scope *)
  | Bg of color * t list     (* Background color scope *)
  | Bold of t list           (* Bold text scope *)
  | Italic of t list         (* Italic text scope *)
  | Underline of t list      (* Underlined text scope *)
  | Strikethrough of t list  (* Strikethrough text scope *)
  | Reverse of t list        (* Reverse video scope *)
  | Blink of t list          (* Blinking text scope *)
  | Dim of t list            (* Dimmed text scope *)
  | Text of string           (* Plain text *)
  | MoveCursor of int * int  (* Move to absolute position x, y *)
  | MoveUp of int            (* Move cursor up n lines *)
  | MoveDown of int          (* Move cursor down n lines *)
  | MoveLeft of int          (* Move cursor left n columns *)
  | MoveRight of int         (* Move cursor right n columns *)
  | SaveCursor               (* Save cursor position *)
  | RestoreCursor            (* Restore cursor position *)
  | Clear                    (* Clear screen *)
  | ClearLine                (* Clear entire line *)
  | ClearToEOL               (* Clear to end of line *)
  | ClearToBOL               (* Clear to beginning of line *)
  | ShowCursor               (* Make cursor visible *)
  | HideCursor               (* Make cursor invisible *)
  | BeginSync                (* Begin synchronized update *)
  | EndSync                  (* End synchronized update *)

(** Helper constructors for common patterns *)
let text s = Text s
let seq ops = Seq ops
let fg color children = Fg (color, children)
let bg color children = Bg (color, children)
let bold children = Bold children
let italic children = Italic children
let underline children = Underline children
let move_to x y = MoveCursor (x, y)

(** Optimize the AST by merging adjacent operations *)
let rec optimize = function
  | Seq ops -> Seq (optimize_seq ops)
  | Fg (c, children) -> Fg (c, optimize_seq children)
  | Bg (c, children) -> Bg (c, optimize_seq children)
  | Bold children -> Bold (optimize_seq children)
  | Italic children -> Italic (optimize_seq children)
  | Underline children -> Underline (optimize_seq children)
  | Strikethrough children -> Strikethrough (optimize_seq children)
  | Reverse children -> Reverse (optimize_seq children)
  | Blink children -> Blink (optimize_seq children)
  | Dim children -> Dim (optimize_seq children)
  | other -> other

and optimize_seq ops =
  let rec opt acc = function
    | [] -> List.rev acc
    
    (* Merge adjacent text nodes *)
    | Text s1 :: Text s2 :: rest ->
        opt acc (Text (s1 ^ s2) :: rest)
        
    (* Merge adjacent cursor movements *)
    | MoveUp n1 :: MoveUp n2 :: rest ->
        opt acc (MoveUp (n1 + n2) :: rest)
        
    | MoveDown n1 :: MoveDown n2 :: rest ->
        opt acc (MoveDown (n1 + n2) :: rest)
        
    | MoveLeft n1 :: MoveLeft n2 :: rest ->
        opt acc (MoveLeft (n1 + n2) :: rest)
        
    | MoveRight n1 :: MoveRight n2 :: rest ->
        opt acc (MoveRight (n1 + n2) :: rest)
        
    (* Eliminate zero movements *)
    | MoveUp 0 :: rest | MoveDown 0 :: rest 
    | MoveLeft 0 :: rest | MoveRight 0 :: rest ->
        opt acc rest
        
    (* Flatten nested sequences *)
    | Seq inner :: rest ->
        opt acc (inner @ rest)
        
    (* Eliminate duplicate cursor operations *)
    | HideCursor :: HideCursor :: rest ->
        opt acc (HideCursor :: rest)
        
    | ShowCursor :: ShowCursor :: rest ->
        opt acc (ShowCursor :: rest)

    (* Eliminate nullified operations *)
    | HideCursor :: ShowCursor :: rest
    | ShowCursor :: HideCursor :: rest ->
        opt acc rest
        
    (* Recursively optimize nested structures *)
    | op :: rest ->
        opt (optimize op :: acc) rest
  in
  opt [] ops

(** Convert color to ANSI foreground code *)
let color_to_fg_ansi = function
  | Tty.Color.RGB (r, g, b) -> format "\x1b[38;2;%d;%d;%dm" r g b
  | Tty.Color.ANSI c -> format "\x1b[%dm" (30 + c)
  | Tty.Color.ANSI256 c -> format "\x1b[38;5;%dm" c
  | Tty.Color.No_color -> ""

(** Convert color to ANSI background code *)
let color_to_bg_ansi = function
  | Tty.Color.RGB (r, g, b) -> format "\x1b[48;2;%d;%d;%dm" r g b
  | Tty.Color.ANSI c -> format "\x1b[%dm" (40 + c)
  | Tty.Color.ANSI256 c -> format "\x1b[48;5;%dm" c
  | Tty.Color.No_color -> ""

(** Serialize AST to ANSI escape sequences *)
let rec serialize ast =
  let buf = Buffer.create 1024 in
  serialize_to_buffer buf ast;
  Buffer.contents buf

and serialize_to_buffer buf = function
  | Seq ops ->
      List.iter (serialize_to_buffer buf) ops
      
  | Fg (color, children) ->
      Buffer.add_string buf (color_to_fg_ansi color);
      List.iter (serialize_to_buffer buf) children;
      Buffer.add_string buf "\x1b[39m"  (* Reset fg color *)
      
  | Bg (color, children) ->
      Buffer.add_string buf (color_to_bg_ansi color);
      List.iter (serialize_to_buffer buf) children;
      Buffer.add_string buf "\x1b[49m"  (* Reset bg color *)
      
  | Bold children ->
      Buffer.add_string buf "\x1b[1m";
      List.iter (serialize_to_buffer buf) children;
      Buffer.add_string buf "\x1b[22m"  (* Reset bold/dim *)
      
  | Italic children ->
      Buffer.add_string buf "\x1b[3m";
      List.iter (serialize_to_buffer buf) children;
      Buffer.add_string buf "\x1b[23m"  (* Reset italic *)
      
  | Underline children ->
      Buffer.add_string buf "\x1b[4m";
      List.iter (serialize_to_buffer buf) children;
      Buffer.add_string buf "\x1b[24m"  (* Reset underline *)
      
  | Strikethrough children ->
      Buffer.add_string buf "\x1b[9m";
      List.iter (serialize_to_buffer buf) children;
      Buffer.add_string buf "\x1b[29m"  (* Reset strikethrough *)
      
  | Reverse children ->
      Buffer.add_string buf "\x1b[7m";
      List.iter (serialize_to_buffer buf) children;
      Buffer.add_string buf "\x1b[27m"  (* Reset reverse *)
      
  | Blink children ->
      Buffer.add_string buf "\x1b[5m";
      List.iter (serialize_to_buffer buf) children;
      Buffer.add_string buf "\x1b[25m"  (* Reset blink *)
      
  | Dim children ->
      Buffer.add_string buf "\x1b[2m";
      List.iter (serialize_to_buffer buf) children;
      Buffer.add_string buf "\x1b[22m"  (* Reset dim/bold *)
      
  | Text s ->
      Buffer.add_string buf s
      
  | MoveCursor (x, y) ->
      Buffer.add_string buf (format "\x1b[%d;%dH" (y + 1) (x + 1))
      
  | MoveUp n when n > 0 ->
      Buffer.add_string buf (format "\x1b[%dA" n)
      
  | MoveDown n when n > 0 ->
      Buffer.add_string buf (format "\x1b[%dB" n)
      
  | MoveLeft n when n > 0 ->
      Buffer.add_string buf (format "\x1b[%dD" n)
      
  | MoveRight n when n > 0 ->
      Buffer.add_string buf (format "\x1b[%dC" n)
      
  | SaveCursor ->
      Buffer.add_string buf "\x1b[s"
      
  | RestoreCursor ->
      Buffer.add_string buf "\x1b[u"
      
  | Clear ->
      Buffer.add_string buf "\x1b[2J"
      
  | ClearLine ->
      Buffer.add_string buf "\x1b[2K"
      
  | ClearToEOL ->
      Buffer.add_string buf "\x1b[K"
      
  | ClearToBOL ->
      Buffer.add_string buf "\x1b[1K"
      
  | ShowCursor ->
      Buffer.add_string buf "\x1b[?25h"
      
  | HideCursor ->
      Buffer.add_string buf "\x1b[?25l"
      
  | BeginSync ->
      Buffer.add_string buf "\x1b[?2026h"
      
  | EndSync ->
      Buffer.add_string buf "\x1b[?2026l"
      
  | _ -> ()

(** Main render function: optimize then serialize *)
let render ast =
  ast |> optimize |> serialize
