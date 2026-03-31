open Std

type mouse_mode =
  Cell_motion
  | All_motion

type t =
  | Noop
  | Quit
  | HideCursor
  | ShowCursor
  | ExitAltScreen
  | EnterAltScreen
  | EnableMouse of mouse_mode
  | DisableMouse
  | EnableBracketedPaste
  | DisableBracketedPaste
  | EnableFocusTracking
  | DisableFocusTracking
  | SetWindowTitle of string
  | Seq of t list
  | SetTimer of { ref: Timer.id Ref.t; duration: Time.Duration.t; }

let timer = fun ~after ->
  let ref = Ref.make () in
  (ref, SetTimer {ref; duration = after})
