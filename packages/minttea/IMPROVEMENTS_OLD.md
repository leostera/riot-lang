# Minttea Improvements - Completed ✅

Based on analysis of bubbletea, lipgloss, bubbles, and reflow, the following improvements have been implemented:

## What's New

### 🖱️ Mouse Support
- **Mouse events**: Click, Release, Motion for Left/Middle/Right buttons + Wheel
- **Mouse modes**: Cell_motion (drag tracking) and All_motion (hover tracking)
- **Position tracking**: x/y coordinates with modifier support
- **Enable/Disable**: `Command.Enable_mouse` and `Command.Disable_mouse`

```ocaml
let update event model =
  match event with
  | Event.Mouse { button = Left; event_type = Click; x; y; _ } ->
      handle_click model x y
  | Event.Mouse { button = Wheel_up; _ } ->
      scroll_up model
  | _ -> (model, Command.Noop)
```

### ⌨️ Extended Keyboard
- **New modifiers**: Alt, Shift, and all combinations (Ctrl_alt, etc.)
- **New keys**: Tab, Delete, Insert, Home, End, Page_up, Page_down
- **Function keys**: F1-F12 via `F of int`
- **Better strings**: `modifier_to_string` and improved `key_to_string`

```ocaml
| Event.KeyDown (F 1, No_modifier) -> show_help model
| Event.KeyDown (Key "s", Ctrl_alt) -> save_file model
| Event.KeyDown (Page_down, No_modifier) -> scroll_down model
```

### 🪟 Window Events
- **Resize events**: `Event.Resize { width; height }`
- **Window title**: `Command.Set_window_title "My App"`
- **Focus tracking**: `Event.Focus_gained` and `Event.Focus_lost`

```ocaml
| Event.Resize { width; height } ->
    ({ model with width; height }, Command.Noop)
| Event.Focus_lost ->
    ({ model with paused = true }, Command.Noop)
```

### 📋 Bracketed Paste
- **Paste events**: `Event.Paste of string`
- **Enable/Disable**: `Command.Enable_bracketed_paste`
- **Security**: Distinguishes pasted from typed text

```ocaml
| Event.Paste content ->
    insert_text model content
```

### ⚡ Batch Commands
- **Batch execution**: `Command.Batch` for concurrent execution
- **Sequential execution**: `Command.Sequence` for ordered execution  
- **Helper functions**: `Command.batch`, `Command.sequence`, `Command.timer`

```ocaml
let init model =
  Command.batch [
    Command.Enter_alt_screen;
    Command.Enable_mouse Cell_motion;
    Command.Set_window_title "My App";
  ]
```

## Migration Guide

### Backward Compatible ✅
All existing code continues to work. New events can be ignored with catch-all patterns:

```ocaml
let update event model =
  match event with
  | Event.KeyDown (Key "q", Ctrl) -> (model, Command.Quit)
  | _ -> (model, Command.Noop)  (* Ignores all new events *)
```

### Using New Features

Just add new cases to your update function:

```ocaml
let update event model =
  match event with
  (* Old events still work *)
  | Event.KeyDown (Key "q", Ctrl) -> (model, Command.Quit)
  
  (* Add new events as needed *)
  | Event.Mouse { button = Left; event_type = Click; x; y; _ } ->
      handle_click model x y
  | Event.Resize { width; height } ->
      ({ model with width; height }, Command.Noop)
  | Event.Paste content ->
      insert_text model content
  | Event.Focus_gained ->
      ({ model with paused = false }, refresh_data ())
      
  | _ -> (model, Command.Noop)
```

### Deprecation Note

`Command.Seq` is deprecated. Use:
- `Command.Batch` for concurrent execution
- `Command.Sequence` for sequential execution

## Implementation Details

### ANSI Escape Sequences
- Mouse tracking: SGR mode (1006), Cell motion (1002), All motion (1003)
- Bracketed paste: 2004
- Focus tracking: 1004
- Window title: OSC 2

### Terminal Compatibility
Works in most modern terminals:
- ✅ iTerm2, Terminal.app, Alacritty
- ✅ xterm-compatible terminals
- ⚠️ tmux/screen may need configuration

## Next Steps

See the roadmap in `3rdparty/minttea_improvements/README.md` for:
- Styling system (from lipgloss)
- Text reflow (from reflow)
- Component library (from bubbles)
- Animation system (from harmonica)

## Built Successfully ✅

```bash
cd packages/minttea
tusk build
```

All improvements compiled and ready to use!
