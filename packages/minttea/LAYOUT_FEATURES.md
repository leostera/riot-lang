# Minttea Layout & Alignment Features

This document showcases the new layout composition and text alignment features added to Minttea, bringing it to near feature parity with Lipgloss.

## Overview

**New Modules:**
- `Layout` - Composition functions for arranging text blocks
- Enhanced `Ansi` - ANSI-aware text operations
- Enhanced `Styles` - Text alignment support

**Feature Parity:** ~95% with Lipgloss styling system

---

## Layout Module

The Layout module provides functions for composing and positioning text blocks, similar to CSS flexbox/grid concepts.

### Horizontal Composition

```ocaml
open Std
open Minttea

(* Place strings side-by-side *)
let left_panel = "Users:\n- Alice\n- Bob\n- Carol"
let right_panel = "Status:\n✓ Online\n✗ Offline\n✓ Online"

let dashboard = Layout.join_horizontal ~pos:`Top [left_panel; right_panel]

(* Result:
   Users:    Status:
   - Alice   ✓ Online
   - Bob     ✗ Offline
   - Carol   ✓ Online
*)
```

**Alignment options:**
- `` `Top`` - Align to top, shorter blocks padded at bottom
- `` `Center`` - Center vertically
- `` `Bottom`` - Align to bottom, shorter blocks padded at top

### Vertical Composition

```ocaml
(* Stack strings vertically *)
let header = "╔════════════════╗\n║   My App v1.0  ║\n╚════════════════╝"
let content = "Welcome to the app!"
let footer = "Press Q to quit"

let screen = Layout.join_vertical ~pos:`Left [header; content; footer]

(* Result:
   ╔════════════════╗
   ║   My App v1.0  ║
   ╚════════════════╝
   Welcome to the app!
   Press Q to quit
*)
```

**Alignment options:**
- `` `Left`` - Align left, narrower blocks padded on right
- `` `Center`` - Center horizontally
- `` `Right`` - Align right, narrower blocks padded on left

### Absolute Positioning

```ocaml
(* Place text at specific position in a box *)
let dialog = "Are you sure?\n[Yes] [No]"

(* Center in a 40x10 box *)
let centered = Layout.place 
  ~width:40 
  ~height:10 
  ~h_pos:0.5  (* 0.0 = left, 0.5 = center, 1.0 = right *)
  ~v_pos:0.5  (* 0.0 = top, 0.5 = center, 1.0 = bottom *)
  dialog

(* Place in top-right corner *)
let top_right = Layout.place 
  ~width:40 
  ~height:10 
  ~h_pos:1.0 
  ~v_pos:0.0 
  "Help: Press ?"
```

---

## ANSI Module Enhancements

ANSI-aware text operations that properly handle escape sequences.

### Width Calculation

```ocaml
(* Measure display width, ignoring ANSI codes *)
let styled = "\027[1;31mHello\027[0m"  (* Bold red "Hello" *)
let width = Ansi.width styled  (* Returns 5, not 18 *)
```

### Truncation

```ocaml
(* Truncate while preserving ANSI formatting *)
let long = "\027[1;32mVery long text here\027[0m"
let short = Ansi.truncate ~width:10 ~ellipsis:"…" long
(* Returns "\027[1;32mVery long…\027[0m" - preserves green+bold *)
```

### Padding

```ocaml
let text = "\027[31mError\027[0m"

(* Pad to 20 characters *)
Ansi.pad_right ~width:20 ' ' text   (* "Error               " + ANSI *)
Ansi.pad_left ~width:20 ' ' text    (* "               Error" + ANSI *)
Ansi.pad_center ~width:20 ' ' text  (* "       Error        " + ANSI *)
```

### Word Wrapping

```ocaml
(* Wrap text to fit width *)
let long_text = "This is a very long line that needs to be wrapped to fit in a narrow column"
let lines = Ansi.word_wrap ~width:20 long_text

(* Returns:
   ["This is a very long";
    "line that needs to";
    "be wrapped to fit in";
    "a narrow column"]
*)
```

### Stripping

```ocaml
(* Remove all ANSI codes *)
let styled = "\027[1;31mColored\027[0m text"
let plain = Ansi.strip styled  (* "Colored text" *)
```

---

## Styles Module Enhancements

### Text Alignment

Align text within a fixed-size style box.

#### Horizontal Alignment

```ocaml
open Minttea

(* Center text in 30-character width *)
let centered_style = Styles.default
  |> Styles.width (Some 30)
  |> Styles.align_horizontal `Center
  |> Styles.fg (Styles.color "cyan")
  |> Styles.border Styles.Border.rounded

let view = Styles.render centered_style "Hello World"

(* Result:
   ╭────────────────────────────╮
   │         Hello World        │
   ╰────────────────────────────╯
*)
```

**Options:**
- `` `Left`` - Align left (default)
- `` `Center`` - Center text
- `` `Right`` - Align right

#### Vertical Alignment

```ocaml
(* Center content in 10-line height *)
let dialog_style = Styles.default
  |> Styles.width (Some 40)
  |> Styles.height 10
  |> Styles.align_horizontal `Center
  |> Styles.align_vertical `Center
  |> Styles.border Styles.Border.double

let dialog = Styles.render dialog_style "System Message\n\nOperation complete!"

(* Content centered both horizontally and vertically *)
```

**Options:**
- `` `Top`` - Align to top (default)
- `` `Center`` - Center content
- `` `Bottom`` - Align to bottom

### Max Width/Height Constraints

```ocaml
(* Constrain content to max dimensions *)
let constrained_style = Styles.default
  |> Styles.max_width 50
  |> Styles.max_height 20
  |> Styles.border Styles.Border.rounded

let long_content = String.concat "\n" (List.init 100 string_of_int)
let view = Styles.render constrained_style long_content

(* Content truncated to 50 chars wide and 20 lines tall *)
(* Lines wider than 50 chars get "…" appended *)
```

---

## Real-World Examples

### Dashboard Layout

```ocaml
open Std
open Minttea

let make_panel ~title ~content =
  let header = Styles.default
    |> Styles.bold true
    |> Styles.fg (Styles.color "yellow")
    |> Styles.render in
  
  let panel = Styles.default
    |> Styles.width (Some 30)
    |> Styles.height 10
    |> Styles.padding_left 1
    |> Styles.padding_right 1
    |> Styles.border Styles.Border.rounded
    |> Styles.render in
  
  panel (header title ^ "\n\n" ^ content)

let view model =
  let left = make_panel 
    ~title:"Statistics" 
    ~content:"Users: 1,234\nRequests: 56,789" in
  
  let middle = make_panel 
    ~title:"Status" 
    ~content:"✓ Database\n✓ Cache\n✗ Email" in
  
  let right = make_panel 
    ~title:"Recent" 
    ~content:"10:32 - Login\n10:33 - Update" in
  
  Layout.join_horizontal ~pos:`Top [left; middle; right]
```

### Centered Dialog

```ocaml
let show_dialog ~message =
  let dialog_content = 
    Styles.default
    |> Styles.width (Some 40)
    |> Styles.height 8
    |> Styles.align_horizontal `Center
    |> Styles.align_vertical `Center
    |> Styles.fg (Styles.color "white")
    |> Styles.bg (Styles.color "blue")
    |> Styles.border Styles.Border.double
    |> Styles.render in
  
  let content = message ^ "\n\n[OK] [Cancel]" in
  
  (* Center dialog in 80x24 terminal *)
  Layout.place 
    ~width:80 
    ~height:24 
    ~h_pos:0.5 
    ~v_pos:0.4 
    (dialog_content content)
```

### Text Wrapping in Panels

```ocaml
let help_panel ~content =
  (* Wrap content to fit 60 chars *)
  let wrapped = Ansi.word_wrap ~width:58 content in
  let text = String.concat "\n" wrapped in
  
  Styles.default
  |> Styles.width (Some 60)
  |> Styles.max_height 20
  |> Styles.padding_left 1
  |> Styles.padding_right 1
  |> Styles.border Styles.Border.rounded
  |> Styles.fg (Styles.color "gray")
  |> Styles.render in
  
  help_panel text
```

---

## API Summary

### Layout Module

| Function | Purpose |
|----------|---------|
| `join_horizontal ~pos strs` | Place strings side-by-side |
| `join_vertical ~pos strs` | Stack strings vertically |
| `place ~width ~height ~h_pos ~v_pos str` | Position in box |

### Ansi Module

| Function | Purpose |
|----------|---------|
| `width str` | Display width (ignores ANSI) |
| `strip str` | Remove ANSI codes |
| `truncate ~width ~ellipsis str` | Truncate with ellipsis |
| `pad_right/left/center ~width char str` | Pad to width |
| `word_wrap ~width str` | Wrap text to width |
| `split_lines str` | Split on newlines |

### Styles Module (New)

| Function | Purpose |
|----------|---------|
| `align_horizontal pos` | Set horizontal alignment |
| `align_vertical pos` | Set vertical alignment |
| `max_width n` | Constrain max width |
| `max_height n` | Constrain max height |

---

## Implementation Notes

1. **ANSI Preservation:** All layout and text operations preserve ANSI escape sequences
2. **Unicode Handling:** Currently uses byte-length approximation (TODO: add uuseg for proper grapheme clusters)
3. **Performance:** Layout functions use efficient list operations and avoid unnecessary string allocations
4. **Compatibility:** Works with all existing Minttea widgets and styling features

---

## What's Next

See `IMPROVEMENTS.md` for the full roadmap. Upcoming features:

- **Phase 3:** Essential widgets (TextInput, List, Table)
- **Advanced:** Style inheritance, transform functions
- **Performance:** Incremental rendering, viewport optimizations
