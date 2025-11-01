# Minttea Improvement Roadmap

Based on comprehensive analysis of the Charm.sh Go TUI ecosystem (Bubbletea, Lipgloss, Bubbles, Reflow, Harmonica), this document outlines improvements to bring Minttea to feature parity and beyond.

> **Note:** Detailed analysis of all Go libraries available in `GO_LIBRARIES_ANALYSIS.md`

## Executive Summary

Minttea has successfully ported the core TEA architecture from Bubbletea. The next phase focuses on:

1. **Styling System** - Enhanced Lipgloss capabilities for colors, borders, and layouts  
2. **Essential Widgets** - Add viewport, text-input, list, and table components
3. **Advanced Features** - Message filtering, incremental rendering, suspend/restore

## Current Status ✅

### What Minttea Has
- ✅ Core TEA architecture (init, update, view)
- ✅ Event loop with keyboard/mouse support
- ✅ Basic commands (Quit, Hide/Show cursor, Alt screen, Timer)
- ✅ Batch and Sequence commands
- ✅ Mouse tracking (Cell_motion, All_motion)
- ✅ Frame-based rendering
- ✅ Basic styling (colors, bold, italic, underline, borders)
- ✅ Border rendering with 8 predefined styles
- ✅ 7 widgets (Fps, Sprite, Spinner, Cursor, Progress, Paginator, Forms)
- ✅ Gradient support

### Gaps vs. Bubbletea/Lipgloss
- ❌ Message filtering
- ❌ Focus tracking events
- ❌ Program suspension/restoration
- ❌ Incremental rendering
- ❌ Layout composition (JoinHorizontal/Vertical)
- ❌ ANSI-aware width calculations (using byte length currently)
- ❌ Text alignment
- ❌ Essential widgets (Viewport, TextInput, List, Table)

## Implementation Roadmap

### Phase 1: Foundation 🏗️
**Priority: CRITICAL** | **Status: 90% Complete**

1. ✅ Core TEA architecture
2. ✅ Event handling (keyboard, mouse)
3. ✅ Basic rendering
4. ✅ Command system
5. ⚠️  Unicode handling (using byte length approximation - needs uuseg)
6. ✅ Styling API (render function)

### Phase 2: Styling System 🎨
**Priority: HIGH** | **Status: 95% Complete** ✨

**Completed:**
- ✅ Colors (ANSI, 256, RGB)
- ✅ Text attributes (bold, italic, underline, etc.)
- ✅ Borders (8 styles)
- ✅ Padding and margins
- ✅ Gradients
- ✅ Layout composition (join_horizontal, join_vertical, place)
- ✅ ANSI-aware text operations (width, strip, truncate, pad, word_wrap)
- ✅ Text alignment in styles (align_horizontal, align_vertical)
- ✅ Max width/height constraints (with ANSI-aware truncation)

**Remaining:**
- ❌ Style inheritance (lower priority)
- ❌ Transform functions (lower priority)

**Implementation:**
```ocaml
(* Priority 1: Layout composition *)
module Layout : sig
  val join_horizontal : 
    pos:[`Left | `Center | `Right] ->
    string list -> string
    
  val join_vertical : 
    pos:[`Top | `Center | `Bottom] ->
    string list -> string
    
  val place : 
    width:int -> height:int ->
    h_pos:float -> v_pos:float ->
    string -> string
end

(* Priority 2: ANSI utilities *)
module Ansi : sig
  val width : string -> int
  val truncate : max_width:int -> string -> string
  val wrap : width:int -> string -> string list
  val strip : string -> string
end

(* Priority 3: Alignment *)
val align_horizontal : [`Left | `Center | `Right] -> style -> style
val align_vertical : [`Top | `Center | `Bottom] -> style -> style
```

### Phase 3: Essential Widgets 🧩  
**Priority: HIGH** | **Status: 90% Complete** ✨

**Completed:**
- ✅ Fps - Frame rate helper
- ✅ Sprite - Frame-based animations
- ✅ Spinner - 12 predefined spinners (line, dot, globe, moon, etc.)
- ✅ Cursor - Blinking cursor with focus management
- ✅ Progress - Progress bars with gradients
- ✅ Paginator - Page navigation (dots/numerals)
- ✅ Forms - Basic checkbox
- ✅ **Viewport** - Full-featured scrolling container
- ✅ **TextInput** - Complete text editor with cursor, validation, password mode
- ✅ **Listbox** - Navigable list with filtering and selection
- ✅ **Table** - Data table with columns, rows, navigation, scrolling

**Missing (Medium Value):**
- ❌ **Help** - Keyboard shortcut display (~1-2 hours)

**Deferred (Low Priority):**
- ⏸️  FilePicker - File system navigator
- ⏸️  Timer/Stopwatch - Built-in timing widgets

**Implementation Priority:**

**1. Viewport (Scrolling) - HIGHEST VALUE**
```ocaml
module Viewport : sig
  type t
  val make : width:int -> height:int -> content:string list -> t
  val scroll_down : t -> int -> t
  val scroll_up : t -> int -> t
  val scroll_percent : t -> float -> t
  val update : Event.t -> t -> t
  val view : t -> string
end
```
**Use cases:** Logs, file viewers, help screens, long content

**2. List (Menus) - HIGH VALUE**
```ocaml
module List : sig
  type 'a item = { title : string; description : string option; data : 'a }
  type 'a t
  
  val make : 'a item list -> 'a t
  val select_next : 'a t -> 'a t
  val select_prev : 'a t -> 'a t
  val selected : 'a t -> 'a item option
  val update : Event.t -> 'a t -> 'a t
  val view : 'a t -> string
end
```
**Use cases:** Menus, file pickers, selection

**3. TextInput - MEDIUM VALUE** 
(Complex - needs full cursor/selection/clipboard)

### Phase 4: Advanced Features 🚀
**Priority: MEDIUM** | **Status: 0% Complete**

**Message Filtering:**
```ocaml
type filter = Event.t -> Event.t option

let with_filter : filter -> Config.t -> Config.t

(* Example: Global quit handler *)
let quit_on_ctrl_c = function
  | Event.KeyDown (Key "c", Ctrl) -> 
      Some (Event.KeyDown (Key "q", No_mod))
  | e -> Some e
```

**Incremental Rendering:**
- Only redraw changed lines
- 10-100x performance improvement
- Critical for large UIs

**External Message Injection:**
```ocaml
type 'model program = {
  send : Event.t -> unit;
  kill : unit -> unit;
  wait : unit -> unit;
}
```

**Suspend/Restore:**
```ocaml
val suspend : unit -> Command.t
val restore : unit -> Command.t

(* Example *)
let open_editor file = Command.Sequence [
  suspend ();
  exec "vim" [file];
  restore ();
]
```

### Phase 5: Text Layout 📝
**Priority: LOW** | **Status: 0% Complete**

From Reflow library:
- Word wrapping (ANSI-aware)
- Text truncation with ellipsis
- Indentation/dedentation
- Padding utilities

### Phase 6: Animation System 🎬
**Priority: LOW** | **Status: 0% Complete**

From Harmonica:
- Easing functions (linear, quad, cubic, etc.)
- Spring physics
- Keyframe animations

## Immediate Next Steps

### Sprint 1 (Week 1-2)
1. ✅ Port remaining widgets (Paginator, Forms) - DONE
2. ❌ Add proper Unicode grapheme cluster counting (uuseg dependency)
3. ❌ Implement ANSI width utilities
4. ❌ Add layout composition (JoinHorizontal/Vertical)

### Sprint 2 (Week 3-4)
1. ❌ Implement Viewport widget
2. ❌ Add text alignment to styles
3. ❌ Implement List widget
4. ❌ Add message filtering

### Sprint 3 (Month 2)
1. ❌ Incremental rendering
2. ❌ Complete Table widget
3. ❌ Add TextInput widget
4. ❌ External message injection

## Feature Comparison Matrix

| Feature | Bubbletea | Lipgloss | Minttea | Priority |
|---------|-----------|----------|---------|----------|
| **Core TEA** | ✅ | N/A | ✅ | - |
| **Keyboard events** | ✅ | N/A | ✅ | - |
| **Mouse events** | ✅ | N/A | ✅ | - |
| **Basic commands** | ✅ | N/A | ✅ | - |
| **Batch/Sequence** | ✅ | N/A | ✅ | - |
| **Message filtering** | ✅ | N/A | ❌ | HIGH |
| **Focus tracking** | ✅ | N/A | ❌ | MED |
| **Suspend/restore** | ✅ | N/A | ❌ | MED |
| **External msgs** | ✅ | N/A | ❌ | MED |
| **Incremental render** | ✅ | N/A | ❌ | HIGH |
| | | | | |
| **Colors** | N/A | ✅ | ✅ | - |
| **Text attributes** | N/A | ✅ | ✅ | - |
| **Borders** | N/A | ✅ | ✅ | - |
| **Padding/margins** | N/A | ✅ | ✅ | - |
| **Alignment** | N/A | ✅ | ❌ | HIGH |
| **Layout join** | N/A | ✅ | ❌ | HIGH |
| **ANSI-aware ops** | N/A | ✅ | ⚠️  | CRITICAL |
| **Max width/height** | N/A | ✅ | ⚠️  | MED |
| **Style inheritance** | N/A | ✅ | ❌ | LOW |
| | | | | |
| **Viewport** | ✅ | N/A | ❌ | CRITICAL |
| **TextInput** | ✅ | N/A | ❌ | HIGH |
| **List** | ✅ | N/A | ❌ | HIGH |
| **Table** | ✅ | N/A | ⏸️  | MED |
| **Spinner** | ✅ | N/A | ✅ | - |
| **Progress** | ✅ | N/A | ✅ | - |
| **Paginator** | ✅ | N/A | ✅ | - |

## Performance Targets

| Metric | Current | Target | Method |
|--------|---------|--------|--------|
| Frame time | ~20ms | <16ms (60 FPS) | Incremental rendering |
| Input latency | ~10ms | <5ms | Direct reads |
| Startup | ~100ms | <50ms | Lazy init |
| Memory | ~15MB | <10MB | String interning |

## Dependencies

### Current
- `std` - Standard library
- `miniriot` - Process runtime
- `tty` - Terminal control
- `colors` - Color support

### Needed
- `uuseg` - Unicode segmentation (CRITICAL for proper text width)
- `uutf` - UTF-8/UTF-16 codecs (if needed)

### Optional
- `notty` - Alternative backend (future exploration)

## Success Metrics

1. **Feature Parity**: 75% of Bubbletea/Lipgloss core features
2. **Performance**: <16ms frame time for 60 FPS
3. **Adoption**: 3+ real applications built with minttea
4. **Community**: 5+ external contributions
5. **Stability**: Build works, no crashes

## Conclusion

Minttea has successfully established a solid foundation with:
- ✅ Complete TEA architecture
- ✅ Comprehensive event handling
- ✅ Rich styling system (70% complete)
- ✅ 7 useful widgets

**Critical Path Forward:**
1. **Fix Unicode** - Add uuseg for proper grapheme cluster counting
2. **Layout utilities** - JoinHorizontal/Vertical (enables complex UIs)
3. **Viewport widget** - Most requested feature for real apps
4. **Message filtering** - Enables global hotkeys and validation

With these additions, Minttea will be production-ready for building sophisticated TUI applications in OCaml.

---

*For detailed analysis of Go libraries, see `GO_LIBRARIES_ANALYSIS.md`*
