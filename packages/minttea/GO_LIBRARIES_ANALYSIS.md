# Comprehensive Analysis of Go TUI Libraries for Minttea

This document analyzes the Charm.sh ecosystem of TUI libraries to identify features and improvements that can be incorporated into our OCaml Minttea port.

---

## 1. Bubbletea - Core TUI Framework

### Purpose
The foundational framework implementing The Elm Architecture (TEA) for terminal applications. Handles the event loop, rendering, input processing, and terminal state management.

### Key Features

#### Program Options & Lifecycle
- **`WithAltScreen()`** - Start in alternate screen buffer (tea.go:109)
- **`WithMouseCellMotion()` / `WithMouseAllMotion()`** - Mouse tracking modes (options.go:122-162)
- **`WithoutBracketedPaste()`** - Disable bracketed paste mode (options.go:115)
- **`WithReportFocus()`** - Focus gain/loss events (options.go:241)
- **`WithFilter(func(Model, Msg) Msg)`** - Pre-process messages before update (options.go:197-229)
- **`WithFPS(int)`** - Custom rendering framerate (options.go:232)
- **`WithContext(context.Context)`** - Cancellable program context (options.go:20)
- **`WithInput/WithOutput/WithEnvironment`** - Custom I/O and env vars (options.go:28-67)
- **`WithoutSignalHandler()` / `WithoutCatchPanics()`** - Disable built-in handlers (options.go:69-84)

#### Advanced Keyboard Support
- **Modifier combinations**: Ctrl+Shift, Alt+Shift, Ctrl+Alt, Ctrl+Alt+Shift (key.go:226-237)
- **Extended keys**: F13-F20, Insert, PgUp/PgDown with modifiers (key.go:238-257)
- **Key sequences**: Comprehensive ANSI escape sequence parsing (key.go:354-532)
- **Paste detection**: `Key.Paste` field distinguishes pasted from typed input (key.go:58)

#### Mouse Events (mouse.go)
- **Mouse buttons**: Left, Middle, Right, Wheel (Up/Down/Left/Right), Backward, Forward, Button10, Button11 (mouse.go:106-119)
- **Mouse actions**: Press, Release, Motion (mouse.go:74-78)
- **Mouse event fields**: X, Y coordinates, Shift, Alt, Ctrl modifiers (mouse.go:17-28)
- **SGR and X10 encoding**: Support for both modern (SGR) and legacy (X10) mouse protocols (mouse.go:172-223)

#### Commands & Batching
- **`Batch(cmds...)`** - Concurrent command execution (commands.go:15)
- **`Sequence(cmds...)`** - Sequential command execution (commands.go:25)
- **`Every(duration, fn)`** - System clock-synced ticking (commands.go:102)
- **`Tick(duration, fn)`** - Independent interval ticking (commands.go:154)
- **`SetWindowTitle(string)`** - Set terminal title (commands.go:205)
- **`WindowSize()`** - Query current terminal size (commands.go:218)

#### Rendering Architecture (renderer.go, standard_renderer.go)
- **Framerate-based rendering**: Default 60 FPS, configurable 1-120 FPS (standard_renderer.go:18-19)
- **Incremental rendering**: Only renders changed lines (standard_renderer.go:214-224)
- **ANSI compressor**: Optional redundant ANSI sequence removal (standard_renderer.go:79-81)
- **Alternate screen support**: Separate rendering state for altscreen (standard_renderer.go:47-48)
- **Cursor visibility control**: `showCursor()` / `hideCursor()` (renderer.go:36-37)
- **Line ignore map**: Exclude specific lines from rendering (standard_renderer.go:60)

#### Focus Tracking
- **`FocusMsg` / `BlurMsg`** - Terminal focus gain/loss events
- **`enableReportFocus()` / `disableReportFocus()`** - Control focus reporting

#### Process Lifecycle
- **`Program.Kill()`** - Force immediate termination (tea.go:795)
- **`Program.Wait()`** - Block until program exits (tea.go:799)
- **`Program.Send(Msg)`** - Inject messages externally (tea.go:774)
- **`Program.Println/Printf`** - Print above TUI (tea.go:919-941)
- **`ReleaseTerminal()` / `RestoreTerminal()`** - Suspend/resume TUI mode (tea.go:862-916)
- **Panic recovery**: Automatic terminal restoration on panic (tea.go:633-640)
- **Signal handling**: SIGINT, SIGTERM, SIGWINCH (tea.go:273-312)

### Missing in Minttea
1. ❌ Message filtering (`WithFilter`)
2. ❌ Custom FPS control
3. ❌ Context-based cancellation
4. ❌ Focus tracking events
5. ❌ External message injection (`Send`)
6. ❌ Program suspension/restoration
7. ❌ Incremental rendering optimization
8. ❌ Print-above-TUI functionality
9. ❌ Ctrl+Shift and other complex modifiers
10. ❌ Mouse button 8-11 support
11. ❌ `Every` command (system clock sync)
12. ❌ Window size query command

### Recommended Additions
1. **Message filtering** - Pre-process events (useful for global hotkeys, validation)
2. **Context support** - Enable graceful cancellation from parent processes
3. **Incremental rendering** - Only redraw changed lines (major performance improvement)
4. **External message injection** - Allow programmatic control from outside TEA loop
5. **Suspend/restore** - Critical for integrating with external processes (editors, shells)

---

## 2. Lipgloss - Styling Library

### Purpose
Declarative styling for terminal output with ANSI-aware text layout, borders, padding, margins, and alignment.

### Key Features

#### Style Properties (style.go, set.go)
- **Text attributes**: Bold, Italic, Underline, Strikethrough, Reverse, Blink, Faint (set.go:162-206)
- **Colors**: Foreground, Background, ANSI/256/TrueColor support (set.go:208-224)
- **Box model**: Width, Height, MaxWidth, MaxHeight (set.go:226-239, 256-262)
- **Padding**: Top, Right, Bottom, Left with shorthand methods (set.go:269-318)
- **Margins**: Top, Right, Bottom, Left with shorthand methods (set.go:331-388)
- **Margin background**: Color the margin area (set.go:382-388)
- **Alignment**: Horizontal (Left/Center/Right), Vertical (Top/Center/Bottom) (set.go:241-267)
- **Tab width**: Configurable tab expansion (set.go:262-266)
- **Transform function**: Custom text transformations (set.go:267)
- **Style inheritance**: `Inherit(Style)` overlays styles (style.go:199-231)
- **Inline mode**: Render without newlines (style.go:272)

#### Borders (borders.go)
- **Pre-defined borders**: Normal, Rounded, Thick, Double, Block, Hidden, Markdown (borders.go:68-200)
- **Half-block borders**: Outer/Inner half-block characters (borders.go:119-139)
- **Per-side borders**: Independent top, right, bottom, left (borders.go:52-65)
- **Border colors**: Foreground and background per side (set.go:390-467)
- **Custom borders**: Define your own Border struct (borders.go:14-28)

#### Layout Functions (join.go, position.go)
- **`JoinHorizontal(pos, strs...)`** - Join blocks horizontally with alignment (join.go:28)
- **`JoinVertical(pos, strs...)`** - Join blocks vertically with alignment (join.go:116)
- **`Place(w, h, hPos, vPos, str)`** - Position text in a box (position.go:36)
- **`PlaceHorizontal(w, pos, str)`** - Horizontal positioning (position.go:49)
- **`PlaceVertical(h, pos, str)`** - Vertical positioning (position.go:103)
- **Position type**: Float64 (0.0-1.0) for flexible positioning (position.go:19)

#### Advanced Features
- **ANSI-aware width calculation**: Handles escape codes correctly
- **Whitespace options**: Control space rendering in joins/placement
- **Style composition**: Chain methods fluently
- **Renderer customization**: Per-renderer color profiles
- **Unicode support**: Proper handling of wide characters, grapheme clusters

### Missing in Minttea
1. ❌ **All styling functionality** - Minttea currently has no styling system
2. ❌ Border rendering
3. ❌ Padding and margins
4. ❌ Text alignment
5. ❌ Layout composition (JoinHorizontal/Vertical)
6. ❌ Color support (ANSI/256/TrueColor)
7. ❌ Style inheritance
8. ❌ ANSI-aware text operations

### Recommended Additions
1. **Basic styling module** - Bold, italic, underline, colors (essential for any TUI)
2. **Border rendering** - At least Normal and Rounded borders
3. **Layout utilities** - JoinHorizontal/JoinVertical for composing views
4. **Padding/margins** - Critical for proper spacing in complex UIs
5. **ANSI width utilities** - Calculate display width correctly (already critical for rendering)

---

## 3. Bubbles - Component Library

### Purpose
Reusable, composable UI components following the Bubble Tea architecture. Each component is a self-contained Model with Init/Update/View.

### Available Components

#### 1. **Spinner** (spinner/spinner.go)
- **Purpose**: Loading indicator animation
- **Pre-defined spinners**: Line, Dot, MiniDot, Jump, Pulse, Points, Globe, Moon, Monkey, Meter, Hamburger, Ellipsis (spinner.go:27-84)
- **Customizable**: Frame strings, FPS, style
- **Features**: Internal ID management, ticker-based animation
- **API**: `New() Model`, `Update(Msg) (Model, Cmd)`, `View() string`

#### 2. **Text Input** (textinput/textinput.go)
- **Purpose**: Single-line text editor with cursor
- **Features**:
  - Cursor movement: char/word forward/backward, line start/end
  - Deletion: char/word backward/forward, kill line
  - Echo modes: Normal, Password, None
  - Character limit enforcement
  - Horizontal scrolling viewport
  - Input validation with custom function
  - Placeholder text
  - Clipboard support (paste)
  - Suggestions with autocomplete
  - Customizable key bindings
  - Styling: Prompt, Text, Placeholder, Cursor, Completion
- **KeyMap**: 15 configurable key bindings (textinput.go:48-86)
- **API**: Rich model with Focus/Blur, SetValue, Reset, etc.

#### 3. **Text Area** (textarea/textarea.go)
- **Purpose**: Multi-line text editor
- **Features**:
  - Multi-line editing with cursor
  - Line wrapping
  - Vertical scrolling
  - Clipboard support (copy/paste)
  - Line numbers (optional)
  - Character/line limits
  - Validation
  - Syntax highlighting support
  - Memoization for performance

#### 4. **Table** (table/table.go)
- **Purpose**: Tabular data display with navigation
- **Features**:
  - Column definitions with width, title, key
  - Row navigation with cursor
  - Vertical scrolling
  - Customizable styles (header, selected row, cell)
  - Optional borders
  - Keyboard navigation

#### 5. **List** (list/list.go)
- **Purpose**: Navigable item list with filtering
- **Features**:
  - Fuzzy filtering
  - Pagination
  - Item selection
  - Status messages
  - Loading spinner
  - Default and custom delegates
  - Built-in help view
  - Customizable styles
  - Items implement `Item` interface

#### 6. **Viewport** (viewport/viewport.go)
- **Purpose**: Scrollable content area
- **Features**:
  - Vertical/horizontal scrolling
  - Mouse wheel support
  - Scroll position tracking (AtTop, AtBottom, ScrollPercent)
  - Page up/down
  - Half-page scrolling
  - Go to top/bottom
  - Line-by-line scrolling
  - High-performance rendering mode (deprecated)
  - Style support (borders, padding)

#### 7. **Progress** (progress/progress.go)
- **Purpose**: Progress bar indicator
- **Features**:
  - Solid and gradient fills
  - Percentage display (customizable)
  - Custom empty/filled characters
  - Width customization
  - Animation support (via Harmonica)
  - Color customization

#### 8. **Paginator** (paginator/paginator.go)
- **Purpose**: Page navigation logic and UI
- **Features**:
  - Dot-style (iOS-like) pagination
  - Numeric pagination (1/10, etc.)
  - Per-page item count
  - Total items tracking
  - Active/inactive dot styling
  - Customizable rendering

#### 9. **Timer** (timer/timer.go)
- **Purpose**: Countdown timer
- **Features**:
  - Duration-based countdown
  - Tick interval customization
  - Start/Stop/Toggle/Reset
  - Timeout event
  - ID for multiple timers

#### 10. **Stopwatch** (stopwatch/stopwatch.go)
- **Purpose**: Elapsed time counter
- **Features**:
  - Start/Stop/Reset/Toggle
  - Tick interval
  - ID management

#### 11. **File Picker** (filepicker/filepicker.go)
- **Purpose**: File system navigator
- **Features**:
  - Directory traversal
  - File selection
  - Extension filtering
  - Show/hide hidden files
  - Current directory tracking
  - File size display

#### 12. **Help** (help/help.go)
- **Purpose**: Auto-generated help view from keybindings
- **Features**:
  - Single/multi-line modes
  - Graceful truncation
  - Group keybindings
  - Show/hide full help
  - Ellipsis indicator

#### 13. **Key** (key/key.go)
- **Purpose**: Keybinding management (non-visual)
- **Features**:
  - Bind multiple keys to action
  - Help text generation
  - Enable/disable bindings
  - Match key events
  - Unbind keys

#### 14. **Cursor** (cursor/cursor.go)
- **Purpose**: Blinking cursor component
- **Features**:
  - Blink animation
  - Custom blink speed
  - Show/hide
  - Style customization
  - Multiple modes (Line, Block, Underline)

### Missing in Minttea
1. ❌ **All components** - No pre-built widgets exist
2. ❌ Text editing primitives (cursor, selection)
3. ❌ Scrolling logic
4. ❌ Filtering/pagination logic
5. ❌ Animation framework
6. ❌ Keybinding abstraction

### Recommended Additions
1. **Viewport** - Essential for scrollable content (logs, help, content)
2. **Text Input** - Critical for forms, search, command input
3. **List** - Common pattern for menus, selections, file browsers
4. **Spinner** - Simple loading indicator (easy to implement)
5. **Keybinding abstraction** - Make rebindable controls easier

---

## 4. Reflow - Text Layout Library

### Purpose
ANSI-aware text manipulation: wrapping, indentation, padding, truncation, margins. Works with styled text without breaking ANSI codes.

### Key Features

#### Word Wrapping (wordwrap/)
- **`wordwrap.String(str, limit)`** - Wrap at word boundaries (README.md:20)
- **`wordwrap.NewWriter(limit)`** - io.Writer interface for streaming
- **Customizable breakpoints**: Define where to break (`:`, `,`, etc.)
- **Custom newline**: Set newline character(s)
- **ANSI-preserving**: Maintains escape codes across wraps

#### Unconditional Wrapping (wrap/)
- **`wrap.String(str, limit)`** - Hard wrap at character limit
- **`wrap.NewWriter(limit)`** - io.Writer interface
- **KeepNewlines**: Preserve existing line breaks
- **PreserveSpace**: Keep leading spaces
- **TabWidth**: Expand tabs to spaces

#### Indentation (indent/)
- **`indent.String(str, width)`** - Add leading spaces
- **`indent.NewWriter(width, func)`** - Custom indent function (dots, arrows, etc.)
- **Per-line indentation**: Works with multi-line strings

#### Dedentation (dedent/)
- **`dedent.String(str)`** - Remove common leading whitespace
- **ANSI-aware**: Correctly handles colored/styled text

#### Padding (padding/)
- **`padding.String(str, width)`** - Right-pad to width
- **`padding.NewWriter(width, func)`** - Custom padding function
- **ANSI-preserving**: Escape codes don't affect padding calculation

#### Margin (margin/)
- **`margin.String(str, width)`** - Add margins around text
- **Left/right margins**: Configurable spacing

#### Truncation (truncate/)
- **`truncate.String(str, width)`** - Truncate with ellipsis
- **ANSI-aware**: Correctly measures display width

#### ANSI Buffer (ansi/)
- **`ansi.Writer`** - Track ANSI state across writes
- **`ansi.Buffer`** - Buffered ANSI-aware operations
- **State tracking**: Remember colors/styles for continuation

### Missing in Minttea
1. ❌ **All text layout functionality**
2. ❌ Word wrapping
3. ❌ ANSI-aware truncation
4. ❌ Indentation utilities
5. ❌ Padding utilities

### Recommended Additions
1. **Word wrapping** - Essential for displaying long text (help, messages)
2. **ANSI-aware width calculation** - Already partially needed for rendering
3. **Truncation** - Critical for fitting text in constrained spaces (tables, lists)
4. **Padding utilities** - Useful for alignment and spacing
5. **Indentation** - Useful for nested content, code blocks

---

## 5. Harmonica - Animation Library
**Note**: Not found in 3rdparty. Referenced in bubbles/progress README.

### Purpose (from context)
Provides easing functions and animation interpolation for smooth transitions.

### Expected Features (inferred)
- Easing functions (ease-in, ease-out, ease-in-out)
- Spring physics
- Linear interpolation
- Frame-based animation
- Duration-based transitions

### Missing in Minttea
- ❌ All animation support

### Recommended Additions
- **Easing utilities** - Would enhance progress bars, transitions
- **Spring physics** - Natural-feeling animations
- (Lower priority - can be implemented as needed)

---

## Implementation Priority Ranking

### Phase 1: Core Improvements (High Impact, Low Effort)
1. ✅ **Mouse support** - Already implemented
2. ✅ **Extended keyboard** - Already implemented
3. ✅ **Batch/Sequence commands** - Already implemented
4. **Message filtering** - Enable global hotkeys, validation
5. **Incremental rendering** - Major performance boost
6. **ANSI width utilities** - Foundation for layout

### Phase 2: Styling Foundation (High Impact, Medium Effort)
1. **Basic colors** - ANSI 16, 256, TrueColor support
2. **Text attributes** - Bold, italic, underline, etc.
3. **Border rendering** - Normal, Rounded borders at minimum
4. **Padding/margins** - Box model implementation
5. **Layout joins** - JoinHorizontal, JoinVertical

### Phase 3: Text Layout (Medium Impact, Medium Effort)
1. **Word wrapping** - Essential for text display
2. **ANSI-aware truncation** - Fit text in constrained space
3. **Alignment utilities** - Left, Center, Right
4. **Indentation** - Nested content support

### Phase 4: Essential Components (High Impact, High Effort)
1. **Viewport** - Scrollable content (very common pattern)
2. **Text input** - Forms, search, command line
3. **List** - Menus, selections, navigation
4. **Spinner** - Loading states

### Phase 5: Advanced Features (Medium Impact, Variable Effort)
1. **Context cancellation** - Graceful shutdown
2. **External message injection** - Programmatic control
3. **Suspend/restore** - Editor integration
4. **Table widget** - Tabular data display
5. **Progress bar** - Status indication

### Phase 6: Nice-to-Have (Lower Priority)
1. Text area widget
2. File picker widget
3. Help generator
4. Animation framework
5. Custom FPS control
6. Focus tracking

---

## Architecture Recommendations

### 1. Separate Styling Module
Create a `minttea.Style` module (or `minttea_style` package) with:
- Style record type with fluent builder pattern
- Color variants (ANSI/256/RGB)
- ANSI code generation
- Width calculation utilities

### 2. Layout Module
Create `minttea.Layout` with:
- Join functions
- Alignment types
- Border rendering
- Box model calculations

### 3. Component Pattern
Each component should:
- Be a separate module (e.g., `minttea_viewport`)
- Expose `Model` type
- Provide `init`, `update`, `view` functions
- Be composable (can be used in larger models)
- Have minimal dependencies

### 4. ANSI Utilities
Core module for:
- Escape code parsing
- Width calculation (accounting for escapes)
- Truncation
- State tracking

### 5. Backward Compatibility
- Keep existing APIs stable
- Add new features as optional modules
- Use feature flags if needed
- Document migration paths

---

## Example Usage Patterns (from Go)

### Styling
```go
// Go (lipgloss)
style := lipgloss.NewStyle().
    Bold(true).
    Foreground(lipgloss.Color("#FAFAFA")).
    Background(lipgloss.Color("#7D56F4")).
    Padding(1, 2).
    Border(lipgloss.RoundedBorder())

fmt.Println(style.Render("Hello, World!"))
```

### Layout
```go
// Go (lipgloss)
left := lipgloss.NewStyle().Width(20).Render("Left")
right := lipgloss.NewStyle().Width(20).Render("Right")
joined := lipgloss.JoinHorizontal(lipgloss.Top, left, right)
```

### Components
```go
// Go (bubbles/textinput)
type model struct {
    input textinput.Model
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
    var cmd tea.Cmd
    m.input, cmd = m.input.Update(msg)
    return m, cmd
}
```

---

## Testing Strategy

### Unit Tests
- ANSI parsing and width calculation
- Border rendering
- Layout join operations
- Text wrapping algorithms

### Integration Tests
- Component composition
- Style inheritance
- Event flow through components

### Visual Tests
- Snapshot testing for rendering output
- Compare rendered ANSI against expected

### Performance Tests
- Incremental rendering vs full redraw
- Large list scrolling
- Wide text truncation

---

## Documentation Needs

1. **Styling guide** - How to use colors, attributes, borders
2. **Layout guide** - Composing views with joins, alignment
3. **Component guide** - How to use and build components
4. **Migration guide** - Updating from current minttea
5. **API reference** - Generated docs for all modules
6. **Examples** - Small, focused examples for each feature

---

## Conclusion

The Charm.sh ecosystem demonstrates a mature, well-architected approach to terminal UIs:

1. **Separation of concerns**: Framework (bubbletea), styling (lipgloss), layout (reflow), components (bubbles)
2. **Composability**: Everything builds on simple primitives
3. **ANSI awareness**: Escape codes are first-class concerns
4. **Developer ergonomics**: Fluent APIs, sensible defaults, extensive customization

For Minttea, the highest-value additions are:
- **Styling system** (lipgloss basics)
- **Layout utilities** (reflow + lipgloss layout)
- **Core components** (viewport, input, list)
- **Performance** (incremental rendering)
- **ANSI utilities** (width, truncation)

This will transform Minttea from a basic TEA implementation into a full-featured TUI framework competitive with the Go ecosystem.
