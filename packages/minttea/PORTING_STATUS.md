# Minttea Porting Status

## ✅ Completed: Core Framework (Task 1 - Partial)

Successfully ported Minttea core to use only `Std`:

### Core Modules Ported
- **timer_ref.ml** - Timer identification using `unit Std.Ref.t`
- **event.ml** - Terminal events (KeyDown, Timer, Frame, Custom)
- **command.ml** - Commands (Quit, cursor control, screen modes, timers)
- **app.ml** - Elm-style app definition (init, update, view)
- **config.ml** - Render mode and FPS configuration
- **io_loop.ml** - Input/output event loop
- **renderer.ml** - Terminal rendering with frame diffs
- **program.ml** - Main runtime loop with process coordination
- **Minttea.ml** - Main facade module

### Key Adaptations Made
1. **Riot → Std/Miniriot**
   - `Riot.Ref` → `unit Std.Ref.t` for timer refs
   - `Riot.Process` → `Std.Process` / top-level functions
   - `Riot.Timer` → `Std.Timer`
   - `Ptime.t` → `Std.Time.Instant.t`

2. **API Adjustments**
   - `Process.send` → `send` (top-level)
   - `Process.receive_any` → `receive_any` (top-level)
   - `Process.self` → `self` (top-level)
   - `Log.trace (fun f -> f "msg")` → `Log.trace "msg"`
   - `Result.get_ok` → `Result.expect ~msg`
   - `Timer.send_interval ~every` → `Timer.send_interval ~interval` (takes seconds as float)

3. **Sleep Implementation**
   ```ocaml
   let sleep timeout =
     let selector _ = `skip in
     try receive ~selector ~timeout () |> ignore with _ -> ()
   ```

4. **Module Shadowing**
   - Avoided `open Std` in Minttea.ml to prevent conflict with `Std.Command`
   - Used explicit `Std.Log` references

## 📋 TODO: Remaining Work

### Task 1: Complete Minttea Port

#### Styles (formerly Spices) - 4 modules
- [ ] `packages/minttea/src/styles/border.ml` - Border drawing utilities
- [ ] `packages/minttea/src/styles/formatter.ml` - Text formatting  
- [ ] `packages/minttea/src/styles/gradient.ml` - Gradient effects
- [ ] `packages/minttea/src/styles/styles.ml` - Main facade

#### Widgets (formerly Leaves) - 10 modules
- [ ] `packages/minttea/src/widgets/cursor.ml` - Cursor positioning
- [ ] `packages/minttea/src/widgets/text_input.ml` - Text input component
- [ ] `packages/minttea/src/widgets/progress.ml` - Progress bars
- [ ] `packages/minttea/src/widgets/spinner.ml` - Loading spinners
- [ ] `packages/minttea/src/widgets/table.ml` - Table widget
- [ ] `packages/minttea/src/widgets/paginator.ml` - Pagination
- [ ] `packages/minttea/src/widgets/fps.ml` - FPS counter
- [ ] `packages/minttea/src/widgets/sprite.ml` - Sprite animations
- [ ] `packages/minttea/src/widgets/filtered_list.ml` - Filtered list widget
- [ ] `packages/minttea/src/widgets/forms.ml` - Form components

### Task 2: Port Examples

18 examples to port (in `packages/minttea/examples/`):
- [ ] basic - Simple counter
- [ ] counter - Counter with styling  
- [ ] altscreen-toggle - Alternate screen mode
- [ ] fullscreen - Fullscreen mode
- [ ] stopwatch - Stopwatch with timers
- [ ] fps - FPS counter demo
- [ ] emoji - Emoji rendering
- [ ] border - Border styling
- [ ] layout - Layout examples
- [ ] list - List widget
- [ ] text-input - Text input demo
- [ ] progress - Progress bar demo
- [ ] spinner - Spinner demo
- [ ] paginator - Paginator demo
- [ ] table - Table demo
- [ ] views - Multiple views example

### Task 3: Analyze Go Libraries

Analyze these libraries in `./3rdparty/`:
1. **bubbletea** - TEA TUI framework (compare with our core)
2. **lipgloss** - Styling library (inform our styles modules)
3. **bubbles** - Component library (inform our widgets)
4. **harmonica** - Physics-based animations (potential new feature)
5. **reflow** - Text reflow algorithms (potential new feature)

**Deliverable**: Create `IMPROVEMENTS.md` with:
- Feature comparison matrix
- Missing features we should add
- API improvements we should make
- New capabilities to implement

### Task 4: Implement Improvements

Based on analysis, implement top improvements:
- [ ] Enhanced styling API (from lipgloss analysis)
- [ ] Missing widget types (from bubbles analysis)
- [ ] Animation support (from harmonica analysis)
- [ ] Text reflow algorithms (from reflow analysis)
- [ ] Better color support
- [ ] Layout system improvements

## Build Status

✅ Core minttea builds successfully
- Location: `packages/minttea/`
- Dependencies: std, miniriot, tty, colors

## Next Session Tasks

1. Port styles modules (4 files)
2. Port widgets modules (10 files)
3. Port basic examples (start with simplest ones)
4. Begin Go library analysis
