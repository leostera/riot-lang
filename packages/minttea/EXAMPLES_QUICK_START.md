# Quick Start: Priority Examples to Port

## Immediate Priority (Can implement today)

### 1. Clock Example (from Textual)
**Source**: textual/examples/clock.py
**Concepts**: Timer-based updates, digit rendering
**Implementation**:
```ocaml
(* Use Minttea.Timer.create to update every second *)
(* Render time using large ASCII art digits or Unicode box drawing *)
```

### 2. Simple List Selection (from Bubbletea result example)
**Source**: bubbletea/examples/result/main.go
**Concepts**: List navigation, selection, return value
**Implementation**:
```ocaml
(* Use Listbox component *)
(* Arrow keys to navigate, Enter to select *)
(* Return selected item on quit *)
```

### 3. Tabs Interface (from Bubbletea)
**Source**: bubbletea/examples/tabs/main.go
**Concepts**: Tab switching, content panels
**Implementation**:
```ocaml
(* Row layout with tab headers *)
(* Conditional rendering based on active tab *)
(* Left/Right arrow keys to switch *)
```

### 4. Text Input with Validation
**Source**: Combine patterns from multiple examples
**Concepts**: Real-time validation, error display
**Implementation**:
```ocaml
(* TextInput component with validator function *)
(* Show error message below input *)
(* Email/phone number validation *)
```

### 5. Split Panes (from Bubbletea)
**Source**: bubbletea/examples/split-editors/main.go
**Concepts**: Multiple text areas, focus management
**Implementation**:
```ocaml
(* Two TextArea components side by side *)
(* Tab to switch focus *)
(* Visual indication of active pane *)
```

### 6. Simple Chat Interface (from Bubbletea)
**Source**: bubbletea/examples/chat/main.go
**Concepts**: Viewport for history, input at bottom
**Implementation**:
```ocaml
(* Viewport component for message history *)
(* TextInput at bottom for new messages *)
(* Auto-scroll to bottom on new message *)
```

### 7. Progress with Multiple Bars
**Source**: Combine patterns
**Concepts**: Multiple progress bars updating independently
**Implementation**:
```ocaml
(* Multiple Progress components *)
(* Different update rates *)
(* Labels and percentages *)
```

### 8. File Picker
**Source**: bubbletea/examples/file-picker
**Concepts**: Directory navigation, file selection
**Implementation**:
```ocaml
(* List of files/directories *)
(* Enter to navigate into directory *)
(* Show current path *)
```

### 9. Autocomplete
**Source**: bubbletea/examples/autocomplete/main.go
**Concepts**: Suggestions dropdown, filtering
**Implementation**:
```ocaml
(* TextInput with suggestion list below *)
(* Filter suggestions based on input *)
(* Arrow keys to select suggestion *)
```

### 10. Modal Dialog
**Source**: Common pattern in many TUI apps
**Concepts**: Overlay, focus trap, escape to close
**Implementation**:
```ocaml
(* Centered box with dimmed background *)
(* Escape key to close *)
(* Focus trapped within modal *)
```

## Implementation Notes

### Common Patterns to Extract

1. **Focus Management**
```ocaml
type focus = InputFocused | ListFocused | NoneFocused
```

2. **Multi-component State**
```ocaml
type model = {
  input: TextInput.t;
  list: Listbox.t;
  viewport: Viewport.t;
  focus: focus;
}
```

3. **Keyboard Shortcuts**
```ocaml
let handle_key = function
  | "tab" -> switch_focus model
  | "ctrl+c" | "q" -> Minttea.Quit
  | key -> handle_component_key model key
```

4. **Layout Composition**
```ocaml
let view model =
  column [
    header model;
    flex @@ content model;
    fixed 3 @@ footer model;
  ]
```

## Testing Each Example

For each example, create:
1. A standalone `.ml` file
2. A `tusk.toml` entry for building
3. A simple test that runs the example with simulated input

## Success Criteria

Each example should:
- Run without errors
- Demonstrate clear UI patterns
- Be under 200 lines of code
- Include helpful comments
- Show proper use of Minttea components

## Next Steps

1. Start with examples 1-3 (Clock, List, Tabs)
2. Test component integration
3. Identify missing APIs
4. Create helper utilities as needed
5. Document patterns that emerge
