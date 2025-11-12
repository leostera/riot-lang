# Minttea Examples Porting Plan

## Overview
This document outlines a comprehensive plan to port 30+ examples from various TUI libraries (bubbletea, textual, clay-tui, notcurses) to our Minttea OCaml library. The goal is to provide a rich set of examples that demonstrate all capabilities of the library and serve as learning resources.

## Available Components in Minttea

### Core Components
- **Cursor** - Cursor positioning and control
- **FPS** - Frame rate counter/limiter
- **Forms** - Form management system
- **Listbox** - Selectable list with navigation
- **Paginator** - Page navigation component
- **Progress** - Progress bar with percentage/custom display
- **Spinner** - Animated loading spinners
- **Sprite** - Animation frames/sprite rendering
- **Table** - Tabular data display with headers
- **TextArea** - Multi-line text editor with cursor
- **TextInput** - Single-line text input with validation
- **Viewport** - Scrollable content area

### Style System
- Colors (256 color, true color support)
- Text decorations (bold, italic, underline, etc.)
- Borders and padding
- Flexbox-style layouts

## Example Categories and Implementation Plan

### Category 1: Basic Components (Foundation)
These examples demonstrate individual components in isolation.

#### ✅ Already Implemented
1. **001_hello_world.ml** - Basic text display
2. **002_spinner.ml** - Spinner animation
3. **003_progress.ml** - Progress bar

#### 📋 To Implement
4. **004_clock.ml** - Digital clock display (port from textual)
   - Real-time updates every second
   - Large digit display using box drawing

5. **005_fps_counter.ml** - Show frame rate
   - Display current FPS
   - Performance monitoring

6. **006_colors.ml** - Color palette showcase
   - All 256 colors
   - True color gradients
   - Port from textual's pride.py concept

### Category 2: Text Input & Forms

7. **007_textinput_basic.ml** - Simple text input
   - Basic input with placeholder
   - Show typed value

8. **008_textinput_validation.ml** - Input validation
   - Email/phone validation
   - Error messages

9. **009_textarea_editor.ml** - Multi-line editor
   - Line numbers
   - Basic editing

10. **010_form_simple.ml** - Basic form
    - Name, email, submit
    - Tab navigation between fields

11. **011_form_credit_card.ml** - Credit card form (port from bubbletea)
    - Card number formatting
    - Expiry date, CVV
    - Real-time validation

### Category 3: Lists & Navigation

12. **012_listbox_simple.ml** - Basic list selection
    - Arrow key navigation
    - Enter to select

13. **013_listbox_filtering.ml** - Searchable list
    - Filter items as you type
    - Highlight matches

14. **014_menu_navigation.ml** - Multi-level menu
    - Nested menus
    - Breadcrumb display

15. **015_file_browser.ml** - File system navigator
    - Directory tree
    - File selection

### Category 4: Tables & Data Display

16. **016_table_static.ml** - Static data table
    - Headers and rows
    - Column alignment

17. **017_table_sortable.ml** - Sortable columns
    - Click headers to sort
    - Sort indicators

18. **018_table_paginated.ml** - Large dataset with pagination
    - Page navigation
    - Items per page selector

### Category 5: Layout Examples

19. **019_layout_split_panes.ml** - Split view (port from bubbletea)
    - Resizable panes
    - Focus management

20. **020_layout_tabs.ml** - Tab interface (port from bubbletea)
    - Tab switching
    - Content panels

21. **021_layout_dashboard.ml** - Multi-widget dashboard
    - Grid layout
    - Multiple updating widgets

22. **022_layout_modal.ml** - Modal dialogs
    - Overlay with dimmed background
    - Escape to close

### Category 6: Animation & Graphics

23. **023_animation_sprite.ml** - Sprite animation
    - Frame-based animation
    - Multiple sprites

24. **024_animation_loading.ml** - Various loading animations
    - Different spinner styles
    - Progress with animation

25. **025_animation_transition.ml** - Smooth transitions
    - Fade in/out
    - Slide effects

### Category 7: Advanced Interactions

26. **026_chat_interface.ml** - Chat UI (port from bubbletea)
    - Message history with viewport
    - Input at bottom
    - Timestamps

27. **027_autocomplete.ml** - Autocomplete suggestions (port from bubbletea)
    - Dropdown suggestions
    - Async data fetching simulation

28. **028_calculator.ml** - Interactive calculator (port from textual)
    - Button grid
    - Display panel
    - Keyboard support

29. **029_markdown_viewer.ml** - Markdown rendering
    - Basic formatting
    - Scrollable viewport

30. **030_code_editor.ml** - Simple code editor
    - Syntax highlighting simulation
    - Line numbers
    - Basic editing

### Category 8: System Integration

31. **031_exec_command.ml** - Execute external commands
    - Show command output
    - Progress indication

32. **032_realtime_logs.ml** - Log viewer
    - Tail-like following
    - Color-coded levels

33. **033_system_monitor.ml** - System stats dashboard
    - CPU usage
    - Memory usage
    - Update every second

### Category 9: Games & Fun

34. **034_snake_game.ml** - Simple snake game
    - Arrow key controls
    - Score tracking

35. **035_tetris_mini.ml** - Minimal tetris
    - Basic shapes
    - Line clearing

36. **036_game_of_life.ml** - Conway's Game of Life
    - Grid display
    - Play/pause controls

### Category 10: Professional Applications

37. **037_todo_app.ml** - Todo list manager
    - Add/remove items
    - Mark complete
    - Persistent storage

38. **038_json_tree.ml** - JSON viewer (port from textual)
    - Expandable tree
    - Syntax highlighting

39. **039_sql_client.ml** - Simple SQL query interface
    - Query input
    - Result table
    - History

40. **040_package_manager.ml** - Package manager UI (port from bubbletea)
    - Install/uninstall
    - Search packages
    - Progress indicators

## Implementation Priority

### Phase 1: Core Examples (Week 1)
- Text input examples (7-11)
- List examples (12-15)
- Basic animations (23-25)

### Phase 2: Data & Layout (Week 2)
- Table examples (16-18)
- Layout examples (19-22)
- System integration (31-33)

### Phase 3: Advanced (Week 3)
- Chat interface (26)
- Autocomplete (27)
- Calculator (28)
- Code editor (30)

### Phase 4: Applications (Week 4)
- Todo app (37)
- JSON viewer (38)
- Games (34-36)
- Package manager (40)

## Testing Strategy

Each example should include:
1. A README explaining the concepts demonstrated
2. Comments explaining key patterns
3. Keyboard shortcuts documentation
4. Screenshots or recordings (where applicable)

## Success Metrics

- All examples compile and run without errors
- Examples cover all component APIs
- Progressive complexity for learning
- Clear documentation and comments
- Performance: All examples run at 60 FPS where applicable

## Notes on Porting

### From Bubbletea (Go)
- Adapt the Cmd pattern to Minttea's effect system
- Port lipgloss styles to Minttea's style system
- Convert Go's interface{} messages to OCaml variants

### From Textual (Python)
- Adapt class-based components to functional style
- Convert CSS to Minttea styles
- Port reactive properties to MVU updates

### From Clay-TUI (C)
- Focus on layout algorithms
- Adapt immediate mode to retained mode where needed

### From Notcurses (C)
- Extract high-level patterns
- Simplify complex rendering to Minttea's abstraction level

## Example Naming Convention

```
NNN_category_description.ml
```
Where:
- NNN = three-digit number for ordering
- category = component/feature category
- description = specific example focus

## Documentation Structure

Each example should have:
```ocaml
(**
 * Example: [Name]
 * 
 * This example demonstrates:
 * - Feature 1
 * - Feature 2
 * 
 * Key concepts:
 * - Concept explanation
 * 
 * Controls:
 * - Key bindings list
 *)
```

## Repository Structure

```
packages/minttea/
├── examples/
│   ├── README.md (index of all examples)
│   ├── 001_hello_world.ml
│   ├── 002_spinner.ml
│   ├── ...
│   └── 040_package_manager.ml
├── examples_old/ (archive of old examples)
└── tusk.toml (with example build targets)
```

## Build Configuration

Add to tusk.toml:
```toml
[[task.examples]]
name = "build-examples"
run = "tusk build //packages/minttea/examples/..."

[[task.examples]]
name = "run-example"
run = "tusk run //packages/minttea/examples/${1}"
```

This plan provides a comprehensive roadmap for creating a rich set of examples that will make Minttea a well-documented and approachable TUI library for OCaml developers.
