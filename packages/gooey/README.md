# gooey

Terminal UI primitives for Riot.

`gooey` is the lower-level TUI package in the Riot stack. It gives you
elements, styles, layout, rendering, geometry, and terminal renderer backends
without imposing an Elm-style application loop.

## Install

```sh
riot add gooey
```

## Should you use this or `minttea`?

Use `gooey` when you want direct control over layout and rendering.

Use `minttea` when you want the higher-level "model, update, view" workflow and
event loop for interactive terminal apps.

## What is inside

- layout primitives such as rows, columns, spacing, and grow behavior;
- styling and ANSI formatting;
- geometry and viewport helpers;
- renderers for inline and fullscreen terminal output.

## Good places to start

- `examples/simple.ml` and `examples/layout_rows.ml` show the core shape.
- `examples/terminal_demo.ml` is useful if you want to see a fuller interface.
- the tests under `tests/` are small and readable if you want to understand the
  layout model precisely.
