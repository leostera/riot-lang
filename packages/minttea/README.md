# minttea

Elm-style terminal UI programming for Riot.

`minttea` is the package to reach for when you want to build interactive TUIs
around a `model -> update -> view` loop. It sits on top of `gooey` and `tty`
and gives you a clean, explicit application structure instead of making you
mix event handling, terminal state, and rendering by hand.

## Install

```sh
riot add minttea
```

## Mental model

Every app is defined in three pieces:

- `init` to set up the initial model and first command;
- `update` to react to keyboard, mouse, timer, and custom events;
- `view` to render the current model into `Gooey.Element` values.

That keeps side effects explicit and makes terminal apps feel much more like
small state machines than a pile of callbacks.

## Example

```ocaml
open Std
open Minttea

type model = string

let init = fun model -> (model, Command.Noop)

let update = fun event model ->
  match event with
  | Event.KeyDown (Event.Key "q", _)
  | Event.KeyDown (Event.Escape, _) -> (model, Command.Quit)
  | _ -> (model, Command.Noop)

let view = fun model -> Element.text model

let app = App.make ~init ~update ~view ()

let () = Minttea.start app "Hello, World!"
```

The package already ships a good examples gallery:

```sh
riot run -p minttea 001_hello_world
riot run -p minttea 004_clock
riot run -p minttea 008_table
```

Press `q` in those demos to quit.

## What `minttea` includes

- `Minttea.App` for constructing apps;
- `Minttea.Event` for keyboard, mouse, timer, resize, paste, and focus events;
- `Minttea.Command` for quit/cursor/screen/timer and terminal-control effects;
- `Minttea.Element` and `Minttea.Style`, re-exported from `gooey`;
- `Minttea.Component` for reusable UI building blocks.

## Related packages

- `gooey` provides the terminal layout and styling primitives.
- `tty` handles lower-level terminal mechanics.
