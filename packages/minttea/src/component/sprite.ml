open Std
open Std.Collections

type t = {
  frames: string array;
  current_frame: int;
  fps: Fps.t;
  loop: bool;
}

let make = fun ?(starting_frame = 0) ?(loop = true) ~fps frames ->
  {
    frames;
    fps;
    loop;
    current_frame = starting_frame;
  }

let advance_frame = fun m ->
  let next_frame = m.current_frame + 1 in
  if m.loop then
    next_frame mod Array.length m.frames
  else
    let last_frame = Array.length m.frames - 1 in
    min last_frame next_frame

let update = fun ?now m ->
  if Fps.tick ?now m.fps = `frame then
    let current_frame = advance_frame m in
    { m with current_frame }
  else
    m

let view = fun s ->
  if s.current_frame < 0 || s.current_frame >= Array.length s.frames then
    panic "Sprite.view: current frame out of bounds"
  else
    Array.get_unchecked s.frames ~at:s.current_frame

let current_frame_index = fun s -> s.current_frame
