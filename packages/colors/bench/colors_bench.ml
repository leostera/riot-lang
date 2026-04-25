open Std
open Std.Collections
open Colors

let ansi_inputs =
  [|
    0;
    9;
    42;
    103;
    196;
    231;
    255;
  |]

let rgb_inputs =
  [|
    `rgb (0, 0, 0);
    `rgb (255, 255, 255);
    `rgb (255, 0, 0);
    `rgb (0, 255, 0);
    `rgb (0, 0, 255);
    `rgb (255, 255, 0);
    `rgb (12, 34, 56);
    `rgb (255, 128, 0);
    `rgb (17, 200, 123);
    `rgb (90, 40, 210);
  |]

let xyz_inputs = Array.init ~count:(Array.length rgb_inputs) ~fn:(
  fun index -> RGB.to_xyz (Array.get_unchecked rgb_inputs ~at:index)
)

let grayscale_inputs =
  [|
    0;
    1;
    12;
    64;
    128;
    193;
    255;
  |]

let linear_inputs =
  [|
    0.0;
    0.000_303_526_983_548_837_5;
    0.003_676_507_324_047_436;
    0.215_860_500_113_899_26;
    0.75;
    1.0;
  |]

let next_index = fun state values ->
  let index = state mod Array.length values in (index, state + 1)

let ansi_state = ref 0

let rgb_state = ref 0

let xyz_state = ref 0

let gray_state = ref 0

let linear_state = ref 0

let cube_level = fun level ->
  match level with
  | 0 -> 0
  | 1 -> 95
  | 2 -> 135
  | 3 -> 175
  | 4 -> 215
  | _ -> 255

let computed_ansi_to_rgb = fun (`ansi index) ->
  let index = Int.min 255 (Int.max 0 index) in
  if index < 16 then
    ANSI.to_rgb (`ansi index)
  else
    if index < 232 then
      let normalized = index - 16 in
      let red = normalized / 36 in
      let green = (normalized / 6) mod 6 in
      let blue = normalized mod 6 in `rgb (cube_level red, cube_level green, cube_level blue)
    else
      let shade = 8 + ((index - 232) * 10) in `rgb (shade, shade, shade)

let channel_lut = Array.init ~count:256 ~fn:(
  fun channel ->
    match Linear_RGB.linearize (`rgb (channel, channel, channel)) with
    | `lrgb (value, _, _) -> value
)

let bench_ansi_to_rgb_table = fun () ->
  let index, next = next_index !ansi_state ansi_inputs in
  ansi_state := next;
  let _ = ANSI.to_rgb (`ansi (Array.get_unchecked ansi_inputs ~at:index)) in ()

let bench_ansi_to_rgb_computed = fun () ->
  let index, next = next_index !ansi_state ansi_inputs in
  ansi_state := next;
  let _ = computed_ansi_to_rgb (`ansi (Array.get_unchecked ansi_inputs ~at:index)) in ()

let bench_ansi_nearest = fun () ->
  let index, next = next_index !rgb_state rgb_inputs in
  rgb_state := next;
  let _ = ANSI.nearest (Array.get_unchecked rgb_inputs ~at:index) in ()

let bench_linearize_formula = fun () ->
  let index, next = next_index !gray_state grayscale_inputs in
  gray_state := next;
  let channel = Array.get_unchecked grayscale_inputs ~at:index in
  let _ = Linear_RGB.linearize (`rgb (channel, channel, channel)) in ()

let bench_linearize_lut = fun () ->
  let index, next = next_index !gray_state grayscale_inputs in
  gray_state := next;
  let channel = Array.get_unchecked grayscale_inputs ~at:index in
  let _ = Array.get_unchecked channel_lut ~at:channel in ()

let bench_delinearize = fun () ->
  let index, next = next_index !linear_state linear_inputs in
  linear_state := next;
  let value = Array.get_unchecked linear_inputs ~at:index in
  let _ = Linear_RGB.delinearize (`lrgb (value, value, value)) in ()

let bench_xyz_to_luv = fun () ->
  let index, next = next_index !xyz_state xyz_inputs in
  xyz_state := next;
  let _ = XYZ.to_luv (Array.get_unchecked xyz_inputs ~at:index) in ()

let bench_rgb_to_luv = fun () ->
  let index, next = next_index !rgb_state rgb_inputs in
  rgb_state := next;
  let _ = RGB.to_luv (Array.get_unchecked rgb_inputs ~at:index) in ()

let bench_rgb_blend = fun () ->
  let index, next = next_index !rgb_state rgb_inputs in
  rgb_state := next;
  let left = Array.get_unchecked rgb_inputs ~at:index in
  let right = Array.get_unchecked rgb_inputs ~at:((index + 1) mod Array.length rgb_inputs) in
  let _ = RGB.blend left right ~mix:0.5 in ()

let bench_rgb_gradient_64 = fun () ->
  let _ = RGB.gradient (`rgb (255, 0, 0)) (`rgb (0, 0, 255)) ~steps:64 in ()

let medium: Bench.bench_config = { iterations = 500; warmup = 50 }

let heavy: Bench.bench_config = { iterations = 200; warmup = 20 }

let benchmarks = Bench.[
  with_config ~config:medium "colors ansi.to_rgb table lookup" bench_ansi_to_rgb_table;
  with_config ~config:medium "colors ansi.to_rgb computed formula" bench_ansi_to_rgb_computed;
  with_config ~config:medium "colors ansi.nearest" bench_ansi_nearest;
  with_config ~config:medium "colors linear_rgb.linearize formula" bench_linearize_formula;
  with_config ~config:medium "colors linear_rgb.linearize lut" bench_linearize_lut;
  with_config ~config:medium "colors linear_rgb.delinearize" bench_delinearize;
  with_config ~config:medium "colors xyz.to_luv" bench_xyz_to_luv;
  with_config ~config:medium "colors rgb.to_luv" bench_rgb_to_luv;
  with_config ~config:medium "colors rgb.blend" bench_rgb_blend;
  with_config ~config:heavy "colors rgb.gradient 64" bench_rgb_gradient_64;
]

let main ~args = Bench.Cli.main ~name:"colors benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
