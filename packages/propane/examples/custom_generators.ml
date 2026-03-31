(** Examples showing how to create custom generators and arbitraries *)
open Std
open Propane

(* === Custom Type === *)

type color =
  Red
  | Green
  | Blue
  | Yellow
  | Black
  | White

let color_to_string =
  function
  | Red -> "Red"
  | Green -> "Green"
  | Blue -> "Blue"
  | Yellow -> "Yellow"
  | Black -> "Black"
  | White -> "White"

(* Custom generator for colors *)

let color_gen = Generator.one_of
[
  Generator.return Red;
  Generator.return Green;
  Generator.return Blue;
  Generator.return Yellow;
  Generator.return Black;
  Generator.return White;

]

(* Custom printer for colors *)

let color_printer = color_to_string

(* Custom arbitrary for colors *)

let color_arb = Arbitrary.make ~print:color_printer color_gen

(* === Custom Type with Fields === *)

type point = {
  x : int;
  y : int;
}

(* Generator using map and pair *)

let point_gen =
  Generator.map
  (fun ((x, y)) -> {x; y})
  (Generator.pair (Generator.int_range (-100) 100) (Generator.int_range (-100) 100))

(* Custom shrinker for points - shrink towards origin *)

let point_shrinker = fun point ->
  let x_shrunk = Shrinker.shrink (Shrinker.towards 0) point.x in
  let y_shrunk = Shrinker.shrink (Shrinker.towards 0) point.y in
  (* Combine shrinking on both axes *)
  let x_only =
    List.map (fun x -> {x; y = point.y}) x_shrunk
  in
  let y_only =
    List.map (fun y -> {x = point.x; y}) y_shrunk
  in
  x_only @ y_only

let point_printer = fun p -> "(" ^ Int.to_string p.x ^ ", " ^ Int.to_string p.y ^ ")"

let point_arb =
  Arbitrary.make ~shrink:point_shrinker ~print:point_printer
    ~small:(fun p ->
      let abs_x =
        if p.x < 0 then
          -p.x
        else
          p.x
      in
      let abs_y =
        if p.y < 0 then
          -p.y
        else
          p.y
      in
      abs_x + abs_y)
    point_gen

(* === Properties Using Custom Types === *)

let color_reflexive_prop =
  property "color equality is reflexive" color_arb (fun c -> c = c)

let point_distance_positive_prop =
  property "distance from origin is non-negative" point_arb
    (fun p ->
      let dist_sq = p.x * p.x + p.y * p.y in
      dist_sq >= 0)

let point_translation_prop =
  property "translating by zero preserves point" point_arb
    (fun p ->
      let translated = {x = p.x + 0; y = p.y + 0} in
      translated.x = p.x && translated.y = p.y)

(* === Generator Combinators === *)

(* Generate non-empty lists *)

let non_empty_list_prop =
  property
  "non-empty list generator never produces empty list"
  Arbitrary.(make (Generator.non_empty_list Generator.int))
  (fun lst -> List.length lst > 0)

(* Generate lists with specific sizes *)

let fixed_size_list_prop =
  property
  "list of size 5 always has 5 elements"
  Arbitrary.(make (Generator.list_repeat 5 Generator.int))
  (fun lst -> List.length lst = 5)

(* Frequency-based generation *)

let mostly_small_ints_prop =
  property "frequency weighted generator"
    Arbitrary.(
      make ~shrink:Shrinker.int ~print:Printer.int
        ~small:(fun x ->
          if x < 0 then
            -x
          else
            x)
        (Generator.frequency [ (9, Generator.int_range 0 10); (1, Generator.int_range 100 1_000);  ])
    )
    (fun n -> n >= 0)

(* Just verify non-negative *)

let tests = [
  color_reflexive_prop;
  point_distance_positive_prop;
  point_translation_prop;
  non_empty_list_prop;
  fixed_size_list_prop;
  mostly_small_ints_prop;

]

let () =
  Miniriot.run
  ~main:(fun ~args -> Test.Cli.main ~name:"propane-custom-examples" ~tests ~args)
  ~args:Env.args
  ()
