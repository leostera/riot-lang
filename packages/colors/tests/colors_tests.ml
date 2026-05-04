open Std
open Std.Collections
open Colors

let ( let* ) = fun result next ->
  match result with
  | Ok value -> next value
  | Error _ as error -> error

let expect = fun condition message ->
  if condition then
    Ok ()
  else
    Error message

let expect_string_equal = fun ~label ~expected ~actual ->
  expect
    (String.equal expected actual)
    (label ^ ": expected " ^ expected ^ ", got " ^ actual)

let expect_prefix = fun ~label ~prefix ~actual ->
  expect
    (String.starts_with ~prefix actual)
    (label ^ ": expected prefix " ^ prefix ^ ", got " ^ actual)

let expect_int_equal = fun ~label ~expected ~actual ->
  expect
    (Int.equal expected actual)
    (label ^ ": expected " ^ Int.to_string expected ^ ", got " ^ Int.to_string actual)

let expect_array_length = fun ~label ~expected ~actual ->
  expect_int_equal
    ~label
    ~expected
    ~actual:(Array.length actual)

let expect_ansi_equal = fun ~label ~expected ->
  fun (`ansi actual) -> expect_int_equal ~label ~expected ~actual

let expect_float_close = fun ~label ~epsilon ~expected ~actual ->
  expect
    (Float.abs (expected -. actual) <= epsilon)
    (label ^ ": expected " ^ Float.to_string expected ^ ", got " ^ Float.to_string actual)

let expect_finite = fun ~label value ->
  expect
    (Float.is_finite value)
    (label ^ ": expected finite float")

let expect_rgb_equal = fun ~label ~expected:(er, eg, eb) ->
  fun (`rgb (r, g, b)) ->
    let* () = expect_int_equal ~label:(label ^ " red") ~expected:er ~actual:r in
    let* () = expect_int_equal ~label:(label ^ " green") ~expected:eg ~actual:g in
    expect_int_equal ~label:(label ^ " blue") ~expected:eb ~actual:b

let expect_rgb_within = fun ~label ~tolerance ~expected:(er, eg, eb) ->
  fun (`rgb (r, g, b)) ->
    let within left right = Int.abs (left - right) <= tolerance in
    expect
      (within er r && within eg g && within eb b)
      (label
      ^ ": expected approx RGB("
      ^ Int.to_string er
      ^ ","
      ^ Int.to_string eg
      ^ ","
      ^ Int.to_string eb
      ^ "), got RGB("
      ^ Int.to_string r
      ^ ","
      ^ Int.to_string g
      ^ ","
      ^ Int.to_string b
      ^ ")")

let expect_lrgb_close = fun ~label ~epsilon ~expected:(er, eg, eb) ->
  fun (`lrgb (r, g, b)) ->
    let* () = expect_float_close ~label:(label ^ " red") ~epsilon ~expected:er ~actual:r in
    let* () = expect_float_close ~label:(label ^ " green") ~epsilon ~expected:eg ~actual:g in
    expect_float_close ~label:(label ^ " blue") ~epsilon ~expected:eb ~actual:b

let expect_xyz_close = fun ~label ~epsilon ~expected:(ex, ey, ez) ->
  fun (`xyz (x, y, z)) ->
    let* () = expect_float_close ~label:(label ^ " x") ~epsilon ~expected:ex ~actual:x in
    let* () = expect_float_close ~label:(label ^ " y") ~epsilon ~expected:ey ~actual:y in
    expect_float_close ~label:(label ^ " z") ~epsilon ~expected:ez ~actual:z

let expect_luv_close = fun ~label ~epsilon ~expected:(el, eu, ev) ->
  fun (`luv (l, u, v)) ->
    let* () = expect_float_close ~label:(label ^ " l") ~epsilon ~expected:el ~actual:l in
    let* () = expect_float_close ~label:(label ^ " u") ~epsilon ~expected:eu ~actual:u in
    expect_float_close ~label:(label ^ " v") ~epsilon ~expected:ev ~actual:v

let expect_uv_close = fun ~label ~epsilon ~expected:(eu, ev) ->
  fun (`uv (u, v)) ->
    let* () = expect_float_close ~label:(label ^ " u") ~epsilon ~expected:eu ~actual:u in
    expect_float_close ~label:(label ^ " v") ~epsilon ~expected:ev ~actual:v

let expect_xyz_finite = fun ~label ->
  fun (`xyz (x, y, z)) ->
    let* () = expect_finite ~label:(label ^ " x") x in
    let* () = expect_finite ~label:(label ^ " y") y in
    expect_finite ~label:(label ^ " z") z

let expect_luv_finite = fun ~label ->
  fun (`luv (l, u, v)) ->
    let* () = expect_finite ~label:(label ^ " l") l in
    let* () = expect_finite ~label:(label ^ " u") u in
    expect_finite ~label:(label ^ " v") v

let rgb_tuple_of = fun (`rgb (red, green, blue)) -> (red, green, blue)

let linearize_channel = fun channel ->
  match Linear_RGB.linearize (`rgb (channel, channel, channel)) with
  | `lrgb (value, _, _) -> value

let delinearize_channel = fun channel ->
  match Linear_RGB.delinearize (`lrgb (channel, channel, channel)) with
  | `rgb (value, _, _) -> value

let cube_level = fun level ->
  match level with
  | 0 -> 0
  | 1 -> 95
  | 2 -> 135
  | 3 -> 175
  | 4 -> 215
  | _ -> 255

let edge_case_colors = [
  `rgb (0, 0, 0);
  `rgb (255, 255, 255);
  `rgb (255, 0, 0);
  `rgb (0, 255, 0);
  `rgb (0, 0, 255);
  `rgb (255, 255, 0);
  `rgb (255, 0, 255);
  `rgb (0, 255, 255);
  `rgb (1, 1, 1);
  `rgb (4, 4, 4);
  `rgb (12, 12, 12);
  `rgb (98, 98, 98);
  `rgb (128, 128, 128);
  `rgb (193, 193, 193);
  `rgb (254, 254, 254);
  `rgb (200, 150, 100);
  `rgb (12, 34, 56);
  `rgb (255, 128, 0);
  `rgb (17, 200, 123);
  `rgb (90, 40, 210);
]

let blend_corpus = [
  `rgb (0, 0, 0);
  `rgb (255, 255, 255);
  `rgb (255, 0, 0);
  `rgb (0, 255, 0);
  `rgb (0, 0, 255);
  `rgb (255, 255, 0);
  `rgb (255, 0, 255);
  `rgb (0, 255, 255);
  `rgb (128, 128, 128);
  `rgb (255, 128, 0);
]

let mixes = [ (-1.0); 0.0; 0.25; 0.5; 0.75; 1.0; 2.0; ]

let rec for_each = fun items ~fn ->
  match items with
  | [] -> Ok ()
  | item :: rest ->
      let* () = fn item in
      for_each rest ~fn

let canonical_palette_index_for_rgb = fun rgb ->
  let target = rgb_tuple_of rgb in
  let rec loop index =
    if index > 255 then
      None
    else if rgb_tuple_of (ANSI.to_rgb (`ansi index)) = target then
      Some index
    else
      loop (index + 1)
  in
  loop 0

let test_to_string_formats_public_variants = fun _ctx ->
  let* () =
    expect_string_equal
      ~label:"ansi to_string"
      ~expected:"ANSI(9)"
      ~actual:(to_string (`ansi 9 :> color))
  in
  let* () =
    expect_string_equal
      ~label:"rgb to_string"
      ~expected:"RGB(255,128,0)"
      ~actual:(to_string (`rgb (255, 128, 0) :> color))
  in
  let* () =
    expect_prefix
      ~label:"linear rgb to_string"
      ~prefix:"LinearRGB("
      ~actual:(to_string (`lrgb (1.0, 0.5, 0.25) :> color))
  in
  let* () =
    expect_prefix
      ~label:"xyz to_string"
      ~prefix:"XYZ("
      ~actual:(to_string (`xyz (0.1, 0.2, 0.3) :> color))
  in
  let* () =
    expect_prefix
      ~label:"luv to_string"
      ~prefix:"LUV("
      ~actual:(to_string (`luv (0.1, 0.2, 0.3) :> color))
  in
  expect_prefix ~label:"uv to_string" ~prefix:"UV(" ~actual:(to_string (`uv (0.1, 0.2) :> color))

let test_ansi_known_values_and_clamping = fun _ctx ->
  let known_values = [
    ((-1), (0, 0, 0));
    (0, (0, 0, 0));
    (1, (128, 0, 0));
    (7, (192, 192, 192));
    (8, (128, 128, 128));
    (9, (255, 0, 0));
    (10, (0, 255, 0));
    (11, (255, 255, 0));
    (12, (0, 0, 255));
    (13, (255, 0, 255));
    (14, (0, 255, 255));
    (15, (255, 255, 255));
    (16, (0, 0, 0));
    (21, (0, 0, 255));
    (46, (0, 255, 0));
    (51, (0, 255, 255));
    (196, (255, 0, 0));
    (231, (255, 255, 255));
    (232, (8, 8, 8));
    (255, (238, 238, 238));
    (999, (238, 238, 238));
  ]
  in
  for_each
    known_values
    ~fn:(fun (index, expected) ->
      ANSI.to_rgb (`ansi index)
      |> expect_rgb_equal ~label:("ansi " ^ Int.to_string index) ~expected)

let test_ansi_formula_segments = fun _ctx ->
  let rec check_cube index =
    if index > 231 then
      Ok ()
    else
      let normalized = index - 16 in
      let red = normalized / 36 in
      let green = (normalized / 6) mod 6 in
      let blue = normalized mod 6 in
      let expected = (cube_level red, cube_level green, cube_level blue) in
      let* () =
        ANSI.to_rgb (`ansi index)
        |> expect_rgb_equal ~label:("ansi cube " ^ Int.to_string index) ~expected
      in
      check_cube (index + 1)
  in
  let rec check_grayscale index =
    if index > 255 then
      Ok ()
    else
      let shade = 8 + ((index - 232) * 10) in
      let expected = (shade, shade, shade) in
      let* () =
        ANSI.to_rgb (`ansi index)
        |> expect_rgb_equal ~label:("ansi grayscale " ^ Int.to_string index) ~expected
      in
      check_grayscale (index + 1)
  in
  let* () = check_cube 16 in
  check_grayscale 232

let test_ansi_palette_channels_stay_in_byte_range = fun _ctx ->
  let rec loop index =
    if index > 255 then
      Ok ()
    else
      match ANSI.to_rgb (`ansi index) with
      | `rgb (r, g, b) ->
          let in_range value = value >= 0 && value <= 255 in
          let* () =
            expect
              (in_range r && in_range g && in_range b)
              ("ansi index out of range: " ^ Int.to_string index)
          in
          loop (index + 1)
  in
  loop 0

let test_ansi_nearest_canonicalizes_palette_duplicates = fun _ctx ->
  let canonical_values = [
    ((0, 0, 0), 0);
    ((255, 0, 0), 9);
    ((0, 255, 0), 10);
    ((255, 255, 0), 11);
    ((0, 0, 255), 12);
    ((255, 0, 255), 13);
    ((0, 255, 255), 14);
    ((255, 255, 255), 15);
    ((95, 95, 95), 59);
    ((135, 135, 175), 103);
  ]
  in
  for_each
    canonical_values
    ~fn:(fun ((red, green, blue), expected) ->
      ANSI.nearest (`rgb (red, green, blue))
      |> expect_ansi_equal
        ~label:("ansi nearest exact RGB("
        ^ Int.to_string red
        ^ ","
        ^ Int.to_string green
        ^ ","
        ^ Int.to_string blue
        ^ ")")
        ~expected)

let test_ansi_nearest_roundtrips_palette_entries_to_canonical_indices = fun _ctx ->
  let rec loop index =
    if index > 255 then
      Ok ()
    else
      let palette_rgb = ANSI.to_rgb (`ansi index) in
      match canonical_palette_index_for_rgb palette_rgb with
      | None ->
          Error ("expected palette RGB to appear in the palette table for index "
          ^ Int.to_string index)
      | Some expected ->
          let* () =
            ANSI.nearest palette_rgb
            |> expect_ansi_equal
              ~label:("ansi nearest palette roundtrip " ^ Int.to_string index)
              ~expected
          in
          loop (index + 1)
  in
  loop 0

let test_ansi_nearest_representative_inputs = fun _ctx ->
  let representative_values = [
    (((-20), (-10), 5), 0);
    ((250, 10, 10), 9);
    ((2, 240, 240), 14);
    ((130, 140, 170), 103);
    ((100, 100, 100), 241);
    ((12, 34, 56), 235);
    ((17, 200, 123), 42);
    ((90, 40, 210), 56);
  ]
  in
  for_each
    representative_values
    ~fn:(fun ((red, green, blue), expected) ->
      ANSI.nearest (`rgb (red, green, blue))
      |> expect_ansi_equal
        ~label:("ansi nearest representative RGB("
        ^ Int.to_string red
        ^ ","
        ^ Int.to_string green
        ^ ","
        ^ Int.to_string blue
        ^ ")")
        ~expected)

let test_linear_rgb_known_values = fun _ctx ->
  let* () =
    Linear_RGB.linearize (`rgb (0, 0, 0))
    |> expect_lrgb_close ~label:"linearize black" ~epsilon:1.e-12 ~expected:(0.0, 0.0, 0.0)
  in
  let* () =
    Linear_RGB.linearize (`rgb (255, 255, 255))
    |> expect_lrgb_close ~label:"linearize white" ~epsilon:1.e-12 ~expected:(1.0, 1.0, 1.0)
  in
  let* () =
    Linear_RGB.linearize (`rgb (255, 0, 0))
    |> expect_lrgb_close ~label:"linearize red" ~epsilon:1.e-12 ~expected:(1.0, 0.0, 0.0)
  in
  let* () =
    Linear_RGB.linearize (`rgb (0, 255, 0))
    |> expect_lrgb_close ~label:"linearize green" ~epsilon:1.e-12 ~expected:(0.0, 1.0, 0.0)
  in
  let* () =
    Linear_RGB.linearize (`rgb (0, 0, 255))
    |> expect_lrgb_close ~label:"linearize blue" ~epsilon:1.e-12 ~expected:(0.0, 0.0, 1.0)
  in
  let* () =
    expect_float_close
      ~label:"linearize 128"
      ~epsilon:1.e-10
      ~expected:0.215_860_500_113_899_26
      ~actual:(linearize_channel 128)
  in
  let* () =
    expect_float_close
      ~label:"linearize 1"
      ~epsilon:1.e-12
      ~expected:0.000_303_526_983_548_837_5
      ~actual:(linearize_channel 1)
  in
  let* () =
    expect_float_close
      ~label:"linearize 12"
      ~epsilon:1.e-12
      ~expected:0.003_676_507_324_047_436
      ~actual:(linearize_channel 12)
  in
  let* () = expect_int_equal ~label:"delinearize 0" ~expected:0 ~actual:(delinearize_channel 0.0) in
  let* () = expect_int_equal ~label:"delinearize 1" ~expected:255 ~actual:(delinearize_channel 1.0) in
  let* () =
    expect_int_equal
      ~label:"delinearize mid gray"
      ~expected:128
      ~actual:(delinearize_channel 0.215_860_500_113_899_26)
  in
  let* () =
    expect_int_equal
      ~label:"delinearize channel 1"
      ~expected:1
      ~actual:(delinearize_channel 0.000_303_526_983_548_837_5)
  in
  let* () =
    expect_int_equal
      ~label:"delinearize negative clamp"
      ~expected:0
      ~actual:(delinearize_channel (-0.5))
  in
  expect_int_equal ~label:"delinearize upper clamp" ~expected:255 ~actual:(delinearize_channel 1.5)

let test_linear_rgb_exhaustive_channel_roundtrip = fun _ctx ->
  let rec loop channel =
    if channel > 255 then
      Ok ()
    else
      let linear = linearize_channel channel in
      let* () =
        expect
          (linear >= 0.0 && linear <= 1.0)
          ("linearize out of range for channel " ^ Int.to_string channel)
      in
      let* () =
        expect_int_equal
          ~label:("channel roundtrip " ^ Int.to_string channel)
          ~expected:channel
          ~actual:(delinearize_channel linear)
      in
      loop (channel + 1)
  in
  loop 0

let test_xyz_known_conversions = fun _ctx ->
  let* () =
    Linear_RGB.to_xyz (`lrgb (0.0, 0.0, 0.0))
    |> expect_xyz_close ~label:"xyz black" ~epsilon:1.e-12 ~expected:(0.0, 0.0, 0.0)
  in
  let* () =
    Linear_RGB.to_xyz (`lrgb (1.0, 1.0, 1.0))
    |> expect_xyz_close ~label:"xyz white" ~epsilon:3.e-4 ~expected:(0.950_47, 1.0, 1.088_83)
  in
  let* () =
    Linear_RGB.to_xyz (`lrgb (1.0, 0.0, 0.0))
    |> expect_xyz_close
      ~label:"xyz red"
      ~epsilon:1.e-12
      ~expected:(0.412_390_799_265_959_48, 0.212_639_005_871_510_36, 0.019_330_818_715_591_851)
  in
  let* () =
    Linear_RGB.to_xyz (`lrgb (0.0, 1.0, 0.0))
    |> expect_xyz_close
      ~label:"xyz green"
      ~epsilon:1.e-12
      ~expected:(0.357_584_339_383_877_96, 0.715_168_678_767_755_93, 0.119_194_779_794_625_99)
  in
  let* () =
    Linear_RGB.to_xyz (`lrgb (0.0, 0.0, 1.0))
    |> expect_xyz_close
      ~label:"xyz blue"
      ~epsilon:1.e-12
      ~expected:(0.180_480_788_401_834_29, 0.072_192_315_360_733_715, 0.950_532_152_249_660_58)
  in
  XYZ.to_linear_rgb White_reference.d65
  |> expect_lrgb_close ~label:"d65 to linear rgb" ~epsilon:3.e-4 ~expected:(1.0, 1.0, 1.0)

let test_uv_and_luv_known_values = fun _ctx ->
  let* () =
    XYZ.to_uv (`xyz (0.0, 0.0, 0.0))
    |> expect_uv_close ~label:"uv zero" ~epsilon:1.e-12 ~expected:(0.0, 0.0)
  in
  let* () =
    XYZ.to_uv White_reference.d65
    |> expect_uv_close
      ~label:"uv d65"
      ~epsilon:1.e-12
      ~expected:(0.197_839_824_821_407_77, 0.468_336_302_932_409_7)
  in
  let* () =
    XYZ.to_luv White_reference.d65
    |> expect_luv_close ~label:"luv d65" ~epsilon:1.e-12 ~expected:(1.0, 0.0, 0.0)
  in
  let* () =
    XYZ.to_luv_with_ref White_reference.d65 ~wref:White_reference.d65
    |> expect_luv_close ~label:"luv explicit d65" ~epsilon:1.e-12 ~expected:(1.0, 0.0, 0.0)
  in
  LUV.to_xyz (`luv (1.0, 0.0, 0.0))
  |> expect_xyz_close
    ~label:"xyz from normalized d65"
    ~epsilon:1.e-12
    ~expected:(0.950_47, 1.0, 1.088_83)

let test_custom_white_reference_roundtrip_and_validation = fun _ctx ->
  let custom_white = `xyz (1.0, 1.0, 1.0) in
  let sample = `xyz (0.2, 0.3, 0.4) in
  let luv = XYZ.to_luv_with_ref sample ~wref:custom_white in
  let* () =
    LUV.to_xyz_with_ref luv ~wref:custom_white
    |> expect_xyz_close ~label:"custom white roundtrip" ~epsilon:1.e-9 ~expected:(0.2, 0.3, 0.4)
  in
  let invalid_xyz_ref =
    try
      let _ = XYZ.to_luv_with_ref sample ~wref:(`xyz (0.0, 0.0, 0.0)) in
      Error "expected invalid white reference to raise in XYZ.to_luv_with_ref"
    with
    | Invalid_argument _ -> Ok ()
  in
  let* () = invalid_xyz_ref in
  try
    let _ = LUV.to_xyz_with_ref luv ~wref:(`xyz (0.0, 0.0, 0.0)) in
    Error "expected invalid white reference to raise in LUV.to_xyz_with_ref"
  with
  | Invalid_argument _ -> Ok ()

let test_named_white_references_stay_stable = fun _ctx ->
  let references = [
    ("d50", White_reference.d50, (0.209_160_052_820_386_27, 0.488_073_384_544_885_14));
    ("d55", White_reference.d55, (0.204_434_630_305_924_43, 0.480_736_103_121_099_05));
    ("d65", White_reference.d65, (0.197_839_824_821_407_77, 0.468_336_302_932_409_7));
    ("d75", White_reference.d75, (0.193_535_437_106_383_16, 0.458_508_543_033_064_6));
    (
      "equal_energy",
      White_reference.equal_energy,
      (0.210_526_315_789_473_67, 0.473_684_210_526_315_76)
    );
  ]
  in
  let* () =
    for_each
      references
      ~fn:(fun (label, wref, expected_uv) ->
        XYZ.to_uv wref
        |> expect_uv_close ~label:("white reference " ^ label) ~epsilon:1.e-12 ~expected:expected_uv)
  in
  let sample = RGB.to_xyz (`rgb (200, 150, 100)) in
  let* () =
    for_each
      references
      ~fn:(fun (label, wref, _) ->
        let luv = XYZ.to_luv_with_ref sample ~wref in
        LUV.to_xyz_with_ref luv ~wref
        |> expect_xyz_close
          ~label:("white reference roundtrip " ^ label)
          ~epsilon:1.e-7
          ~expected:(
            match sample with
            | `xyz (x, y, z) -> (x, y, z)
          ))
  in
  match (
    XYZ.to_luv_with_ref sample ~wref:White_reference.d50,
    XYZ.to_luv_with_ref sample ~wref:White_reference.d65
  ) with
  | (`luv (_, u50, v50), `luv (_, u65, v65)) ->
      expect
        (not (Float.equal u50 u65 && Float.equal v50 v65))
        "expected different white references to change LUV chromatic coordinates"

let test_rgb_xyz_roundtrip_corpus = fun _ctx ->
  for_each
    edge_case_colors
    ~fn:(fun rgb ->
      let expected =
        match rgb with
        | `rgb (r, g, b) -> (r, g, b)
      in
      RGB.to_xyz rgb
      |> XYZ.to_rgb
      |> expect_rgb_within ~label:"rgb xyz roundtrip" ~tolerance:1 ~expected)

let test_xyz_luv_roundtrip_corpus_and_finite_values = fun _ctx ->
  for_each
    edge_case_colors
    ~fn:(fun rgb ->
      let xyz = RGB.to_xyz rgb in
      let luv = RGB.to_luv rgb in
      let* () = expect_xyz_finite ~label:"rgb to xyz finite" xyz in
      let* () = expect_luv_finite ~label:"rgb to luv finite" luv in
      let* () =
        LUV.to_xyz luv
        |> expect_xyz_finite ~label:"luv to xyz finite"
      in
      let* () =
        LUV.to_xyz luv
        |> expect_xyz_close
          ~label:"xyz luv xyz roundtrip"
          ~epsilon:1.e-7
          ~expected:(
            match xyz with
            | `xyz (x, y, z) -> (x, y, z)
          )
      in
      let expected =
        match rgb with
        | `rgb (r, g, b) -> (r, g, b)
      in
      LUV.to_rgb luv
      |> expect_rgb_within ~label:"rgb luv roundtrip" ~tolerance:1 ~expected)

let test_rgb_hex_known_values_and_clamping = fun _ctx ->
  let* () =
    expect_string_equal
      ~label:"to_hex orange"
      ~expected:"#ff8000"
      ~actual:(RGB.to_hex (`rgb (255, 128, 0)))
  in
  let* () =
    expect_string_equal
      ~label:"to_hex clamps channels"
      ~expected:"#00ff10"
      ~actual:(RGB.to_hex (`rgb ((-20), 300, 16)))
  in
  let* () =
    match RGB.from_hex "#ff8000" with
    | Ok rgb -> expect_rgb_equal ~label:"from_hex long lowercase" ~expected:(255, 128, 0) rgb
    | Error message -> Error ("expected RGB.from_hex #ff8000 to succeed, got error: " ^ message)
  in
  let* () =
    match RGB.from_hex "FF8000" with
    | Ok rgb ->
        expect_rgb_equal ~label:"from_hex uppercase without hash" ~expected:(255, 128, 0) rgb
    | Error message -> Error ("expected RGB.from_hex FF8000 to succeed, got error: " ^ message)
  in
  match RGB.from_hex "  #00Ff7F  " with
  | Ok rgb -> expect_rgb_equal ~label:"from_hex trims whitespace" ~expected:(0, 255, 127) rgb
  | Error message ->
      Error ("expected RGB.from_hex whitespace-trimmed input to succeed, got error: " ^ message)

let test_rgb_hex_rejects_invalid_inputs = fun _ctx ->
  let invalid_values = [ ""; "#12345"; "#1234567"; "#gg0000"; "xyzxyz"; "#12_345"; ] in
  for_each
    invalid_values
    ~fn:(fun value ->
      match RGB.from_hex value with
      | Ok rgb ->
          Error ("expected RGB.from_hex to reject " ^ value ^ ", got " ^ to_string (rgb :> color))
      | Error _ -> Ok ())

let test_rgb_hex_roundtrips_edge_case_corpus = fun _ctx ->
  for_each
    edge_case_colors
    ~fn:(fun rgb ->
      let expected = rgb_tuple_of rgb in
      match RGB.from_hex (RGB.to_hex rgb) with
      | Ok parsed -> expect_rgb_equal ~label:"rgb hex roundtrip" ~expected parsed
      | Error message ->
          Error ("expected RGB hex roundtrip to parse successfully, got error: " ^ message))

let test_metrics_and_distance_helpers = fun _ctx ->
  let sample_luv_a = `luv (0.1, (-0.2), 0.3) in
  let sample_luv_b = `luv (0.4, 0.2, (-0.1)) in
  let* () =
    expect_float_close
      ~label:"luv distance normalized lightness"
      ~epsilon:1.e-12
      ~expected:1.0
      ~actual:(LUV.distance (`luv (0.0, 0.0, 0.0)) (`luv (1.0, 0.0, 0.0)))
  in
  let* () =
    expect_float_close
      ~label:"luv distance symmetry"
      ~epsilon:1.e-12
      ~expected:(LUV.distance sample_luv_a sample_luv_b)
      ~actual:(LUV.distance sample_luv_b sample_luv_a)
  in
  let* () =
    expect_float_close
      ~label:"rgb distance_luv black white"
      ~epsilon:1.e-6
      ~expected:1.0
      ~actual:(RGB.distance_luv (`rgb (0, 0, 0)) (`rgb (255, 255, 255)))
  in
  let* () =
    expect_float_close
      ~label:"relative luminance black"
      ~epsilon:1.e-12
      ~expected:0.0
      ~actual:(RGB.relative_luminance (`rgb (0, 0, 0)))
  in
  let* () =
    expect_float_close
      ~label:"relative luminance white"
      ~epsilon:1.e-12
      ~expected:1.0
      ~actual:(RGB.relative_luminance (`rgb (255, 255, 255)))
  in
  let* () =
    expect_float_close
      ~label:"relative luminance mid gray"
      ~epsilon:1.e-10
      ~expected:0.215_860_500_113_899_26
      ~actual:(RGB.relative_luminance (`rgb (128, 128, 128)))
  in
  let* () =
    expect_float_close
      ~label:"relative luminance red"
      ~epsilon:1.e-12
      ~expected:0.212_639_005_871_510_36
      ~actual:(RGB.relative_luminance (`rgb (255, 0, 0)))
  in
  let* () =
    expect_float_close
      ~label:"contrast ratio black white"
      ~epsilon:1.e-10
      ~expected:21.0
      ~actual:(RGB.contrast_ratio (`rgb (0, 0, 0)) (`rgb (255, 255, 255)))
  in
  let* () =
    expect_float_close
      ~label:"contrast ratio identical"
      ~epsilon:1.e-12
      ~expected:1.0
      ~actual:(RGB.contrast_ratio (`rgb (12, 34, 56)) (`rgb (12, 34, 56)))
  in
  expect_float_close
    ~label:"contrast ratio symmetry"
    ~epsilon:1.e-12
    ~expected:(RGB.contrast_ratio (`rgb (255, 255, 255)) (`rgb (0, 0, 255)))
    ~actual:(RGB.contrast_ratio (`rgb (0, 0, 255)) (`rgb (255, 255, 255)))

let test_blend_behavior_and_ranges = fun _ctx ->
  let luv_a = `luv (0.2, (-0.4), 0.8) in
  let luv_b = `luv (0.6, 0.2, (-0.4)) in
  let gray_a = `rgb (32, 32, 32) in
  let gray_b = `rgb (224, 224, 224) in
  let identical = `rgb (12, 34, 56) in
  let* () =
    LUV.blend luv_a luv_b ~mix:0.0
    |> expect_luv_close ~label:"luv blend start" ~epsilon:1.e-12 ~expected:(0.2, (-0.4), 0.8)
  in
  let* () =
    LUV.blend luv_a luv_b ~mix:1.0
    |> expect_luv_close ~label:"luv blend end" ~epsilon:1.e-12 ~expected:(0.6, 0.2, (-0.4))
  in
  let* () =
    LUV.blend luv_a luv_b ~mix:0.5
    |> expect_luv_close ~label:"luv blend midpoint" ~epsilon:1.e-12 ~expected:(0.4, (-0.1), 0.2)
  in
  let* () =
    LUV.blend luv_a luv_b ~mix:(-1.0)
    |> expect_luv_close ~label:"luv blend lower clamp" ~epsilon:1.e-12 ~expected:(0.2, (-0.4), 0.8)
  in
  let* () =
    LUV.blend luv_a luv_b ~mix:2.0
    |> expect_luv_close ~label:"luv blend upper clamp" ~epsilon:1.e-12 ~expected:(0.6, 0.2, (-0.4))
  in
  let* () =
    RGB.blend identical identical ~mix:0.25
    |> expect_rgb_equal ~label:"rgb blend identical 0.25" ~expected:(12, 34, 56)
  in
  let* () =
    RGB.blend identical identical ~mix:0.5
    |> expect_rgb_equal ~label:"rgb blend identical 0.5" ~expected:(12, 34, 56)
  in
  let* () =
    RGB.blend identical identical ~mix:0.75
    |> expect_rgb_equal ~label:"rgb blend identical 0.75" ~expected:(12, 34, 56)
  in
  let* () =
    RGB.blend gray_a gray_b ~mix:0.0
    |> expect_rgb_equal ~label:"rgb blend clamp start" ~expected:(32, 32, 32)
  in
  let* () =
    RGB.blend gray_a gray_b ~mix:1.0
    |> expect_rgb_equal ~label:"rgb blend clamp end" ~expected:(224, 224, 224)
  in
  let* () =
    match RGB.blend gray_a gray_b ~mix:0.5 with
    | `rgb (r, g, b) -> expect (r = g && g = b) "expected black/white midpoint to stay grayscale"
  in
  let* () =
    match RGB.blend gray_a gray_b ~mix:0.25 with
    | `rgb (r, g, b) -> expect (r = g && g = b) "expected grayscale blend to stay grayscale"
  in
  let rec check_pairs lefts =
    match lefts with
    | [] -> Ok ()
    | left :: rest ->
        let* () =
          for_each
            blend_corpus
            ~fn:(fun right ->
              for_each
                mixes
                ~fn:(fun mix ->
                  match RGB.blend left right ~mix with
                  | `rgb (r, g, b) ->
                      let in_range value = value >= 0 && value <= 255 in
                      expect
                        (in_range r && in_range g && in_range b)
                        "expected blended rgb channels to stay within byte range"))
        in
        check_pairs rest
  in
  check_pairs blend_corpus

let test_blend_regressions_and_gradients = fun _ctx ->
  let blue = `rgb (0, 0, 255) in
  let yellow = `rgb (255, 255, 0) in
  let red = `rgb (255, 0, 0) in
  let magenta = `rgb (255, 0, 255) in
  let green = `rgb (0, 255, 0) in
  let gray_start = `rgb (32, 32, 32) in
  let gray_finish = `rgb (224, 224, 224) in
  let* () =
    LUV.blend_unclamped (`luv (0.2, (-0.4), 0.8)) (`luv (0.6, 0.2, (-0.4))) ~mix:(-0.5)
    |> expect_luv_close
      ~label:"luv blend_unclamped extrapolates below zero"
      ~epsilon:1.e-12
      ~expected:(0.0, (-0.7), 1.4)
  in
  let* () =
    LUV.blend_unclamped (`luv (0.2, (-0.4), 0.8)) (`luv (0.6, 0.2, (-0.4))) ~mix:1.5
    |> expect_luv_close
      ~label:"luv blend_unclamped extrapolates above one"
      ~epsilon:1.e-12
      ~expected:(0.8, 0.5, (-1.0))
  in
  let* () =
    RGB.blend red blue ~mix:(-1.0)
    |> expect_rgb_equal ~label:"rgb blend clamps below zero" ~expected:(255, 0, 0)
  in
  let* () =
    RGB.blend red blue ~mix:2.0
    |> expect_rgb_equal ~label:"rgb blend clamps above one" ~expected:(0, 0, 255)
  in
  let* () =
    RGB.blend blue yellow ~mix:0.5
    |> expect_rgb_equal ~label:"rgb blend blue yellow midpoint regression" ~expected:(156, 156, 171)
  in
  let* () =
    RGB.blend red blue ~mix:0.5
    |> expect_rgb_equal ~label:"rgb blend red blue midpoint regression" ~expected:(190, 0, 144)
  in
  let* () =
    RGB.blend green magenta ~mix:0.5
    |> expect_rgb_equal
      ~label:"rgb blend green magenta midpoint regression"
      ~expected:(183, 182, 183)
  in
  let* () =
    expect_array_length
      ~label:"luv gradient empty"
      ~expected:0
      ~actual:(LUV.gradient (`luv (0.1, 0.2, 0.3)) (`luv (0.4, 0.5, 0.6)) ~steps:0)
  in
  let luv_single = LUV.gradient (`luv (0.1, 0.2, 0.3)) (`luv (0.4, 0.5, 0.6)) ~steps:1 in
  let* () = expect_array_length ~label:"luv gradient single" ~expected:1 ~actual:luv_single in
  let* () =
    Array.get_unchecked luv_single ~at:0
    |> expect_luv_close
      ~label:"luv gradient single endpoint"
      ~epsilon:1.e-12
      ~expected:(0.1, 0.2, 0.3)
  in
  let rgb_empty = RGB.gradient blue yellow ~steps:0 in
  let* () = expect_array_length ~label:"rgb gradient empty" ~expected:0 ~actual:rgb_empty in
  let rgb_single = RGB.gradient blue yellow ~steps:1 in
  let* () = expect_array_length ~label:"rgb gradient single" ~expected:1 ~actual:rgb_single in
  let* () =
    Array.get_unchecked rgb_single ~at:0
    |> expect_rgb_equal ~label:"rgb gradient single endpoint" ~expected:(0, 0, 255)
  in
  let gradient = RGB.gradient gray_start gray_finish ~steps:10 in
  let* () = expect_array_length ~label:"rgb gradient size" ~expected:10 ~actual:gradient in
  let* () =
    Array.get_unchecked gradient ~at:0
    |> expect_rgb_equal ~label:"rgb gradient first endpoint" ~expected:(32, 32, 32)
  in
  let* () =
    Array.get_unchecked gradient ~at:9
    |> expect_rgb_equal ~label:"rgb gradient last endpoint" ~expected:(224, 224, 224)
  in
  let midpoint_gradient = RGB.gradient red blue ~steps:3 in
  let* () =
    Array.get_unchecked midpoint_gradient ~at:1
    |> expect_rgb_equal ~label:"rgb gradient midpoint matches blend" ~expected:(190, 0, 144)
  in
  let rec check_gray_gradient index previous =
    if index >= Array.length gradient then
      Ok ()
    else
      match Array.get_unchecked gradient ~at:index with
      | `rgb (red, green, blue) ->
          let* () =
            expect (red = green && green = blue) "expected grayscale gradient to stay grayscale"
          in
          let* () = expect (red >= previous) "expected grayscale gradient to stay monotone" in
          check_gray_gradient (index + 1) red
  in
  check_gray_gradient 0 0

let tests =
  Test.[
    case "to_string formats every public color variant" test_to_string_formats_public_variants;
    case "ANSI known values and clamp behavior stay stable" test_ansi_known_values_and_clamping;
    case "ANSI cube and grayscale segments match their formulas" test_ansi_formula_segments;
    case "ANSI palette outputs stay within byte range" test_ansi_palette_channels_stay_in_byte_range;
    case
      "ANSI.nearest canonicalizes duplicate palette colors"
      test_ansi_nearest_canonicalizes_palette_duplicates;
    case
      "ANSI.nearest roundtrips palette entries to canonical indices"
      test_ansi_nearest_roundtrips_palette_entries_to_canonical_indices;
    case
      "ANSI.nearest matches representative off-palette inputs"
      test_ansi_nearest_representative_inputs;
    case "linear RGB known values and clamps stay correct" test_linear_rgb_known_values;
    case
      "linear RGB roundtrips every byte channel exactly"
      test_linear_rgb_exhaustive_channel_roundtrip;
    case "XYZ conversions match known matrix values" test_xyz_known_conversions;
    case "UV and normalized LUV conversions match known values" test_uv_and_luv_known_values;
    case
      "custom white references roundtrip and invalid refs raise"
      test_custom_white_reference_roundtrip_and_validation;
    case
      "named white references stay stable and roundtrip correctly"
      test_named_white_references_stay_stable;
    case "RGB XYZ RGB roundtrips stay within quantization tolerance" test_rgb_xyz_roundtrip_corpus;
    case
      "XYZ and LUV conversions stay finite on an edge-case corpus"
      test_xyz_luv_roundtrip_corpus_and_finite_values;
    case
      "RGB hex codecs handle known values and clamp output"
      test_rgb_hex_known_values_and_clamping;
    case "RGB hex parsing rejects invalid inputs" test_rgb_hex_rejects_invalid_inputs;
    case "RGB hex codecs roundtrip the edge-case corpus" test_rgb_hex_roundtrips_edge_case_corpus;
    case
      "distance and accessibility helpers stay numerically stable"
      test_metrics_and_distance_helpers;
    case "blend behavior clamps endpoints and keeps outputs in range" test_blend_behavior_and_ranges;
    case "blend regressions and gradient helpers stay stable" test_blend_regressions_and_gradients;
  ]

let main ~args = Test.Cli.main ~name:"colors" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
