open Kernel

type error =
  | Entropy of Kernel.Random.Source.error
  | InvalidIntBound of { bound: int }
  | InvalidIntRange of { min: int; max: int }
  | InvalidInt32Bound of { bound: int32 }
  | InvalidInt32Range of { min: int32; max: int32 }
  | InvalidInt64Bound of { bound: int64 }
  | InvalidInt64Range of { min: int64; max: int64 }
  | InvalidFloatRange of { min: float; max: float }
  | InvalidProbability of { probability: float }
  | EmptyPopulation
  | InvalidSampleSize of { requested: int; available: int }

let ( let* ) = Result.and_then

let error_to_string = function
  | Entropy error -> String.concat
    ""
    [ "entropy failure: "; Kernel.Random.Source.error_to_string error ]
  | InvalidIntBound { bound } -> String.concat "" [ "invalid int bound: "; Int.to_string bound ]
  | InvalidIntRange { min; max } -> String.concat
    ""
    [ "invalid int range: "; Int.to_string min; " > "; Int.to_string max ]
  | InvalidInt32Bound { bound } -> String.concat
    ""
    [ "invalid int32 bound: "; Int32.to_string bound ]
  | InvalidInt32Range { min; max } -> String.concat
    ""
    [ "invalid int32 range: "; Int32.to_string min; " > "; Int32.to_string max ]
  | InvalidInt64Bound { bound } -> String.concat
    ""
    [ "invalid int64 bound: "; Int64.to_string bound ]
  | InvalidInt64Range { min; max } -> String.concat
    ""
    [ "invalid int64 range: "; Int64.to_string min; " > "; Int64.to_string max ]
  | InvalidFloatRange { min; max } -> String.concat
    ""
    [ "invalid float range: "; Float.to_string min; " > "; Float.to_string max ]
  | InvalidProbability { probability } -> String.concat
    ""
    [ "invalid probability: "; Float.to_string probability ]
  | EmptyPopulation -> "empty population"
  | InvalidSampleSize { requested; available } -> String.concat
    ""
    [
      "invalid sample size: requested ";
      Int.to_string requested;
      ", available ";
      Int.to_string available;
    ]

module Rng = struct
  type t =
    | Rng: {
        state: 'state;
        fill_bytes: 'state -> bytes -> unit;
      } -> t

  let make = fun ~state ~fill_bytes -> Rng { state; fill_bytes }

  let fill_bytes = fun (Rng rng) out ->
    rng.fill_bytes rng.state out

  let load_le32 = fun source offset ->
    let byte0 = Int32.of_int (Char.to_int (Bytes.get source offset)) in
    let byte1 = Int32.shift_left (Int32.of_int (Char.to_int (Bytes.get source (offset + 1)))) 8 in
    let byte2 = Int32.shift_left (Int32.of_int (Char.to_int (Bytes.get source (offset + 2)))) 16 in
    let byte3 = Int32.shift_left (Int32.of_int (Char.to_int (Bytes.get source (offset + 3)))) 24 in
    Int32.logor byte0 (Int32.logor byte1 (Int32.logor byte2 byte3))

  let store_le32 = fun target offset value ->
    Bytes.set target offset (Char.chr (Int32.to_int (Int32.logand value 0xffl)));
    Bytes.set
      target
      (offset + 1)
      (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical value 8) 0xffl)));
    Bytes.set
      target
      (offset + 2)
      (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical value 16) 0xffl)));
    Bytes.set
      target
      (offset + 3)
      (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical value 24) 0xffl)))

  let bits64 = fun rng ->
    let buffer = Bytes.create 8 in
    fill_bytes rng buffer;
    let low = Int64.of_int32 (load_le32 buffer 0) in
    let high = Int64.shift_left (Int64.of_int32 (load_le32 buffer 4)) 32 in
    Int64.logor low high

  let bits32 = fun rng ->
    let buffer = Bytes.create 4 in
    fill_bytes rng buffer;
    load_le32 buffer 0

  let bits = fun rng -> Int64.to_int (Int64.logand (bits64 rng) 0x3fff_ffffL)

  let standard_int = fun rng -> Int64.to_int (Int64.shift_right (bits64 rng) 1)

  let standard_float = fun rng -> Int64.to_float (Int64.shift_right_logical (bits64 rng) 11) /. 9_007_199_254_740_992.0

  module Standard = struct
    type state = {
      words: Int32.t array;
      buffer: bytes;
      mutable offset: int;
    }

    let sigma = [|
      Int32.of_string "0x61707865";
      Int32.of_string "0x3320646e";
      Int32.of_string "0x79622d32";
      Int32.of_string "0x6b206574";
    |]

    let rotl = fun value amount ->
      Int32.logor (Int32.shift_left value amount) (Int32.shift_right_logical value (32 - amount))

    let quarter_round = fun words a b c d ->
      let a_value = Array.get words a in
      let b_value = Array.get words b in
      let d_value = Array.get words d in
      let a_value = Int32.add a_value b_value in
      let d_value = rotl (Int32.logxor d_value a_value) 16 in
      Array.set words a a_value;
      Array.set words d d_value;
      let c_value = Int32.add (Array.get words c) d_value in
      let b_value = rotl (Int32.logxor b_value c_value) 12 in
      Array.set words c c_value;
      Array.set words b b_value;
      let a_value = Int32.add a_value b_value in
      let d_value = rotl (Int32.logxor d_value a_value) 8 in
      Array.set words a a_value;
      Array.set words d d_value;
      let c_value = Int32.add c_value d_value in
      let b_value = rotl (Int32.logxor b_value c_value) 7 in
      Array.set words c c_value;
      Array.set words b b_value

    let increment_counter = fun state ->
      let low = Int32.add (Array.get state.words 12) 1l in
      Array.set state.words 12 low;
      if Int32.equal low 0l then
        Array.set state.words 13 (Int32.add (Array.get state.words 13) 1l)

    let refill = fun state ->
      let working =
        Array.init 16
          (fun index ->
            Array.get state.words index)
      in
      for _round = 0 to 9 do
        quarter_round working 0 4 8 12;
        quarter_round working 1 5 9 13;
        quarter_round working 2 6 10 14;
        quarter_round working 3 7 11 15;
        quarter_round working 0 5 10 15;
        quarter_round working 1 6 11 12;
        quarter_round working 2 7 8 13;
        quarter_round working 3 4 9 14
      done;
      for index = 0 to 15 do
        store_le32
          state.buffer
          (index * 4)
          (Int32.add (Array.get working index) (Array.get state.words index))
      done;
      increment_counter state;
      state.offset <- 0

    let fill_bytes = fun state out ->
      let rec loop out_offset remaining =
        if remaining <= 0 then
          ()
        else (
          if state.offset >= 64 then
            refill state;
          let available = 64 - state.offset in
          let count =
            if remaining < available then
              remaining
            else
              available
          in
          Bytes.blit state.buffer state.offset out out_offset count;
          state.offset <- state.offset + count;
          loop (out_offset + count) (remaining - count)
        )
      in
      loop 0 (Bytes.length out)

    let derive_seed_bytes = fun seed ->
      let hash1 = Crypto.Hash.to_bytes (Crypto.Sha256.hash_string seed) in
      let hash2 = Crypto.Hash.to_bytes (Crypto.Sha256.hash_string (seed ^ "\001")) in
      let out = Bytes.create 40 in
      Bytes.blit hash1 0 out 0 32;
      Bytes.blit hash2 0 out 32 8;
      out

    let seed_bytes = fun ?seed () ->
      match seed with
      | Some seed -> Ok (derive_seed_bytes seed)
      | None ->
          let out = Bytes.create 40 in
          let* () =
            Result.map_error (fun error -> Entropy error) (Kernel.Random.Source.fill_bytes out)
          in
          Ok out

    let of_seed_bytes = fun seed ->
      let words = Array.make 16 0l in
      for index = 0 to 3 do
        Array.set words index (Array.get sigma index)
      done;
      for index = 0 to 7 do
        Array.set words (4 + index) (load_le32 seed (index * 4))
      done;
      Array.set words 12 0l;
      Array.set words 13 0l;
      Array.set words 14 (load_le32 seed 32);
      Array.set words 15 (load_le32 seed 36);
      { words; buffer = Bytes.create 64; offset = 64 }

    let create = fun ?seed () ->
      let* seed = seed_bytes ?seed () in
      Ok (make ~state:(of_seed_bytes seed) ~fill_bytes)
  end

  let standard = Standard.create
end

type 'value distribution = Rng.t -> ('value, error) Result.t

type 'value cell = {
  mutable value: 'value;
}

let default_rng = { value = None }

let init = fun ?seed () ->
  let* rng = Rng.standard ?seed () in
  default_rng.value <- Some rng;
  Ok ()

let ensure_default_rng = fun () ->
  match default_rng.value with
  | Some rng -> Ok rng
  | None ->
      let* rng = Rng.standard () in
      default_rng.value <- Some rng;
      Ok rng

let sample = fun ?rng distribution ->
  let* rng =
    match rng with
    | Some rng -> Ok rng
    | None -> ensure_default_rng ()
  in
  distribution rng

let int_aux = fun rng bound mask ->
  let rec loop () =
    let random = Int64.to_int (Int64.logand (Rng.bits64 rng) (Int64.of_int mask)) in
    let value = random mod bound in
    if random - value > mask - bound + 1 then
      loop ()
    else
      value
  in
  loop ()

let full_int_sample = fun rng bound ->
  if bound <= 0x3fff_ffff then
    int_aux rng bound 0x3fff_ffff
  else
    int_aux rng bound max_int

let rec int_range_sample = fun rng min max ->
  let span = max - min + 1 in
  if span > 0 then
    min + full_int_sample rng span
  else
    let candidate = Rng.standard_int rng in
    if candidate < min || candidate > max then
      int_range_sample rng min max
    else
      candidate

let int32_aux = fun rng bound ->
  let rec loop () =
    let random = Int32.shift_right_logical (Rng.bits32 rng) 1 in
    let value = Int32.rem random bound in
    let upper = Int32.add (Int32.sub 0x7fff_ffffl bound) 1l in
    if Int32.compare (Int32.sub random value) upper > 0 then
      loop ()
    else
      value
  in
  loop ()

let rec int32_range_sample = fun rng min max ->
  let span = Int32.add (Int32.sub max min) 1l in
  if Int32.compare span 0l <= 0 then
    let candidate = Rng.bits32 rng in
    if Int32.compare candidate min < 0 || Int32.compare candidate max > 0 then
      int32_range_sample rng min max
    else
      candidate
  else
    Int32.add min (int32_aux rng span)

let int64_aux = fun rng bound ->
  let rec loop () =
    let random = Int64.shift_right_logical (Rng.bits64 rng) 1 in
    let value = Int64.rem random bound in
    let upper = Int64.add (Int64.sub 0x7fff_ffff_ffff_ffffL bound) 1L in
    if Int64.compare (Int64.sub random value) upper > 0 then
      loop ()
    else
      value
  in
  loop ()

let rec int64_range_sample = fun rng min max ->
  let span = Int64.add (Int64.sub max min) 1L in
  if Int64.compare span 0L <= 0 then
    let candidate = Rng.bits64 rng in
    if Int64.compare candidate min < 0 || Int64.compare candidate max > 0 then
      int64_range_sample rng min max
    else
      candidate
  else
    Int64.add min (int64_aux rng span)

let float_range_sample = fun rng min max ->
  if Float.equal min max then
    min
  else
    min +. (Rng.standard_float rng *. (max -. min))

let array_to_list = fun values ->
  let rec loop index acc =
    if index < 0 then
      acc
    else
      loop (index - 1) (Array.get values index :: acc)
  in
  loop (Array.length values - 1) []

module Distribution = struct
  type 'value t = 'value distribution

  let sample = sample

  let map = fun fn distribution rng ->
    Result.map fn (distribution rng)

  let map2 = fun fn left right rng ->
    let* left = left rng in
    let* right = right rng in
    Ok (fn left right)

  let tuple = fun left right -> map2 (fun first second -> (first, second)) left right

  let option = fun distribution rng ->
    let pick = Int.rem (Rng.bits rng) 2 = 0 in
    if pick then
      Result.map (fun value -> Some value) (distribution rng)
    else
      Ok None

  let list = fun ~len distribution rng ->
    if len < 0 then
      Error (InvalidSampleSize { requested = len; available = 0 })
    else
      let rec loop count acc =
        if count <= 0 then
          Ok (List.rev acc)
        else
          let* value = distribution rng in
          loop (count - 1) (value :: acc)
      in
      loop len []

  let repeated = fun ~count distribution -> list ~len:count distribution

  let bool = fun rng -> Ok (Int.rem (Rng.bits rng) 2 = 0)

  let char = fun rng -> Ok (Char.chr (Int.rem (Rng.bits rng) 256))

  let standard_int = fun rng -> Ok (Rng.standard_int rng)

  let standard_int32 = fun rng -> Ok (Rng.bits32 rng)

  let standard_int64 = fun rng -> Ok (Rng.bits64 rng)

  let standard_float = fun rng -> Ok (Rng.standard_float rng)

  let bits = fun rng -> Ok (Rng.bits rng)

  let bits32 = fun rng -> Ok (Rng.bits32 rng)

  let bits64 = fun rng -> Ok (Rng.bits64 rng)

  let int = fun bound rng ->
    if bound <= 0 then
      Error (InvalidIntBound { bound })
    else
      Ok (full_int_sample rng bound)

  let int_range = fun ~min ~max rng ->
    if min > max then
      Error (InvalidIntRange { min; max })
    else
      Ok (int_range_sample rng min max)

  let int32 = fun bound rng ->
    if Int32.compare bound 0l <= 0 then
      Error (InvalidInt32Bound { bound })
    else
      Ok (int32_aux rng bound)

  let int32_range = fun ~min ~max rng ->
    if Int32.compare min max > 0 then
      Error (InvalidInt32Range { min; max })
    else
      Ok (int32_range_sample rng min max)

  let int64 = fun bound rng ->
    if Int64.compare bound 0L <= 0 then
      Error (InvalidInt64Bound { bound })
    else
      Ok (int64_aux rng bound)

  let int64_range = fun ~min ~max rng ->
    if Int64.compare min max > 0 then
      Error (InvalidInt64Range { min; max })
    else
      Ok (int64_range_sample rng min max)

  let float = fun bound rng -> Ok (bound *. Rng.standard_float rng)

  let float_range = fun ~min ~max rng ->
    if min > max then
      Error (InvalidFloatRange { min; max })
    else
      Ok (float_range_sample rng min max)

  let bernoulli = fun ~p rng ->
    if p < 0.0 || p > 1.0 then
      Error (InvalidProbability { probability = p })
    else
      Ok (Float.compare (Rng.standard_float rng) p < 0)

  let one_of_array = fun values rng ->
    let length = Array.length values in
    if length = 0 then
      Error EmptyPopulation
    else
      Ok (Array.get values (full_int_sample rng length))

  let one_of = fun values -> one_of_array (Array.of_list values)

  let one_of_vec = fun values -> one_of_array (Collections.Vector.to_array values)

  let choose_n_array = fun values count rng ->
    let length = Array.length values in
    if count < 0 || count > length then
      Error (InvalidSampleSize { requested = count; available = length })
    else
      let copy =
        Array.init length
          (fun index ->
            Array.get values index)
      in
      let rec shuffle index =
        if index >= count then
          Ok (
            Array.init count
              (fun take_index ->
                Array.get copy take_index)
          )
        else
          let swap_index = index + full_int_sample rng (length - index) in
          let current = Array.get copy index in
          let chosen = Array.get copy swap_index in
          Array.set copy index chosen;
          Array.set copy swap_index current;
          shuffle (index + 1)
      in
      shuffle 0

  let choose_n = fun values count rng ->
    Result.map array_to_list (choose_n_array (Array.of_list values) count rng)

  let choose_n_vec = fun values count rng ->
    Result.map
      Collections.Vector.of_list
      (Result.map array_to_list (choose_n_array (Collections.Vector.to_array values) count rng))
end

let bits = fun ?rng () -> sample ?rng Distribution.bits

let bits32 = fun ?rng () -> sample ?rng Distribution.bits32

let bits64 = fun ?rng () -> sample ?rng Distribution.bits64

let bool = fun ?rng () -> sample ?rng Distribution.bool

let char = fun ?rng () -> sample ?rng Distribution.char

let int = fun ?rng bound -> sample ?rng (Distribution.int bound)

let int_range ?rng ~min ~max () = sample ?rng (Distribution.int_range ~min ~max)

let int32 = fun ?rng bound -> sample ?rng (Distribution.int32 bound)

let int32_range ?rng ~min ~max () = sample ?rng (Distribution.int32_range ~min ~max)

let int64 = fun ?rng bound -> sample ?rng (Distribution.int64 bound)

let int64_range ?rng ~min ~max () = sample ?rng (Distribution.int64_range ~min ~max)

let float = fun ?rng bound -> sample ?rng (Distribution.float bound)

let float_range ?rng ~min ~max () = sample ?rng (Distribution.float_range ~min ~max)

let one_of = fun ?rng values -> sample ?rng (Distribution.one_of values)

let one_of_array = fun ?rng values -> sample ?rng (Distribution.one_of_array values)

let one_of_vec = fun ?rng values -> sample ?rng (Distribution.one_of_vec values)

let choose_n = fun ?rng values count -> sample ?rng (Distribution.choose_n values count)

let choose_n_array = fun ?rng values count -> sample ?rng (Distribution.choose_n_array values count)

let choose_n_vec = fun ?rng values count -> sample ?rng (Distribution.choose_n_vec values count)
