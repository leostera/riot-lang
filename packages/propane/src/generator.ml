open Std

module Buffer = IO.Buffer
module Array = Collections.Array

(* Internal representation: a generator is a function from (Random.Rng, size) to value *)

type 'value t = {
  run: Random.Rng.t -> int -> 'value;
}

let sample = fun value -> Result.unwrap value

let random_bits = fun rnd -> sample (Random.bits ~rng:rnd ())

let random_int = fun rnd bound -> sample (Random.int ~rng:rnd bound)

let random_int32 = fun rnd bound -> sample (Random.int32 ~rng:rnd bound)

let random_int64 = fun rnd bound -> sample (Random.int64 ~rng:rnd bound)

let random_float = fun rnd bound -> sample (Random.float ~rng:rnd bound)

let random_bool = fun rnd -> sample (Random.bool ~rng:rnd ())

let random_int_range = fun rnd ~low ~high ->
  sample
    (Random.int_range ~rng:rnd ~min:low ~max:high ())

let random_int32_range = fun rnd ~low ~high ->
  sample
    (Random.int32_range ~rng:rnd ~min:low ~max:high ())

let random_int64_range = fun rnd ~low ~high ->
  sample
    (Random.int64_range ~rng:rnd ~min:low ~max:high ())

let random_float_range = fun rnd ~low ~high ->
  sample
    (Random.float_range ~rng:rnd ~min:low ~max:high ())

let invalid_arg = fun message -> raise (Invalid_argument message)

(* Helper: convert char list to string *)

let string_of_char_list = fun chars ->
  String.concat
    ""
    (List.map chars ~fn:(fun char -> String.make ~len:1 ~char))

let build_list_values = fun rnd size gen len ->
  let rec loop remaining acc =
    if remaining <= 0 then
      List.reverse acc
    else
      loop (remaining - 1) (gen.run rnd size :: acc)
  in
  loop len []

let build_string = fun rnd size char_gen len ->
  let buffer = Buffer.create ~size:len in
  let rec loop remaining =
    if remaining <= 0 then
      Buffer.contents buffer
    else (
      Buffer.add_char buffer (char_gen.run rnd size);
      loop (remaining - 1)
    )
  in
  loop len

(* === CONSTANTS === *)

let return = fun v ->
  {
    run = (fun _rnd _size -> v);
  }

let exactly = return

(* === TRANSFORMATIONS === *)

let map = fun f gen ->
  {
    run = (fun rnd size -> f (gen.run rnd size));
  }

let map2 = fun f gen1 gen2 ->
  {
    run =
      (fun rnd size ->
        let v1 = gen1.run rnd size in
        let v2 = gen2.run rnd size in
        f v1 v2);
  }

let map3 = fun f gen1 gen2 gen3 ->
  {
    run =
      (fun rnd size ->
        let v1 = gen1.run rnd size in
        let v2 = gen2.run rnd size in
        let v3 = gen3.run rnd size in
        f v1 v2 v3);
  }

let and_then = fun gen f ->
  {
    run =
      (fun rnd size ->
        let v = gen.run rnd size in
        let gen' = f v in
        gen'.run rnd size);
  }

(* === CHOICE COMBINATORS === *)

let one_of = fun gens ->
  match gens with
  | [] -> invalid_arg "Generator.one_of: empty list"
  | _ ->
      let choices = Array.from_list gens in
      {
        run =
          (fun rnd size ->
            let idx = random_int rnd (Array.length choices) in
            let gen = Array.get_unchecked choices ~at:idx in
            gen.run rnd size);
      }

let frequency = fun weighted_gens ->
  match weighted_gens with
  | [] -> invalid_arg "Generator.frequency: empty list"
  | _ ->
      let choices = Array.from_list weighted_gens in
      let count = Array.length choices in
      let cumulative = Array.make ~count ~value:0 in
      let rec populate index total =
        if index >= count then
          total
        else
          let (weight, _) = Array.get_unchecked choices ~at:index in
          if weight <= 0 then
            invalid_arg "Generator.frequency: non-positive weight";
        let total = total + weight in
        Array.set_unchecked cumulative ~at:index ~value:total;
        populate (index + 1) total
      in
      let total_weight = populate 0 0 in
      {
        run =
          (fun rnd size ->
            let target = random_int rnd total_weight in
            let rec find low high =
              if low >= high then
                low
              else
                let mid = low + ((high - low) / 2) in
                let boundary = Array.get_unchecked cumulative ~at:mid in
                if target < boundary then
                  find low mid
                else
                  find (mid + 1) high
            in
            let index = find 0 (count - 1) in
            let (_, gen) = Array.get_unchecked choices ~at:index in
            gen.run rnd size);
      }

(* === SIZE CONTROL === *)

let sized = fun f ->
  {
    run = (fun rnd size -> (f size).run rnd size);
  }

let resize = fun new_size gen ->
  {
    run = (fun rnd _size -> gen.run rnd new_size);
  }

(* === RECURSIVE GENERATORS === *)

let delay = fun f ->
  {
    run = (fun rnd size -> (f ()).run rnd size);
  }

let fix = fun f ->
  let rec self n = f self n in
  fun n ->
    {
      run = (fun rnd size -> (self n).run rnd size);
    }

(* === PRIMITIVE GENERATORS === *)

let int = {
  run = (fun rnd _size -> random_int_range rnd ~low:Int.min_int ~high:Int.max_int);
}

let int32 = {
  run = (fun rnd _size -> random_int32_range rnd ~low:Int32.min_int ~high:Int32.max_int);
}

let int64 = {
  run = (fun rnd _size -> random_int64_range rnd ~low:Int64.min_int ~high:Int64.max_int);
}

let int_range = fun low high ->
  if low > high then
    invalid_arg "Generator.int_range: low > high";
  {
    run =
      (fun rnd _size ->
        if low = high then
          low
        else
          random_int_range rnd ~low ~high);
  }

let int32_range = fun low high ->
  if low > high then
    invalid_arg "Generator.int32_range: low > high";
  {
    run =
      (fun rnd _size ->
        if low = high then
          low
        else
          random_int32_range rnd ~low ~high);
  }

let int64_range = fun low high ->
  if low > high then
    invalid_arg "Generator.int64_range: low > high";
  {
    run =
      (fun rnd _size ->
        if low = high then
          low
        else
          random_int64_range rnd ~low ~high);
  }

let int_bound = fun n ->
  if n < 0 then
    invalid_arg "Generator.int_bound: negative bound";
  int_range 0 n

let small_int = int_range 0 100

let big_int = int

let positive_int = int_range 0 Int.max_int

let negative_int = int_range Int.min_int 0

let non_zero_int =
  let pos: int t = int_range 1 Int.max_int in
  let neg: int t = int_range Int.min_int (-1) in
  one_of [ pos; neg ]

(* Floats *)

let float = {
  run = (fun rnd _size -> random_float_range rnd ~low:(-.1_000_000.0) ~high:1_000_000.0);
}

let float_range = fun low high ->
  if low > high then
    invalid_arg "Generator.float_range: low > high";
  {
    run = (fun rnd _size -> random_float_range rnd ~low ~high);
  }

let float_positive = {
  run = (fun rnd _size -> random_float_range rnd ~low:0.0 ~high:1_000_000.0);
}

let float_negative = map (fun f -> -.f) float_positive

(* Booleans *)

let bool = {
  run = (fun rnd _size -> random_bool rnd);
}

let weighted_bool = fun weight_true weight_false ->
  if weight_true <= 0 || weight_false <= 0 then
    invalid_arg "Generator.weighted_bool: non-positive weights";
  frequency [ (weight_true, return true); (weight_false, return false); ]

(* Characters *)

let char = {
  run = (fun rnd _size -> Char.from_int_unchecked (random_int rnd 256));
}

let char_range = fun low high ->
  if Char.code low > Char.code high then
    invalid_arg "Generator.char_range: low > high";
  {
    run =
      (fun rnd _size ->
        let low_code = Char.code low in
        let high_code = Char.code high in
        Char.from_int_unchecked (random_int_range rnd ~low:low_code ~high:high_code));
  }

let char_lowercase = char_range 'a' 'z'

let char_uppercase = char_range 'A' 'Z'

let char_digit = char_range '0' '9'

let char_printable = char_range ' ' '~'

let char_whitespace = one_of [ return ' '; return '\t'; return '\n'; return '\r'; ]

(* Runes - Unicode support *)

let rune = {
  run =
    (fun rnd size ->
      let rec try_gen () =
        let n = random_int rnd 0x11_0000 in
        match Unicode.Rune.from_int n with
        | Some r -> r
        | None -> try_gen ()
      in
      try_gen ());
}

let rune_range = fun low high ->
  let low_int = Unicode.Rune.to_int low in
  let high_int = Unicode.Rune.to_int high in
  if low_int > high_int then
    invalid_arg "Generator.rune_range: low > high";
  {
    run =
      (fun rnd _size ->
        let rec try_gen () =
          let n = random_int_range rnd ~low:low_int ~high:high_int in
          match Unicode.Rune.from_int n with
          | Some r -> r
          | None -> try_gen ()
        in
        try_gen ());
  }

let rune_printable = {
  run =
    (fun rnd _size ->
      let rec try_gen () =
        let candidate = rune.run rnd 0 in
        if Unicode.Rune.is_print candidate then
          candidate
        else
          try_gen ()
      in
      try_gen ());
}

(* Strings *)

let string =
  sized
    (fun size ->
      {
        run =
          (fun rnd _ambient_size ->
            let len = random_int_range rnd ~low:0 ~high:size in
            build_string rnd size char len);
      })

let string_of = fun char_gen ->
  sized
    (fun size ->
      {
        run =
          (fun rnd _ambient_size ->
            let len = random_int_range rnd ~low:0 ~high:size in
            build_string rnd size char_gen len);
      })

let string_size = fun size_gen char_gen ->
  {
    run =
      (fun rnd size ->
        let len = size_gen.run rnd size in
        if len <= 0 then
          ""
        else
          build_string rnd size char_gen len);
  }

let string_printable = string_of char_printable

let string_lowercase = string_of char_lowercase

let string_uppercase = string_of char_uppercase

(* === COLLECTION GENERATORS === *)

let list = fun gen ->
  sized
    (fun size ->
      {
        run =
          (fun rnd _ambient_size ->
            let len = random_int_range rnd ~low:0 ~high:size in
            build_list_values rnd size gen len);
      })

let list_size = fun size_gen gen ->
  {
    run =
      (fun rnd size ->
        let len = size_gen.run rnd size in
        if len <= 0 then
          []
        else
          build_list_values rnd size gen len);
  }

let list_repeat = fun n gen -> list_size (return n) gen

let non_empty_list = fun gen -> and_then (int_range 1 10) (fun len -> list_size (return len) gen)

let array = fun gen -> map Collections.Array.from_list (list gen)

let array_size = fun size_gen gen -> map Collections.Array.from_list (list_size size_gen gen)

(* === TUPLE GENERATORS === *)

let pair = fun gen1 gen2 -> map2 (fun a b -> (a, b)) gen1 gen2

let triple = fun gen1 gen2 gen3 -> map3 (fun a b c -> (a, b, c)) gen1 gen2 gen3

let quad = fun gen1 gen2 gen3 gen4 ->
  {
    run =
      (fun rnd size ->
        let a = gen1.run rnd size in
        let b = gen2.run rnd size in
        let c = gen3.run rnd size in
        let d = gen4.run rnd size in
        (a, b, c, d));
  }

(* Std Collections *)

let vector = fun gen -> map Collections.Vector.from_list (list gen)

let vector_size = fun size_gen gen -> map Collections.Vector.from_list (list_size size_gen gen)

let hashmap = fun key_gen value_gen ->
  let pair_gen = pair key_gen value_gen in
  map Collections.HashMap.from_list (list pair_gen)

let hashmap_size = fun size_gen key_gen value_gen ->
  let pair_gen = pair key_gen value_gen in
  map Collections.HashMap.from_list (list_size size_gen pair_gen)

let hashset = fun gen -> map Collections.HashSet.from_list (list gen)

let hashset_size = fun size_gen gen -> map Collections.HashSet.from_list (list_size size_gen gen)

let queue = fun gen -> map Collections.Queue.from_list (list gen)

let queue_size = fun size_gen gen -> map Collections.Queue.from_list (list_size size_gen gen)

let deque = fun gen ->
  map
    (fun lst ->
      let d = Collections.Deque.create () in
      List.for_each lst ~fn:(fun value -> Collections.Deque.push_back d ~value);
      d)
    (list gen)

let deque_size = fun size_gen gen ->
  map
    (fun lst ->
      let d = Collections.Deque.create () in
      List.for_each lst ~fn:(fun value -> Collections.Deque.push_back d ~value);
      d)
    (list_size size_gen gen)

let heap = fun gen ->
  map
    (fun lst ->
      let h = Collections.Heap.create () in
      List.for_each lst ~fn:(fun value -> Collections.Heap.push h ~value);
      h)
    (list gen)

let heap_size = fun size_gen gen ->
  map
    (fun lst ->
      let h = Collections.Heap.create () in
      List.for_each lst ~fn:(fun value -> Collections.Heap.push h ~value);
      h)
    (list_size size_gen gen)

(* === OPTION & RESULT GENERATORS === *)

let option = fun gen ->
  frequency
    [
      (1, return None);
      (3, map (fun v -> Some v) gen);
    ]

let weighted_option = fun weight_some weight_none gen ->
  frequency
    [
      (weight_none, return None);
      (weight_some, map (fun v -> Some v) gen);
    ]

let result = fun ok_gen err_gen ->
  frequency
    [
      (3, map (fun v -> Ok v) ok_gen);
      (1, map (fun e -> Error e) err_gen);
    ]

let weighted_result = fun weight_ok weight_error ok_gen err_gen ->
  frequency
    [
      (weight_ok, map (fun v -> Ok v) ok_gen);
      (weight_error, map (fun e -> Error e) err_gen);
    ]

(* === LOW-LEVEL INTERFACE === *)

let generate = fun rnd gen -> gen.run rnd 10

let generate_with_size = fun rnd size gen -> gen.run rnd size
