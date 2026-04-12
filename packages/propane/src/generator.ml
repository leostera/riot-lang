open Std

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

(* Helper: convert char list to string *)

let string_of_char_list = fun chars ->
  String.concat "" (List.map (String.make 1) chars)

(* === CONSTANTS === *)

let return = fun v -> { run = fun _rnd _size -> v }

let exactly = return

(* === TRANSFORMATIONS === *)

let map = fun f gen -> { run = fun rnd size -> f (gen.run rnd size) }

let map2 = fun f gen1 gen2 ->
  {
    run =
      fun rnd size ->
        let v1 = gen1.run rnd size in
        let v2 = gen2.run rnd size in
        f v1 v2;
  }

let map3 = fun f gen1 gen2 gen3 ->
  {
    run =
      fun rnd size ->
        let v1 = gen1.run rnd size in
        let v2 = gen2.run rnd size in
        let v3 = gen3.run rnd size in
        f v1 v2 v3;
  }

let and_then = fun gen f ->
  {
    run =
      fun rnd size ->
        let v = gen.run rnd size in
        let gen' = f v in
        gen'.run rnd size;
  }

(* === CHOICE COMBINATORS === *)

let one_of = fun gens ->
  match gens with
  | [] -> panic "one_of: empty list"
  | _ ->
      {
        run =
          fun rnd size ->
            let n = List.length gens in
            let idx = random_int rnd n in
            let rec get_nth lst i =
              match lst with
              | [] -> panic "one_of: index out of bounds"
              | x :: _ when i = 0 -> x
              | _ :: xs -> get_nth xs (i - 1)
            in
            let gen = get_nth gens idx in
            gen.run rnd size;
      }

let frequency = fun weighted_gens ->
  match weighted_gens with
  | [] -> panic "frequency: empty list"
  | _ ->
      (* Validate weights *)
      let () =
        List.iter
          (fun ((w, _)) ->
            if w <= 0 then
              panic "frequency: non-positive weight")
          weighted_gens
      in
      let total_weight =
        List.fold_left (fun acc ((w, _)) -> acc + w) 0 weighted_gens
      in
      {
        run =
          fun rnd size ->
            let target = random_int rnd total_weight in
            let rec find acc remaining =
              match remaining with
              | [] -> panic "frequency: impossible - empty after fold"
              | (w, gen) :: rest ->
                  let acc' = acc + w in
                  if target < acc' then
                    gen.run rnd size
                  else
                    find acc' rest
            in
            find 0 weighted_gens;
      }

(* === SIZE CONTROL === *)

let sized = fun f -> { run = fun rnd size -> (f size).run rnd size }

let resize = fun new_size gen ->
  {
    run =
      fun rnd _size ->
        gen.run rnd new_size;
  }

(* === RECURSIVE GENERATORS === *)

let delay = fun f -> { run = fun rnd size -> (f ()).run rnd size }

let fix = fun f ->
  let rec self n = f self n in
  fun n -> { run = fun rnd size -> (self n).run rnd size }

(* === PRIMITIVE GENERATORS === *)

(* Integers *)

let int = { run = fun rnd _size -> random_bits rnd }

let int32 = { run = fun rnd _size -> random_int32 rnd Int32.max_int }

let int64 = { run = fun rnd _size -> random_int64 rnd Int64.max_int }

let int_range = fun low high ->
  if low > high then
    panic "int_range: low > high";
  {
    run =
      fun rnd _size ->
        if low = high then
          low
        else
          low + random_int rnd (high - low + 1);
  }

let int32_range = fun low high ->
  if low > high then
    panic "int32_range: low > high";
  {
    run =
      fun rnd _size ->
        if low = high then
          low
        else
          Int32.add low (random_int32 rnd (Int32.sub (Int32.add high 1l) low));
  }

let int64_range = fun low high ->
  if low > high then
    panic "int64_range: low > high";
  {
    run =
      fun rnd _size ->
        if low = high then
          low
        else
          Int64.add low (random_int64 rnd (Int64.sub (Int64.add high 1L) low));
  }

let int_bound = fun n ->
  if n < 0 then
    panic "int_bound: negative bound";
  int_range 0 n

let small_int = int_range 0 100

let big_int = int

let positive_int = { run = fun rnd _size -> random_bits rnd land max_int }

let negative_int =
  map (fun n -> -n) positive_int

let non_zero_int =
  let pos: int t = int_range 1 max_int in
  let neg: int t = int_range min_int (-1) in
  one_of [ pos; neg ]

(* Floats *)

let float = { run = fun rnd _size -> random_float rnd Float.max_float }

let float_range = fun low high ->
  {
    run =
      fun rnd _size ->
        let range = high -. low in
        low +. random_float rnd range;
  }

let float_positive = { run = fun rnd _size -> random_float rnd Float.max_float }

let float_negative =
  map (fun f -> -.f) float_positive

(* Booleans *)

let bool = { run = fun rnd _size -> random_bool rnd }

let weighted_bool = fun weight_true weight_false ->
  if weight_true <= 0 || weight_false <= 0 then
    panic "weighted_bool: non-positive weights";
  frequency [ (weight_true, return true); (weight_false, return false) ]

(* Characters *)

let char = { run = fun rnd _size -> Char.chr (random_int rnd 256) }

let char_range = fun low high ->
  {
    run =
      fun rnd _size ->
        let low_code = Char.code low in
        let high_code = Char.code high in
        let range = high_code - low_code + 1 in
        Char.chr (low_code + random_int rnd range);
  }

let char_lowercase = char_range 'a' 'z'

let char_uppercase = char_range 'A' 'Z'

let char_digit = char_range '0' '9'

let char_printable = one_of [ char_range ' ' '~'; return '\n'; ]

let char_whitespace = one_of [ return ' '; return '\t'; return '\n'; return '\r'; ]

(* Runes - Unicode support *)

let rune = {
  run =
    fun rnd size ->
      let rec try_gen () =
        let n = random_int rnd 0x11_0000 in
        match Unicode.Rune.of_int n with
        | Some r -> r
        | None -> try_gen ()
      in
      try_gen ();
}

let rune_range = fun low high ->
  let low_int = Unicode.Rune.to_int low in
  let high_int = Unicode.Rune.to_int high in
  {
    run =
      fun rnd size ->
        let rec try_gen () =
          let n = low_int + random_int rnd (high_int - low_int + 1) in
          match Unicode.Rune.of_int n with
          | Some r -> r
          | None -> try_gen ()
        in
        try_gen ();
  }

let rune_printable =
  (* Simplified - just ASCII printable for now *)
  map
    (fun c ->
      match Unicode.Rune.of_int (Char.code c) with
      | Some r -> r
      | None -> Unicode.Rune.replacement)
    char_printable

(* Strings *)

let string =
  sized
    (fun size ->
      let len_gen = int_range 0 size in
      and_then len_gen
        (fun len ->
          let rec build_string acc n =
            if n <= 0 then
              return (string_of_char_list (List.rev acc))
            else
              and_then char (fun c -> build_string (c :: acc) (n - 1))
          in
          build_string [] len))

let string_of = fun char_gen ->
  sized
    (fun size ->
      let len_gen = int_range 0 size in
      and_then len_gen
        (fun len ->
          let rec build_string acc n =
            if n <= 0 then
              return (string_of_char_list (List.rev acc))
            else
              and_then char_gen (fun c -> build_string (c :: acc) (n - 1))
          in
          build_string [] len))

let string_size = fun size_gen char_gen ->
  and_then size_gen
    (fun len ->
      let rec build_string acc n =
        if n <= 0 then
          return (string_of_char_list (List.rev acc))
        else
          and_then char_gen (fun c -> build_string (c :: acc) (n - 1))
      in
      build_string [] len)

let string_printable = string_of char_printable

let string_lowercase = string_of char_lowercase

let string_uppercase = string_of char_uppercase

(* === COLLECTION GENERATORS === *)

let list = fun gen ->
  sized
    (fun size ->
      let len_gen = int_range 0 size in
      and_then len_gen
        (fun len ->
          let rec build_list acc n =
            if n <= 0 then
              return (List.rev acc)
            else
              and_then gen (fun v -> build_list (v :: acc) (n - 1))
          in
          build_list [] len))

let list_size = fun size_gen gen ->
  and_then size_gen
    (fun len ->
      let rec build_list acc n =
        if n <= 0 then
          return (List.rev acc)
        else
          and_then gen (fun v -> build_list (v :: acc) (n - 1))
      in
      build_list [] len)

let list_repeat = fun n gen -> list_size (return n) gen

let non_empty_list = fun gen -> and_then (int_range 1 10) (fun len -> list_size (return len) gen)

let array = fun gen -> map Collections.Array.of_list (list gen)

let array_size = fun size_gen gen -> map Collections.Array.of_list (list_size size_gen gen)

(* === TUPLE GENERATORS === *)

let pair = fun gen1 gen2 -> map2 (fun a b -> (a, b)) gen1 gen2

let triple = fun gen1 gen2 gen3 -> map3 (fun a b c -> (a, b, c)) gen1 gen2 gen3

let quad = fun gen1 gen2 gen3 gen4 ->
  {
    run =
      fun rnd size ->
        let a = gen1.run rnd size in
        let b = gen2.run rnd size in
        let c = gen3.run rnd size in
        let d = gen4.run rnd size in
        (a, b, c, d);
  }

(* Std Collections *)

let vector = fun gen -> map Collections.Vector.of_list (list gen)

let vector_size = fun size_gen gen -> map Collections.Vector.of_list (list_size size_gen gen)

let hashmap = fun key_gen value_gen ->
  let pair_gen = pair key_gen value_gen in
  map Collections.HashMap.of_list (list pair_gen)

let hashmap_size = fun size_gen key_gen value_gen ->
  let pair_gen = pair key_gen value_gen in
  map Collections.HashMap.of_list (list_size size_gen pair_gen)

let hashset = fun gen -> map Collections.HashSet.of_list (list gen)

let hashset_size = fun size_gen gen -> map Collections.HashSet.of_list (list_size size_gen gen)

let queue = fun gen -> map Collections.Queue.of_list (list gen)

let queue_size = fun size_gen gen -> map Collections.Queue.of_list (list_size size_gen gen)

let deque = fun gen ->
  map
    (fun lst ->
      let d = Collections.Deque.create () in
      List.iter (Collections.Deque.push_back d) lst;
      d)
    (list gen)

let deque_size = fun size_gen gen ->
  map
    (fun lst ->
      let d = Collections.Deque.create () in
      List.iter (Collections.Deque.push_back d) lst;
      d)
    (list_size size_gen gen)

let heap = fun gen ->
  map
    (fun lst ->
      let h = Collections.Heap.create () in
      List.iter (Collections.Heap.push h) lst;
      h)
    (list gen)

let heap_size = fun size_gen gen ->
  map
    (fun lst ->
      let h = Collections.Heap.create () in
      List.iter (Collections.Heap.push h) lst;
      h)
    (list_size size_gen gen)

(* === OPTION & RESULT GENERATORS === *)

let option = fun gen -> frequency [ (1, return None); (3, map (fun v -> Some v) gen); ]

let weighted_option = fun weight_some weight_none gen ->
  frequency [ (weight_none, return None); (weight_some, map (fun v -> Some v) gen); ]

let result = fun ok_gen err_gen ->
  frequency [ (3, map (fun v -> Ok v) ok_gen); (1, map (fun e -> Error e) err_gen); ]

let weighted_result = fun weight_ok weight_error ok_gen err_gen ->
  frequency
    [ (weight_ok, map (fun v -> Ok v) ok_gen); (weight_error, map (fun e -> Error e) err_gen); ]

(* === LOW-LEVEL INTERFACE === *)

let generate = fun rnd gen ->
  gen.run rnd 10

let generate_with_size = fun rnd size gen ->
  gen.run rnd size
