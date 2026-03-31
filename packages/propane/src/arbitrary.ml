open Std

type 'value t = {
  gen: 'value Generator.t;
  shrink: 'value Shrinker.t option;
  print: 'value Printer.t option;
  small: ('value -> int) option;
}

(* === BUILDING ARBITRARIES === *)

let make = fun ?shrink ?print ?small gen -> {gen; shrink; print; small}

(* === PRIMITIVE ARBITRARIES === *)

let int = {
  gen = Generator.int;
  shrink = Some Shrinker.int;
  print = Some Printer.int;
  small = Some abs;

}

let int32 = {
  gen = Generator.int32;
  shrink = Some Shrinker.int32;
  print = Some Printer.int32;
  small = Some (fun n -> Int32.to_int (Int32.abs n));

}

let int64 = {
  gen = Generator.int64;
  shrink = Some Shrinker.int64;
  print = Some Printer.int64;
  small = Some (fun n -> Int64.to_int (Int64.abs n));

}

let bool = {
  gen = Generator.bool;
  shrink = Some Shrinker.bool;
  print = Some Printer.bool;
  small = Some (fun b ->
    if b then
      1
    else
      0);

}

let float = {
  gen = Generator.float;
  shrink = Some Shrinker.float;
  print = Some Printer.float;
  small = Some (fun f -> int_of_float (Float.abs f));

}

let char = {
  gen = Generator.char;
  shrink = Some Shrinker.char;
  print = Some Printer.char;
  small = Some Char.code;

}

let rune = {
  gen = Generator.rune;
  shrink = Some Shrinker.rune;
  print = Some Printer.rune;
  small = Some Unicode.Rune.to_int;

}

let string = {
  gen = Generator.string;
  shrink = Some Shrinker.string;
  print = Some Printer.string;
  small = Some String.length;

}

(* === COLLECTION ARBITRARIES === *)

let list = fun elem_arb ->
    {gen = Generator.list elem_arb.gen; shrink = (
        match elem_arb.shrink with
        | Some elem_shrinker -> Some (Shrinker.list elem_shrinker)
        | None -> Some (Shrinker.list Shrinker.nil)
      ); print = (
        match elem_arb.print with
        | Some elem_printer -> Some (Printer.list elem_printer)
        | None -> None
      ); small = Some List.length; }

let array = fun elem_arb ->
    {gen = Generator.array elem_arb.gen; shrink = (
        match elem_arb.shrink with
        | Some elem_shrinker -> Some (Shrinker.array elem_shrinker)
        | None -> Some (Shrinker.array Shrinker.nil)
      ); print = (
        match elem_arb.print with
        | Some elem_printer -> Some (Printer.array elem_printer)
        | None -> None
      ); small = Some Collections.Array.length; }

let vector = fun elem_arb ->
    {gen = Generator.vector elem_arb.gen; shrink = (
        match elem_arb.shrink with
        | Some elem_shrinker -> Some (Shrinker.vector elem_shrinker)
        | None -> Some (Shrinker.vector Shrinker.nil)
      ); print = (
        match elem_arb.print with
        | Some elem_printer -> Some (Printer.vector elem_printer)
        | None -> None
      ); small = Some Collections.Vector.len; }

let hashmap = fun key_arb value_arb ->
    {gen = Generator.hashmap key_arb.gen value_arb.gen; shrink = (
        match key_arb.shrink, value_arb.shrink with
        | Some key_shrinker, Some value_shrinker -> Some (Shrinker.hashmap key_shrinker value_shrinker)
        | _ -> Some (Shrinker.hashmap Shrinker.nil Shrinker.nil)
      ); print = (
        match key_arb.print, value_arb.print with
        | Some key_printer, Some value_printer -> Some (Printer.hashmap key_printer value_printer)
        | _ -> None
      ); small = Some Collections.HashMap.len; }

let hashset = fun elem_arb ->
    {gen = Generator.hashset elem_arb.gen; shrink = (
        match elem_arb.shrink with
        | Some elem_shrinker -> Some (Shrinker.hashset elem_shrinker)
        | None -> Some (Shrinker.hashset Shrinker.nil)
      ); print = (
        match elem_arb.print with
        | Some elem_printer -> Some (Printer.hashset elem_printer)
        | None -> None
      ); small = Some Collections.HashSet.len; }

let queue = fun elem_arb ->
    {gen = Generator.queue elem_arb.gen; shrink = (
        match elem_arb.shrink with
        | Some elem_shrinker -> Some (Shrinker.queue elem_shrinker)
        | None -> Some (Shrinker.queue Shrinker.nil)
      ); print = (
        match elem_arb.print with
        | Some elem_printer -> Some (Printer.queue elem_printer)
        | None -> None
      ); small = Some Collections.Queue.len; }

let deque = fun elem_arb ->
    {gen = Generator.deque elem_arb.gen; shrink = (
        match elem_arb.shrink with
        | Some elem_shrinker -> Some (Shrinker.deque elem_shrinker)
        | None -> Some (Shrinker.deque Shrinker.nil)
      ); print = (
        match elem_arb.print with
        | Some elem_printer -> Some (Printer.deque elem_printer)
        | None -> None
      ); small = Some Collections.Deque.len; }

let heap = fun elem_arb ->
    {gen = Generator.heap elem_arb.gen; shrink = (
        match elem_arb.shrink with
        | Some elem_shrinker -> Some (Shrinker.heap elem_shrinker)
        | None -> Some (Shrinker.heap Shrinker.nil)
      ); print = (
        match elem_arb.print with
        | Some elem_printer -> Some (Printer.heap elem_printer)
        | None -> None
      ); small = Some Collections.Heap.size; }

(* === TUPLE ARBITRARIES === *)

let pair = fun arb_a arb_b ->
    {gen = Generator.pair arb_a.gen arb_b.gen; shrink = (
        match arb_a.shrink, arb_b.shrink with
        | Some shrinker_a, Some shrinker_b -> Some (Shrinker.pair shrinker_a shrinker_b)
        | _ -> None
      ); print = (
        match arb_a.print, arb_b.print with
        | Some printer_a, Some printer_b -> Some (Printer.pair printer_a printer_b)
        | _ -> None
      ); small = None; }

let triple = fun arb_a arb_b arb_c ->
    {gen = Generator.triple arb_a.gen arb_b.gen arb_c.gen; shrink = (
        match arb_a.shrink, arb_b.shrink, arb_c.shrink with
        | Some shrinker_a, Some shrinker_b, Some shrinker_c -> Some (Shrinker.triple
          shrinker_a
          shrinker_b
          shrinker_c)
        | _ -> None
      ); print = (
        match arb_a.print, arb_b.print, arb_c.print with
        | Some printer_a, Some printer_b, Some printer_c -> Some (Printer.triple
          printer_a
          printer_b
          printer_c)
        | _ -> None
      ); small = None; }

(* === OPTION & RESULT ARBITRARIES === *)

let option = fun elem_arb ->
    {gen = Generator.option elem_arb.gen; shrink = (
        match elem_arb.shrink with
        | Some elem_shrinker -> Some (Shrinker.option elem_shrinker)
        | None -> Some (Shrinker.option Shrinker.nil)
      ); print = (
        match elem_arb.print with
        | Some elem_printer -> Some (Printer.option elem_printer)
        | None -> None
      ); small = None; }

let result = fun ok_arb err_arb ->
    {gen = Generator.result ok_arb.gen err_arb.gen; shrink = (
        match ok_arb.shrink, err_arb.shrink with
        | Some ok_shrinker, Some err_shrinker -> Some (Shrinker.result ok_shrinker err_shrinker)
        | _ -> None
      ); print = (
        match ok_arb.print, err_arb.print with
        | Some ok_printer, Some err_printer -> Some (Printer.result ok_printer err_printer)
        | _ -> None
      ); small = None; }

(* === COMBINATORS === *)

let map = fun f f_inv arb ->
    {gen = Generator.map f arb.gen; shrink = (
        match arb.shrink with
        | Some shrinker -> Some (Shrinker.map f f_inv shrinker)
        | None -> None
      ); print = (
        match arb.print with
        | Some printer -> Some (fun b -> printer (f_inv b))
        | None -> None
      ); small = (
        match arb.small with
        | Some small_fn -> Some (fun b -> small_fn (f_inv b))
        | None -> None
      ); }

let map_gen = fun gen arb -> {arb with gen}
