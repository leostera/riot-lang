type t = {
  mutable bytes: Kernel.Bytes.t;
  mutable length: int;
}

let panic_invalid_range = fun fn ~offset ~length ~total ->
  Kernel.SystemError.panic
    (Kernel.String.concat
      ""
      [
        "StringBuilder.";
        fn;
        " received an invalid range: offset=";
        Kernel.Int.to_string offset;
        " length=";
        Kernel.Int.to_string length;
        " total=";
        Kernel.Int.to_string total;
      ])

let create = fun ~size ->
  let initial_capacity = Kernel.Int.max 0 size in
  { bytes = Kernel.Bytes.create ~size:initial_capacity; length = 0 }

let clear = fun buffer -> buffer.length <- 0

let length = fun buffer -> buffer.length

let ensure_capacity = fun buffer additional ->
  let required = Kernel.Int.add buffer.length additional in
  let current = Kernel.Bytes.length buffer.bytes in
  match Kernel.Int.compare required current with
  | Kernel.Order.LT
  | Kernel.Order.EQ -> ()
  | Kernel.Order.GT ->
      let rec next_capacity capacity =
        match Kernel.Int.compare capacity required with
        | Kernel.Order.GT
        | Kernel.Order.EQ -> capacity
        | Kernel.Order.LT ->
            if Kernel.Int.equal capacity 0 then
              next_capacity 1
            else
              next_capacity (Kernel.Int.mul capacity 2)
      in
      let grown = Kernel.Bytes.create ~size:(next_capacity current) in
      Kernel.Bytes.blit_unchecked
        buffer.bytes
        ~src_offset:0
        ~dst:grown
        ~dst_offset:0
        ~len:buffer.length;
      buffer.bytes <- grown

let get = fun buffer ~at ->
  match Kernel.Int.compare at 0 with
  | Kernel.Order.LT -> None
  | Kernel.Order.EQ
  | Kernel.Order.GT ->
      match Kernel.Int.compare at buffer.length with
      | Kernel.Order.LT -> Some (Kernel.Bytes.get_unchecked buffer.bytes ~at)
      | Kernel.Order.EQ
      | Kernel.Order.GT -> None

let get_unchecked = fun buffer ~at -> Kernel.Bytes.get_unchecked buffer.bytes ~at

let add_char = fun buffer value ->
  ensure_capacity buffer 1;
  Kernel.Bytes.set_unchecked buffer.bytes ~at:buffer.length ~char:value;
  buffer.length <- Kernel.Int.add buffer.length 1

let add_subbytes = fun buffer source offset slice_length ->
  let source_length = Kernel.Bytes.length source in
  match Kernel.Int.compare offset 0 with
  | Kernel.Order.LT ->
      panic_invalid_range "add_subbytes" ~offset ~length:slice_length ~total:source_length
  | Kernel.Order.EQ
  | Kernel.Order.GT ->
      match Kernel.Int.compare slice_length 0 with
      | Kernel.Order.LT ->
          panic_invalid_range "add_subbytes" ~offset ~length:slice_length ~total:source_length
      | Kernel.Order.EQ
      | Kernel.Order.GT ->
          match Kernel.Int.compare offset (Kernel.Int.sub source_length slice_length) with
          | Kernel.Order.GT ->
              panic_invalid_range "add_subbytes" ~offset ~length:slice_length ~total:source_length
          | Kernel.Order.LT
          | Kernel.Order.EQ ->
              if Kernel.Int.equal slice_length 0 then
                ()
              else (
                ensure_capacity buffer slice_length;
                Kernel.Bytes.blit_unchecked
                  source
                  ~src_offset:offset
                  ~dst:buffer.bytes
                  ~dst_offset:buffer.length
                  ~len:slice_length;
                buffer.length <- Kernel.Int.add buffer.length slice_length
              )

let add_bytes = fun buffer source -> add_subbytes buffer source 0 (Kernel.Bytes.length source)

let add_substring = fun buffer source offset slice_length ->
  let source_length = Kernel.String.length source in
  match Kernel.Int.compare offset 0 with
  | Kernel.Order.LT ->
      panic_invalid_range "add_substring" ~offset ~length:slice_length ~total:source_length
  | Kernel.Order.EQ
  | Kernel.Order.GT ->
      match Kernel.Int.compare slice_length 0 with
      | Kernel.Order.LT ->
          panic_invalid_range "add_substring" ~offset ~length:slice_length ~total:source_length
      | Kernel.Order.EQ
      | Kernel.Order.GT ->
          match Kernel.Int.compare offset (Kernel.Int.sub source_length slice_length) with
          | Kernel.Order.GT ->
              panic_invalid_range "add_substring" ~offset ~length:slice_length ~total:source_length
          | Kernel.Order.LT
          | Kernel.Order.EQ ->
              if Kernel.Int.equal slice_length 0 then
                ()
              else
                (
                  let source_bytes = Kernel.String.to_bytes source in
                  ensure_capacity buffer slice_length;
                  Kernel.Bytes.blit_unchecked
                    source_bytes
                    ~src_offset:offset
                    ~dst:buffer.bytes
                    ~dst_offset:buffer.length
                    ~len:slice_length;
                  buffer.length <- Kernel.Int.add buffer.length slice_length
                )

let add_string = fun buffer source -> add_substring buffer source 0 (Kernel.String.length source)

let add_utf_8_uchar = fun buffer rune -> add_string buffer (Kernel.Unicode.Rune.to_string rune)

let contents = fun buffer ->
  let bytes = Kernel.Bytes.sub_unchecked buffer.bytes ~offset:0 ~len:buffer.length in
  Kernel.Bytes.to_string bytes
