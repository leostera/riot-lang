(* Auto-generated from Go's unicode/tables.go - DO NOT EDIT *)
(* Unicode Version: 15.0.0 *)

(** Range of 16-bit Unicode code points *)
type range16 = {
  lo : int;      (** Start of range *)
  hi : int;      (** End of range *)
  stride : int;  (** Step (1 = all chars, 2 = every other) *)
}

(** Range of 32-bit Unicode code points *)
type range32 = {
  lo : int;      (** Start of range *)
  hi : int;      (** End of range *)
  stride : int;  (** Step *)
}

(** Table of Unicode ranges for a category *)
type range_table = {
  r16 : range16 array;  (** Ranges for code points < 0x10000 *)
  r32 : range32 array;  (** Ranges for code points >= 0x10000 *)
  latin_offset : int;   (** Number of R16 entries with Hi <= 0xFF *)
}

(** Check if code point is in 16-bit range *)
let in_range16 (r : range16) (code : int) : bool =
  code >= r.lo && code <= r.hi &&
  (r.stride = 1 || (code - r.lo) mod r.stride = 0)

(** Check if code point is in 32-bit range *)
let in_range32 (r : range32) (code : int) : bool =
  code >= r.lo && code <= r.hi &&
  (r.stride = 1 || (code - r.lo) mod r.stride = 0)

(** Check if code point is in range table using binary search *)
let in_table (tbl : range_table) (code : int) : bool =
  if code < 0 || code > 0x10FFFF then false
  else if code < 0x10000 then begin
    (* Binary search in R16 *)
    let rec search lo hi =
      if lo > hi then false
      else
        let mid = (lo + hi) / 2 in
        let range = tbl.r16.(mid) in
        if code < range.lo then search lo (mid - 1)
        else if code > range.hi then search (mid + 1) hi
        else in_range16 range code
    in
    let len = Array.length tbl.r16 in
    if len = 0 then false else search 0 (len - 1)
  end else begin
    (* Binary search in R32 *)
    let rec search lo hi =
      if lo > hi then false
      else
        let mid = (lo + hi) / 2 in
        let range = tbl.r32.(mid) in
        if code < range.lo then search lo (mid - 1)
        else if code > range.hi then search (mid + 1) hi
        else in_range32 range code
    in
    let len = Array.length tbl.r32 in
    if len = 0 then false else search 0 (len - 1)
  end

(* ============================================ *)
(* Unicode Category Tables                     *)
(* ============================================ *)

(* c *)
let _c = {
  r16 = [|
    { lo = 0x0000; hi = 0x001f; stride = 1 };
    { lo = 0x007f; hi = 0x009f; stride = 1 };
    { lo = 0x00ad; hi = 0x0378; stride = 715 };
    { lo = 0x0379; hi = 0x0380; stride = 7 };
    { lo = 0x0381; hi = 0x0383; stride = 1 };
    { lo = 0x038b; hi = 0x038d; stride = 2 };
    { lo = 0x03a2; hi = 0x0530; stride = 398 };
    { lo = 0x0557; hi = 0x0558; stride = 1 };
    { lo = 0x058b; hi = 0x058c; stride = 1 };
    { lo = 0x0590; hi = 0x05c8; stride = 56 };
    { lo = 0x05c9; hi = 0x05cf; stride = 1 };
    { lo = 0x05eb; hi = 0x05ee; stride = 1 };
    { lo = 0x05f5; hi = 0x0605; stride = 1 };
    { lo = 0x061c; hi = 0x06dd; stride = 193 };
    { lo = 0x070e; hi = 0x070f; stride = 1 };
    { lo = 0x074b; hi = 0x074c; stride = 1 };
    { lo = 0x07b2; hi = 0x07bf; stride = 1 };
    { lo = 0x07fb; hi = 0x07fc; stride = 1 };
    { lo = 0x082e; hi = 0x082f; stride = 1 };
    { lo = 0x083f; hi = 0x085c; stride = 29 };
    { lo = 0x085d; hi = 0x085f; stride = 2 };
    { lo = 0x086b; hi = 0x086f; stride = 1 };
    { lo = 0x088f; hi = 0x0897; stride = 1 };
    { lo = 0x08e2; hi = 0x0984; stride = 162 };
    { lo = 0x098d; hi = 0x098e; stride = 1 };
    { lo = 0x0991; hi = 0x0992; stride = 1 };
    { lo = 0x09a9; hi = 0x09b1; stride = 8 };
    { lo = 0x09b3; hi = 0x09b5; stride = 1 };
    { lo = 0x09ba; hi = 0x09bb; stride = 1 };
    { lo = 0x09c5; hi = 0x09c6; stride = 1 };
    { lo = 0x09c9; hi = 0x09ca; stride = 1 };
    { lo = 0x09cf; hi = 0x09d6; stride = 1 };
    { lo = 0x09d8; hi = 0x09db; stride = 1 };
    { lo = 0x09de; hi = 0x09e4; stride = 6 };
    { lo = 0x09e5; hi = 0x09ff; stride = 26 };
    { lo = 0x0a00; hi = 0x0a04; stride = 4 };
    { lo = 0x0a0b; hi = 0x0a0e; stride = 1 };
    { lo = 0x0a11; hi = 0x0a12; stride = 1 };
    { lo = 0x0a29; hi = 0x0a31; stride = 8 };
    { lo = 0x0a34; hi = 0x0a3a; stride = 3 };
    { lo = 0x0a3b; hi = 0x0a3d; stride = 2 };
    { lo = 0x0a43; hi = 0x0a46; stride = 1 };
    { lo = 0x0a49; hi = 0x0a4a; stride = 1 };
    { lo = 0x0a4e; hi = 0x0a50; stride = 1 };
    { lo = 0x0a52; hi = 0x0a58; stride = 1 };
    { lo = 0x0a5d; hi = 0x0a5f; stride = 2 };
    { lo = 0x0a60; hi = 0x0a65; stride = 1 };
    { lo = 0x0a77; hi = 0x0a80; stride = 1 };
    { lo = 0x0a84; hi = 0x0a8e; stride = 10 };
    { lo = 0x0a92; hi = 0x0aa9; stride = 23 };
    { lo = 0x0ab1; hi = 0x0ab4; stride = 3 };
    { lo = 0x0aba; hi = 0x0abb; stride = 1 };
    { lo = 0x0ac6; hi = 0x0ace; stride = 4 };
    { lo = 0x0acf; hi = 0x0ad1; stride = 2 };
    { lo = 0x0ad2; hi = 0x0adf; stride = 1 };
    { lo = 0x0ae4; hi = 0x0ae5; stride = 1 };
    { lo = 0x0af2; hi = 0x0af8; stride = 1 };
    { lo = 0x0b00; hi = 0x0b04; stride = 4 };
    { lo = 0x0b0d; hi = 0x0b0e; stride = 1 };
    { lo = 0x0b11; hi = 0x0b12; stride = 1 };
    { lo = 0x0b29; hi = 0x0b31; stride = 8 };
    { lo = 0x0b34; hi = 0x0b3a; stride = 6 };
    { lo = 0x0b3b; hi = 0x0b45; stride = 10 };
    { lo = 0x0b46; hi = 0x0b49; stride = 3 };
    { lo = 0x0b4a; hi = 0x0b4e; stride = 4 };
    { lo = 0x0b4f; hi = 0x0b54; stride = 1 };
    { lo = 0x0b58; hi = 0x0b5b; stride = 1 };
    { lo = 0x0b5e; hi = 0x0b64; stride = 6 };
    { lo = 0x0b65; hi = 0x0b78; stride = 19 };
    { lo = 0x0b79; hi = 0x0b81; stride = 1 };
    { lo = 0x0b84; hi = 0x0b8b; stride = 7 };
    { lo = 0x0b8c; hi = 0x0b8d; stride = 1 };
    { lo = 0x0b91; hi = 0x0b96; stride = 5 };
    { lo = 0x0b97; hi = 0x0b98; stride = 1 };
    { lo = 0x0b9b; hi = 0x0b9d; stride = 2 };
    { lo = 0x0ba0; hi = 0x0ba2; stride = 1 };
    { lo = 0x0ba5; hi = 0x0ba7; stride = 1 };
    { lo = 0x0bab; hi = 0x0bad; stride = 1 };
    { lo = 0x0bba; hi = 0x0bbd; stride = 1 };
    { lo = 0x0bc3; hi = 0x0bc5; stride = 1 };
    { lo = 0x0bc9; hi = 0x0bce; stride = 5 };
    { lo = 0x0bcf; hi = 0x0bd1; stride = 2 };
    { lo = 0x0bd2; hi = 0x0bd6; stride = 1 };
    { lo = 0x0bd8; hi = 0x0be5; stride = 1 };
    { lo = 0x0bfb; hi = 0x0bff; stride = 1 };
    { lo = 0x0c0d; hi = 0x0c11; stride = 4 };
    { lo = 0x0c29; hi = 0x0c3a; stride = 17 };
    { lo = 0x0c3b; hi = 0x0c45; stride = 10 };
    { lo = 0x0c49; hi = 0x0c4e; stride = 5 };
    { lo = 0x0c4f; hi = 0x0c54; stride = 1 };
    { lo = 0x0c57; hi = 0x0c5b; stride = 4 };
    { lo = 0x0c5c; hi = 0x0c5e; stride = 2 };
    { lo = 0x0c5f; hi = 0x0c64; stride = 5 };
    { lo = 0x0c65; hi = 0x0c70; stride = 11 };
    { lo = 0x0c71; hi = 0x0c76; stride = 1 };
    { lo = 0x0c8d; hi = 0x0c91; stride = 4 };
    { lo = 0x0ca9; hi = 0x0cb4; stride = 11 };
    { lo = 0x0cba; hi = 0x0cbb; stride = 1 };
    { lo = 0x0cc5; hi = 0x0cc9; stride = 4 };
    { lo = 0x0cce; hi = 0x0cd4; stride = 1 };
    { lo = 0x0cd7; hi = 0x0cdc; stride = 1 };
    { lo = 0x0cdf; hi = 0x0ce4; stride = 5 };
    { lo = 0x0ce5; hi = 0x0cf0; stride = 11 };
    { lo = 0x0cf4; hi = 0x0cff; stride = 1 };
    { lo = 0x0d0d; hi = 0x0d11; stride = 4 };
    { lo = 0x0d45; hi = 0x0d49; stride = 4 };
    { lo = 0x0d50; hi = 0x0d53; stride = 1 };
    { lo = 0x0d64; hi = 0x0d65; stride = 1 };
    { lo = 0x0d80; hi = 0x0d84; stride = 4 };
    { lo = 0x0d97; hi = 0x0d99; stride = 1 };
    { lo = 0x0db2; hi = 0x0dbc; stride = 10 };
    { lo = 0x0dbe; hi = 0x0dbf; stride = 1 };
    { lo = 0x0dc7; hi = 0x0dc9; stride = 1 };
    { lo = 0x0dcb; hi = 0x0dce; stride = 1 };
    { lo = 0x0dd5; hi = 0x0dd7; stride = 2 };
    { lo = 0x0de0; hi = 0x0de5; stride = 1 };
    { lo = 0x0df0; hi = 0x0df1; stride = 1 };
    { lo = 0x0df5; hi = 0x0e00; stride = 1 };
    { lo = 0x0e3b; hi = 0x0e3e; stride = 1 };
    { lo = 0x0e5c; hi = 0x0e80; stride = 1 };
    { lo = 0x0e83; hi = 0x0e85; stride = 2 };
    { lo = 0x0e8b; hi = 0x0ea4; stride = 25 };
    { lo = 0x0ea6; hi = 0x0ebe; stride = 24 };
    { lo = 0x0ebf; hi = 0x0ec5; stride = 6 };
    { lo = 0x0ec7; hi = 0x0ecf; stride = 8 };
    { lo = 0x0eda; hi = 0x0edb; stride = 1 };
    { lo = 0x0ee0; hi = 0x0eff; stride = 1 };
    { lo = 0x0f48; hi = 0x0f6d; stride = 37 };
    { lo = 0x0f6e; hi = 0x0f70; stride = 1 };
    { lo = 0x0f98; hi = 0x0fbd; stride = 37 };
    { lo = 0x0fcd; hi = 0x0fdb; stride = 14 };
    { lo = 0x0fdc; hi = 0x0fff; stride = 1 };
    { lo = 0x10c6; hi = 0x10c8; stride = 2 };
    { lo = 0x10c9; hi = 0x10cc; stride = 1 };
    { lo = 0x10ce; hi = 0x10cf; stride = 1 };
    { lo = 0x1249; hi = 0x124e; stride = 5 };
    { lo = 0x124f; hi = 0x1257; stride = 8 };
    { lo = 0x1259; hi = 0x125e; stride = 5 };
    { lo = 0x125f; hi = 0x1289; stride = 42 };
    { lo = 0x128e; hi = 0x128f; stride = 1 };
    { lo = 0x12b1; hi = 0x12b6; stride = 5 };
    { lo = 0x12b7; hi = 0x12bf; stride = 8 };
    { lo = 0x12c1; hi = 0x12c6; stride = 5 };
    { lo = 0x12c7; hi = 0x12d7; stride = 16 };
    { lo = 0x1311; hi = 0x1316; stride = 5 };
    { lo = 0x1317; hi = 0x135b; stride = 68 };
    { lo = 0x135c; hi = 0x137d; stride = 33 };
    { lo = 0x137e; hi = 0x137f; stride = 1 };
    { lo = 0x139a; hi = 0x139f; stride = 1 };
    { lo = 0x13f6; hi = 0x13f7; stride = 1 };
    { lo = 0x13fe; hi = 0x13ff; stride = 1 };
    { lo = 0x169d; hi = 0x169f; stride = 1 };
    { lo = 0x16f9; hi = 0x16ff; stride = 1 };
    { lo = 0x1716; hi = 0x171e; stride = 1 };
    { lo = 0x1737; hi = 0x173f; stride = 1 };
    { lo = 0x1754; hi = 0x175f; stride = 1 };
    { lo = 0x176d; hi = 0x1771; stride = 4 };
    { lo = 0x1774; hi = 0x177f; stride = 1 };
    { lo = 0x17de; hi = 0x17df; stride = 1 };
    { lo = 0x17ea; hi = 0x17ef; stride = 1 };
    { lo = 0x17fa; hi = 0x17ff; stride = 1 };
    { lo = 0x180e; hi = 0x181a; stride = 12 };
    { lo = 0x181b; hi = 0x181f; stride = 1 };
    { lo = 0x1879; hi = 0x187f; stride = 1 };
    { lo = 0x18ab; hi = 0x18af; stride = 1 };
    { lo = 0x18f6; hi = 0x18ff; stride = 1 };
    { lo = 0x191f; hi = 0x192c; stride = 13 };
    { lo = 0x192d; hi = 0x192f; stride = 1 };
    { lo = 0x193c; hi = 0x193f; stride = 1 };
    { lo = 0x1941; hi = 0x1943; stride = 1 };
    { lo = 0x196e; hi = 0x196f; stride = 1 };
    { lo = 0x1975; hi = 0x197f; stride = 1 };
    { lo = 0x19ac; hi = 0x19af; stride = 1 };
    { lo = 0x19ca; hi = 0x19cf; stride = 1 };
    { lo = 0x19db; hi = 0x19dd; stride = 1 };
    { lo = 0x1a1c; hi = 0x1a1d; stride = 1 };
    { lo = 0x1a5f; hi = 0x1a7d; stride = 30 };
    { lo = 0x1a7e; hi = 0x1a8a; stride = 12 };
    { lo = 0x1a8b; hi = 0x1a8f; stride = 1 };
    { lo = 0x1a9a; hi = 0x1a9f; stride = 1 };
    { lo = 0x1aae; hi = 0x1aaf; stride = 1 };
    { lo = 0x1acf; hi = 0x1aff; stride = 1 };
    { lo = 0x1b4d; hi = 0x1b4f; stride = 1 };
    { lo = 0x1b7f; hi = 0x1bf4; stride = 117 };
    { lo = 0x1bf5; hi = 0x1bfb; stride = 1 };
    { lo = 0x1c38; hi = 0x1c3a; stride = 1 };
    { lo = 0x1c4a; hi = 0x1c4c; stride = 1 };
    { lo = 0x1c89; hi = 0x1c8f; stride = 1 };
    { lo = 0x1cbb; hi = 0x1cbc; stride = 1 };
    { lo = 0x1cc8; hi = 0x1ccf; stride = 1 };
    { lo = 0x1cfb; hi = 0x1cff; stride = 1 };
    { lo = 0x1f16; hi = 0x1f17; stride = 1 };
    { lo = 0x1f1e; hi = 0x1f1f; stride = 1 };
    { lo = 0x1f46; hi = 0x1f47; stride = 1 };
    { lo = 0x1f4e; hi = 0x1f4f; stride = 1 };
    { lo = 0x1f58; hi = 0x1f5e; stride = 2 };
    { lo = 0x1f7e; hi = 0x1f7f; stride = 1 };
    { lo = 0x1fb5; hi = 0x1fc5; stride = 16 };
    { lo = 0x1fd4; hi = 0x1fd5; stride = 1 };
    { lo = 0x1fdc; hi = 0x1ff0; stride = 20 };
    { lo = 0x1ff1; hi = 0x1ff5; stride = 4 };
    { lo = 0x1fff; hi = 0x200b; stride = 12 };
    { lo = 0x200c; hi = 0x200f; stride = 1 };
    { lo = 0x202a; hi = 0x202e; stride = 1 };
    { lo = 0x2060; hi = 0x206f; stride = 1 };
    { lo = 0x2072; hi = 0x2073; stride = 1 };
    { lo = 0x208f; hi = 0x209d; stride = 14 };
    { lo = 0x209e; hi = 0x209f; stride = 1 };
    { lo = 0x20c1; hi = 0x20cf; stride = 1 };
    { lo = 0x20f1; hi = 0x20ff; stride = 1 };
    { lo = 0x218c; hi = 0x218f; stride = 1 };
    { lo = 0x2427; hi = 0x243f; stride = 1 };
    { lo = 0x244b; hi = 0x245f; stride = 1 };
    { lo = 0x2b74; hi = 0x2b75; stride = 1 };
    { lo = 0x2b96; hi = 0x2cf4; stride = 350 };
    { lo = 0x2cf5; hi = 0x2cf8; stride = 1 };
    { lo = 0x2d26; hi = 0x2d28; stride = 2 };
    { lo = 0x2d29; hi = 0x2d2c; stride = 1 };
    { lo = 0x2d2e; hi = 0x2d2f; stride = 1 };
    { lo = 0x2d68; hi = 0x2d6e; stride = 1 };
    { lo = 0x2d71; hi = 0x2d7e; stride = 1 };
    { lo = 0x2d97; hi = 0x2d9f; stride = 1 };
    { lo = 0x2da7; hi = 0x2ddf; stride = 8 };
    { lo = 0x2e5e; hi = 0x2e7f; stride = 1 };
    { lo = 0x2e9a; hi = 0x2ef4; stride = 90 };
    { lo = 0x2ef5; hi = 0x2eff; stride = 1 };
    { lo = 0x2fd6; hi = 0x2fef; stride = 1 };
    { lo = 0x2ffc; hi = 0x2fff; stride = 1 };
    { lo = 0x3040; hi = 0x3097; stride = 87 };
    { lo = 0x3098; hi = 0x3100; stride = 104 };
    { lo = 0x3101; hi = 0x3104; stride = 1 };
    { lo = 0x3130; hi = 0x318f; stride = 95 };
    { lo = 0x31e4; hi = 0x31ef; stride = 1 };
    { lo = 0x321f; hi = 0xa48d; stride = 29294 };
    { lo = 0xa48e; hi = 0xa48f; stride = 1 };
    { lo = 0xa4c7; hi = 0xa4cf; stride = 1 };
    { lo = 0xa62c; hi = 0xa63f; stride = 1 };
    { lo = 0xa6f8; hi = 0xa6ff; stride = 1 };
    { lo = 0xa7cb; hi = 0xa7cf; stride = 1 };
    { lo = 0xa7d2; hi = 0xa7d4; stride = 2 };
    { lo = 0xa7da; hi = 0xa7f1; stride = 1 };
    { lo = 0xa82d; hi = 0xa82f; stride = 1 };
    { lo = 0xa83a; hi = 0xa83f; stride = 1 };
    { lo = 0xa878; hi = 0xa87f; stride = 1 };
    { lo = 0xa8c6; hi = 0xa8cd; stride = 1 };
    { lo = 0xa8da; hi = 0xa8df; stride = 1 };
    { lo = 0xa954; hi = 0xa95e; stride = 1 };
    { lo = 0xa97d; hi = 0xa97f; stride = 1 };
    { lo = 0xa9ce; hi = 0xa9da; stride = 12 };
    { lo = 0xa9db; hi = 0xa9dd; stride = 1 };
    { lo = 0xa9ff; hi = 0xaa37; stride = 56 };
    { lo = 0xaa38; hi = 0xaa3f; stride = 1 };
    { lo = 0xaa4e; hi = 0xaa4f; stride = 1 };
    { lo = 0xaa5a; hi = 0xaa5b; stride = 1 };
    { lo = 0xaac3; hi = 0xaada; stride = 1 };
    { lo = 0xaaf7; hi = 0xab00; stride = 1 };
    { lo = 0xab07; hi = 0xab08; stride = 1 };
    { lo = 0xab0f; hi = 0xab10; stride = 1 };
    { lo = 0xab17; hi = 0xab1f; stride = 1 };
    { lo = 0xab27; hi = 0xab2f; stride = 8 };
    { lo = 0xab6c; hi = 0xab6f; stride = 1 };
    { lo = 0xabee; hi = 0xabef; stride = 1 };
    { lo = 0xabfa; hi = 0xabff; stride = 1 };
    { lo = 0xd7a4; hi = 0xd7af; stride = 1 };
    { lo = 0xd7c7; hi = 0xd7ca; stride = 1 };
    { lo = 0xd7fc; hi = 0xf8ff; stride = 1 };
    { lo = 0xfa6e; hi = 0xfa6f; stride = 1 };
    { lo = 0xfada; hi = 0xfaff; stride = 1 };
    { lo = 0xfb07; hi = 0xfb12; stride = 1 };
    { lo = 0xfb18; hi = 0xfb1c; stride = 1 };
    { lo = 0xfb37; hi = 0xfb3d; stride = 6 };
    { lo = 0xfb3f; hi = 0xfb45; stride = 3 };
    { lo = 0xfbc3; hi = 0xfbd2; stride = 1 };
    { lo = 0xfd90; hi = 0xfd91; stride = 1 };
    { lo = 0xfdc8; hi = 0xfdce; stride = 1 };
    { lo = 0xfdd0; hi = 0xfdef; stride = 1 };
    { lo = 0xfe1a; hi = 0xfe1f; stride = 1 };
    { lo = 0xfe53; hi = 0xfe67; stride = 20 };
    { lo = 0xfe6c; hi = 0xfe6f; stride = 1 };
    { lo = 0xfe75; hi = 0xfefd; stride = 136 };
    { lo = 0xfefe; hi = 0xff00; stride = 1 };
    { lo = 0xffbf; hi = 0xffc1; stride = 1 };
    { lo = 0xffc8; hi = 0xffc9; stride = 1 };
    { lo = 0xffd0; hi = 0xffd1; stride = 1 };
    { lo = 0xffd8; hi = 0xffd9; stride = 1 };
    { lo = 0xffdd; hi = 0xffdf; stride = 1 };
    { lo = 0xffe7; hi = 0xffef; stride = 8 };
    { lo = 0xfff0; hi = 0xfffb; stride = 1 };
    { lo = 0xfffe; hi = 0xffff; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* cc *)
let _cc = {
  r16 = [|
    { lo = 0x0000; hi = 0x001f; stride = 1 };
    { lo = 0x007f; hi = 0x009f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* cf *)
let _cf = {
  r16 = [|
    { lo = 0x00ad; hi = 0x0600; stride = 1363 };
    { lo = 0x0601; hi = 0x0605; stride = 1 };
    { lo = 0x061c; hi = 0x06dd; stride = 193 };
    { lo = 0x070f; hi = 0x0890; stride = 385 };
    { lo = 0x0891; hi = 0x08e2; stride = 81 };
    { lo = 0x180e; hi = 0x200b; stride = 2045 };
    { lo = 0x200c; hi = 0x200f; stride = 1 };
    { lo = 0x202a; hi = 0x202e; stride = 1 };
    { lo = 0x2060; hi = 0x2064; stride = 1 };
    { lo = 0x2066; hi = 0x206f; stride = 1 };
    { lo = 0xfeff; hi = 0xfff9; stride = 250 };
    { lo = 0xfffa; hi = 0xfffb; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* cn *)
let _cn = {
  r16 = [|
    { lo = 0x0378; hi = 0x0379; stride = 1 };
    { lo = 0x0380; hi = 0x0383; stride = 1 };
    { lo = 0x038b; hi = 0x038d; stride = 2 };
    { lo = 0x03a2; hi = 0x0530; stride = 398 };
    { lo = 0x0557; hi = 0x0558; stride = 1 };
    { lo = 0x058b; hi = 0x058c; stride = 1 };
    { lo = 0x0590; hi = 0x05c8; stride = 56 };
    { lo = 0x05c9; hi = 0x05cf; stride = 1 };
    { lo = 0x05eb; hi = 0x05ee; stride = 1 };
    { lo = 0x05f5; hi = 0x05ff; stride = 1 };
    { lo = 0x070e; hi = 0x074b; stride = 61 };
    { lo = 0x074c; hi = 0x07b2; stride = 102 };
    { lo = 0x07b3; hi = 0x07bf; stride = 1 };
    { lo = 0x07fb; hi = 0x07fc; stride = 1 };
    { lo = 0x082e; hi = 0x082f; stride = 1 };
    { lo = 0x083f; hi = 0x085c; stride = 29 };
    { lo = 0x085d; hi = 0x085f; stride = 2 };
    { lo = 0x086b; hi = 0x086f; stride = 1 };
    { lo = 0x088f; hi = 0x0892; stride = 3 };
    { lo = 0x0893; hi = 0x0897; stride = 1 };
    { lo = 0x0984; hi = 0x098d; stride = 9 };
    { lo = 0x098e; hi = 0x0991; stride = 3 };
    { lo = 0x0992; hi = 0x09a9; stride = 23 };
    { lo = 0x09b1; hi = 0x09b3; stride = 2 };
    { lo = 0x09b4; hi = 0x09b5; stride = 1 };
    { lo = 0x09ba; hi = 0x09bb; stride = 1 };
    { lo = 0x09c5; hi = 0x09c6; stride = 1 };
    { lo = 0x09c9; hi = 0x09ca; stride = 1 };
    { lo = 0x09cf; hi = 0x09d6; stride = 1 };
    { lo = 0x09d8; hi = 0x09db; stride = 1 };
    { lo = 0x09de; hi = 0x09e4; stride = 6 };
    { lo = 0x09e5; hi = 0x09ff; stride = 26 };
    { lo = 0x0a00; hi = 0x0a04; stride = 4 };
    { lo = 0x0a0b; hi = 0x0a0e; stride = 1 };
    { lo = 0x0a11; hi = 0x0a12; stride = 1 };
    { lo = 0x0a29; hi = 0x0a31; stride = 8 };
    { lo = 0x0a34; hi = 0x0a3a; stride = 3 };
    { lo = 0x0a3b; hi = 0x0a3d; stride = 2 };
    { lo = 0x0a43; hi = 0x0a46; stride = 1 };
    { lo = 0x0a49; hi = 0x0a4a; stride = 1 };
    { lo = 0x0a4e; hi = 0x0a50; stride = 1 };
    { lo = 0x0a52; hi = 0x0a58; stride = 1 };
    { lo = 0x0a5d; hi = 0x0a5f; stride = 2 };
    { lo = 0x0a60; hi = 0x0a65; stride = 1 };
    { lo = 0x0a77; hi = 0x0a80; stride = 1 };
    { lo = 0x0a84; hi = 0x0a8e; stride = 10 };
    { lo = 0x0a92; hi = 0x0aa9; stride = 23 };
    { lo = 0x0ab1; hi = 0x0ab4; stride = 3 };
    { lo = 0x0aba; hi = 0x0abb; stride = 1 };
    { lo = 0x0ac6; hi = 0x0ace; stride = 4 };
    { lo = 0x0acf; hi = 0x0ad1; stride = 2 };
    { lo = 0x0ad2; hi = 0x0adf; stride = 1 };
    { lo = 0x0ae4; hi = 0x0ae5; stride = 1 };
    { lo = 0x0af2; hi = 0x0af8; stride = 1 };
    { lo = 0x0b00; hi = 0x0b04; stride = 4 };
    { lo = 0x0b0d; hi = 0x0b0e; stride = 1 };
    { lo = 0x0b11; hi = 0x0b12; stride = 1 };
    { lo = 0x0b29; hi = 0x0b31; stride = 8 };
    { lo = 0x0b34; hi = 0x0b3a; stride = 6 };
    { lo = 0x0b3b; hi = 0x0b45; stride = 10 };
    { lo = 0x0b46; hi = 0x0b49; stride = 3 };
    { lo = 0x0b4a; hi = 0x0b4e; stride = 4 };
    { lo = 0x0b4f; hi = 0x0b54; stride = 1 };
    { lo = 0x0b58; hi = 0x0b5b; stride = 1 };
    { lo = 0x0b5e; hi = 0x0b64; stride = 6 };
    { lo = 0x0b65; hi = 0x0b78; stride = 19 };
    { lo = 0x0b79; hi = 0x0b81; stride = 1 };
    { lo = 0x0b84; hi = 0x0b8b; stride = 7 };
    { lo = 0x0b8c; hi = 0x0b8d; stride = 1 };
    { lo = 0x0b91; hi = 0x0b96; stride = 5 };
    { lo = 0x0b97; hi = 0x0b98; stride = 1 };
    { lo = 0x0b9b; hi = 0x0b9d; stride = 2 };
    { lo = 0x0ba0; hi = 0x0ba2; stride = 1 };
    { lo = 0x0ba5; hi = 0x0ba7; stride = 1 };
    { lo = 0x0bab; hi = 0x0bad; stride = 1 };
    { lo = 0x0bba; hi = 0x0bbd; stride = 1 };
    { lo = 0x0bc3; hi = 0x0bc5; stride = 1 };
    { lo = 0x0bc9; hi = 0x0bce; stride = 5 };
    { lo = 0x0bcf; hi = 0x0bd1; stride = 2 };
    { lo = 0x0bd2; hi = 0x0bd6; stride = 1 };
    { lo = 0x0bd8; hi = 0x0be5; stride = 1 };
    { lo = 0x0bfb; hi = 0x0bff; stride = 1 };
    { lo = 0x0c0d; hi = 0x0c11; stride = 4 };
    { lo = 0x0c29; hi = 0x0c3a; stride = 17 };
    { lo = 0x0c3b; hi = 0x0c45; stride = 10 };
    { lo = 0x0c49; hi = 0x0c4e; stride = 5 };
    { lo = 0x0c4f; hi = 0x0c54; stride = 1 };
    { lo = 0x0c57; hi = 0x0c5b; stride = 4 };
    { lo = 0x0c5c; hi = 0x0c5e; stride = 2 };
    { lo = 0x0c5f; hi = 0x0c64; stride = 5 };
    { lo = 0x0c65; hi = 0x0c70; stride = 11 };
    { lo = 0x0c71; hi = 0x0c76; stride = 1 };
    { lo = 0x0c8d; hi = 0x0c91; stride = 4 };
    { lo = 0x0ca9; hi = 0x0cb4; stride = 11 };
    { lo = 0x0cba; hi = 0x0cbb; stride = 1 };
    { lo = 0x0cc5; hi = 0x0cc9; stride = 4 };
    { lo = 0x0cce; hi = 0x0cd4; stride = 1 };
    { lo = 0x0cd7; hi = 0x0cdc; stride = 1 };
    { lo = 0x0cdf; hi = 0x0ce4; stride = 5 };
    { lo = 0x0ce5; hi = 0x0cf0; stride = 11 };
    { lo = 0x0cf4; hi = 0x0cff; stride = 1 };
    { lo = 0x0d0d; hi = 0x0d11; stride = 4 };
    { lo = 0x0d45; hi = 0x0d49; stride = 4 };
    { lo = 0x0d50; hi = 0x0d53; stride = 1 };
    { lo = 0x0d64; hi = 0x0d65; stride = 1 };
    { lo = 0x0d80; hi = 0x0d84; stride = 4 };
    { lo = 0x0d97; hi = 0x0d99; stride = 1 };
    { lo = 0x0db2; hi = 0x0dbc; stride = 10 };
    { lo = 0x0dbe; hi = 0x0dbf; stride = 1 };
    { lo = 0x0dc7; hi = 0x0dc9; stride = 1 };
    { lo = 0x0dcb; hi = 0x0dce; stride = 1 };
    { lo = 0x0dd5; hi = 0x0dd7; stride = 2 };
    { lo = 0x0de0; hi = 0x0de5; stride = 1 };
    { lo = 0x0df0; hi = 0x0df1; stride = 1 };
    { lo = 0x0df5; hi = 0x0e00; stride = 1 };
    { lo = 0x0e3b; hi = 0x0e3e; stride = 1 };
    { lo = 0x0e5c; hi = 0x0e80; stride = 1 };
    { lo = 0x0e83; hi = 0x0e85; stride = 2 };
    { lo = 0x0e8b; hi = 0x0ea4; stride = 25 };
    { lo = 0x0ea6; hi = 0x0ebe; stride = 24 };
    { lo = 0x0ebf; hi = 0x0ec5; stride = 6 };
    { lo = 0x0ec7; hi = 0x0ecf; stride = 8 };
    { lo = 0x0eda; hi = 0x0edb; stride = 1 };
    { lo = 0x0ee0; hi = 0x0eff; stride = 1 };
    { lo = 0x0f48; hi = 0x0f6d; stride = 37 };
    { lo = 0x0f6e; hi = 0x0f70; stride = 1 };
    { lo = 0x0f98; hi = 0x0fbd; stride = 37 };
    { lo = 0x0fcd; hi = 0x0fdb; stride = 14 };
    { lo = 0x0fdc; hi = 0x0fff; stride = 1 };
    { lo = 0x10c6; hi = 0x10c8; stride = 2 };
    { lo = 0x10c9; hi = 0x10cc; stride = 1 };
    { lo = 0x10ce; hi = 0x10cf; stride = 1 };
    { lo = 0x1249; hi = 0x124e; stride = 5 };
    { lo = 0x124f; hi = 0x1257; stride = 8 };
    { lo = 0x1259; hi = 0x125e; stride = 5 };
    { lo = 0x125f; hi = 0x1289; stride = 42 };
    { lo = 0x128e; hi = 0x128f; stride = 1 };
    { lo = 0x12b1; hi = 0x12b6; stride = 5 };
    { lo = 0x12b7; hi = 0x12bf; stride = 8 };
    { lo = 0x12c1; hi = 0x12c6; stride = 5 };
    { lo = 0x12c7; hi = 0x12d7; stride = 16 };
    { lo = 0x1311; hi = 0x1316; stride = 5 };
    { lo = 0x1317; hi = 0x135b; stride = 68 };
    { lo = 0x135c; hi = 0x137d; stride = 33 };
    { lo = 0x137e; hi = 0x137f; stride = 1 };
    { lo = 0x139a; hi = 0x139f; stride = 1 };
    { lo = 0x13f6; hi = 0x13f7; stride = 1 };
    { lo = 0x13fe; hi = 0x13ff; stride = 1 };
    { lo = 0x169d; hi = 0x169f; stride = 1 };
    { lo = 0x16f9; hi = 0x16ff; stride = 1 };
    { lo = 0x1716; hi = 0x171e; stride = 1 };
    { lo = 0x1737; hi = 0x173f; stride = 1 };
    { lo = 0x1754; hi = 0x175f; stride = 1 };
    { lo = 0x176d; hi = 0x1771; stride = 4 };
    { lo = 0x1774; hi = 0x177f; stride = 1 };
    { lo = 0x17de; hi = 0x17df; stride = 1 };
    { lo = 0x17ea; hi = 0x17ef; stride = 1 };
    { lo = 0x17fa; hi = 0x17ff; stride = 1 };
    { lo = 0x181a; hi = 0x181f; stride = 1 };
    { lo = 0x1879; hi = 0x187f; stride = 1 };
    { lo = 0x18ab; hi = 0x18af; stride = 1 };
    { lo = 0x18f6; hi = 0x18ff; stride = 1 };
    { lo = 0x191f; hi = 0x192c; stride = 13 };
    { lo = 0x192d; hi = 0x192f; stride = 1 };
    { lo = 0x193c; hi = 0x193f; stride = 1 };
    { lo = 0x1941; hi = 0x1943; stride = 1 };
    { lo = 0x196e; hi = 0x196f; stride = 1 };
    { lo = 0x1975; hi = 0x197f; stride = 1 };
    { lo = 0x19ac; hi = 0x19af; stride = 1 };
    { lo = 0x19ca; hi = 0x19cf; stride = 1 };
    { lo = 0x19db; hi = 0x19dd; stride = 1 };
    { lo = 0x1a1c; hi = 0x1a1d; stride = 1 };
    { lo = 0x1a5f; hi = 0x1a7d; stride = 30 };
    { lo = 0x1a7e; hi = 0x1a8a; stride = 12 };
    { lo = 0x1a8b; hi = 0x1a8f; stride = 1 };
    { lo = 0x1a9a; hi = 0x1a9f; stride = 1 };
    { lo = 0x1aae; hi = 0x1aaf; stride = 1 };
    { lo = 0x1acf; hi = 0x1aff; stride = 1 };
    { lo = 0x1b4d; hi = 0x1b4f; stride = 1 };
    { lo = 0x1b7f; hi = 0x1bf4; stride = 117 };
    { lo = 0x1bf5; hi = 0x1bfb; stride = 1 };
    { lo = 0x1c38; hi = 0x1c3a; stride = 1 };
    { lo = 0x1c4a; hi = 0x1c4c; stride = 1 };
    { lo = 0x1c89; hi = 0x1c8f; stride = 1 };
    { lo = 0x1cbb; hi = 0x1cbc; stride = 1 };
    { lo = 0x1cc8; hi = 0x1ccf; stride = 1 };
    { lo = 0x1cfb; hi = 0x1cff; stride = 1 };
    { lo = 0x1f16; hi = 0x1f17; stride = 1 };
    { lo = 0x1f1e; hi = 0x1f1f; stride = 1 };
    { lo = 0x1f46; hi = 0x1f47; stride = 1 };
    { lo = 0x1f4e; hi = 0x1f4f; stride = 1 };
    { lo = 0x1f58; hi = 0x1f5e; stride = 2 };
    { lo = 0x1f7e; hi = 0x1f7f; stride = 1 };
    { lo = 0x1fb5; hi = 0x1fc5; stride = 16 };
    { lo = 0x1fd4; hi = 0x1fd5; stride = 1 };
    { lo = 0x1fdc; hi = 0x1ff0; stride = 20 };
    { lo = 0x1ff1; hi = 0x1ff5; stride = 4 };
    { lo = 0x1fff; hi = 0x2065; stride = 102 };
    { lo = 0x2072; hi = 0x2073; stride = 1 };
    { lo = 0x208f; hi = 0x209d; stride = 14 };
    { lo = 0x209e; hi = 0x209f; stride = 1 };
    { lo = 0x20c1; hi = 0x20cf; stride = 1 };
    { lo = 0x20f1; hi = 0x20ff; stride = 1 };
    { lo = 0x218c; hi = 0x218f; stride = 1 };
    { lo = 0x2427; hi = 0x243f; stride = 1 };
    { lo = 0x244b; hi = 0x245f; stride = 1 };
    { lo = 0x2b74; hi = 0x2b75; stride = 1 };
    { lo = 0x2b96; hi = 0x2cf4; stride = 350 };
    { lo = 0x2cf5; hi = 0x2cf8; stride = 1 };
    { lo = 0x2d26; hi = 0x2d28; stride = 2 };
    { lo = 0x2d29; hi = 0x2d2c; stride = 1 };
    { lo = 0x2d2e; hi = 0x2d2f; stride = 1 };
    { lo = 0x2d68; hi = 0x2d6e; stride = 1 };
    { lo = 0x2d71; hi = 0x2d7e; stride = 1 };
    { lo = 0x2d97; hi = 0x2d9f; stride = 1 };
    { lo = 0x2da7; hi = 0x2ddf; stride = 8 };
    { lo = 0x2e5e; hi = 0x2e7f; stride = 1 };
    { lo = 0x2e9a; hi = 0x2ef4; stride = 90 };
    { lo = 0x2ef5; hi = 0x2eff; stride = 1 };
    { lo = 0x2fd6; hi = 0x2fef; stride = 1 };
    { lo = 0x2ffc; hi = 0x2fff; stride = 1 };
    { lo = 0x3040; hi = 0x3097; stride = 87 };
    { lo = 0x3098; hi = 0x3100; stride = 104 };
    { lo = 0x3101; hi = 0x3104; stride = 1 };
    { lo = 0x3130; hi = 0x318f; stride = 95 };
    { lo = 0x31e4; hi = 0x31ef; stride = 1 };
    { lo = 0x321f; hi = 0xa48d; stride = 29294 };
    { lo = 0xa48e; hi = 0xa48f; stride = 1 };
    { lo = 0xa4c7; hi = 0xa4cf; stride = 1 };
    { lo = 0xa62c; hi = 0xa63f; stride = 1 };
    { lo = 0xa6f8; hi = 0xa6ff; stride = 1 };
    { lo = 0xa7cb; hi = 0xa7cf; stride = 1 };
    { lo = 0xa7d2; hi = 0xa7d4; stride = 2 };
    { lo = 0xa7da; hi = 0xa7f1; stride = 1 };
    { lo = 0xa82d; hi = 0xa82f; stride = 1 };
    { lo = 0xa83a; hi = 0xa83f; stride = 1 };
    { lo = 0xa878; hi = 0xa87f; stride = 1 };
    { lo = 0xa8c6; hi = 0xa8cd; stride = 1 };
    { lo = 0xa8da; hi = 0xa8df; stride = 1 };
    { lo = 0xa954; hi = 0xa95e; stride = 1 };
    { lo = 0xa97d; hi = 0xa97f; stride = 1 };
    { lo = 0xa9ce; hi = 0xa9da; stride = 12 };
    { lo = 0xa9db; hi = 0xa9dd; stride = 1 };
    { lo = 0xa9ff; hi = 0xaa37; stride = 56 };
    { lo = 0xaa38; hi = 0xaa3f; stride = 1 };
    { lo = 0xaa4e; hi = 0xaa4f; stride = 1 };
    { lo = 0xaa5a; hi = 0xaa5b; stride = 1 };
    { lo = 0xaac3; hi = 0xaada; stride = 1 };
    { lo = 0xaaf7; hi = 0xab00; stride = 1 };
    { lo = 0xab07; hi = 0xab08; stride = 1 };
    { lo = 0xab0f; hi = 0xab10; stride = 1 };
    { lo = 0xab17; hi = 0xab1f; stride = 1 };
    { lo = 0xab27; hi = 0xab2f; stride = 8 };
    { lo = 0xab6c; hi = 0xab6f; stride = 1 };
    { lo = 0xabee; hi = 0xabef; stride = 1 };
    { lo = 0xabfa; hi = 0xabff; stride = 1 };
    { lo = 0xd7a4; hi = 0xd7af; stride = 1 };
    { lo = 0xd7c7; hi = 0xd7ca; stride = 1 };
    { lo = 0xd7fc; hi = 0xd7ff; stride = 1 };
    { lo = 0xfa6e; hi = 0xfa6f; stride = 1 };
    { lo = 0xfada; hi = 0xfaff; stride = 1 };
    { lo = 0xfb07; hi = 0xfb12; stride = 1 };
    { lo = 0xfb18; hi = 0xfb1c; stride = 1 };
    { lo = 0xfb37; hi = 0xfb3d; stride = 6 };
    { lo = 0xfb3f; hi = 0xfb45; stride = 3 };
    { lo = 0xfbc3; hi = 0xfbd2; stride = 1 };
    { lo = 0xfd90; hi = 0xfd91; stride = 1 };
    { lo = 0xfdc8; hi = 0xfdce; stride = 1 };
    { lo = 0xfdd0; hi = 0xfdef; stride = 1 };
    { lo = 0xfe1a; hi = 0xfe1f; stride = 1 };
    { lo = 0xfe53; hi = 0xfe67; stride = 20 };
    { lo = 0xfe6c; hi = 0xfe6f; stride = 1 };
    { lo = 0xfe75; hi = 0xfefd; stride = 136 };
    { lo = 0xfefe; hi = 0xff00; stride = 2 };
    { lo = 0xffbf; hi = 0xffc1; stride = 1 };
    { lo = 0xffc8; hi = 0xffc9; stride = 1 };
    { lo = 0xffd0; hi = 0xffd1; stride = 1 };
    { lo = 0xffd8; hi = 0xffd9; stride = 1 };
    { lo = 0xffdd; hi = 0xffdf; stride = 1 };
    { lo = 0xffe7; hi = 0xffef; stride = 8 };
    { lo = 0xfff0; hi = 0xfff8; stride = 1 };
    { lo = 0xfffe; hi = 0xffff; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* co *)
let _co = {
  r16 = [|
    { lo = 0xe000; hi = 0xf8ff; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* cs *)
let _cs = {
  r16 = [|
    { lo = 0xd800; hi = 0xdfff; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* l *)
let _l = {
  r16 = [|
    { lo = 0x0041; hi = 0x005a; stride = 1 };
    { lo = 0x0061; hi = 0x007a; stride = 1 };
    { lo = 0x00aa; hi = 0x00b5; stride = 11 };
    { lo = 0x00ba; hi = 0x00c0; stride = 6 };
    { lo = 0x00c1; hi = 0x00d6; stride = 1 };
    { lo = 0x00d8; hi = 0x00f6; stride = 1 };
    { lo = 0x00f8; hi = 0x02c1; stride = 1 };
    { lo = 0x02c6; hi = 0x02d1; stride = 1 };
    { lo = 0x02e0; hi = 0x02e4; stride = 1 };
    { lo = 0x02ec; hi = 0x02ee; stride = 2 };
    { lo = 0x0370; hi = 0x0374; stride = 1 };
    { lo = 0x0376; hi = 0x0377; stride = 1 };
    { lo = 0x037a; hi = 0x037d; stride = 1 };
    { lo = 0x037f; hi = 0x0386; stride = 7 };
    { lo = 0x0388; hi = 0x038a; stride = 1 };
    { lo = 0x038c; hi = 0x038e; stride = 2 };
    { lo = 0x038f; hi = 0x03a1; stride = 1 };
    { lo = 0x03a3; hi = 0x03f5; stride = 1 };
    { lo = 0x03f7; hi = 0x0481; stride = 1 };
    { lo = 0x048a; hi = 0x052f; stride = 1 };
    { lo = 0x0531; hi = 0x0556; stride = 1 };
    { lo = 0x0559; hi = 0x0560; stride = 7 };
    { lo = 0x0561; hi = 0x0588; stride = 1 };
    { lo = 0x05d0; hi = 0x05ea; stride = 1 };
    { lo = 0x05ef; hi = 0x05f2; stride = 1 };
    { lo = 0x0620; hi = 0x064a; stride = 1 };
    { lo = 0x066e; hi = 0x066f; stride = 1 };
    { lo = 0x0671; hi = 0x06d3; stride = 1 };
    { lo = 0x06d5; hi = 0x06e5; stride = 16 };
    { lo = 0x06e6; hi = 0x06ee; stride = 8 };
    { lo = 0x06ef; hi = 0x06fa; stride = 11 };
    { lo = 0x06fb; hi = 0x06fc; stride = 1 };
    { lo = 0x06ff; hi = 0x0710; stride = 17 };
    { lo = 0x0712; hi = 0x072f; stride = 1 };
    { lo = 0x074d; hi = 0x07a5; stride = 1 };
    { lo = 0x07b1; hi = 0x07ca; stride = 25 };
    { lo = 0x07cb; hi = 0x07ea; stride = 1 };
    { lo = 0x07f4; hi = 0x07f5; stride = 1 };
    { lo = 0x07fa; hi = 0x0800; stride = 6 };
    { lo = 0x0801; hi = 0x0815; stride = 1 };
    { lo = 0x081a; hi = 0x0824; stride = 10 };
    { lo = 0x0828; hi = 0x0840; stride = 24 };
    { lo = 0x0841; hi = 0x0858; stride = 1 };
    { lo = 0x0860; hi = 0x086a; stride = 1 };
    { lo = 0x0870; hi = 0x0887; stride = 1 };
    { lo = 0x0889; hi = 0x088e; stride = 1 };
    { lo = 0x08a0; hi = 0x08c9; stride = 1 };
    { lo = 0x0904; hi = 0x0939; stride = 1 };
    { lo = 0x093d; hi = 0x0950; stride = 19 };
    { lo = 0x0958; hi = 0x0961; stride = 1 };
    { lo = 0x0971; hi = 0x0980; stride = 1 };
    { lo = 0x0985; hi = 0x098c; stride = 1 };
    { lo = 0x098f; hi = 0x0990; stride = 1 };
    { lo = 0x0993; hi = 0x09a8; stride = 1 };
    { lo = 0x09aa; hi = 0x09b0; stride = 1 };
    { lo = 0x09b2; hi = 0x09b6; stride = 4 };
    { lo = 0x09b7; hi = 0x09b9; stride = 1 };
    { lo = 0x09bd; hi = 0x09ce; stride = 17 };
    { lo = 0x09dc; hi = 0x09dd; stride = 1 };
    { lo = 0x09df; hi = 0x09e1; stride = 1 };
    { lo = 0x09f0; hi = 0x09f1; stride = 1 };
    { lo = 0x09fc; hi = 0x0a05; stride = 9 };
    { lo = 0x0a06; hi = 0x0a0a; stride = 1 };
    { lo = 0x0a0f; hi = 0x0a10; stride = 1 };
    { lo = 0x0a13; hi = 0x0a28; stride = 1 };
    { lo = 0x0a2a; hi = 0x0a30; stride = 1 };
    { lo = 0x0a32; hi = 0x0a33; stride = 1 };
    { lo = 0x0a35; hi = 0x0a36; stride = 1 };
    { lo = 0x0a38; hi = 0x0a39; stride = 1 };
    { lo = 0x0a59; hi = 0x0a5c; stride = 1 };
    { lo = 0x0a5e; hi = 0x0a72; stride = 20 };
    { lo = 0x0a73; hi = 0x0a74; stride = 1 };
    { lo = 0x0a85; hi = 0x0a8d; stride = 1 };
    { lo = 0x0a8f; hi = 0x0a91; stride = 1 };
    { lo = 0x0a93; hi = 0x0aa8; stride = 1 };
    { lo = 0x0aaa; hi = 0x0ab0; stride = 1 };
    { lo = 0x0ab2; hi = 0x0ab3; stride = 1 };
    { lo = 0x0ab5; hi = 0x0ab9; stride = 1 };
    { lo = 0x0abd; hi = 0x0ad0; stride = 19 };
    { lo = 0x0ae0; hi = 0x0ae1; stride = 1 };
    { lo = 0x0af9; hi = 0x0b05; stride = 12 };
    { lo = 0x0b06; hi = 0x0b0c; stride = 1 };
    { lo = 0x0b0f; hi = 0x0b10; stride = 1 };
    { lo = 0x0b13; hi = 0x0b28; stride = 1 };
    { lo = 0x0b2a; hi = 0x0b30; stride = 1 };
    { lo = 0x0b32; hi = 0x0b33; stride = 1 };
    { lo = 0x0b35; hi = 0x0b39; stride = 1 };
    { lo = 0x0b3d; hi = 0x0b5c; stride = 31 };
    { lo = 0x0b5d; hi = 0x0b5f; stride = 2 };
    { lo = 0x0b60; hi = 0x0b61; stride = 1 };
    { lo = 0x0b71; hi = 0x0b83; stride = 18 };
    { lo = 0x0b85; hi = 0x0b8a; stride = 1 };
    { lo = 0x0b8e; hi = 0x0b90; stride = 1 };
    { lo = 0x0b92; hi = 0x0b95; stride = 1 };
    { lo = 0x0b99; hi = 0x0b9a; stride = 1 };
    { lo = 0x0b9c; hi = 0x0b9e; stride = 2 };
    { lo = 0x0b9f; hi = 0x0ba3; stride = 4 };
    { lo = 0x0ba4; hi = 0x0ba8; stride = 4 };
    { lo = 0x0ba9; hi = 0x0baa; stride = 1 };
    { lo = 0x0bae; hi = 0x0bb9; stride = 1 };
    { lo = 0x0bd0; hi = 0x0c05; stride = 53 };
    { lo = 0x0c06; hi = 0x0c0c; stride = 1 };
    { lo = 0x0c0e; hi = 0x0c10; stride = 1 };
    { lo = 0x0c12; hi = 0x0c28; stride = 1 };
    { lo = 0x0c2a; hi = 0x0c39; stride = 1 };
    { lo = 0x0c3d; hi = 0x0c58; stride = 27 };
    { lo = 0x0c59; hi = 0x0c5a; stride = 1 };
    { lo = 0x0c5d; hi = 0x0c60; stride = 3 };
    { lo = 0x0c61; hi = 0x0c80; stride = 31 };
    { lo = 0x0c85; hi = 0x0c8c; stride = 1 };
    { lo = 0x0c8e; hi = 0x0c90; stride = 1 };
    { lo = 0x0c92; hi = 0x0ca8; stride = 1 };
    { lo = 0x0caa; hi = 0x0cb3; stride = 1 };
    { lo = 0x0cb5; hi = 0x0cb9; stride = 1 };
    { lo = 0x0cbd; hi = 0x0cdd; stride = 32 };
    { lo = 0x0cde; hi = 0x0ce0; stride = 2 };
    { lo = 0x0ce1; hi = 0x0cf1; stride = 16 };
    { lo = 0x0cf2; hi = 0x0d04; stride = 18 };
    { lo = 0x0d05; hi = 0x0d0c; stride = 1 };
    { lo = 0x0d0e; hi = 0x0d10; stride = 1 };
    { lo = 0x0d12; hi = 0x0d3a; stride = 1 };
    { lo = 0x0d3d; hi = 0x0d4e; stride = 17 };
    { lo = 0x0d54; hi = 0x0d56; stride = 1 };
    { lo = 0x0d5f; hi = 0x0d61; stride = 1 };
    { lo = 0x0d7a; hi = 0x0d7f; stride = 1 };
    { lo = 0x0d85; hi = 0x0d96; stride = 1 };
    { lo = 0x0d9a; hi = 0x0db1; stride = 1 };
    { lo = 0x0db3; hi = 0x0dbb; stride = 1 };
    { lo = 0x0dbd; hi = 0x0dc0; stride = 3 };
    { lo = 0x0dc1; hi = 0x0dc6; stride = 1 };
    { lo = 0x0e01; hi = 0x0e30; stride = 1 };
    { lo = 0x0e32; hi = 0x0e33; stride = 1 };
    { lo = 0x0e40; hi = 0x0e46; stride = 1 };
    { lo = 0x0e81; hi = 0x0e82; stride = 1 };
    { lo = 0x0e84; hi = 0x0e86; stride = 2 };
    { lo = 0x0e87; hi = 0x0e8a; stride = 1 };
    { lo = 0x0e8c; hi = 0x0ea3; stride = 1 };
    { lo = 0x0ea5; hi = 0x0ea7; stride = 2 };
    { lo = 0x0ea8; hi = 0x0eb0; stride = 1 };
    { lo = 0x0eb2; hi = 0x0eb3; stride = 1 };
    { lo = 0x0ebd; hi = 0x0ec0; stride = 3 };
    { lo = 0x0ec1; hi = 0x0ec4; stride = 1 };
    { lo = 0x0ec6; hi = 0x0edc; stride = 22 };
    { lo = 0x0edd; hi = 0x0edf; stride = 1 };
    { lo = 0x0f00; hi = 0x0f40; stride = 64 };
    { lo = 0x0f41; hi = 0x0f47; stride = 1 };
    { lo = 0x0f49; hi = 0x0f6c; stride = 1 };
    { lo = 0x0f88; hi = 0x0f8c; stride = 1 };
    { lo = 0x1000; hi = 0x102a; stride = 1 };
    { lo = 0x103f; hi = 0x1050; stride = 17 };
    { lo = 0x1051; hi = 0x1055; stride = 1 };
    { lo = 0x105a; hi = 0x105d; stride = 1 };
    { lo = 0x1061; hi = 0x1065; stride = 4 };
    { lo = 0x1066; hi = 0x106e; stride = 8 };
    { lo = 0x106f; hi = 0x1070; stride = 1 };
    { lo = 0x1075; hi = 0x1081; stride = 1 };
    { lo = 0x108e; hi = 0x10a0; stride = 18 };
    { lo = 0x10a1; hi = 0x10c5; stride = 1 };
    { lo = 0x10c7; hi = 0x10cd; stride = 6 };
    { lo = 0x10d0; hi = 0x10fa; stride = 1 };
    { lo = 0x10fc; hi = 0x1248; stride = 1 };
    { lo = 0x124a; hi = 0x124d; stride = 1 };
    { lo = 0x1250; hi = 0x1256; stride = 1 };
    { lo = 0x1258; hi = 0x125a; stride = 2 };
    { lo = 0x125b; hi = 0x125d; stride = 1 };
    { lo = 0x1260; hi = 0x1288; stride = 1 };
    { lo = 0x128a; hi = 0x128d; stride = 1 };
    { lo = 0x1290; hi = 0x12b0; stride = 1 };
    { lo = 0x12b2; hi = 0x12b5; stride = 1 };
    { lo = 0x12b8; hi = 0x12be; stride = 1 };
    { lo = 0x12c0; hi = 0x12c2; stride = 2 };
    { lo = 0x12c3; hi = 0x12c5; stride = 1 };
    { lo = 0x12c8; hi = 0x12d6; stride = 1 };
    { lo = 0x12d8; hi = 0x1310; stride = 1 };
    { lo = 0x1312; hi = 0x1315; stride = 1 };
    { lo = 0x1318; hi = 0x135a; stride = 1 };
    { lo = 0x1380; hi = 0x138f; stride = 1 };
    { lo = 0x13a0; hi = 0x13f5; stride = 1 };
    { lo = 0x13f8; hi = 0x13fd; stride = 1 };
    { lo = 0x1401; hi = 0x166c; stride = 1 };
    { lo = 0x166f; hi = 0x167f; stride = 1 };
    { lo = 0x1681; hi = 0x169a; stride = 1 };
    { lo = 0x16a0; hi = 0x16ea; stride = 1 };
    { lo = 0x16f1; hi = 0x16f8; stride = 1 };
    { lo = 0x1700; hi = 0x1711; stride = 1 };
    { lo = 0x171f; hi = 0x1731; stride = 1 };
    { lo = 0x1740; hi = 0x1751; stride = 1 };
    { lo = 0x1760; hi = 0x176c; stride = 1 };
    { lo = 0x176e; hi = 0x1770; stride = 1 };
    { lo = 0x1780; hi = 0x17b3; stride = 1 };
    { lo = 0x17d7; hi = 0x17dc; stride = 5 };
    { lo = 0x1820; hi = 0x1878; stride = 1 };
    { lo = 0x1880; hi = 0x1884; stride = 1 };
    { lo = 0x1887; hi = 0x18a8; stride = 1 };
    { lo = 0x18aa; hi = 0x18b0; stride = 6 };
    { lo = 0x18b1; hi = 0x18f5; stride = 1 };
    { lo = 0x1900; hi = 0x191e; stride = 1 };
    { lo = 0x1950; hi = 0x196d; stride = 1 };
    { lo = 0x1970; hi = 0x1974; stride = 1 };
    { lo = 0x1980; hi = 0x19ab; stride = 1 };
    { lo = 0x19b0; hi = 0x19c9; stride = 1 };
    { lo = 0x1a00; hi = 0x1a16; stride = 1 };
    { lo = 0x1a20; hi = 0x1a54; stride = 1 };
    { lo = 0x1aa7; hi = 0x1b05; stride = 94 };
    { lo = 0x1b06; hi = 0x1b33; stride = 1 };
    { lo = 0x1b45; hi = 0x1b4c; stride = 1 };
    { lo = 0x1b83; hi = 0x1ba0; stride = 1 };
    { lo = 0x1bae; hi = 0x1baf; stride = 1 };
    { lo = 0x1bba; hi = 0x1be5; stride = 1 };
    { lo = 0x1c00; hi = 0x1c23; stride = 1 };
    { lo = 0x1c4d; hi = 0x1c4f; stride = 1 };
    { lo = 0x1c5a; hi = 0x1c7d; stride = 1 };
    { lo = 0x1c80; hi = 0x1c88; stride = 1 };
    { lo = 0x1c90; hi = 0x1cba; stride = 1 };
    { lo = 0x1cbd; hi = 0x1cbf; stride = 1 };
    { lo = 0x1ce9; hi = 0x1cec; stride = 1 };
    { lo = 0x1cee; hi = 0x1cf3; stride = 1 };
    { lo = 0x1cf5; hi = 0x1cf6; stride = 1 };
    { lo = 0x1cfa; hi = 0x1d00; stride = 6 };
    { lo = 0x1d01; hi = 0x1dbf; stride = 1 };
    { lo = 0x1e00; hi = 0x1f15; stride = 1 };
    { lo = 0x1f18; hi = 0x1f1d; stride = 1 };
    { lo = 0x1f20; hi = 0x1f45; stride = 1 };
    { lo = 0x1f48; hi = 0x1f4d; stride = 1 };
    { lo = 0x1f50; hi = 0x1f57; stride = 1 };
    { lo = 0x1f59; hi = 0x1f5f; stride = 2 };
    { lo = 0x1f60; hi = 0x1f7d; stride = 1 };
    { lo = 0x1f80; hi = 0x1fb4; stride = 1 };
    { lo = 0x1fb6; hi = 0x1fbc; stride = 1 };
    { lo = 0x1fbe; hi = 0x1fc2; stride = 4 };
    { lo = 0x1fc3; hi = 0x1fc4; stride = 1 };
    { lo = 0x1fc6; hi = 0x1fcc; stride = 1 };
    { lo = 0x1fd0; hi = 0x1fd3; stride = 1 };
    { lo = 0x1fd6; hi = 0x1fdb; stride = 1 };
    { lo = 0x1fe0; hi = 0x1fec; stride = 1 };
    { lo = 0x1ff2; hi = 0x1ff4; stride = 1 };
    { lo = 0x1ff6; hi = 0x1ffc; stride = 1 };
    { lo = 0x2071; hi = 0x207f; stride = 14 };
    { lo = 0x2090; hi = 0x209c; stride = 1 };
    { lo = 0x2102; hi = 0x2107; stride = 5 };
    { lo = 0x210a; hi = 0x2113; stride = 1 };
    { lo = 0x2115; hi = 0x2119; stride = 4 };
    { lo = 0x211a; hi = 0x211d; stride = 1 };
    { lo = 0x2124; hi = 0x212a; stride = 2 };
    { lo = 0x212b; hi = 0x212d; stride = 1 };
    { lo = 0x212f; hi = 0x2139; stride = 1 };
    { lo = 0x213c; hi = 0x213f; stride = 1 };
    { lo = 0x2145; hi = 0x2149; stride = 1 };
    { lo = 0x214e; hi = 0x2183; stride = 53 };
    { lo = 0x2184; hi = 0x2c00; stride = 2684 };
    { lo = 0x2c01; hi = 0x2ce4; stride = 1 };
    { lo = 0x2ceb; hi = 0x2cee; stride = 1 };
    { lo = 0x2cf2; hi = 0x2cf3; stride = 1 };
    { lo = 0x2d00; hi = 0x2d25; stride = 1 };
    { lo = 0x2d27; hi = 0x2d2d; stride = 6 };
    { lo = 0x2d30; hi = 0x2d67; stride = 1 };
    { lo = 0x2d6f; hi = 0x2d80; stride = 17 };
    { lo = 0x2d81; hi = 0x2d96; stride = 1 };
    { lo = 0x2da0; hi = 0x2da6; stride = 1 };
    { lo = 0x2da8; hi = 0x2dae; stride = 1 };
    { lo = 0x2db0; hi = 0x2db6; stride = 1 };
    { lo = 0x2db8; hi = 0x2dbe; stride = 1 };
    { lo = 0x2dc0; hi = 0x2dc6; stride = 1 };
    { lo = 0x2dc8; hi = 0x2dce; stride = 1 };
    { lo = 0x2dd0; hi = 0x2dd6; stride = 1 };
    { lo = 0x2dd8; hi = 0x2dde; stride = 1 };
    { lo = 0x2e2f; hi = 0x3005; stride = 470 };
    { lo = 0x3006; hi = 0x3031; stride = 43 };
    { lo = 0x3032; hi = 0x3035; stride = 1 };
    { lo = 0x303b; hi = 0x303c; stride = 1 };
    { lo = 0x3041; hi = 0x3096; stride = 1 };
    { lo = 0x309d; hi = 0x309f; stride = 1 };
    { lo = 0x30a1; hi = 0x30fa; stride = 1 };
    { lo = 0x30fc; hi = 0x30ff; stride = 1 };
    { lo = 0x3105; hi = 0x312f; stride = 1 };
    { lo = 0x3131; hi = 0x318e; stride = 1 };
    { lo = 0x31a0; hi = 0x31bf; stride = 1 };
    { lo = 0x31f0; hi = 0x31ff; stride = 1 };
    { lo = 0x3400; hi = 0x4dbf; stride = 1 };
    { lo = 0x4e00; hi = 0xa48c; stride = 1 };
    { lo = 0xa4d0; hi = 0xa4fd; stride = 1 };
    { lo = 0xa500; hi = 0xa60c; stride = 1 };
    { lo = 0xa610; hi = 0xa61f; stride = 1 };
    { lo = 0xa62a; hi = 0xa62b; stride = 1 };
    { lo = 0xa640; hi = 0xa66e; stride = 1 };
    { lo = 0xa67f; hi = 0xa69d; stride = 1 };
    { lo = 0xa6a0; hi = 0xa6e5; stride = 1 };
    { lo = 0xa717; hi = 0xa71f; stride = 1 };
    { lo = 0xa722; hi = 0xa788; stride = 1 };
    { lo = 0xa78b; hi = 0xa7ca; stride = 1 };
    { lo = 0xa7d0; hi = 0xa7d1; stride = 1 };
    { lo = 0xa7d3; hi = 0xa7d5; stride = 2 };
    { lo = 0xa7d6; hi = 0xa7d9; stride = 1 };
    { lo = 0xa7f2; hi = 0xa801; stride = 1 };
    { lo = 0xa803; hi = 0xa805; stride = 1 };
    { lo = 0xa807; hi = 0xa80a; stride = 1 };
    { lo = 0xa80c; hi = 0xa822; stride = 1 };
    { lo = 0xa840; hi = 0xa873; stride = 1 };
    { lo = 0xa882; hi = 0xa8b3; stride = 1 };
    { lo = 0xa8f2; hi = 0xa8f7; stride = 1 };
    { lo = 0xa8fb; hi = 0xa8fd; stride = 2 };
    { lo = 0xa8fe; hi = 0xa90a; stride = 12 };
    { lo = 0xa90b; hi = 0xa925; stride = 1 };
    { lo = 0xa930; hi = 0xa946; stride = 1 };
    { lo = 0xa960; hi = 0xa97c; stride = 1 };
    { lo = 0xa984; hi = 0xa9b2; stride = 1 };
    { lo = 0xa9cf; hi = 0xa9e0; stride = 17 };
    { lo = 0xa9e1; hi = 0xa9e4; stride = 1 };
    { lo = 0xa9e6; hi = 0xa9ef; stride = 1 };
    { lo = 0xa9fa; hi = 0xa9fe; stride = 1 };
    { lo = 0xaa00; hi = 0xaa28; stride = 1 };
    { lo = 0xaa40; hi = 0xaa42; stride = 1 };
    { lo = 0xaa44; hi = 0xaa4b; stride = 1 };
    { lo = 0xaa60; hi = 0xaa76; stride = 1 };
    { lo = 0xaa7a; hi = 0xaa7e; stride = 4 };
    { lo = 0xaa7f; hi = 0xaaaf; stride = 1 };
    { lo = 0xaab1; hi = 0xaab5; stride = 4 };
    { lo = 0xaab6; hi = 0xaab9; stride = 3 };
    { lo = 0xaaba; hi = 0xaabd; stride = 1 };
    { lo = 0xaac0; hi = 0xaac2; stride = 2 };
    { lo = 0xaadb; hi = 0xaadd; stride = 1 };
    { lo = 0xaae0; hi = 0xaaea; stride = 1 };
    { lo = 0xaaf2; hi = 0xaaf4; stride = 1 };
    { lo = 0xab01; hi = 0xab06; stride = 1 };
    { lo = 0xab09; hi = 0xab0e; stride = 1 };
    { lo = 0xab11; hi = 0xab16; stride = 1 };
    { lo = 0xab20; hi = 0xab26; stride = 1 };
    { lo = 0xab28; hi = 0xab2e; stride = 1 };
    { lo = 0xab30; hi = 0xab5a; stride = 1 };
    { lo = 0xab5c; hi = 0xab69; stride = 1 };
    { lo = 0xab70; hi = 0xabe2; stride = 1 };
    { lo = 0xac00; hi = 0xd7a3; stride = 1 };
    { lo = 0xd7b0; hi = 0xd7c6; stride = 1 };
    { lo = 0xd7cb; hi = 0xd7fb; stride = 1 };
    { lo = 0xf900; hi = 0xfa6d; stride = 1 };
    { lo = 0xfa70; hi = 0xfad9; stride = 1 };
    { lo = 0xfb00; hi = 0xfb06; stride = 1 };
    { lo = 0xfb13; hi = 0xfb17; stride = 1 };
    { lo = 0xfb1d; hi = 0xfb1f; stride = 2 };
    { lo = 0xfb20; hi = 0xfb28; stride = 1 };
    { lo = 0xfb2a; hi = 0xfb36; stride = 1 };
    { lo = 0xfb38; hi = 0xfb3c; stride = 1 };
    { lo = 0xfb3e; hi = 0xfb40; stride = 2 };
    { lo = 0xfb41; hi = 0xfb43; stride = 2 };
    { lo = 0xfb44; hi = 0xfb46; stride = 2 };
    { lo = 0xfb47; hi = 0xfbb1; stride = 1 };
    { lo = 0xfbd3; hi = 0xfd3d; stride = 1 };
    { lo = 0xfd50; hi = 0xfd8f; stride = 1 };
    { lo = 0xfd92; hi = 0xfdc7; stride = 1 };
    { lo = 0xfdf0; hi = 0xfdfb; stride = 1 };
    { lo = 0xfe70; hi = 0xfe74; stride = 1 };
    { lo = 0xfe76; hi = 0xfefc; stride = 1 };
    { lo = 0xff21; hi = 0xff3a; stride = 1 };
    { lo = 0xff41; hi = 0xff5a; stride = 1 };
    { lo = 0xff66; hi = 0xffbe; stride = 1 };
    { lo = 0xffc2; hi = 0xffc7; stride = 1 };
    { lo = 0xffca; hi = 0xffcf; stride = 1 };
    { lo = 0xffd2; hi = 0xffd7; stride = 1 };
    { lo = 0xffda; hi = 0xffdc; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* lc *)
let _lc = {
  r16 = [|
    { lo = 0x0041; hi = 0x005a; stride = 1 };
    { lo = 0x0061; hi = 0x007a; stride = 1 };
    { lo = 0x00b5; hi = 0x00c0; stride = 11 };
    { lo = 0x00c1; hi = 0x00d6; stride = 1 };
    { lo = 0x00d8; hi = 0x00f6; stride = 1 };
    { lo = 0x00f8; hi = 0x01ba; stride = 1 };
    { lo = 0x01bc; hi = 0x01bf; stride = 1 };
    { lo = 0x01c4; hi = 0x0293; stride = 1 };
    { lo = 0x0295; hi = 0x02af; stride = 1 };
    { lo = 0x0370; hi = 0x0373; stride = 1 };
    { lo = 0x0376; hi = 0x0377; stride = 1 };
    { lo = 0x037b; hi = 0x037d; stride = 1 };
    { lo = 0x037f; hi = 0x0386; stride = 7 };
    { lo = 0x0388; hi = 0x038a; stride = 1 };
    { lo = 0x038c; hi = 0x038e; stride = 2 };
    { lo = 0x038f; hi = 0x03a1; stride = 1 };
    { lo = 0x03a3; hi = 0x03f5; stride = 1 };
    { lo = 0x03f7; hi = 0x0481; stride = 1 };
    { lo = 0x048a; hi = 0x052f; stride = 1 };
    { lo = 0x0531; hi = 0x0556; stride = 1 };
    { lo = 0x0560; hi = 0x0588; stride = 1 };
    { lo = 0x10a0; hi = 0x10c5; stride = 1 };
    { lo = 0x10c7; hi = 0x10cd; stride = 6 };
    { lo = 0x10d0; hi = 0x10fa; stride = 1 };
    { lo = 0x10fd; hi = 0x10ff; stride = 1 };
    { lo = 0x13a0; hi = 0x13f5; stride = 1 };
    { lo = 0x13f8; hi = 0x13fd; stride = 1 };
    { lo = 0x1c80; hi = 0x1c88; stride = 1 };
    { lo = 0x1c90; hi = 0x1cba; stride = 1 };
    { lo = 0x1cbd; hi = 0x1cbf; stride = 1 };
    { lo = 0x1d00; hi = 0x1d2b; stride = 1 };
    { lo = 0x1d6b; hi = 0x1d77; stride = 1 };
    { lo = 0x1d79; hi = 0x1d9a; stride = 1 };
    { lo = 0x1e00; hi = 0x1f15; stride = 1 };
    { lo = 0x1f18; hi = 0x1f1d; stride = 1 };
    { lo = 0x1f20; hi = 0x1f45; stride = 1 };
    { lo = 0x1f48; hi = 0x1f4d; stride = 1 };
    { lo = 0x1f50; hi = 0x1f57; stride = 1 };
    { lo = 0x1f59; hi = 0x1f5f; stride = 2 };
    { lo = 0x1f60; hi = 0x1f7d; stride = 1 };
    { lo = 0x1f80; hi = 0x1fb4; stride = 1 };
    { lo = 0x1fb6; hi = 0x1fbc; stride = 1 };
    { lo = 0x1fbe; hi = 0x1fc2; stride = 4 };
    { lo = 0x1fc3; hi = 0x1fc4; stride = 1 };
    { lo = 0x1fc6; hi = 0x1fcc; stride = 1 };
    { lo = 0x1fd0; hi = 0x1fd3; stride = 1 };
    { lo = 0x1fd6; hi = 0x1fdb; stride = 1 };
    { lo = 0x1fe0; hi = 0x1fec; stride = 1 };
    { lo = 0x1ff2; hi = 0x1ff4; stride = 1 };
    { lo = 0x1ff6; hi = 0x1ffc; stride = 1 };
    { lo = 0x2102; hi = 0x2107; stride = 5 };
    { lo = 0x210a; hi = 0x2113; stride = 1 };
    { lo = 0x2115; hi = 0x2119; stride = 4 };
    { lo = 0x211a; hi = 0x211d; stride = 1 };
    { lo = 0x2124; hi = 0x212a; stride = 2 };
    { lo = 0x212b; hi = 0x212d; stride = 1 };
    { lo = 0x212f; hi = 0x2134; stride = 1 };
    { lo = 0x2139; hi = 0x213c; stride = 3 };
    { lo = 0x213d; hi = 0x213f; stride = 1 };
    { lo = 0x2145; hi = 0x2149; stride = 1 };
    { lo = 0x214e; hi = 0x2183; stride = 53 };
    { lo = 0x2184; hi = 0x2c00; stride = 2684 };
    { lo = 0x2c01; hi = 0x2c7b; stride = 1 };
    { lo = 0x2c7e; hi = 0x2ce4; stride = 1 };
    { lo = 0x2ceb; hi = 0x2cee; stride = 1 };
    { lo = 0x2cf2; hi = 0x2cf3; stride = 1 };
    { lo = 0x2d00; hi = 0x2d25; stride = 1 };
    { lo = 0x2d27; hi = 0x2d2d; stride = 6 };
    { lo = 0xa640; hi = 0xa66d; stride = 1 };
    { lo = 0xa680; hi = 0xa69b; stride = 1 };
    { lo = 0xa722; hi = 0xa76f; stride = 1 };
    { lo = 0xa771; hi = 0xa787; stride = 1 };
    { lo = 0xa78b; hi = 0xa78e; stride = 1 };
    { lo = 0xa790; hi = 0xa7ca; stride = 1 };
    { lo = 0xa7d0; hi = 0xa7d1; stride = 1 };
    { lo = 0xa7d3; hi = 0xa7d5; stride = 2 };
    { lo = 0xa7d6; hi = 0xa7d9; stride = 1 };
    { lo = 0xa7f5; hi = 0xa7f6; stride = 1 };
    { lo = 0xa7fa; hi = 0xab30; stride = 822 };
    { lo = 0xab31; hi = 0xab5a; stride = 1 };
    { lo = 0xab60; hi = 0xab68; stride = 1 };
    { lo = 0xab70; hi = 0xabbf; stride = 1 };
    { lo = 0xfb00; hi = 0xfb06; stride = 1 };
    { lo = 0xfb13; hi = 0xfb17; stride = 1 };
    { lo = 0xff21; hi = 0xff3a; stride = 1 };
    { lo = 0xff41; hi = 0xff5a; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* ll *)
let _ll = {
  r16 = [|
    { lo = 0x0061; hi = 0x007a; stride = 1 };
    { lo = 0x00b5; hi = 0x00df; stride = 42 };
    { lo = 0x00e0; hi = 0x00f6; stride = 1 };
    { lo = 0x00f8; hi = 0x00ff; stride = 1 };
    { lo = 0x0101; hi = 0x0137; stride = 2 };
    { lo = 0x0138; hi = 0x0148; stride = 2 };
    { lo = 0x0149; hi = 0x0177; stride = 2 };
    { lo = 0x017a; hi = 0x017e; stride = 2 };
    { lo = 0x017f; hi = 0x0180; stride = 1 };
    { lo = 0x0183; hi = 0x0185; stride = 2 };
    { lo = 0x0188; hi = 0x018c; stride = 4 };
    { lo = 0x018d; hi = 0x0192; stride = 5 };
    { lo = 0x0195; hi = 0x0199; stride = 4 };
    { lo = 0x019a; hi = 0x019b; stride = 1 };
    { lo = 0x019e; hi = 0x01a1; stride = 3 };
    { lo = 0x01a3; hi = 0x01a5; stride = 2 };
    { lo = 0x01a8; hi = 0x01aa; stride = 2 };
    { lo = 0x01ab; hi = 0x01ad; stride = 2 };
    { lo = 0x01b0; hi = 0x01b4; stride = 4 };
    { lo = 0x01b6; hi = 0x01b9; stride = 3 };
    { lo = 0x01ba; hi = 0x01bd; stride = 3 };
    { lo = 0x01be; hi = 0x01bf; stride = 1 };
    { lo = 0x01c6; hi = 0x01cc; stride = 3 };
    { lo = 0x01ce; hi = 0x01dc; stride = 2 };
    { lo = 0x01dd; hi = 0x01ef; stride = 2 };
    { lo = 0x01f0; hi = 0x01f3; stride = 3 };
    { lo = 0x01f5; hi = 0x01f9; stride = 4 };
    { lo = 0x01fb; hi = 0x0233; stride = 2 };
    { lo = 0x0234; hi = 0x0239; stride = 1 };
    { lo = 0x023c; hi = 0x023f; stride = 3 };
    { lo = 0x0240; hi = 0x0242; stride = 2 };
    { lo = 0x0247; hi = 0x024f; stride = 2 };
    { lo = 0x0250; hi = 0x0293; stride = 1 };
    { lo = 0x0295; hi = 0x02af; stride = 1 };
    { lo = 0x0371; hi = 0x0373; stride = 2 };
    { lo = 0x0377; hi = 0x037b; stride = 4 };
    { lo = 0x037c; hi = 0x037d; stride = 1 };
    { lo = 0x0390; hi = 0x03ac; stride = 28 };
    { lo = 0x03ad; hi = 0x03ce; stride = 1 };
    { lo = 0x03d0; hi = 0x03d1; stride = 1 };
    { lo = 0x03d5; hi = 0x03d7; stride = 1 };
    { lo = 0x03d9; hi = 0x03ef; stride = 2 };
    { lo = 0x03f0; hi = 0x03f3; stride = 1 };
    { lo = 0x03f5; hi = 0x03fb; stride = 3 };
    { lo = 0x03fc; hi = 0x0430; stride = 52 };
    { lo = 0x0431; hi = 0x045f; stride = 1 };
    { lo = 0x0461; hi = 0x0481; stride = 2 };
    { lo = 0x048b; hi = 0x04bf; stride = 2 };
    { lo = 0x04c2; hi = 0x04ce; stride = 2 };
    { lo = 0x04cf; hi = 0x052f; stride = 2 };
    { lo = 0x0560; hi = 0x0588; stride = 1 };
    { lo = 0x10d0; hi = 0x10fa; stride = 1 };
    { lo = 0x10fd; hi = 0x10ff; stride = 1 };
    { lo = 0x13f8; hi = 0x13fd; stride = 1 };
    { lo = 0x1c80; hi = 0x1c88; stride = 1 };
    { lo = 0x1d00; hi = 0x1d2b; stride = 1 };
    { lo = 0x1d6b; hi = 0x1d77; stride = 1 };
    { lo = 0x1d79; hi = 0x1d9a; stride = 1 };
    { lo = 0x1e01; hi = 0x1e95; stride = 2 };
    { lo = 0x1e96; hi = 0x1e9d; stride = 1 };
    { lo = 0x1e9f; hi = 0x1eff; stride = 2 };
    { lo = 0x1f00; hi = 0x1f07; stride = 1 };
    { lo = 0x1f10; hi = 0x1f15; stride = 1 };
    { lo = 0x1f20; hi = 0x1f27; stride = 1 };
    { lo = 0x1f30; hi = 0x1f37; stride = 1 };
    { lo = 0x1f40; hi = 0x1f45; stride = 1 };
    { lo = 0x1f50; hi = 0x1f57; stride = 1 };
    { lo = 0x1f60; hi = 0x1f67; stride = 1 };
    { lo = 0x1f70; hi = 0x1f7d; stride = 1 };
    { lo = 0x1f80; hi = 0x1f87; stride = 1 };
    { lo = 0x1f90; hi = 0x1f97; stride = 1 };
    { lo = 0x1fa0; hi = 0x1fa7; stride = 1 };
    { lo = 0x1fb0; hi = 0x1fb4; stride = 1 };
    { lo = 0x1fb6; hi = 0x1fb7; stride = 1 };
    { lo = 0x1fbe; hi = 0x1fc2; stride = 4 };
    { lo = 0x1fc3; hi = 0x1fc4; stride = 1 };
    { lo = 0x1fc6; hi = 0x1fc7; stride = 1 };
    { lo = 0x1fd0; hi = 0x1fd3; stride = 1 };
    { lo = 0x1fd6; hi = 0x1fd7; stride = 1 };
    { lo = 0x1fe0; hi = 0x1fe7; stride = 1 };
    { lo = 0x1ff2; hi = 0x1ff4; stride = 1 };
    { lo = 0x1ff6; hi = 0x1ff7; stride = 1 };
    { lo = 0x210a; hi = 0x210e; stride = 4 };
    { lo = 0x210f; hi = 0x2113; stride = 4 };
    { lo = 0x212f; hi = 0x2139; stride = 5 };
    { lo = 0x213c; hi = 0x213d; stride = 1 };
    { lo = 0x2146; hi = 0x2149; stride = 1 };
    { lo = 0x214e; hi = 0x2184; stride = 54 };
    { lo = 0x2c30; hi = 0x2c5f; stride = 1 };
    { lo = 0x2c61; hi = 0x2c65; stride = 4 };
    { lo = 0x2c66; hi = 0x2c6c; stride = 2 };
    { lo = 0x2c71; hi = 0x2c73; stride = 2 };
    { lo = 0x2c74; hi = 0x2c76; stride = 2 };
    { lo = 0x2c77; hi = 0x2c7b; stride = 1 };
    { lo = 0x2c81; hi = 0x2ce3; stride = 2 };
    { lo = 0x2ce4; hi = 0x2cec; stride = 8 };
    { lo = 0x2cee; hi = 0x2cf3; stride = 5 };
    { lo = 0x2d00; hi = 0x2d25; stride = 1 };
    { lo = 0x2d27; hi = 0x2d2d; stride = 6 };
    { lo = 0xa641; hi = 0xa66d; stride = 2 };
    { lo = 0xa681; hi = 0xa69b; stride = 2 };
    { lo = 0xa723; hi = 0xa72f; stride = 2 };
    { lo = 0xa730; hi = 0xa731; stride = 1 };
    { lo = 0xa733; hi = 0xa771; stride = 2 };
    { lo = 0xa772; hi = 0xa778; stride = 1 };
    { lo = 0xa77a; hi = 0xa77c; stride = 2 };
    { lo = 0xa77f; hi = 0xa787; stride = 2 };
    { lo = 0xa78c; hi = 0xa78e; stride = 2 };
    { lo = 0xa791; hi = 0xa793; stride = 2 };
    { lo = 0xa794; hi = 0xa795; stride = 1 };
    { lo = 0xa797; hi = 0xa7a9; stride = 2 };
    { lo = 0xa7af; hi = 0xa7b5; stride = 6 };
    { lo = 0xa7b7; hi = 0xa7c3; stride = 2 };
    { lo = 0xa7c8; hi = 0xa7ca; stride = 2 };
    { lo = 0xa7d1; hi = 0xa7d9; stride = 2 };
    { lo = 0xa7f6; hi = 0xa7fa; stride = 4 };
    { lo = 0xab30; hi = 0xab5a; stride = 1 };
    { lo = 0xab60; hi = 0xab68; stride = 1 };
    { lo = 0xab70; hi = 0xabbf; stride = 1 };
    { lo = 0xfb00; hi = 0xfb06; stride = 1 };
    { lo = 0xfb13; hi = 0xfb17; stride = 1 };
    { lo = 0xff41; hi = 0xff5a; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* lm *)
let _lm = {
  r16 = [|
    { lo = 0x02b0; hi = 0x02c1; stride = 1 };
    { lo = 0x02c6; hi = 0x02d1; stride = 1 };
    { lo = 0x02e0; hi = 0x02e4; stride = 1 };
    { lo = 0x02ec; hi = 0x02ee; stride = 2 };
    { lo = 0x0374; hi = 0x037a; stride = 6 };
    { lo = 0x0559; hi = 0x0640; stride = 231 };
    { lo = 0x06e5; hi = 0x06e6; stride = 1 };
    { lo = 0x07f4; hi = 0x07f5; stride = 1 };
    { lo = 0x07fa; hi = 0x081a; stride = 32 };
    { lo = 0x0824; hi = 0x0828; stride = 4 };
    { lo = 0x08c9; hi = 0x0971; stride = 168 };
    { lo = 0x0e46; hi = 0x0ec6; stride = 128 };
    { lo = 0x10fc; hi = 0x17d7; stride = 1755 };
    { lo = 0x1843; hi = 0x1aa7; stride = 612 };
    { lo = 0x1c78; hi = 0x1c7d; stride = 1 };
    { lo = 0x1d2c; hi = 0x1d6a; stride = 1 };
    { lo = 0x1d78; hi = 0x1d9b; stride = 35 };
    { lo = 0x1d9c; hi = 0x1dbf; stride = 1 };
    { lo = 0x2071; hi = 0x207f; stride = 14 };
    { lo = 0x2090; hi = 0x209c; stride = 1 };
    { lo = 0x2c7c; hi = 0x2c7d; stride = 1 };
    { lo = 0x2d6f; hi = 0x2e2f; stride = 192 };
    { lo = 0x3005; hi = 0x3031; stride = 44 };
    { lo = 0x3032; hi = 0x3035; stride = 1 };
    { lo = 0x303b; hi = 0x309d; stride = 98 };
    { lo = 0x309e; hi = 0x30fc; stride = 94 };
    { lo = 0x30fd; hi = 0x30fe; stride = 1 };
    { lo = 0xa015; hi = 0xa4f8; stride = 1251 };
    { lo = 0xa4f9; hi = 0xa4fd; stride = 1 };
    { lo = 0xa60c; hi = 0xa67f; stride = 115 };
    { lo = 0xa69c; hi = 0xa69d; stride = 1 };
    { lo = 0xa717; hi = 0xa71f; stride = 1 };
    { lo = 0xa770; hi = 0xa788; stride = 24 };
    { lo = 0xa7f2; hi = 0xa7f4; stride = 1 };
    { lo = 0xa7f8; hi = 0xa7f9; stride = 1 };
    { lo = 0xa9cf; hi = 0xa9e6; stride = 23 };
    { lo = 0xaa70; hi = 0xaadd; stride = 109 };
    { lo = 0xaaf3; hi = 0xaaf4; stride = 1 };
    { lo = 0xab5c; hi = 0xab5f; stride = 1 };
    { lo = 0xab69; hi = 0xff70; stride = 21511 };
    { lo = 0xff9e; hi = 0xff9f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* lo *)
let _lo = {
  r16 = [|
    { lo = 0x00aa; hi = 0x00ba; stride = 16 };
    { lo = 0x01bb; hi = 0x01c0; stride = 5 };
    { lo = 0x01c1; hi = 0x01c3; stride = 1 };
    { lo = 0x0294; hi = 0x05d0; stride = 828 };
    { lo = 0x05d1; hi = 0x05ea; stride = 1 };
    { lo = 0x05ef; hi = 0x05f2; stride = 1 };
    { lo = 0x0620; hi = 0x063f; stride = 1 };
    { lo = 0x0641; hi = 0x064a; stride = 1 };
    { lo = 0x066e; hi = 0x066f; stride = 1 };
    { lo = 0x0671; hi = 0x06d3; stride = 1 };
    { lo = 0x06d5; hi = 0x06ee; stride = 25 };
    { lo = 0x06ef; hi = 0x06fa; stride = 11 };
    { lo = 0x06fb; hi = 0x06fc; stride = 1 };
    { lo = 0x06ff; hi = 0x0710; stride = 17 };
    { lo = 0x0712; hi = 0x072f; stride = 1 };
    { lo = 0x074d; hi = 0x07a5; stride = 1 };
    { lo = 0x07b1; hi = 0x07ca; stride = 25 };
    { lo = 0x07cb; hi = 0x07ea; stride = 1 };
    { lo = 0x0800; hi = 0x0815; stride = 1 };
    { lo = 0x0840; hi = 0x0858; stride = 1 };
    { lo = 0x0860; hi = 0x086a; stride = 1 };
    { lo = 0x0870; hi = 0x0887; stride = 1 };
    { lo = 0x0889; hi = 0x088e; stride = 1 };
    { lo = 0x08a0; hi = 0x08c8; stride = 1 };
    { lo = 0x0904; hi = 0x0939; stride = 1 };
    { lo = 0x093d; hi = 0x0950; stride = 19 };
    { lo = 0x0958; hi = 0x0961; stride = 1 };
    { lo = 0x0972; hi = 0x0980; stride = 1 };
    { lo = 0x0985; hi = 0x098c; stride = 1 };
    { lo = 0x098f; hi = 0x0990; stride = 1 };
    { lo = 0x0993; hi = 0x09a8; stride = 1 };
    { lo = 0x09aa; hi = 0x09b0; stride = 1 };
    { lo = 0x09b2; hi = 0x09b6; stride = 4 };
    { lo = 0x09b7; hi = 0x09b9; stride = 1 };
    { lo = 0x09bd; hi = 0x09ce; stride = 17 };
    { lo = 0x09dc; hi = 0x09dd; stride = 1 };
    { lo = 0x09df; hi = 0x09e1; stride = 1 };
    { lo = 0x09f0; hi = 0x09f1; stride = 1 };
    { lo = 0x09fc; hi = 0x0a05; stride = 9 };
    { lo = 0x0a06; hi = 0x0a0a; stride = 1 };
    { lo = 0x0a0f; hi = 0x0a10; stride = 1 };
    { lo = 0x0a13; hi = 0x0a28; stride = 1 };
    { lo = 0x0a2a; hi = 0x0a30; stride = 1 };
    { lo = 0x0a32; hi = 0x0a33; stride = 1 };
    { lo = 0x0a35; hi = 0x0a36; stride = 1 };
    { lo = 0x0a38; hi = 0x0a39; stride = 1 };
    { lo = 0x0a59; hi = 0x0a5c; stride = 1 };
    { lo = 0x0a5e; hi = 0x0a72; stride = 20 };
    { lo = 0x0a73; hi = 0x0a74; stride = 1 };
    { lo = 0x0a85; hi = 0x0a8d; stride = 1 };
    { lo = 0x0a8f; hi = 0x0a91; stride = 1 };
    { lo = 0x0a93; hi = 0x0aa8; stride = 1 };
    { lo = 0x0aaa; hi = 0x0ab0; stride = 1 };
    { lo = 0x0ab2; hi = 0x0ab3; stride = 1 };
    { lo = 0x0ab5; hi = 0x0ab9; stride = 1 };
    { lo = 0x0abd; hi = 0x0ad0; stride = 19 };
    { lo = 0x0ae0; hi = 0x0ae1; stride = 1 };
    { lo = 0x0af9; hi = 0x0b05; stride = 12 };
    { lo = 0x0b06; hi = 0x0b0c; stride = 1 };
    { lo = 0x0b0f; hi = 0x0b10; stride = 1 };
    { lo = 0x0b13; hi = 0x0b28; stride = 1 };
    { lo = 0x0b2a; hi = 0x0b30; stride = 1 };
    { lo = 0x0b32; hi = 0x0b33; stride = 1 };
    { lo = 0x0b35; hi = 0x0b39; stride = 1 };
    { lo = 0x0b3d; hi = 0x0b5c; stride = 31 };
    { lo = 0x0b5d; hi = 0x0b5f; stride = 2 };
    { lo = 0x0b60; hi = 0x0b61; stride = 1 };
    { lo = 0x0b71; hi = 0x0b83; stride = 18 };
    { lo = 0x0b85; hi = 0x0b8a; stride = 1 };
    { lo = 0x0b8e; hi = 0x0b90; stride = 1 };
    { lo = 0x0b92; hi = 0x0b95; stride = 1 };
    { lo = 0x0b99; hi = 0x0b9a; stride = 1 };
    { lo = 0x0b9c; hi = 0x0b9e; stride = 2 };
    { lo = 0x0b9f; hi = 0x0ba3; stride = 4 };
    { lo = 0x0ba4; hi = 0x0ba8; stride = 4 };
    { lo = 0x0ba9; hi = 0x0baa; stride = 1 };
    { lo = 0x0bae; hi = 0x0bb9; stride = 1 };
    { lo = 0x0bd0; hi = 0x0c05; stride = 53 };
    { lo = 0x0c06; hi = 0x0c0c; stride = 1 };
    { lo = 0x0c0e; hi = 0x0c10; stride = 1 };
    { lo = 0x0c12; hi = 0x0c28; stride = 1 };
    { lo = 0x0c2a; hi = 0x0c39; stride = 1 };
    { lo = 0x0c3d; hi = 0x0c58; stride = 27 };
    { lo = 0x0c59; hi = 0x0c5a; stride = 1 };
    { lo = 0x0c5d; hi = 0x0c60; stride = 3 };
    { lo = 0x0c61; hi = 0x0c80; stride = 31 };
    { lo = 0x0c85; hi = 0x0c8c; stride = 1 };
    { lo = 0x0c8e; hi = 0x0c90; stride = 1 };
    { lo = 0x0c92; hi = 0x0ca8; stride = 1 };
    { lo = 0x0caa; hi = 0x0cb3; stride = 1 };
    { lo = 0x0cb5; hi = 0x0cb9; stride = 1 };
    { lo = 0x0cbd; hi = 0x0cdd; stride = 32 };
    { lo = 0x0cde; hi = 0x0ce0; stride = 2 };
    { lo = 0x0ce1; hi = 0x0cf1; stride = 16 };
    { lo = 0x0cf2; hi = 0x0d04; stride = 18 };
    { lo = 0x0d05; hi = 0x0d0c; stride = 1 };
    { lo = 0x0d0e; hi = 0x0d10; stride = 1 };
    { lo = 0x0d12; hi = 0x0d3a; stride = 1 };
    { lo = 0x0d3d; hi = 0x0d4e; stride = 17 };
    { lo = 0x0d54; hi = 0x0d56; stride = 1 };
    { lo = 0x0d5f; hi = 0x0d61; stride = 1 };
    { lo = 0x0d7a; hi = 0x0d7f; stride = 1 };
    { lo = 0x0d85; hi = 0x0d96; stride = 1 };
    { lo = 0x0d9a; hi = 0x0db1; stride = 1 };
    { lo = 0x0db3; hi = 0x0dbb; stride = 1 };
    { lo = 0x0dbd; hi = 0x0dc0; stride = 3 };
    { lo = 0x0dc1; hi = 0x0dc6; stride = 1 };
    { lo = 0x0e01; hi = 0x0e30; stride = 1 };
    { lo = 0x0e32; hi = 0x0e33; stride = 1 };
    { lo = 0x0e40; hi = 0x0e45; stride = 1 };
    { lo = 0x0e81; hi = 0x0e82; stride = 1 };
    { lo = 0x0e84; hi = 0x0e86; stride = 2 };
    { lo = 0x0e87; hi = 0x0e8a; stride = 1 };
    { lo = 0x0e8c; hi = 0x0ea3; stride = 1 };
    { lo = 0x0ea5; hi = 0x0ea7; stride = 2 };
    { lo = 0x0ea8; hi = 0x0eb0; stride = 1 };
    { lo = 0x0eb2; hi = 0x0eb3; stride = 1 };
    { lo = 0x0ebd; hi = 0x0ec0; stride = 3 };
    { lo = 0x0ec1; hi = 0x0ec4; stride = 1 };
    { lo = 0x0edc; hi = 0x0edf; stride = 1 };
    { lo = 0x0f00; hi = 0x0f40; stride = 64 };
    { lo = 0x0f41; hi = 0x0f47; stride = 1 };
    { lo = 0x0f49; hi = 0x0f6c; stride = 1 };
    { lo = 0x0f88; hi = 0x0f8c; stride = 1 };
    { lo = 0x1000; hi = 0x102a; stride = 1 };
    { lo = 0x103f; hi = 0x1050; stride = 17 };
    { lo = 0x1051; hi = 0x1055; stride = 1 };
    { lo = 0x105a; hi = 0x105d; stride = 1 };
    { lo = 0x1061; hi = 0x1065; stride = 4 };
    { lo = 0x1066; hi = 0x106e; stride = 8 };
    { lo = 0x106f; hi = 0x1070; stride = 1 };
    { lo = 0x1075; hi = 0x1081; stride = 1 };
    { lo = 0x108e; hi = 0x1100; stride = 114 };
    { lo = 0x1101; hi = 0x1248; stride = 1 };
    { lo = 0x124a; hi = 0x124d; stride = 1 };
    { lo = 0x1250; hi = 0x1256; stride = 1 };
    { lo = 0x1258; hi = 0x125a; stride = 2 };
    { lo = 0x125b; hi = 0x125d; stride = 1 };
    { lo = 0x1260; hi = 0x1288; stride = 1 };
    { lo = 0x128a; hi = 0x128d; stride = 1 };
    { lo = 0x1290; hi = 0x12b0; stride = 1 };
    { lo = 0x12b2; hi = 0x12b5; stride = 1 };
    { lo = 0x12b8; hi = 0x12be; stride = 1 };
    { lo = 0x12c0; hi = 0x12c2; stride = 2 };
    { lo = 0x12c3; hi = 0x12c5; stride = 1 };
    { lo = 0x12c8; hi = 0x12d6; stride = 1 };
    { lo = 0x12d8; hi = 0x1310; stride = 1 };
    { lo = 0x1312; hi = 0x1315; stride = 1 };
    { lo = 0x1318; hi = 0x135a; stride = 1 };
    { lo = 0x1380; hi = 0x138f; stride = 1 };
    { lo = 0x1401; hi = 0x166c; stride = 1 };
    { lo = 0x166f; hi = 0x167f; stride = 1 };
    { lo = 0x1681; hi = 0x169a; stride = 1 };
    { lo = 0x16a0; hi = 0x16ea; stride = 1 };
    { lo = 0x16f1; hi = 0x16f8; stride = 1 };
    { lo = 0x1700; hi = 0x1711; stride = 1 };
    { lo = 0x171f; hi = 0x1731; stride = 1 };
    { lo = 0x1740; hi = 0x1751; stride = 1 };
    { lo = 0x1760; hi = 0x176c; stride = 1 };
    { lo = 0x176e; hi = 0x1770; stride = 1 };
    { lo = 0x1780; hi = 0x17b3; stride = 1 };
    { lo = 0x17dc; hi = 0x1820; stride = 68 };
    { lo = 0x1821; hi = 0x1842; stride = 1 };
    { lo = 0x1844; hi = 0x1878; stride = 1 };
    { lo = 0x1880; hi = 0x1884; stride = 1 };
    { lo = 0x1887; hi = 0x18a8; stride = 1 };
    { lo = 0x18aa; hi = 0x18b0; stride = 6 };
    { lo = 0x18b1; hi = 0x18f5; stride = 1 };
    { lo = 0x1900; hi = 0x191e; stride = 1 };
    { lo = 0x1950; hi = 0x196d; stride = 1 };
    { lo = 0x1970; hi = 0x1974; stride = 1 };
    { lo = 0x1980; hi = 0x19ab; stride = 1 };
    { lo = 0x19b0; hi = 0x19c9; stride = 1 };
    { lo = 0x1a00; hi = 0x1a16; stride = 1 };
    { lo = 0x1a20; hi = 0x1a54; stride = 1 };
    { lo = 0x1b05; hi = 0x1b33; stride = 1 };
    { lo = 0x1b45; hi = 0x1b4c; stride = 1 };
    { lo = 0x1b83; hi = 0x1ba0; stride = 1 };
    { lo = 0x1bae; hi = 0x1baf; stride = 1 };
    { lo = 0x1bba; hi = 0x1be5; stride = 1 };
    { lo = 0x1c00; hi = 0x1c23; stride = 1 };
    { lo = 0x1c4d; hi = 0x1c4f; stride = 1 };
    { lo = 0x1c5a; hi = 0x1c77; stride = 1 };
    { lo = 0x1ce9; hi = 0x1cec; stride = 1 };
    { lo = 0x1cee; hi = 0x1cf3; stride = 1 };
    { lo = 0x1cf5; hi = 0x1cf6; stride = 1 };
    { lo = 0x1cfa; hi = 0x2135; stride = 1083 };
    { lo = 0x2136; hi = 0x2138; stride = 1 };
    { lo = 0x2d30; hi = 0x2d67; stride = 1 };
    { lo = 0x2d80; hi = 0x2d96; stride = 1 };
    { lo = 0x2da0; hi = 0x2da6; stride = 1 };
    { lo = 0x2da8; hi = 0x2dae; stride = 1 };
    { lo = 0x2db0; hi = 0x2db6; stride = 1 };
    { lo = 0x2db8; hi = 0x2dbe; stride = 1 };
    { lo = 0x2dc0; hi = 0x2dc6; stride = 1 };
    { lo = 0x2dc8; hi = 0x2dce; stride = 1 };
    { lo = 0x2dd0; hi = 0x2dd6; stride = 1 };
    { lo = 0x2dd8; hi = 0x2dde; stride = 1 };
    { lo = 0x3006; hi = 0x303c; stride = 54 };
    { lo = 0x3041; hi = 0x3096; stride = 1 };
    { lo = 0x309f; hi = 0x30a1; stride = 2 };
    { lo = 0x30a2; hi = 0x30fa; stride = 1 };
    { lo = 0x30ff; hi = 0x3105; stride = 6 };
    { lo = 0x3106; hi = 0x312f; stride = 1 };
    { lo = 0x3131; hi = 0x318e; stride = 1 };
    { lo = 0x31a0; hi = 0x31bf; stride = 1 };
    { lo = 0x31f0; hi = 0x31ff; stride = 1 };
    { lo = 0x3400; hi = 0x4dbf; stride = 1 };
    { lo = 0x4e00; hi = 0xa014; stride = 1 };
    { lo = 0xa016; hi = 0xa48c; stride = 1 };
    { lo = 0xa4d0; hi = 0xa4f7; stride = 1 };
    { lo = 0xa500; hi = 0xa60b; stride = 1 };
    { lo = 0xa610; hi = 0xa61f; stride = 1 };
    { lo = 0xa62a; hi = 0xa62b; stride = 1 };
    { lo = 0xa66e; hi = 0xa6a0; stride = 50 };
    { lo = 0xa6a1; hi = 0xa6e5; stride = 1 };
    { lo = 0xa78f; hi = 0xa7f7; stride = 104 };
    { lo = 0xa7fb; hi = 0xa801; stride = 1 };
    { lo = 0xa803; hi = 0xa805; stride = 1 };
    { lo = 0xa807; hi = 0xa80a; stride = 1 };
    { lo = 0xa80c; hi = 0xa822; stride = 1 };
    { lo = 0xa840; hi = 0xa873; stride = 1 };
    { lo = 0xa882; hi = 0xa8b3; stride = 1 };
    { lo = 0xa8f2; hi = 0xa8f7; stride = 1 };
    { lo = 0xa8fb; hi = 0xa8fd; stride = 2 };
    { lo = 0xa8fe; hi = 0xa90a; stride = 12 };
    { lo = 0xa90b; hi = 0xa925; stride = 1 };
    { lo = 0xa930; hi = 0xa946; stride = 1 };
    { lo = 0xa960; hi = 0xa97c; stride = 1 };
    { lo = 0xa984; hi = 0xa9b2; stride = 1 };
    { lo = 0xa9e0; hi = 0xa9e4; stride = 1 };
    { lo = 0xa9e7; hi = 0xa9ef; stride = 1 };
    { lo = 0xa9fa; hi = 0xa9fe; stride = 1 };
    { lo = 0xaa00; hi = 0xaa28; stride = 1 };
    { lo = 0xaa40; hi = 0xaa42; stride = 1 };
    { lo = 0xaa44; hi = 0xaa4b; stride = 1 };
    { lo = 0xaa60; hi = 0xaa6f; stride = 1 };
    { lo = 0xaa71; hi = 0xaa76; stride = 1 };
    { lo = 0xaa7a; hi = 0xaa7e; stride = 4 };
    { lo = 0xaa7f; hi = 0xaaaf; stride = 1 };
    { lo = 0xaab1; hi = 0xaab5; stride = 4 };
    { lo = 0xaab6; hi = 0xaab9; stride = 3 };
    { lo = 0xaaba; hi = 0xaabd; stride = 1 };
    { lo = 0xaac0; hi = 0xaac2; stride = 2 };
    { lo = 0xaadb; hi = 0xaadc; stride = 1 };
    { lo = 0xaae0; hi = 0xaaea; stride = 1 };
    { lo = 0xaaf2; hi = 0xab01; stride = 15 };
    { lo = 0xab02; hi = 0xab06; stride = 1 };
    { lo = 0xab09; hi = 0xab0e; stride = 1 };
    { lo = 0xab11; hi = 0xab16; stride = 1 };
    { lo = 0xab20; hi = 0xab26; stride = 1 };
    { lo = 0xab28; hi = 0xab2e; stride = 1 };
    { lo = 0xabc0; hi = 0xabe2; stride = 1 };
    { lo = 0xac00; hi = 0xd7a3; stride = 1 };
    { lo = 0xd7b0; hi = 0xd7c6; stride = 1 };
    { lo = 0xd7cb; hi = 0xd7fb; stride = 1 };
    { lo = 0xf900; hi = 0xfa6d; stride = 1 };
    { lo = 0xfa70; hi = 0xfad9; stride = 1 };
    { lo = 0xfb1d; hi = 0xfb1f; stride = 2 };
    { lo = 0xfb20; hi = 0xfb28; stride = 1 };
    { lo = 0xfb2a; hi = 0xfb36; stride = 1 };
    { lo = 0xfb38; hi = 0xfb3c; stride = 1 };
    { lo = 0xfb3e; hi = 0xfb40; stride = 2 };
    { lo = 0xfb41; hi = 0xfb43; stride = 2 };
    { lo = 0xfb44; hi = 0xfb46; stride = 2 };
    { lo = 0xfb47; hi = 0xfbb1; stride = 1 };
    { lo = 0xfbd3; hi = 0xfd3d; stride = 1 };
    { lo = 0xfd50; hi = 0xfd8f; stride = 1 };
    { lo = 0xfd92; hi = 0xfdc7; stride = 1 };
    { lo = 0xfdf0; hi = 0xfdfb; stride = 1 };
    { lo = 0xfe70; hi = 0xfe74; stride = 1 };
    { lo = 0xfe76; hi = 0xfefc; stride = 1 };
    { lo = 0xff66; hi = 0xff6f; stride = 1 };
    { lo = 0xff71; hi = 0xff9d; stride = 1 };
    { lo = 0xffa0; hi = 0xffbe; stride = 1 };
    { lo = 0xffc2; hi = 0xffc7; stride = 1 };
    { lo = 0xffca; hi = 0xffcf; stride = 1 };
    { lo = 0xffd2; hi = 0xffd7; stride = 1 };
    { lo = 0xffda; hi = 0xffdc; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* lt *)
let _lt = {
  r16 = [|
    { lo = 0x01c5; hi = 0x01cb; stride = 3 };
    { lo = 0x01f2; hi = 0x1f88; stride = 7574 };
    { lo = 0x1f89; hi = 0x1f8f; stride = 1 };
    { lo = 0x1f98; hi = 0x1f9f; stride = 1 };
    { lo = 0x1fa8; hi = 0x1faf; stride = 1 };
    { lo = 0x1fbc; hi = 0x1fcc; stride = 16 };
    { lo = 0x1ffc; hi = 0x1ffc; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* lu *)
let _lu = {
  r16 = [|
    { lo = 0x0041; hi = 0x005a; stride = 1 };
    { lo = 0x00c0; hi = 0x00d6; stride = 1 };
    { lo = 0x00d8; hi = 0x00de; stride = 1 };
    { lo = 0x0100; hi = 0x0136; stride = 2 };
    { lo = 0x0139; hi = 0x0147; stride = 2 };
    { lo = 0x014a; hi = 0x0178; stride = 2 };
    { lo = 0x0179; hi = 0x017d; stride = 2 };
    { lo = 0x0181; hi = 0x0182; stride = 1 };
    { lo = 0x0184; hi = 0x0186; stride = 2 };
    { lo = 0x0187; hi = 0x0189; stride = 2 };
    { lo = 0x018a; hi = 0x018b; stride = 1 };
    { lo = 0x018e; hi = 0x0191; stride = 1 };
    { lo = 0x0193; hi = 0x0194; stride = 1 };
    { lo = 0x0196; hi = 0x0198; stride = 1 };
    { lo = 0x019c; hi = 0x019d; stride = 1 };
    { lo = 0x019f; hi = 0x01a0; stride = 1 };
    { lo = 0x01a2; hi = 0x01a6; stride = 2 };
    { lo = 0x01a7; hi = 0x01a9; stride = 2 };
    { lo = 0x01ac; hi = 0x01ae; stride = 2 };
    { lo = 0x01af; hi = 0x01b1; stride = 2 };
    { lo = 0x01b2; hi = 0x01b3; stride = 1 };
    { lo = 0x01b5; hi = 0x01b7; stride = 2 };
    { lo = 0x01b8; hi = 0x01bc; stride = 4 };
    { lo = 0x01c4; hi = 0x01cd; stride = 3 };
    { lo = 0x01cf; hi = 0x01db; stride = 2 };
    { lo = 0x01de; hi = 0x01ee; stride = 2 };
    { lo = 0x01f1; hi = 0x01f4; stride = 3 };
    { lo = 0x01f6; hi = 0x01f8; stride = 1 };
    { lo = 0x01fa; hi = 0x0232; stride = 2 };
    { lo = 0x023a; hi = 0x023b; stride = 1 };
    { lo = 0x023d; hi = 0x023e; stride = 1 };
    { lo = 0x0241; hi = 0x0243; stride = 2 };
    { lo = 0x0244; hi = 0x0246; stride = 1 };
    { lo = 0x0248; hi = 0x024e; stride = 2 };
    { lo = 0x0370; hi = 0x0372; stride = 2 };
    { lo = 0x0376; hi = 0x037f; stride = 9 };
    { lo = 0x0386; hi = 0x0388; stride = 2 };
    { lo = 0x0389; hi = 0x038a; stride = 1 };
    { lo = 0x038c; hi = 0x038e; stride = 2 };
    { lo = 0x038f; hi = 0x0391; stride = 2 };
    { lo = 0x0392; hi = 0x03a1; stride = 1 };
    { lo = 0x03a3; hi = 0x03ab; stride = 1 };
    { lo = 0x03cf; hi = 0x03d2; stride = 3 };
    { lo = 0x03d3; hi = 0x03d4; stride = 1 };
    { lo = 0x03d8; hi = 0x03ee; stride = 2 };
    { lo = 0x03f4; hi = 0x03f7; stride = 3 };
    { lo = 0x03f9; hi = 0x03fa; stride = 1 };
    { lo = 0x03fd; hi = 0x042f; stride = 1 };
    { lo = 0x0460; hi = 0x0480; stride = 2 };
    { lo = 0x048a; hi = 0x04c0; stride = 2 };
    { lo = 0x04c1; hi = 0x04cd; stride = 2 };
    { lo = 0x04d0; hi = 0x052e; stride = 2 };
    { lo = 0x0531; hi = 0x0556; stride = 1 };
    { lo = 0x10a0; hi = 0x10c5; stride = 1 };
    { lo = 0x10c7; hi = 0x10cd; stride = 6 };
    { lo = 0x13a0; hi = 0x13f5; stride = 1 };
    { lo = 0x1c90; hi = 0x1cba; stride = 1 };
    { lo = 0x1cbd; hi = 0x1cbf; stride = 1 };
    { lo = 0x1e00; hi = 0x1e94; stride = 2 };
    { lo = 0x1e9e; hi = 0x1efe; stride = 2 };
    { lo = 0x1f08; hi = 0x1f0f; stride = 1 };
    { lo = 0x1f18; hi = 0x1f1d; stride = 1 };
    { lo = 0x1f28; hi = 0x1f2f; stride = 1 };
    { lo = 0x1f38; hi = 0x1f3f; stride = 1 };
    { lo = 0x1f48; hi = 0x1f4d; stride = 1 };
    { lo = 0x1f59; hi = 0x1f5f; stride = 2 };
    { lo = 0x1f68; hi = 0x1f6f; stride = 1 };
    { lo = 0x1fb8; hi = 0x1fbb; stride = 1 };
    { lo = 0x1fc8; hi = 0x1fcb; stride = 1 };
    { lo = 0x1fd8; hi = 0x1fdb; stride = 1 };
    { lo = 0x1fe8; hi = 0x1fec; stride = 1 };
    { lo = 0x1ff8; hi = 0x1ffb; stride = 1 };
    { lo = 0x2102; hi = 0x2107; stride = 5 };
    { lo = 0x210b; hi = 0x210d; stride = 1 };
    { lo = 0x2110; hi = 0x2112; stride = 1 };
    { lo = 0x2115; hi = 0x2119; stride = 4 };
    { lo = 0x211a; hi = 0x211d; stride = 1 };
    { lo = 0x2124; hi = 0x212a; stride = 2 };
    { lo = 0x212b; hi = 0x212d; stride = 1 };
    { lo = 0x2130; hi = 0x2133; stride = 1 };
    { lo = 0x213e; hi = 0x213f; stride = 1 };
    { lo = 0x2145; hi = 0x2183; stride = 62 };
    { lo = 0x2c00; hi = 0x2c2f; stride = 1 };
    { lo = 0x2c60; hi = 0x2c62; stride = 2 };
    { lo = 0x2c63; hi = 0x2c64; stride = 1 };
    { lo = 0x2c67; hi = 0x2c6d; stride = 2 };
    { lo = 0x2c6e; hi = 0x2c70; stride = 1 };
    { lo = 0x2c72; hi = 0x2c75; stride = 3 };
    { lo = 0x2c7e; hi = 0x2c80; stride = 1 };
    { lo = 0x2c82; hi = 0x2ce2; stride = 2 };
    { lo = 0x2ceb; hi = 0x2ced; stride = 2 };
    { lo = 0x2cf2; hi = 0xa640; stride = 31054 };
    { lo = 0xa642; hi = 0xa66c; stride = 2 };
    { lo = 0xa680; hi = 0xa69a; stride = 2 };
    { lo = 0xa722; hi = 0xa72e; stride = 2 };
    { lo = 0xa732; hi = 0xa76e; stride = 2 };
    { lo = 0xa779; hi = 0xa77d; stride = 2 };
    { lo = 0xa77e; hi = 0xa786; stride = 2 };
    { lo = 0xa78b; hi = 0xa78d; stride = 2 };
    { lo = 0xa790; hi = 0xa792; stride = 2 };
    { lo = 0xa796; hi = 0xa7aa; stride = 2 };
    { lo = 0xa7ab; hi = 0xa7ae; stride = 1 };
    { lo = 0xa7b0; hi = 0xa7b4; stride = 1 };
    { lo = 0xa7b6; hi = 0xa7c4; stride = 2 };
    { lo = 0xa7c5; hi = 0xa7c7; stride = 1 };
    { lo = 0xa7c9; hi = 0xa7d0; stride = 7 };
    { lo = 0xa7d6; hi = 0xa7d8; stride = 2 };
    { lo = 0xa7f5; hi = 0xff21; stride = 22316 };
    { lo = 0xff22; hi = 0xff3a; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* m *)
let _m = {
  r16 = [|
    { lo = 0x0300; hi = 0x036f; stride = 1 };
    { lo = 0x0483; hi = 0x0489; stride = 1 };
    { lo = 0x0591; hi = 0x05bd; stride = 1 };
    { lo = 0x05bf; hi = 0x05c1; stride = 2 };
    { lo = 0x05c2; hi = 0x05c4; stride = 2 };
    { lo = 0x05c5; hi = 0x05c7; stride = 2 };
    { lo = 0x0610; hi = 0x061a; stride = 1 };
    { lo = 0x064b; hi = 0x065f; stride = 1 };
    { lo = 0x0670; hi = 0x06d6; stride = 102 };
    { lo = 0x06d7; hi = 0x06dc; stride = 1 };
    { lo = 0x06df; hi = 0x06e4; stride = 1 };
    { lo = 0x06e7; hi = 0x06e8; stride = 1 };
    { lo = 0x06ea; hi = 0x06ed; stride = 1 };
    { lo = 0x0711; hi = 0x0730; stride = 31 };
    { lo = 0x0731; hi = 0x074a; stride = 1 };
    { lo = 0x07a6; hi = 0x07b0; stride = 1 };
    { lo = 0x07eb; hi = 0x07f3; stride = 1 };
    { lo = 0x07fd; hi = 0x0816; stride = 25 };
    { lo = 0x0817; hi = 0x0819; stride = 1 };
    { lo = 0x081b; hi = 0x0823; stride = 1 };
    { lo = 0x0825; hi = 0x0827; stride = 1 };
    { lo = 0x0829; hi = 0x082d; stride = 1 };
    { lo = 0x0859; hi = 0x085b; stride = 1 };
    { lo = 0x0898; hi = 0x089f; stride = 1 };
    { lo = 0x08ca; hi = 0x08e1; stride = 1 };
    { lo = 0x08e3; hi = 0x0903; stride = 1 };
    { lo = 0x093a; hi = 0x093c; stride = 1 };
    { lo = 0x093e; hi = 0x094f; stride = 1 };
    { lo = 0x0951; hi = 0x0957; stride = 1 };
    { lo = 0x0962; hi = 0x0963; stride = 1 };
    { lo = 0x0981; hi = 0x0983; stride = 1 };
    { lo = 0x09bc; hi = 0x09be; stride = 2 };
    { lo = 0x09bf; hi = 0x09c4; stride = 1 };
    { lo = 0x09c7; hi = 0x09c8; stride = 1 };
    { lo = 0x09cb; hi = 0x09cd; stride = 1 };
    { lo = 0x09d7; hi = 0x09e2; stride = 11 };
    { lo = 0x09e3; hi = 0x09fe; stride = 27 };
    { lo = 0x0a01; hi = 0x0a03; stride = 1 };
    { lo = 0x0a3c; hi = 0x0a3e; stride = 2 };
    { lo = 0x0a3f; hi = 0x0a42; stride = 1 };
    { lo = 0x0a47; hi = 0x0a48; stride = 1 };
    { lo = 0x0a4b; hi = 0x0a4d; stride = 1 };
    { lo = 0x0a51; hi = 0x0a70; stride = 31 };
    { lo = 0x0a71; hi = 0x0a75; stride = 4 };
    { lo = 0x0a81; hi = 0x0a83; stride = 1 };
    { lo = 0x0abc; hi = 0x0abe; stride = 2 };
    { lo = 0x0abf; hi = 0x0ac5; stride = 1 };
    { lo = 0x0ac7; hi = 0x0ac9; stride = 1 };
    { lo = 0x0acb; hi = 0x0acd; stride = 1 };
    { lo = 0x0ae2; hi = 0x0ae3; stride = 1 };
    { lo = 0x0afa; hi = 0x0aff; stride = 1 };
    { lo = 0x0b01; hi = 0x0b03; stride = 1 };
    { lo = 0x0b3c; hi = 0x0b3e; stride = 2 };
    { lo = 0x0b3f; hi = 0x0b44; stride = 1 };
    { lo = 0x0b47; hi = 0x0b48; stride = 1 };
    { lo = 0x0b4b; hi = 0x0b4d; stride = 1 };
    { lo = 0x0b55; hi = 0x0b57; stride = 1 };
    { lo = 0x0b62; hi = 0x0b63; stride = 1 };
    { lo = 0x0b82; hi = 0x0bbe; stride = 60 };
    { lo = 0x0bbf; hi = 0x0bc2; stride = 1 };
    { lo = 0x0bc6; hi = 0x0bc8; stride = 1 };
    { lo = 0x0bca; hi = 0x0bcd; stride = 1 };
    { lo = 0x0bd7; hi = 0x0c00; stride = 41 };
    { lo = 0x0c01; hi = 0x0c04; stride = 1 };
    { lo = 0x0c3c; hi = 0x0c3e; stride = 2 };
    { lo = 0x0c3f; hi = 0x0c44; stride = 1 };
    { lo = 0x0c46; hi = 0x0c48; stride = 1 };
    { lo = 0x0c4a; hi = 0x0c4d; stride = 1 };
    { lo = 0x0c55; hi = 0x0c56; stride = 1 };
    { lo = 0x0c62; hi = 0x0c63; stride = 1 };
    { lo = 0x0c81; hi = 0x0c83; stride = 1 };
    { lo = 0x0cbc; hi = 0x0cbe; stride = 2 };
    { lo = 0x0cbf; hi = 0x0cc4; stride = 1 };
    { lo = 0x0cc6; hi = 0x0cc8; stride = 1 };
    { lo = 0x0cca; hi = 0x0ccd; stride = 1 };
    { lo = 0x0cd5; hi = 0x0cd6; stride = 1 };
    { lo = 0x0ce2; hi = 0x0ce3; stride = 1 };
    { lo = 0x0cf3; hi = 0x0d00; stride = 13 };
    { lo = 0x0d01; hi = 0x0d03; stride = 1 };
    { lo = 0x0d3b; hi = 0x0d3c; stride = 1 };
    { lo = 0x0d3e; hi = 0x0d44; stride = 1 };
    { lo = 0x0d46; hi = 0x0d48; stride = 1 };
    { lo = 0x0d4a; hi = 0x0d4d; stride = 1 };
    { lo = 0x0d57; hi = 0x0d62; stride = 11 };
    { lo = 0x0d63; hi = 0x0d81; stride = 30 };
    { lo = 0x0d82; hi = 0x0d83; stride = 1 };
    { lo = 0x0dca; hi = 0x0dcf; stride = 5 };
    { lo = 0x0dd0; hi = 0x0dd4; stride = 1 };
    { lo = 0x0dd6; hi = 0x0dd8; stride = 2 };
    { lo = 0x0dd9; hi = 0x0ddf; stride = 1 };
    { lo = 0x0df2; hi = 0x0df3; stride = 1 };
    { lo = 0x0e31; hi = 0x0e34; stride = 3 };
    { lo = 0x0e35; hi = 0x0e3a; stride = 1 };
    { lo = 0x0e47; hi = 0x0e4e; stride = 1 };
    { lo = 0x0eb1; hi = 0x0eb4; stride = 3 };
    { lo = 0x0eb5; hi = 0x0ebc; stride = 1 };
    { lo = 0x0ec8; hi = 0x0ece; stride = 1 };
    { lo = 0x0f18; hi = 0x0f19; stride = 1 };
    { lo = 0x0f35; hi = 0x0f39; stride = 2 };
    { lo = 0x0f3e; hi = 0x0f3f; stride = 1 };
    { lo = 0x0f71; hi = 0x0f84; stride = 1 };
    { lo = 0x0f86; hi = 0x0f87; stride = 1 };
    { lo = 0x0f8d; hi = 0x0f97; stride = 1 };
    { lo = 0x0f99; hi = 0x0fbc; stride = 1 };
    { lo = 0x0fc6; hi = 0x102b; stride = 101 };
    { lo = 0x102c; hi = 0x103e; stride = 1 };
    { lo = 0x1056; hi = 0x1059; stride = 1 };
    { lo = 0x105e; hi = 0x1060; stride = 1 };
    { lo = 0x1062; hi = 0x1064; stride = 1 };
    { lo = 0x1067; hi = 0x106d; stride = 1 };
    { lo = 0x1071; hi = 0x1074; stride = 1 };
    { lo = 0x1082; hi = 0x108d; stride = 1 };
    { lo = 0x108f; hi = 0x109a; stride = 11 };
    { lo = 0x109b; hi = 0x109d; stride = 1 };
    { lo = 0x135d; hi = 0x135f; stride = 1 };
    { lo = 0x1712; hi = 0x1715; stride = 1 };
    { lo = 0x1732; hi = 0x1734; stride = 1 };
    { lo = 0x1752; hi = 0x1753; stride = 1 };
    { lo = 0x1772; hi = 0x1773; stride = 1 };
    { lo = 0x17b4; hi = 0x17d3; stride = 1 };
    { lo = 0x17dd; hi = 0x180b; stride = 46 };
    { lo = 0x180c; hi = 0x180d; stride = 1 };
    { lo = 0x180f; hi = 0x1885; stride = 118 };
    { lo = 0x1886; hi = 0x18a9; stride = 35 };
    { lo = 0x1920; hi = 0x192b; stride = 1 };
    { lo = 0x1930; hi = 0x193b; stride = 1 };
    { lo = 0x1a17; hi = 0x1a1b; stride = 1 };
    { lo = 0x1a55; hi = 0x1a5e; stride = 1 };
    { lo = 0x1a60; hi = 0x1a7c; stride = 1 };
    { lo = 0x1a7f; hi = 0x1ab0; stride = 49 };
    { lo = 0x1ab1; hi = 0x1ace; stride = 1 };
    { lo = 0x1b00; hi = 0x1b04; stride = 1 };
    { lo = 0x1b34; hi = 0x1b44; stride = 1 };
    { lo = 0x1b6b; hi = 0x1b73; stride = 1 };
    { lo = 0x1b80; hi = 0x1b82; stride = 1 };
    { lo = 0x1ba1; hi = 0x1bad; stride = 1 };
    { lo = 0x1be6; hi = 0x1bf3; stride = 1 };
    { lo = 0x1c24; hi = 0x1c37; stride = 1 };
    { lo = 0x1cd0; hi = 0x1cd2; stride = 1 };
    { lo = 0x1cd4; hi = 0x1ce8; stride = 1 };
    { lo = 0x1ced; hi = 0x1cf4; stride = 7 };
    { lo = 0x1cf7; hi = 0x1cf9; stride = 1 };
    { lo = 0x1dc0; hi = 0x1dff; stride = 1 };
    { lo = 0x20d0; hi = 0x20f0; stride = 1 };
    { lo = 0x2cef; hi = 0x2cf1; stride = 1 };
    { lo = 0x2d7f; hi = 0x2de0; stride = 97 };
    { lo = 0x2de1; hi = 0x2dff; stride = 1 };
    { lo = 0x302a; hi = 0x302f; stride = 1 };
    { lo = 0x3099; hi = 0x309a; stride = 1 };
    { lo = 0xa66f; hi = 0xa672; stride = 1 };
    { lo = 0xa674; hi = 0xa67d; stride = 1 };
    { lo = 0xa69e; hi = 0xa69f; stride = 1 };
    { lo = 0xa6f0; hi = 0xa6f1; stride = 1 };
    { lo = 0xa802; hi = 0xa806; stride = 4 };
    { lo = 0xa80b; hi = 0xa823; stride = 24 };
    { lo = 0xa824; hi = 0xa827; stride = 1 };
    { lo = 0xa82c; hi = 0xa880; stride = 84 };
    { lo = 0xa881; hi = 0xa8b4; stride = 51 };
    { lo = 0xa8b5; hi = 0xa8c5; stride = 1 };
    { lo = 0xa8e0; hi = 0xa8f1; stride = 1 };
    { lo = 0xa8ff; hi = 0xa926; stride = 39 };
    { lo = 0xa927; hi = 0xa92d; stride = 1 };
    { lo = 0xa947; hi = 0xa953; stride = 1 };
    { lo = 0xa980; hi = 0xa983; stride = 1 };
    { lo = 0xa9b3; hi = 0xa9c0; stride = 1 };
    { lo = 0xa9e5; hi = 0xaa29; stride = 68 };
    { lo = 0xaa2a; hi = 0xaa36; stride = 1 };
    { lo = 0xaa43; hi = 0xaa4c; stride = 9 };
    { lo = 0xaa4d; hi = 0xaa7b; stride = 46 };
    { lo = 0xaa7c; hi = 0xaa7d; stride = 1 };
    { lo = 0xaab0; hi = 0xaab2; stride = 2 };
    { lo = 0xaab3; hi = 0xaab4; stride = 1 };
    { lo = 0xaab7; hi = 0xaab8; stride = 1 };
    { lo = 0xaabe; hi = 0xaabf; stride = 1 };
    { lo = 0xaac1; hi = 0xaaeb; stride = 42 };
    { lo = 0xaaec; hi = 0xaaef; stride = 1 };
    { lo = 0xaaf5; hi = 0xaaf6; stride = 1 };
    { lo = 0xabe3; hi = 0xabea; stride = 1 };
    { lo = 0xabec; hi = 0xabed; stride = 1 };
    { lo = 0xfb1e; hi = 0xfe00; stride = 738 };
    { lo = 0xfe01; hi = 0xfe0f; stride = 1 };
    { lo = 0xfe20; hi = 0xfe2f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* mc *)
let _mc = {
  r16 = [|
    { lo = 0x0903; hi = 0x093b; stride = 56 };
    { lo = 0x093e; hi = 0x0940; stride = 1 };
    { lo = 0x0949; hi = 0x094c; stride = 1 };
    { lo = 0x094e; hi = 0x094f; stride = 1 };
    { lo = 0x0982; hi = 0x0983; stride = 1 };
    { lo = 0x09be; hi = 0x09c0; stride = 1 };
    { lo = 0x09c7; hi = 0x09c8; stride = 1 };
    { lo = 0x09cb; hi = 0x09cc; stride = 1 };
    { lo = 0x09d7; hi = 0x0a03; stride = 44 };
    { lo = 0x0a3e; hi = 0x0a40; stride = 1 };
    { lo = 0x0a83; hi = 0x0abe; stride = 59 };
    { lo = 0x0abf; hi = 0x0ac0; stride = 1 };
    { lo = 0x0ac9; hi = 0x0acb; stride = 2 };
    { lo = 0x0acc; hi = 0x0b02; stride = 54 };
    { lo = 0x0b03; hi = 0x0b3e; stride = 59 };
    { lo = 0x0b40; hi = 0x0b47; stride = 7 };
    { lo = 0x0b48; hi = 0x0b4b; stride = 3 };
    { lo = 0x0b4c; hi = 0x0b57; stride = 11 };
    { lo = 0x0bbe; hi = 0x0bbf; stride = 1 };
    { lo = 0x0bc1; hi = 0x0bc2; stride = 1 };
    { lo = 0x0bc6; hi = 0x0bc8; stride = 1 };
    { lo = 0x0bca; hi = 0x0bcc; stride = 1 };
    { lo = 0x0bd7; hi = 0x0c01; stride = 42 };
    { lo = 0x0c02; hi = 0x0c03; stride = 1 };
    { lo = 0x0c41; hi = 0x0c44; stride = 1 };
    { lo = 0x0c82; hi = 0x0c83; stride = 1 };
    { lo = 0x0cbe; hi = 0x0cc0; stride = 2 };
    { lo = 0x0cc1; hi = 0x0cc4; stride = 1 };
    { lo = 0x0cc7; hi = 0x0cc8; stride = 1 };
    { lo = 0x0cca; hi = 0x0ccb; stride = 1 };
    { lo = 0x0cd5; hi = 0x0cd6; stride = 1 };
    { lo = 0x0cf3; hi = 0x0d02; stride = 15 };
    { lo = 0x0d03; hi = 0x0d3e; stride = 59 };
    { lo = 0x0d3f; hi = 0x0d40; stride = 1 };
    { lo = 0x0d46; hi = 0x0d48; stride = 1 };
    { lo = 0x0d4a; hi = 0x0d4c; stride = 1 };
    { lo = 0x0d57; hi = 0x0d82; stride = 43 };
    { lo = 0x0d83; hi = 0x0dcf; stride = 76 };
    { lo = 0x0dd0; hi = 0x0dd1; stride = 1 };
    { lo = 0x0dd8; hi = 0x0ddf; stride = 1 };
    { lo = 0x0df2; hi = 0x0df3; stride = 1 };
    { lo = 0x0f3e; hi = 0x0f3f; stride = 1 };
    { lo = 0x0f7f; hi = 0x102b; stride = 172 };
    { lo = 0x102c; hi = 0x1031; stride = 5 };
    { lo = 0x1038; hi = 0x103b; stride = 3 };
    { lo = 0x103c; hi = 0x1056; stride = 26 };
    { lo = 0x1057; hi = 0x1062; stride = 11 };
    { lo = 0x1063; hi = 0x1064; stride = 1 };
    { lo = 0x1067; hi = 0x106d; stride = 1 };
    { lo = 0x1083; hi = 0x1084; stride = 1 };
    { lo = 0x1087; hi = 0x108c; stride = 1 };
    { lo = 0x108f; hi = 0x109a; stride = 11 };
    { lo = 0x109b; hi = 0x109c; stride = 1 };
    { lo = 0x1715; hi = 0x1734; stride = 31 };
    { lo = 0x17b6; hi = 0x17be; stride = 8 };
    { lo = 0x17bf; hi = 0x17c5; stride = 1 };
    { lo = 0x17c7; hi = 0x17c8; stride = 1 };
    { lo = 0x1923; hi = 0x1926; stride = 1 };
    { lo = 0x1929; hi = 0x192b; stride = 1 };
    { lo = 0x1930; hi = 0x1931; stride = 1 };
    { lo = 0x1933; hi = 0x1938; stride = 1 };
    { lo = 0x1a19; hi = 0x1a1a; stride = 1 };
    { lo = 0x1a55; hi = 0x1a57; stride = 2 };
    { lo = 0x1a61; hi = 0x1a63; stride = 2 };
    { lo = 0x1a64; hi = 0x1a6d; stride = 9 };
    { lo = 0x1a6e; hi = 0x1a72; stride = 1 };
    { lo = 0x1b04; hi = 0x1b35; stride = 49 };
    { lo = 0x1b3b; hi = 0x1b3d; stride = 2 };
    { lo = 0x1b3e; hi = 0x1b41; stride = 1 };
    { lo = 0x1b43; hi = 0x1b44; stride = 1 };
    { lo = 0x1b82; hi = 0x1ba1; stride = 31 };
    { lo = 0x1ba6; hi = 0x1ba7; stride = 1 };
    { lo = 0x1baa; hi = 0x1be7; stride = 61 };
    { lo = 0x1bea; hi = 0x1bec; stride = 1 };
    { lo = 0x1bee; hi = 0x1bf2; stride = 4 };
    { lo = 0x1bf3; hi = 0x1c24; stride = 49 };
    { lo = 0x1c25; hi = 0x1c2b; stride = 1 };
    { lo = 0x1c34; hi = 0x1c35; stride = 1 };
    { lo = 0x1ce1; hi = 0x1cf7; stride = 22 };
    { lo = 0x302e; hi = 0x302f; stride = 1 };
    { lo = 0xa823; hi = 0xa824; stride = 1 };
    { lo = 0xa827; hi = 0xa880; stride = 89 };
    { lo = 0xa881; hi = 0xa8b4; stride = 51 };
    { lo = 0xa8b5; hi = 0xa8c3; stride = 1 };
    { lo = 0xa952; hi = 0xa953; stride = 1 };
    { lo = 0xa983; hi = 0xa9b4; stride = 49 };
    { lo = 0xa9b5; hi = 0xa9ba; stride = 5 };
    { lo = 0xa9bb; hi = 0xa9be; stride = 3 };
    { lo = 0xa9bf; hi = 0xa9c0; stride = 1 };
    { lo = 0xaa2f; hi = 0xaa30; stride = 1 };
    { lo = 0xaa33; hi = 0xaa34; stride = 1 };
    { lo = 0xaa4d; hi = 0xaa7b; stride = 46 };
    { lo = 0xaa7d; hi = 0xaaeb; stride = 110 };
    { lo = 0xaaee; hi = 0xaaef; stride = 1 };
    { lo = 0xaaf5; hi = 0xabe3; stride = 238 };
    { lo = 0xabe4; hi = 0xabe6; stride = 2 };
    { lo = 0xabe7; hi = 0xabe9; stride = 2 };
    { lo = 0xabea; hi = 0xabec; stride = 2 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* me *)
let _me = {
  r16 = [|
    { lo = 0x0488; hi = 0x0489; stride = 1 };
    { lo = 0x1abe; hi = 0x20dd; stride = 1567 };
    { lo = 0x20de; hi = 0x20e0; stride = 1 };
    { lo = 0x20e2; hi = 0x20e4; stride = 1 };
    { lo = 0xa670; hi = 0xa672; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* mn *)
let _mn = {
  r16 = [|
    { lo = 0x0300; hi = 0x036f; stride = 1 };
    { lo = 0x0483; hi = 0x0487; stride = 1 };
    { lo = 0x0591; hi = 0x05bd; stride = 1 };
    { lo = 0x05bf; hi = 0x05c1; stride = 2 };
    { lo = 0x05c2; hi = 0x05c4; stride = 2 };
    { lo = 0x05c5; hi = 0x05c7; stride = 2 };
    { lo = 0x0610; hi = 0x061a; stride = 1 };
    { lo = 0x064b; hi = 0x065f; stride = 1 };
    { lo = 0x0670; hi = 0x06d6; stride = 102 };
    { lo = 0x06d7; hi = 0x06dc; stride = 1 };
    { lo = 0x06df; hi = 0x06e4; stride = 1 };
    { lo = 0x06e7; hi = 0x06e8; stride = 1 };
    { lo = 0x06ea; hi = 0x06ed; stride = 1 };
    { lo = 0x0711; hi = 0x0730; stride = 31 };
    { lo = 0x0731; hi = 0x074a; stride = 1 };
    { lo = 0x07a6; hi = 0x07b0; stride = 1 };
    { lo = 0x07eb; hi = 0x07f3; stride = 1 };
    { lo = 0x07fd; hi = 0x0816; stride = 25 };
    { lo = 0x0817; hi = 0x0819; stride = 1 };
    { lo = 0x081b; hi = 0x0823; stride = 1 };
    { lo = 0x0825; hi = 0x0827; stride = 1 };
    { lo = 0x0829; hi = 0x082d; stride = 1 };
    { lo = 0x0859; hi = 0x085b; stride = 1 };
    { lo = 0x0898; hi = 0x089f; stride = 1 };
    { lo = 0x08ca; hi = 0x08e1; stride = 1 };
    { lo = 0x08e3; hi = 0x0902; stride = 1 };
    { lo = 0x093a; hi = 0x093c; stride = 2 };
    { lo = 0x0941; hi = 0x0948; stride = 1 };
    { lo = 0x094d; hi = 0x0951; stride = 4 };
    { lo = 0x0952; hi = 0x0957; stride = 1 };
    { lo = 0x0962; hi = 0x0963; stride = 1 };
    { lo = 0x0981; hi = 0x09bc; stride = 59 };
    { lo = 0x09c1; hi = 0x09c4; stride = 1 };
    { lo = 0x09cd; hi = 0x09e2; stride = 21 };
    { lo = 0x09e3; hi = 0x09fe; stride = 27 };
    { lo = 0x0a01; hi = 0x0a02; stride = 1 };
    { lo = 0x0a3c; hi = 0x0a41; stride = 5 };
    { lo = 0x0a42; hi = 0x0a47; stride = 5 };
    { lo = 0x0a48; hi = 0x0a4b; stride = 3 };
    { lo = 0x0a4c; hi = 0x0a4d; stride = 1 };
    { lo = 0x0a51; hi = 0x0a70; stride = 31 };
    { lo = 0x0a71; hi = 0x0a75; stride = 4 };
    { lo = 0x0a81; hi = 0x0a82; stride = 1 };
    { lo = 0x0abc; hi = 0x0ac1; stride = 5 };
    { lo = 0x0ac2; hi = 0x0ac5; stride = 1 };
    { lo = 0x0ac7; hi = 0x0ac8; stride = 1 };
    { lo = 0x0acd; hi = 0x0ae2; stride = 21 };
    { lo = 0x0ae3; hi = 0x0afa; stride = 23 };
    { lo = 0x0afb; hi = 0x0aff; stride = 1 };
    { lo = 0x0b01; hi = 0x0b3c; stride = 59 };
    { lo = 0x0b3f; hi = 0x0b41; stride = 2 };
    { lo = 0x0b42; hi = 0x0b44; stride = 1 };
    { lo = 0x0b4d; hi = 0x0b55; stride = 8 };
    { lo = 0x0b56; hi = 0x0b62; stride = 12 };
    { lo = 0x0b63; hi = 0x0b82; stride = 31 };
    { lo = 0x0bc0; hi = 0x0bcd; stride = 13 };
    { lo = 0x0c00; hi = 0x0c04; stride = 4 };
    { lo = 0x0c3c; hi = 0x0c3e; stride = 2 };
    { lo = 0x0c3f; hi = 0x0c40; stride = 1 };
    { lo = 0x0c46; hi = 0x0c48; stride = 1 };
    { lo = 0x0c4a; hi = 0x0c4d; stride = 1 };
    { lo = 0x0c55; hi = 0x0c56; stride = 1 };
    { lo = 0x0c62; hi = 0x0c63; stride = 1 };
    { lo = 0x0c81; hi = 0x0cbc; stride = 59 };
    { lo = 0x0cbf; hi = 0x0cc6; stride = 7 };
    { lo = 0x0ccc; hi = 0x0ccd; stride = 1 };
    { lo = 0x0ce2; hi = 0x0ce3; stride = 1 };
    { lo = 0x0d00; hi = 0x0d01; stride = 1 };
    { lo = 0x0d3b; hi = 0x0d3c; stride = 1 };
    { lo = 0x0d41; hi = 0x0d44; stride = 1 };
    { lo = 0x0d4d; hi = 0x0d62; stride = 21 };
    { lo = 0x0d63; hi = 0x0d81; stride = 30 };
    { lo = 0x0dca; hi = 0x0dd2; stride = 8 };
    { lo = 0x0dd3; hi = 0x0dd4; stride = 1 };
    { lo = 0x0dd6; hi = 0x0e31; stride = 91 };
    { lo = 0x0e34; hi = 0x0e3a; stride = 1 };
    { lo = 0x0e47; hi = 0x0e4e; stride = 1 };
    { lo = 0x0eb1; hi = 0x0eb4; stride = 3 };
    { lo = 0x0eb5; hi = 0x0ebc; stride = 1 };
    { lo = 0x0ec8; hi = 0x0ece; stride = 1 };
    { lo = 0x0f18; hi = 0x0f19; stride = 1 };
    { lo = 0x0f35; hi = 0x0f39; stride = 2 };
    { lo = 0x0f71; hi = 0x0f7e; stride = 1 };
    { lo = 0x0f80; hi = 0x0f84; stride = 1 };
    { lo = 0x0f86; hi = 0x0f87; stride = 1 };
    { lo = 0x0f8d; hi = 0x0f97; stride = 1 };
    { lo = 0x0f99; hi = 0x0fbc; stride = 1 };
    { lo = 0x0fc6; hi = 0x102d; stride = 103 };
    { lo = 0x102e; hi = 0x1030; stride = 1 };
    { lo = 0x1032; hi = 0x1037; stride = 1 };
    { lo = 0x1039; hi = 0x103a; stride = 1 };
    { lo = 0x103d; hi = 0x103e; stride = 1 };
    { lo = 0x1058; hi = 0x1059; stride = 1 };
    { lo = 0x105e; hi = 0x1060; stride = 1 };
    { lo = 0x1071; hi = 0x1074; stride = 1 };
    { lo = 0x1082; hi = 0x1085; stride = 3 };
    { lo = 0x1086; hi = 0x108d; stride = 7 };
    { lo = 0x109d; hi = 0x135d; stride = 704 };
    { lo = 0x135e; hi = 0x135f; stride = 1 };
    { lo = 0x1712; hi = 0x1714; stride = 1 };
    { lo = 0x1732; hi = 0x1733; stride = 1 };
    { lo = 0x1752; hi = 0x1753; stride = 1 };
    { lo = 0x1772; hi = 0x1773; stride = 1 };
    { lo = 0x17b4; hi = 0x17b5; stride = 1 };
    { lo = 0x17b7; hi = 0x17bd; stride = 1 };
    { lo = 0x17c6; hi = 0x17c9; stride = 3 };
    { lo = 0x17ca; hi = 0x17d3; stride = 1 };
    { lo = 0x17dd; hi = 0x180b; stride = 46 };
    { lo = 0x180c; hi = 0x180d; stride = 1 };
    { lo = 0x180f; hi = 0x1885; stride = 118 };
    { lo = 0x1886; hi = 0x18a9; stride = 35 };
    { lo = 0x1920; hi = 0x1922; stride = 1 };
    { lo = 0x1927; hi = 0x1928; stride = 1 };
    { lo = 0x1932; hi = 0x1939; stride = 7 };
    { lo = 0x193a; hi = 0x193b; stride = 1 };
    { lo = 0x1a17; hi = 0x1a18; stride = 1 };
    { lo = 0x1a1b; hi = 0x1a56; stride = 59 };
    { lo = 0x1a58; hi = 0x1a5e; stride = 1 };
    { lo = 0x1a60; hi = 0x1a62; stride = 2 };
    { lo = 0x1a65; hi = 0x1a6c; stride = 1 };
    { lo = 0x1a73; hi = 0x1a7c; stride = 1 };
    { lo = 0x1a7f; hi = 0x1ab0; stride = 49 };
    { lo = 0x1ab1; hi = 0x1abd; stride = 1 };
    { lo = 0x1abf; hi = 0x1ace; stride = 1 };
    { lo = 0x1b00; hi = 0x1b03; stride = 1 };
    { lo = 0x1b34; hi = 0x1b36; stride = 2 };
    { lo = 0x1b37; hi = 0x1b3a; stride = 1 };
    { lo = 0x1b3c; hi = 0x1b42; stride = 6 };
    { lo = 0x1b6b; hi = 0x1b73; stride = 1 };
    { lo = 0x1b80; hi = 0x1b81; stride = 1 };
    { lo = 0x1ba2; hi = 0x1ba5; stride = 1 };
    { lo = 0x1ba8; hi = 0x1ba9; stride = 1 };
    { lo = 0x1bab; hi = 0x1bad; stride = 1 };
    { lo = 0x1be6; hi = 0x1be8; stride = 2 };
    { lo = 0x1be9; hi = 0x1bed; stride = 4 };
    { lo = 0x1bef; hi = 0x1bf1; stride = 1 };
    { lo = 0x1c2c; hi = 0x1c33; stride = 1 };
    { lo = 0x1c36; hi = 0x1c37; stride = 1 };
    { lo = 0x1cd0; hi = 0x1cd2; stride = 1 };
    { lo = 0x1cd4; hi = 0x1ce0; stride = 1 };
    { lo = 0x1ce2; hi = 0x1ce8; stride = 1 };
    { lo = 0x1ced; hi = 0x1cf4; stride = 7 };
    { lo = 0x1cf8; hi = 0x1cf9; stride = 1 };
    { lo = 0x1dc0; hi = 0x1dff; stride = 1 };
    { lo = 0x20d0; hi = 0x20dc; stride = 1 };
    { lo = 0x20e1; hi = 0x20e5; stride = 4 };
    { lo = 0x20e6; hi = 0x20f0; stride = 1 };
    { lo = 0x2cef; hi = 0x2cf1; stride = 1 };
    { lo = 0x2d7f; hi = 0x2de0; stride = 97 };
    { lo = 0x2de1; hi = 0x2dff; stride = 1 };
    { lo = 0x302a; hi = 0x302d; stride = 1 };
    { lo = 0x3099; hi = 0x309a; stride = 1 };
    { lo = 0xa66f; hi = 0xa674; stride = 5 };
    { lo = 0xa675; hi = 0xa67d; stride = 1 };
    { lo = 0xa69e; hi = 0xa69f; stride = 1 };
    { lo = 0xa6f0; hi = 0xa6f1; stride = 1 };
    { lo = 0xa802; hi = 0xa806; stride = 4 };
    { lo = 0xa80b; hi = 0xa825; stride = 26 };
    { lo = 0xa826; hi = 0xa82c; stride = 6 };
    { lo = 0xa8c4; hi = 0xa8c5; stride = 1 };
    { lo = 0xa8e0; hi = 0xa8f1; stride = 1 };
    { lo = 0xa8ff; hi = 0xa926; stride = 39 };
    { lo = 0xa927; hi = 0xa92d; stride = 1 };
    { lo = 0xa947; hi = 0xa951; stride = 1 };
    { lo = 0xa980; hi = 0xa982; stride = 1 };
    { lo = 0xa9b3; hi = 0xa9b6; stride = 3 };
    { lo = 0xa9b7; hi = 0xa9b9; stride = 1 };
    { lo = 0xa9bc; hi = 0xa9bd; stride = 1 };
    { lo = 0xa9e5; hi = 0xaa29; stride = 68 };
    { lo = 0xaa2a; hi = 0xaa2e; stride = 1 };
    { lo = 0xaa31; hi = 0xaa32; stride = 1 };
    { lo = 0xaa35; hi = 0xaa36; stride = 1 };
    { lo = 0xaa43; hi = 0xaa4c; stride = 9 };
    { lo = 0xaa7c; hi = 0xaab0; stride = 52 };
    { lo = 0xaab2; hi = 0xaab4; stride = 1 };
    { lo = 0xaab7; hi = 0xaab8; stride = 1 };
    { lo = 0xaabe; hi = 0xaabf; stride = 1 };
    { lo = 0xaac1; hi = 0xaaec; stride = 43 };
    { lo = 0xaaed; hi = 0xaaf6; stride = 9 };
    { lo = 0xabe5; hi = 0xabe8; stride = 3 };
    { lo = 0xabed; hi = 0xfb1e; stride = 20273 };
    { lo = 0xfe00; hi = 0xfe0f; stride = 1 };
    { lo = 0xfe20; hi = 0xfe2f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* n *)
let _n = {
  r16 = [|
    { lo = 0x0030; hi = 0x0039; stride = 1 };
    { lo = 0x00b2; hi = 0x00b3; stride = 1 };
    { lo = 0x00b9; hi = 0x00bc; stride = 3 };
    { lo = 0x00bd; hi = 0x00be; stride = 1 };
    { lo = 0x0660; hi = 0x0669; stride = 1 };
    { lo = 0x06f0; hi = 0x06f9; stride = 1 };
    { lo = 0x07c0; hi = 0x07c9; stride = 1 };
    { lo = 0x0966; hi = 0x096f; stride = 1 };
    { lo = 0x09e6; hi = 0x09ef; stride = 1 };
    { lo = 0x09f4; hi = 0x09f9; stride = 1 };
    { lo = 0x0a66; hi = 0x0a6f; stride = 1 };
    { lo = 0x0ae6; hi = 0x0aef; stride = 1 };
    { lo = 0x0b66; hi = 0x0b6f; stride = 1 };
    { lo = 0x0b72; hi = 0x0b77; stride = 1 };
    { lo = 0x0be6; hi = 0x0bf2; stride = 1 };
    { lo = 0x0c66; hi = 0x0c6f; stride = 1 };
    { lo = 0x0c78; hi = 0x0c7e; stride = 1 };
    { lo = 0x0ce6; hi = 0x0cef; stride = 1 };
    { lo = 0x0d58; hi = 0x0d5e; stride = 1 };
    { lo = 0x0d66; hi = 0x0d78; stride = 1 };
    { lo = 0x0de6; hi = 0x0def; stride = 1 };
    { lo = 0x0e50; hi = 0x0e59; stride = 1 };
    { lo = 0x0ed0; hi = 0x0ed9; stride = 1 };
    { lo = 0x0f20; hi = 0x0f33; stride = 1 };
    { lo = 0x1040; hi = 0x1049; stride = 1 };
    { lo = 0x1090; hi = 0x1099; stride = 1 };
    { lo = 0x1369; hi = 0x137c; stride = 1 };
    { lo = 0x16ee; hi = 0x16f0; stride = 1 };
    { lo = 0x17e0; hi = 0x17e9; stride = 1 };
    { lo = 0x17f0; hi = 0x17f9; stride = 1 };
    { lo = 0x1810; hi = 0x1819; stride = 1 };
    { lo = 0x1946; hi = 0x194f; stride = 1 };
    { lo = 0x19d0; hi = 0x19da; stride = 1 };
    { lo = 0x1a80; hi = 0x1a89; stride = 1 };
    { lo = 0x1a90; hi = 0x1a99; stride = 1 };
    { lo = 0x1b50; hi = 0x1b59; stride = 1 };
    { lo = 0x1bb0; hi = 0x1bb9; stride = 1 };
    { lo = 0x1c40; hi = 0x1c49; stride = 1 };
    { lo = 0x1c50; hi = 0x1c59; stride = 1 };
    { lo = 0x2070; hi = 0x2074; stride = 4 };
    { lo = 0x2075; hi = 0x2079; stride = 1 };
    { lo = 0x2080; hi = 0x2089; stride = 1 };
    { lo = 0x2150; hi = 0x2182; stride = 1 };
    { lo = 0x2185; hi = 0x2189; stride = 1 };
    { lo = 0x2460; hi = 0x249b; stride = 1 };
    { lo = 0x24ea; hi = 0x24ff; stride = 1 };
    { lo = 0x2776; hi = 0x2793; stride = 1 };
    { lo = 0x2cfd; hi = 0x3007; stride = 778 };
    { lo = 0x3021; hi = 0x3029; stride = 1 };
    { lo = 0x3038; hi = 0x303a; stride = 1 };
    { lo = 0x3192; hi = 0x3195; stride = 1 };
    { lo = 0x3220; hi = 0x3229; stride = 1 };
    { lo = 0x3248; hi = 0x324f; stride = 1 };
    { lo = 0x3251; hi = 0x325f; stride = 1 };
    { lo = 0x3280; hi = 0x3289; stride = 1 };
    { lo = 0x32b1; hi = 0x32bf; stride = 1 };
    { lo = 0xa620; hi = 0xa629; stride = 1 };
    { lo = 0xa6e6; hi = 0xa6ef; stride = 1 };
    { lo = 0xa830; hi = 0xa835; stride = 1 };
    { lo = 0xa8d0; hi = 0xa8d9; stride = 1 };
    { lo = 0xa900; hi = 0xa909; stride = 1 };
    { lo = 0xa9d0; hi = 0xa9d9; stride = 1 };
    { lo = 0xa9f0; hi = 0xa9f9; stride = 1 };
    { lo = 0xaa50; hi = 0xaa59; stride = 1 };
    { lo = 0xabf0; hi = 0xabf9; stride = 1 };
    { lo = 0xff10; hi = 0xff19; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* nd *)
let _nd = {
  r16 = [|
    { lo = 0x0030; hi = 0x0039; stride = 1 };
    { lo = 0x0660; hi = 0x0669; stride = 1 };
    { lo = 0x06f0; hi = 0x06f9; stride = 1 };
    { lo = 0x07c0; hi = 0x07c9; stride = 1 };
    { lo = 0x0966; hi = 0x096f; stride = 1 };
    { lo = 0x09e6; hi = 0x09ef; stride = 1 };
    { lo = 0x0a66; hi = 0x0a6f; stride = 1 };
    { lo = 0x0ae6; hi = 0x0aef; stride = 1 };
    { lo = 0x0b66; hi = 0x0b6f; stride = 1 };
    { lo = 0x0be6; hi = 0x0bef; stride = 1 };
    { lo = 0x0c66; hi = 0x0c6f; stride = 1 };
    { lo = 0x0ce6; hi = 0x0cef; stride = 1 };
    { lo = 0x0d66; hi = 0x0d6f; stride = 1 };
    { lo = 0x0de6; hi = 0x0def; stride = 1 };
    { lo = 0x0e50; hi = 0x0e59; stride = 1 };
    { lo = 0x0ed0; hi = 0x0ed9; stride = 1 };
    { lo = 0x0f20; hi = 0x0f29; stride = 1 };
    { lo = 0x1040; hi = 0x1049; stride = 1 };
    { lo = 0x1090; hi = 0x1099; stride = 1 };
    { lo = 0x17e0; hi = 0x17e9; stride = 1 };
    { lo = 0x1810; hi = 0x1819; stride = 1 };
    { lo = 0x1946; hi = 0x194f; stride = 1 };
    { lo = 0x19d0; hi = 0x19d9; stride = 1 };
    { lo = 0x1a80; hi = 0x1a89; stride = 1 };
    { lo = 0x1a90; hi = 0x1a99; stride = 1 };
    { lo = 0x1b50; hi = 0x1b59; stride = 1 };
    { lo = 0x1bb0; hi = 0x1bb9; stride = 1 };
    { lo = 0x1c40; hi = 0x1c49; stride = 1 };
    { lo = 0x1c50; hi = 0x1c59; stride = 1 };
    { lo = 0xa620; hi = 0xa629; stride = 1 };
    { lo = 0xa8d0; hi = 0xa8d9; stride = 1 };
    { lo = 0xa900; hi = 0xa909; stride = 1 };
    { lo = 0xa9d0; hi = 0xa9d9; stride = 1 };
    { lo = 0xa9f0; hi = 0xa9f9; stride = 1 };
    { lo = 0xaa50; hi = 0xaa59; stride = 1 };
    { lo = 0xabf0; hi = 0xabf9; stride = 1 };
    { lo = 0xff10; hi = 0xff19; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* nl *)
let _nl = {
  r16 = [|
    { lo = 0x16ee; hi = 0x16f0; stride = 1 };
    { lo = 0x2160; hi = 0x2182; stride = 1 };
    { lo = 0x2185; hi = 0x2188; stride = 1 };
    { lo = 0x3007; hi = 0x3021; stride = 26 };
    { lo = 0x3022; hi = 0x3029; stride = 1 };
    { lo = 0x3038; hi = 0x303a; stride = 1 };
    { lo = 0xa6e6; hi = 0xa6ef; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* no *)
let _no = {
  r16 = [|
    { lo = 0x00b2; hi = 0x00b3; stride = 1 };
    { lo = 0x00b9; hi = 0x00bc; stride = 3 };
    { lo = 0x00bd; hi = 0x00be; stride = 1 };
    { lo = 0x09f4; hi = 0x09f9; stride = 1 };
    { lo = 0x0b72; hi = 0x0b77; stride = 1 };
    { lo = 0x0bf0; hi = 0x0bf2; stride = 1 };
    { lo = 0x0c78; hi = 0x0c7e; stride = 1 };
    { lo = 0x0d58; hi = 0x0d5e; stride = 1 };
    { lo = 0x0d70; hi = 0x0d78; stride = 1 };
    { lo = 0x0f2a; hi = 0x0f33; stride = 1 };
    { lo = 0x1369; hi = 0x137c; stride = 1 };
    { lo = 0x17f0; hi = 0x17f9; stride = 1 };
    { lo = 0x19da; hi = 0x2070; stride = 1686 };
    { lo = 0x2074; hi = 0x2079; stride = 1 };
    { lo = 0x2080; hi = 0x2089; stride = 1 };
    { lo = 0x2150; hi = 0x215f; stride = 1 };
    { lo = 0x2189; hi = 0x2460; stride = 727 };
    { lo = 0x2461; hi = 0x249b; stride = 1 };
    { lo = 0x24ea; hi = 0x24ff; stride = 1 };
    { lo = 0x2776; hi = 0x2793; stride = 1 };
    { lo = 0x2cfd; hi = 0x3192; stride = 1173 };
    { lo = 0x3193; hi = 0x3195; stride = 1 };
    { lo = 0x3220; hi = 0x3229; stride = 1 };
    { lo = 0x3248; hi = 0x324f; stride = 1 };
    { lo = 0x3251; hi = 0x325f; stride = 1 };
    { lo = 0x3280; hi = 0x3289; stride = 1 };
    { lo = 0x32b1; hi = 0x32bf; stride = 1 };
    { lo = 0xa830; hi = 0xa835; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* p *)
let _p = {
  r16 = [|
    { lo = 0x0021; hi = 0x0023; stride = 1 };
    { lo = 0x0025; hi = 0x002a; stride = 1 };
    { lo = 0x002c; hi = 0x002f; stride = 1 };
    { lo = 0x003a; hi = 0x003b; stride = 1 };
    { lo = 0x003f; hi = 0x0040; stride = 1 };
    { lo = 0x005b; hi = 0x005d; stride = 1 };
    { lo = 0x005f; hi = 0x007b; stride = 28 };
    { lo = 0x007d; hi = 0x00a1; stride = 36 };
    { lo = 0x00a7; hi = 0x00ab; stride = 4 };
    { lo = 0x00b6; hi = 0x00b7; stride = 1 };
    { lo = 0x00bb; hi = 0x00bf; stride = 4 };
    { lo = 0x037e; hi = 0x0387; stride = 9 };
    { lo = 0x055a; hi = 0x055f; stride = 1 };
    { lo = 0x0589; hi = 0x058a; stride = 1 };
    { lo = 0x05be; hi = 0x05c0; stride = 2 };
    { lo = 0x05c3; hi = 0x05c6; stride = 3 };
    { lo = 0x05f3; hi = 0x05f4; stride = 1 };
    { lo = 0x0609; hi = 0x060a; stride = 1 };
    { lo = 0x060c; hi = 0x060d; stride = 1 };
    { lo = 0x061b; hi = 0x061d; stride = 2 };
    { lo = 0x061e; hi = 0x061f; stride = 1 };
    { lo = 0x066a; hi = 0x066d; stride = 1 };
    { lo = 0x06d4; hi = 0x0700; stride = 44 };
    { lo = 0x0701; hi = 0x070d; stride = 1 };
    { lo = 0x07f7; hi = 0x07f9; stride = 1 };
    { lo = 0x0830; hi = 0x083e; stride = 1 };
    { lo = 0x085e; hi = 0x0964; stride = 262 };
    { lo = 0x0965; hi = 0x0970; stride = 11 };
    { lo = 0x09fd; hi = 0x0a76; stride = 121 };
    { lo = 0x0af0; hi = 0x0c77; stride = 391 };
    { lo = 0x0c84; hi = 0x0df4; stride = 368 };
    { lo = 0x0e4f; hi = 0x0e5a; stride = 11 };
    { lo = 0x0e5b; hi = 0x0f04; stride = 169 };
    { lo = 0x0f05; hi = 0x0f12; stride = 1 };
    { lo = 0x0f14; hi = 0x0f3a; stride = 38 };
    { lo = 0x0f3b; hi = 0x0f3d; stride = 1 };
    { lo = 0x0f85; hi = 0x0fd0; stride = 75 };
    { lo = 0x0fd1; hi = 0x0fd4; stride = 1 };
    { lo = 0x0fd9; hi = 0x0fda; stride = 1 };
    { lo = 0x104a; hi = 0x104f; stride = 1 };
    { lo = 0x10fb; hi = 0x1360; stride = 613 };
    { lo = 0x1361; hi = 0x1368; stride = 1 };
    { lo = 0x1400; hi = 0x166e; stride = 622 };
    { lo = 0x169b; hi = 0x169c; stride = 1 };
    { lo = 0x16eb; hi = 0x16ed; stride = 1 };
    { lo = 0x1735; hi = 0x1736; stride = 1 };
    { lo = 0x17d4; hi = 0x17d6; stride = 1 };
    { lo = 0x17d8; hi = 0x17da; stride = 1 };
    { lo = 0x1800; hi = 0x180a; stride = 1 };
    { lo = 0x1944; hi = 0x1945; stride = 1 };
    { lo = 0x1a1e; hi = 0x1a1f; stride = 1 };
    { lo = 0x1aa0; hi = 0x1aa6; stride = 1 };
    { lo = 0x1aa8; hi = 0x1aad; stride = 1 };
    { lo = 0x1b5a; hi = 0x1b60; stride = 1 };
    { lo = 0x1b7d; hi = 0x1b7e; stride = 1 };
    { lo = 0x1bfc; hi = 0x1bff; stride = 1 };
    { lo = 0x1c3b; hi = 0x1c3f; stride = 1 };
    { lo = 0x1c7e; hi = 0x1c7f; stride = 1 };
    { lo = 0x1cc0; hi = 0x1cc7; stride = 1 };
    { lo = 0x1cd3; hi = 0x2010; stride = 829 };
    { lo = 0x2011; hi = 0x2027; stride = 1 };
    { lo = 0x2030; hi = 0x2043; stride = 1 };
    { lo = 0x2045; hi = 0x2051; stride = 1 };
    { lo = 0x2053; hi = 0x205e; stride = 1 };
    { lo = 0x207d; hi = 0x207e; stride = 1 };
    { lo = 0x208d; hi = 0x208e; stride = 1 };
    { lo = 0x2308; hi = 0x230b; stride = 1 };
    { lo = 0x2329; hi = 0x232a; stride = 1 };
    { lo = 0x2768; hi = 0x2775; stride = 1 };
    { lo = 0x27c5; hi = 0x27c6; stride = 1 };
    { lo = 0x27e6; hi = 0x27ef; stride = 1 };
    { lo = 0x2983; hi = 0x2998; stride = 1 };
    { lo = 0x29d8; hi = 0x29db; stride = 1 };
    { lo = 0x29fc; hi = 0x29fd; stride = 1 };
    { lo = 0x2cf9; hi = 0x2cfc; stride = 1 };
    { lo = 0x2cfe; hi = 0x2cff; stride = 1 };
    { lo = 0x2d70; hi = 0x2e00; stride = 144 };
    { lo = 0x2e01; hi = 0x2e2e; stride = 1 };
    { lo = 0x2e30; hi = 0x2e4f; stride = 1 };
    { lo = 0x2e52; hi = 0x2e5d; stride = 1 };
    { lo = 0x3001; hi = 0x3003; stride = 1 };
    { lo = 0x3008; hi = 0x3011; stride = 1 };
    { lo = 0x3014; hi = 0x301f; stride = 1 };
    { lo = 0x3030; hi = 0x303d; stride = 13 };
    { lo = 0x30a0; hi = 0x30fb; stride = 91 };
    { lo = 0xa4fe; hi = 0xa4ff; stride = 1 };
    { lo = 0xa60d; hi = 0xa60f; stride = 1 };
    { lo = 0xa673; hi = 0xa67e; stride = 11 };
    { lo = 0xa6f2; hi = 0xa6f7; stride = 1 };
    { lo = 0xa874; hi = 0xa877; stride = 1 };
    { lo = 0xa8ce; hi = 0xa8cf; stride = 1 };
    { lo = 0xa8f8; hi = 0xa8fa; stride = 1 };
    { lo = 0xa8fc; hi = 0xa92e; stride = 50 };
    { lo = 0xa92f; hi = 0xa95f; stride = 48 };
    { lo = 0xa9c1; hi = 0xa9cd; stride = 1 };
    { lo = 0xa9de; hi = 0xa9df; stride = 1 };
    { lo = 0xaa5c; hi = 0xaa5f; stride = 1 };
    { lo = 0xaade; hi = 0xaadf; stride = 1 };
    { lo = 0xaaf0; hi = 0xaaf1; stride = 1 };
    { lo = 0xabeb; hi = 0xfd3e; stride = 20819 };
    { lo = 0xfd3f; hi = 0xfe10; stride = 209 };
    { lo = 0xfe11; hi = 0xfe19; stride = 1 };
    { lo = 0xfe30; hi = 0xfe52; stride = 1 };
    { lo = 0xfe54; hi = 0xfe61; stride = 1 };
    { lo = 0xfe63; hi = 0xfe68; stride = 5 };
    { lo = 0xfe6a; hi = 0xfe6b; stride = 1 };
    { lo = 0xff01; hi = 0xff03; stride = 1 };
    { lo = 0xff05; hi = 0xff0a; stride = 1 };
    { lo = 0xff0c; hi = 0xff0f; stride = 1 };
    { lo = 0xff1a; hi = 0xff1b; stride = 1 };
    { lo = 0xff1f; hi = 0xff20; stride = 1 };
    { lo = 0xff3b; hi = 0xff3d; stride = 1 };
    { lo = 0xff3f; hi = 0xff5b; stride = 28 };
    { lo = 0xff5d; hi = 0xff5f; stride = 2 };
    { lo = 0xff60; hi = 0xff65; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* pc *)
let _pc = {
  r16 = [|
    { lo = 0x005f; hi = 0x203f; stride = 8160 };
    { lo = 0x2040; hi = 0x2054; stride = 20 };
    { lo = 0xfe33; hi = 0xfe34; stride = 1 };
    { lo = 0xfe4d; hi = 0xfe4f; stride = 1 };
    { lo = 0xff3f; hi = 0xff3f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* pd *)
let _pd = {
  r16 = [|
    { lo = 0x002d; hi = 0x058a; stride = 1373 };
    { lo = 0x05be; hi = 0x1400; stride = 3650 };
    { lo = 0x1806; hi = 0x2010; stride = 2058 };
    { lo = 0x2011; hi = 0x2015; stride = 1 };
    { lo = 0x2e17; hi = 0x2e1a; stride = 3 };
    { lo = 0x2e3a; hi = 0x2e3b; stride = 1 };
    { lo = 0x2e40; hi = 0x2e5d; stride = 29 };
    { lo = 0x301c; hi = 0x3030; stride = 20 };
    { lo = 0x30a0; hi = 0xfe31; stride = 52625 };
    { lo = 0xfe32; hi = 0xfe58; stride = 38 };
    { lo = 0xfe63; hi = 0xff0d; stride = 170 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* pe *)
let _pe = {
  r16 = [|
    { lo = 0x0029; hi = 0x005d; stride = 52 };
    { lo = 0x007d; hi = 0x0f3b; stride = 3774 };
    { lo = 0x0f3d; hi = 0x169c; stride = 1887 };
    { lo = 0x2046; hi = 0x207e; stride = 56 };
    { lo = 0x208e; hi = 0x2309; stride = 635 };
    { lo = 0x230b; hi = 0x232a; stride = 31 };
    { lo = 0x2769; hi = 0x2775; stride = 2 };
    { lo = 0x27c6; hi = 0x27e7; stride = 33 };
    { lo = 0x27e9; hi = 0x27ef; stride = 2 };
    { lo = 0x2984; hi = 0x2998; stride = 2 };
    { lo = 0x29d9; hi = 0x29db; stride = 2 };
    { lo = 0x29fd; hi = 0x2e23; stride = 1062 };
    { lo = 0x2e25; hi = 0x2e29; stride = 2 };
    { lo = 0x2e56; hi = 0x2e5c; stride = 2 };
    { lo = 0x3009; hi = 0x3011; stride = 2 };
    { lo = 0x3015; hi = 0x301b; stride = 2 };
    { lo = 0x301e; hi = 0x301f; stride = 1 };
    { lo = 0xfd3e; hi = 0xfe18; stride = 218 };
    { lo = 0xfe36; hi = 0xfe44; stride = 2 };
    { lo = 0xfe48; hi = 0xfe5a; stride = 18 };
    { lo = 0xfe5c; hi = 0xfe5e; stride = 2 };
    { lo = 0xff09; hi = 0xff3d; stride = 52 };
    { lo = 0xff5d; hi = 0xff63; stride = 3 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* pf *)
let _pf = {
  r16 = [|
    { lo = 0x00bb; hi = 0x2019; stride = 8030 };
    { lo = 0x201d; hi = 0x203a; stride = 29 };
    { lo = 0x2e03; hi = 0x2e05; stride = 2 };
    { lo = 0x2e0a; hi = 0x2e0d; stride = 3 };
    { lo = 0x2e1d; hi = 0x2e21; stride = 4 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* pi *)
let _pi = {
  r16 = [|
    { lo = 0x00ab; hi = 0x2018; stride = 8045 };
    { lo = 0x201b; hi = 0x201c; stride = 1 };
    { lo = 0x201f; hi = 0x2039; stride = 26 };
    { lo = 0x2e02; hi = 0x2e04; stride = 2 };
    { lo = 0x2e09; hi = 0x2e0c; stride = 3 };
    { lo = 0x2e1c; hi = 0x2e20; stride = 4 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* po *)
let _po = {
  r16 = [|
    { lo = 0x0021; hi = 0x0023; stride = 1 };
    { lo = 0x0025; hi = 0x0027; stride = 1 };
    { lo = 0x002a; hi = 0x002e; stride = 2 };
    { lo = 0x002f; hi = 0x003a; stride = 11 };
    { lo = 0x003b; hi = 0x003f; stride = 4 };
    { lo = 0x0040; hi = 0x005c; stride = 28 };
    { lo = 0x00a1; hi = 0x00a7; stride = 6 };
    { lo = 0x00b6; hi = 0x00b7; stride = 1 };
    { lo = 0x00bf; hi = 0x037e; stride = 703 };
    { lo = 0x0387; hi = 0x055a; stride = 467 };
    { lo = 0x055b; hi = 0x055f; stride = 1 };
    { lo = 0x0589; hi = 0x05c0; stride = 55 };
    { lo = 0x05c3; hi = 0x05c6; stride = 3 };
    { lo = 0x05f3; hi = 0x05f4; stride = 1 };
    { lo = 0x0609; hi = 0x060a; stride = 1 };
    { lo = 0x060c; hi = 0x060d; stride = 1 };
    { lo = 0x061b; hi = 0x061d; stride = 2 };
    { lo = 0x061e; hi = 0x061f; stride = 1 };
    { lo = 0x066a; hi = 0x066d; stride = 1 };
    { lo = 0x06d4; hi = 0x0700; stride = 44 };
    { lo = 0x0701; hi = 0x070d; stride = 1 };
    { lo = 0x07f7; hi = 0x07f9; stride = 1 };
    { lo = 0x0830; hi = 0x083e; stride = 1 };
    { lo = 0x085e; hi = 0x0964; stride = 262 };
    { lo = 0x0965; hi = 0x0970; stride = 11 };
    { lo = 0x09fd; hi = 0x0a76; stride = 121 };
    { lo = 0x0af0; hi = 0x0c77; stride = 391 };
    { lo = 0x0c84; hi = 0x0df4; stride = 368 };
    { lo = 0x0e4f; hi = 0x0e5a; stride = 11 };
    { lo = 0x0e5b; hi = 0x0f04; stride = 169 };
    { lo = 0x0f05; hi = 0x0f12; stride = 1 };
    { lo = 0x0f14; hi = 0x0f85; stride = 113 };
    { lo = 0x0fd0; hi = 0x0fd4; stride = 1 };
    { lo = 0x0fd9; hi = 0x0fda; stride = 1 };
    { lo = 0x104a; hi = 0x104f; stride = 1 };
    { lo = 0x10fb; hi = 0x1360; stride = 613 };
    { lo = 0x1361; hi = 0x1368; stride = 1 };
    { lo = 0x166e; hi = 0x16eb; stride = 125 };
    { lo = 0x16ec; hi = 0x16ed; stride = 1 };
    { lo = 0x1735; hi = 0x1736; stride = 1 };
    { lo = 0x17d4; hi = 0x17d6; stride = 1 };
    { lo = 0x17d8; hi = 0x17da; stride = 1 };
    { lo = 0x1800; hi = 0x1805; stride = 1 };
    { lo = 0x1807; hi = 0x180a; stride = 1 };
    { lo = 0x1944; hi = 0x1945; stride = 1 };
    { lo = 0x1a1e; hi = 0x1a1f; stride = 1 };
    { lo = 0x1aa0; hi = 0x1aa6; stride = 1 };
    { lo = 0x1aa8; hi = 0x1aad; stride = 1 };
    { lo = 0x1b5a; hi = 0x1b60; stride = 1 };
    { lo = 0x1b7d; hi = 0x1b7e; stride = 1 };
    { lo = 0x1bfc; hi = 0x1bff; stride = 1 };
    { lo = 0x1c3b; hi = 0x1c3f; stride = 1 };
    { lo = 0x1c7e; hi = 0x1c7f; stride = 1 };
    { lo = 0x1cc0; hi = 0x1cc7; stride = 1 };
    { lo = 0x1cd3; hi = 0x2016; stride = 835 };
    { lo = 0x2017; hi = 0x2020; stride = 9 };
    { lo = 0x2021; hi = 0x2027; stride = 1 };
    { lo = 0x2030; hi = 0x2038; stride = 1 };
    { lo = 0x203b; hi = 0x203e; stride = 1 };
    { lo = 0x2041; hi = 0x2043; stride = 1 };
    { lo = 0x2047; hi = 0x2051; stride = 1 };
    { lo = 0x2053; hi = 0x2055; stride = 2 };
    { lo = 0x2056; hi = 0x205e; stride = 1 };
    { lo = 0x2cf9; hi = 0x2cfc; stride = 1 };
    { lo = 0x2cfe; hi = 0x2cff; stride = 1 };
    { lo = 0x2d70; hi = 0x2e00; stride = 144 };
    { lo = 0x2e01; hi = 0x2e06; stride = 5 };
    { lo = 0x2e07; hi = 0x2e08; stride = 1 };
    { lo = 0x2e0b; hi = 0x2e0e; stride = 3 };
    { lo = 0x2e0f; hi = 0x2e16; stride = 1 };
    { lo = 0x2e18; hi = 0x2e19; stride = 1 };
    { lo = 0x2e1b; hi = 0x2e1e; stride = 3 };
    { lo = 0x2e1f; hi = 0x2e2a; stride = 11 };
    { lo = 0x2e2b; hi = 0x2e2e; stride = 1 };
    { lo = 0x2e30; hi = 0x2e39; stride = 1 };
    { lo = 0x2e3c; hi = 0x2e3f; stride = 1 };
    { lo = 0x2e41; hi = 0x2e43; stride = 2 };
    { lo = 0x2e44; hi = 0x2e4f; stride = 1 };
    { lo = 0x2e52; hi = 0x2e54; stride = 1 };
    { lo = 0x3001; hi = 0x3003; stride = 1 };
    { lo = 0x303d; hi = 0x30fb; stride = 190 };
    { lo = 0xa4fe; hi = 0xa4ff; stride = 1 };
    { lo = 0xa60d; hi = 0xa60f; stride = 1 };
    { lo = 0xa673; hi = 0xa67e; stride = 11 };
    { lo = 0xa6f2; hi = 0xa6f7; stride = 1 };
    { lo = 0xa874; hi = 0xa877; stride = 1 };
    { lo = 0xa8ce; hi = 0xa8cf; stride = 1 };
    { lo = 0xa8f8; hi = 0xa8fa; stride = 1 };
    { lo = 0xa8fc; hi = 0xa92e; stride = 50 };
    { lo = 0xa92f; hi = 0xa95f; stride = 48 };
    { lo = 0xa9c1; hi = 0xa9cd; stride = 1 };
    { lo = 0xa9de; hi = 0xa9df; stride = 1 };
    { lo = 0xaa5c; hi = 0xaa5f; stride = 1 };
    { lo = 0xaade; hi = 0xaadf; stride = 1 };
    { lo = 0xaaf0; hi = 0xaaf1; stride = 1 };
    { lo = 0xabeb; hi = 0xfe10; stride = 21029 };
    { lo = 0xfe11; hi = 0xfe16; stride = 1 };
    { lo = 0xfe19; hi = 0xfe30; stride = 23 };
    { lo = 0xfe45; hi = 0xfe46; stride = 1 };
    { lo = 0xfe49; hi = 0xfe4c; stride = 1 };
    { lo = 0xfe50; hi = 0xfe52; stride = 1 };
    { lo = 0xfe54; hi = 0xfe57; stride = 1 };
    { lo = 0xfe5f; hi = 0xfe61; stride = 1 };
    { lo = 0xfe68; hi = 0xfe6a; stride = 2 };
    { lo = 0xfe6b; hi = 0xff01; stride = 150 };
    { lo = 0xff02; hi = 0xff03; stride = 1 };
    { lo = 0xff05; hi = 0xff07; stride = 1 };
    { lo = 0xff0a; hi = 0xff0e; stride = 2 };
    { lo = 0xff0f; hi = 0xff1a; stride = 11 };
    { lo = 0xff1b; hi = 0xff1f; stride = 4 };
    { lo = 0xff20; hi = 0xff3c; stride = 28 };
    { lo = 0xff61; hi = 0xff64; stride = 3 };
    { lo = 0xff65; hi = 0xff65; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* ps *)
let _ps = {
  r16 = [|
    { lo = 0x0028; hi = 0x005b; stride = 51 };
    { lo = 0x007b; hi = 0x0f3a; stride = 3775 };
    { lo = 0x0f3c; hi = 0x169b; stride = 1887 };
    { lo = 0x201a; hi = 0x201e; stride = 4 };
    { lo = 0x2045; hi = 0x207d; stride = 56 };
    { lo = 0x208d; hi = 0x2308; stride = 635 };
    { lo = 0x230a; hi = 0x2329; stride = 31 };
    { lo = 0x2768; hi = 0x2774; stride = 2 };
    { lo = 0x27c5; hi = 0x27e6; stride = 33 };
    { lo = 0x27e8; hi = 0x27ee; stride = 2 };
    { lo = 0x2983; hi = 0x2997; stride = 2 };
    { lo = 0x29d8; hi = 0x29da; stride = 2 };
    { lo = 0x29fc; hi = 0x2e22; stride = 1062 };
    { lo = 0x2e24; hi = 0x2e28; stride = 2 };
    { lo = 0x2e42; hi = 0x2e55; stride = 19 };
    { lo = 0x2e57; hi = 0x2e5b; stride = 2 };
    { lo = 0x3008; hi = 0x3010; stride = 2 };
    { lo = 0x3014; hi = 0x301a; stride = 2 };
    { lo = 0x301d; hi = 0xfd3f; stride = 52514 };
    { lo = 0xfe17; hi = 0xfe35; stride = 30 };
    { lo = 0xfe37; hi = 0xfe43; stride = 2 };
    { lo = 0xfe47; hi = 0xfe59; stride = 18 };
    { lo = 0xfe5b; hi = 0xfe5d; stride = 2 };
    { lo = 0xff08; hi = 0xff3b; stride = 51 };
    { lo = 0xff5b; hi = 0xff5f; stride = 4 };
    { lo = 0xff62; hi = 0xff62; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* s *)
let _s = {
  r16 = [|
    { lo = 0x0024; hi = 0x002b; stride = 7 };
    { lo = 0x003c; hi = 0x003e; stride = 1 };
    { lo = 0x005e; hi = 0x0060; stride = 2 };
    { lo = 0x007c; hi = 0x007e; stride = 2 };
    { lo = 0x00a2; hi = 0x00a6; stride = 1 };
    { lo = 0x00a8; hi = 0x00a9; stride = 1 };
    { lo = 0x00ac; hi = 0x00ae; stride = 2 };
    { lo = 0x00af; hi = 0x00b1; stride = 1 };
    { lo = 0x00b4; hi = 0x00b8; stride = 4 };
    { lo = 0x00d7; hi = 0x00f7; stride = 32 };
    { lo = 0x02c2; hi = 0x02c5; stride = 1 };
    { lo = 0x02d2; hi = 0x02df; stride = 1 };
    { lo = 0x02e5; hi = 0x02eb; stride = 1 };
    { lo = 0x02ed; hi = 0x02ef; stride = 2 };
    { lo = 0x02f0; hi = 0x02ff; stride = 1 };
    { lo = 0x0375; hi = 0x0384; stride = 15 };
    { lo = 0x0385; hi = 0x03f6; stride = 113 };
    { lo = 0x0482; hi = 0x058d; stride = 267 };
    { lo = 0x058e; hi = 0x058f; stride = 1 };
    { lo = 0x0606; hi = 0x0608; stride = 1 };
    { lo = 0x060b; hi = 0x060e; stride = 3 };
    { lo = 0x060f; hi = 0x06de; stride = 207 };
    { lo = 0x06e9; hi = 0x06fd; stride = 20 };
    { lo = 0x06fe; hi = 0x07f6; stride = 248 };
    { lo = 0x07fe; hi = 0x07ff; stride = 1 };
    { lo = 0x0888; hi = 0x09f2; stride = 362 };
    { lo = 0x09f3; hi = 0x09fa; stride = 7 };
    { lo = 0x09fb; hi = 0x0af1; stride = 246 };
    { lo = 0x0b70; hi = 0x0bf3; stride = 131 };
    { lo = 0x0bf4; hi = 0x0bfa; stride = 1 };
    { lo = 0x0c7f; hi = 0x0d4f; stride = 208 };
    { lo = 0x0d79; hi = 0x0e3f; stride = 198 };
    { lo = 0x0f01; hi = 0x0f03; stride = 1 };
    { lo = 0x0f13; hi = 0x0f15; stride = 2 };
    { lo = 0x0f16; hi = 0x0f17; stride = 1 };
    { lo = 0x0f1a; hi = 0x0f1f; stride = 1 };
    { lo = 0x0f34; hi = 0x0f38; stride = 2 };
    { lo = 0x0fbe; hi = 0x0fc5; stride = 1 };
    { lo = 0x0fc7; hi = 0x0fcc; stride = 1 };
    { lo = 0x0fce; hi = 0x0fcf; stride = 1 };
    { lo = 0x0fd5; hi = 0x0fd8; stride = 1 };
    { lo = 0x109e; hi = 0x109f; stride = 1 };
    { lo = 0x1390; hi = 0x1399; stride = 1 };
    { lo = 0x166d; hi = 0x17db; stride = 366 };
    { lo = 0x1940; hi = 0x19de; stride = 158 };
    { lo = 0x19df; hi = 0x19ff; stride = 1 };
    { lo = 0x1b61; hi = 0x1b6a; stride = 1 };
    { lo = 0x1b74; hi = 0x1b7c; stride = 1 };
    { lo = 0x1fbd; hi = 0x1fbf; stride = 2 };
    { lo = 0x1fc0; hi = 0x1fc1; stride = 1 };
    { lo = 0x1fcd; hi = 0x1fcf; stride = 1 };
    { lo = 0x1fdd; hi = 0x1fdf; stride = 1 };
    { lo = 0x1fed; hi = 0x1fef; stride = 1 };
    { lo = 0x1ffd; hi = 0x1ffe; stride = 1 };
    { lo = 0x2044; hi = 0x2052; stride = 14 };
    { lo = 0x207a; hi = 0x207c; stride = 1 };
    { lo = 0x208a; hi = 0x208c; stride = 1 };
    { lo = 0x20a0; hi = 0x20c0; stride = 1 };
    { lo = 0x2100; hi = 0x2101; stride = 1 };
    { lo = 0x2103; hi = 0x2106; stride = 1 };
    { lo = 0x2108; hi = 0x2109; stride = 1 };
    { lo = 0x2114; hi = 0x2116; stride = 2 };
    { lo = 0x2117; hi = 0x2118; stride = 1 };
    { lo = 0x211e; hi = 0x2123; stride = 1 };
    { lo = 0x2125; hi = 0x2129; stride = 2 };
    { lo = 0x212e; hi = 0x213a; stride = 12 };
    { lo = 0x213b; hi = 0x2140; stride = 5 };
    { lo = 0x2141; hi = 0x2144; stride = 1 };
    { lo = 0x214a; hi = 0x214d; stride = 1 };
    { lo = 0x214f; hi = 0x218a; stride = 59 };
    { lo = 0x218b; hi = 0x2190; stride = 5 };
    { lo = 0x2191; hi = 0x2307; stride = 1 };
    { lo = 0x230c; hi = 0x2328; stride = 1 };
    { lo = 0x232b; hi = 0x2426; stride = 1 };
    { lo = 0x2440; hi = 0x244a; stride = 1 };
    { lo = 0x249c; hi = 0x24e9; stride = 1 };
    { lo = 0x2500; hi = 0x2767; stride = 1 };
    { lo = 0x2794; hi = 0x27c4; stride = 1 };
    { lo = 0x27c7; hi = 0x27e5; stride = 1 };
    { lo = 0x27f0; hi = 0x2982; stride = 1 };
    { lo = 0x2999; hi = 0x29d7; stride = 1 };
    { lo = 0x29dc; hi = 0x29fb; stride = 1 };
    { lo = 0x29fe; hi = 0x2b73; stride = 1 };
    { lo = 0x2b76; hi = 0x2b95; stride = 1 };
    { lo = 0x2b97; hi = 0x2bff; stride = 1 };
    { lo = 0x2ce5; hi = 0x2cea; stride = 1 };
    { lo = 0x2e50; hi = 0x2e51; stride = 1 };
    { lo = 0x2e80; hi = 0x2e99; stride = 1 };
    { lo = 0x2e9b; hi = 0x2ef3; stride = 1 };
    { lo = 0x2f00; hi = 0x2fd5; stride = 1 };
    { lo = 0x2ff0; hi = 0x2ffb; stride = 1 };
    { lo = 0x3004; hi = 0x3012; stride = 14 };
    { lo = 0x3013; hi = 0x3020; stride = 13 };
    { lo = 0x3036; hi = 0x3037; stride = 1 };
    { lo = 0x303e; hi = 0x303f; stride = 1 };
    { lo = 0x309b; hi = 0x309c; stride = 1 };
    { lo = 0x3190; hi = 0x3191; stride = 1 };
    { lo = 0x3196; hi = 0x319f; stride = 1 };
    { lo = 0x31c0; hi = 0x31e3; stride = 1 };
    { lo = 0x3200; hi = 0x321e; stride = 1 };
    { lo = 0x322a; hi = 0x3247; stride = 1 };
    { lo = 0x3250; hi = 0x3260; stride = 16 };
    { lo = 0x3261; hi = 0x327f; stride = 1 };
    { lo = 0x328a; hi = 0x32b0; stride = 1 };
    { lo = 0x32c0; hi = 0x33ff; stride = 1 };
    { lo = 0x4dc0; hi = 0x4dff; stride = 1 };
    { lo = 0xa490; hi = 0xa4c6; stride = 1 };
    { lo = 0xa700; hi = 0xa716; stride = 1 };
    { lo = 0xa720; hi = 0xa721; stride = 1 };
    { lo = 0xa789; hi = 0xa78a; stride = 1 };
    { lo = 0xa828; hi = 0xa82b; stride = 1 };
    { lo = 0xa836; hi = 0xa839; stride = 1 };
    { lo = 0xaa77; hi = 0xaa79; stride = 1 };
    { lo = 0xab5b; hi = 0xab6a; stride = 15 };
    { lo = 0xab6b; hi = 0xfb29; stride = 20414 };
    { lo = 0xfbb2; hi = 0xfbc2; stride = 1 };
    { lo = 0xfd40; hi = 0xfd4f; stride = 1 };
    { lo = 0xfdcf; hi = 0xfdfc; stride = 45 };
    { lo = 0xfdfd; hi = 0xfdff; stride = 1 };
    { lo = 0xfe62; hi = 0xfe64; stride = 2 };
    { lo = 0xfe65; hi = 0xfe66; stride = 1 };
    { lo = 0xfe69; hi = 0xff04; stride = 155 };
    { lo = 0xff0b; hi = 0xff1c; stride = 17 };
    { lo = 0xff1d; hi = 0xff1e; stride = 1 };
    { lo = 0xff3e; hi = 0xff40; stride = 2 };
    { lo = 0xff5c; hi = 0xff5e; stride = 2 };
    { lo = 0xffe0; hi = 0xffe6; stride = 1 };
    { lo = 0xffe8; hi = 0xffee; stride = 1 };
    { lo = 0xfffc; hi = 0xfffd; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* sc *)
let _sc = {
  r16 = [|
    { lo = 0x0024; hi = 0x00a2; stride = 126 };
    { lo = 0x00a3; hi = 0x00a5; stride = 1 };
    { lo = 0x058f; hi = 0x060b; stride = 124 };
    { lo = 0x07fe; hi = 0x07ff; stride = 1 };
    { lo = 0x09f2; hi = 0x09f3; stride = 1 };
    { lo = 0x09fb; hi = 0x0af1; stride = 246 };
    { lo = 0x0bf9; hi = 0x0e3f; stride = 582 };
    { lo = 0x17db; hi = 0x20a0; stride = 2245 };
    { lo = 0x20a1; hi = 0x20c0; stride = 1 };
    { lo = 0xa838; hi = 0xfdfc; stride = 21956 };
    { lo = 0xfe69; hi = 0xff04; stride = 155 };
    { lo = 0xffe0; hi = 0xffe1; stride = 1 };
    { lo = 0xffe5; hi = 0xffe6; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* sk *)
let _sk = {
  r16 = [|
    { lo = 0x005e; hi = 0x0060; stride = 2 };
    { lo = 0x00a8; hi = 0x00af; stride = 7 };
    { lo = 0x00b4; hi = 0x00b8; stride = 4 };
    { lo = 0x02c2; hi = 0x02c5; stride = 1 };
    { lo = 0x02d2; hi = 0x02df; stride = 1 };
    { lo = 0x02e5; hi = 0x02eb; stride = 1 };
    { lo = 0x02ed; hi = 0x02ef; stride = 2 };
    { lo = 0x02f0; hi = 0x02ff; stride = 1 };
    { lo = 0x0375; hi = 0x0384; stride = 15 };
    { lo = 0x0385; hi = 0x0888; stride = 1283 };
    { lo = 0x1fbd; hi = 0x1fbf; stride = 2 };
    { lo = 0x1fc0; hi = 0x1fc1; stride = 1 };
    { lo = 0x1fcd; hi = 0x1fcf; stride = 1 };
    { lo = 0x1fdd; hi = 0x1fdf; stride = 1 };
    { lo = 0x1fed; hi = 0x1fef; stride = 1 };
    { lo = 0x1ffd; hi = 0x1ffe; stride = 1 };
    { lo = 0x309b; hi = 0x309c; stride = 1 };
    { lo = 0xa700; hi = 0xa716; stride = 1 };
    { lo = 0xa720; hi = 0xa721; stride = 1 };
    { lo = 0xa789; hi = 0xa78a; stride = 1 };
    { lo = 0xab5b; hi = 0xab6a; stride = 15 };
    { lo = 0xab6b; hi = 0xfbb2; stride = 20551 };
    { lo = 0xfbb3; hi = 0xfbc2; stride = 1 };
    { lo = 0xff3e; hi = 0xff40; stride = 2 };
    { lo = 0xffe3; hi = 0xffe3; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* sm *)
let _sm = {
  r16 = [|
    { lo = 0x002b; hi = 0x003c; stride = 17 };
    { lo = 0x003d; hi = 0x003e; stride = 1 };
    { lo = 0x007c; hi = 0x007e; stride = 2 };
    { lo = 0x00ac; hi = 0x00b1; stride = 5 };
    { lo = 0x00d7; hi = 0x00f7; stride = 32 };
    { lo = 0x03f6; hi = 0x0606; stride = 528 };
    { lo = 0x0607; hi = 0x0608; stride = 1 };
    { lo = 0x2044; hi = 0x2052; stride = 14 };
    { lo = 0x207a; hi = 0x207c; stride = 1 };
    { lo = 0x208a; hi = 0x208c; stride = 1 };
    { lo = 0x2118; hi = 0x2140; stride = 40 };
    { lo = 0x2141; hi = 0x2144; stride = 1 };
    { lo = 0x214b; hi = 0x2190; stride = 69 };
    { lo = 0x2191; hi = 0x2194; stride = 1 };
    { lo = 0x219a; hi = 0x219b; stride = 1 };
    { lo = 0x21a0; hi = 0x21a6; stride = 3 };
    { lo = 0x21ae; hi = 0x21ce; stride = 32 };
    { lo = 0x21cf; hi = 0x21d2; stride = 3 };
    { lo = 0x21d4; hi = 0x21f4; stride = 32 };
    { lo = 0x21f5; hi = 0x22ff; stride = 1 };
    { lo = 0x2320; hi = 0x2321; stride = 1 };
    { lo = 0x237c; hi = 0x239b; stride = 31 };
    { lo = 0x239c; hi = 0x23b3; stride = 1 };
    { lo = 0x23dc; hi = 0x23e1; stride = 1 };
    { lo = 0x25b7; hi = 0x25c1; stride = 10 };
    { lo = 0x25f8; hi = 0x25ff; stride = 1 };
    { lo = 0x266f; hi = 0x27c0; stride = 337 };
    { lo = 0x27c1; hi = 0x27c4; stride = 1 };
    { lo = 0x27c7; hi = 0x27e5; stride = 1 };
    { lo = 0x27f0; hi = 0x27ff; stride = 1 };
    { lo = 0x2900; hi = 0x2982; stride = 1 };
    { lo = 0x2999; hi = 0x29d7; stride = 1 };
    { lo = 0x29dc; hi = 0x29fb; stride = 1 };
    { lo = 0x29fe; hi = 0x2aff; stride = 1 };
    { lo = 0x2b30; hi = 0x2b44; stride = 1 };
    { lo = 0x2b47; hi = 0x2b4c; stride = 1 };
    { lo = 0xfb29; hi = 0xfe62; stride = 825 };
    { lo = 0xfe64; hi = 0xfe66; stride = 1 };
    { lo = 0xff0b; hi = 0xff1c; stride = 17 };
    { lo = 0xff1d; hi = 0xff1e; stride = 1 };
    { lo = 0xff5c; hi = 0xff5e; stride = 2 };
    { lo = 0xffe2; hi = 0xffe9; stride = 7 };
    { lo = 0xffea; hi = 0xffec; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* so *)
let _so = {
  r16 = [|
    { lo = 0x00a6; hi = 0x00a9; stride = 3 };
    { lo = 0x00ae; hi = 0x00b0; stride = 2 };
    { lo = 0x0482; hi = 0x058d; stride = 267 };
    { lo = 0x058e; hi = 0x060e; stride = 128 };
    { lo = 0x060f; hi = 0x06de; stride = 207 };
    { lo = 0x06e9; hi = 0x06fd; stride = 20 };
    { lo = 0x06fe; hi = 0x07f6; stride = 248 };
    { lo = 0x09fa; hi = 0x0b70; stride = 374 };
    { lo = 0x0bf3; hi = 0x0bf8; stride = 1 };
    { lo = 0x0bfa; hi = 0x0c7f; stride = 133 };
    { lo = 0x0d4f; hi = 0x0d79; stride = 42 };
    { lo = 0x0f01; hi = 0x0f03; stride = 1 };
    { lo = 0x0f13; hi = 0x0f15; stride = 2 };
    { lo = 0x0f16; hi = 0x0f17; stride = 1 };
    { lo = 0x0f1a; hi = 0x0f1f; stride = 1 };
    { lo = 0x0f34; hi = 0x0f38; stride = 2 };
    { lo = 0x0fbe; hi = 0x0fc5; stride = 1 };
    { lo = 0x0fc7; hi = 0x0fcc; stride = 1 };
    { lo = 0x0fce; hi = 0x0fcf; stride = 1 };
    { lo = 0x0fd5; hi = 0x0fd8; stride = 1 };
    { lo = 0x109e; hi = 0x109f; stride = 1 };
    { lo = 0x1390; hi = 0x1399; stride = 1 };
    { lo = 0x166d; hi = 0x1940; stride = 723 };
    { lo = 0x19de; hi = 0x19ff; stride = 1 };
    { lo = 0x1b61; hi = 0x1b6a; stride = 1 };
    { lo = 0x1b74; hi = 0x1b7c; stride = 1 };
    { lo = 0x2100; hi = 0x2101; stride = 1 };
    { lo = 0x2103; hi = 0x2106; stride = 1 };
    { lo = 0x2108; hi = 0x2109; stride = 1 };
    { lo = 0x2114; hi = 0x2116; stride = 2 };
    { lo = 0x2117; hi = 0x211e; stride = 7 };
    { lo = 0x211f; hi = 0x2123; stride = 1 };
    { lo = 0x2125; hi = 0x2129; stride = 2 };
    { lo = 0x212e; hi = 0x213a; stride = 12 };
    { lo = 0x213b; hi = 0x214a; stride = 15 };
    { lo = 0x214c; hi = 0x214d; stride = 1 };
    { lo = 0x214f; hi = 0x218a; stride = 59 };
    { lo = 0x218b; hi = 0x2195; stride = 10 };
    { lo = 0x2196; hi = 0x2199; stride = 1 };
    { lo = 0x219c; hi = 0x219f; stride = 1 };
    { lo = 0x21a1; hi = 0x21a2; stride = 1 };
    { lo = 0x21a4; hi = 0x21a5; stride = 1 };
    { lo = 0x21a7; hi = 0x21ad; stride = 1 };
    { lo = 0x21af; hi = 0x21cd; stride = 1 };
    { lo = 0x21d0; hi = 0x21d1; stride = 1 };
    { lo = 0x21d3; hi = 0x21d5; stride = 2 };
    { lo = 0x21d6; hi = 0x21f3; stride = 1 };
    { lo = 0x2300; hi = 0x2307; stride = 1 };
    { lo = 0x230c; hi = 0x231f; stride = 1 };
    { lo = 0x2322; hi = 0x2328; stride = 1 };
    { lo = 0x232b; hi = 0x237b; stride = 1 };
    { lo = 0x237d; hi = 0x239a; stride = 1 };
    { lo = 0x23b4; hi = 0x23db; stride = 1 };
    { lo = 0x23e2; hi = 0x2426; stride = 1 };
    { lo = 0x2440; hi = 0x244a; stride = 1 };
    { lo = 0x249c; hi = 0x24e9; stride = 1 };
    { lo = 0x2500; hi = 0x25b6; stride = 1 };
    { lo = 0x25b8; hi = 0x25c0; stride = 1 };
    { lo = 0x25c2; hi = 0x25f7; stride = 1 };
    { lo = 0x2600; hi = 0x266e; stride = 1 };
    { lo = 0x2670; hi = 0x2767; stride = 1 };
    { lo = 0x2794; hi = 0x27bf; stride = 1 };
    { lo = 0x2800; hi = 0x28ff; stride = 1 };
    { lo = 0x2b00; hi = 0x2b2f; stride = 1 };
    { lo = 0x2b45; hi = 0x2b46; stride = 1 };
    { lo = 0x2b4d; hi = 0x2b73; stride = 1 };
    { lo = 0x2b76; hi = 0x2b95; stride = 1 };
    { lo = 0x2b97; hi = 0x2bff; stride = 1 };
    { lo = 0x2ce5; hi = 0x2cea; stride = 1 };
    { lo = 0x2e50; hi = 0x2e51; stride = 1 };
    { lo = 0x2e80; hi = 0x2e99; stride = 1 };
    { lo = 0x2e9b; hi = 0x2ef3; stride = 1 };
    { lo = 0x2f00; hi = 0x2fd5; stride = 1 };
    { lo = 0x2ff0; hi = 0x2ffb; stride = 1 };
    { lo = 0x3004; hi = 0x3012; stride = 14 };
    { lo = 0x3013; hi = 0x3020; stride = 13 };
    { lo = 0x3036; hi = 0x3037; stride = 1 };
    { lo = 0x303e; hi = 0x303f; stride = 1 };
    { lo = 0x3190; hi = 0x3191; stride = 1 };
    { lo = 0x3196; hi = 0x319f; stride = 1 };
    { lo = 0x31c0; hi = 0x31e3; stride = 1 };
    { lo = 0x3200; hi = 0x321e; stride = 1 };
    { lo = 0x322a; hi = 0x3247; stride = 1 };
    { lo = 0x3250; hi = 0x3260; stride = 16 };
    { lo = 0x3261; hi = 0x327f; stride = 1 };
    { lo = 0x328a; hi = 0x32b0; stride = 1 };
    { lo = 0x32c0; hi = 0x33ff; stride = 1 };
    { lo = 0x4dc0; hi = 0x4dff; stride = 1 };
    { lo = 0xa490; hi = 0xa4c6; stride = 1 };
    { lo = 0xa828; hi = 0xa82b; stride = 1 };
    { lo = 0xa836; hi = 0xa837; stride = 1 };
    { lo = 0xa839; hi = 0xaa77; stride = 574 };
    { lo = 0xaa78; hi = 0xaa79; stride = 1 };
    { lo = 0xfd40; hi = 0xfd4f; stride = 1 };
    { lo = 0xfdcf; hi = 0xfdfd; stride = 46 };
    { lo = 0xfdfe; hi = 0xfdff; stride = 1 };
    { lo = 0xffe4; hi = 0xffe8; stride = 4 };
    { lo = 0xffed; hi = 0xffee; stride = 1 };
    { lo = 0xfffc; hi = 0xfffd; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* z *)
let _z = {
  r16 = [|
    { lo = 0x0020; hi = 0x00a0; stride = 128 };
    { lo = 0x1680; hi = 0x2000; stride = 2432 };
    { lo = 0x2001; hi = 0x200a; stride = 1 };
    { lo = 0x2028; hi = 0x2029; stride = 1 };
    { lo = 0x202f; hi = 0x205f; stride = 48 };
    { lo = 0x3000; hi = 0x3000; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* zl *)
let _zl = {
  r16 = [|
    { lo = 0x2028; hi = 0x2028; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* zp *)
let _zp = {
  r16 = [|
    { lo = 0x2029; hi = 0x2029; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* zs *)
let _zs = {
  r16 = [|
    { lo = 0x0020; hi = 0x00a0; stride = 128 };
    { lo = 0x1680; hi = 0x2000; stride = 2432 };
    { lo = 0x2001; hi = 0x200a; stride = 1 };
    { lo = 0x202f; hi = 0x205f; stride = 48 };
    { lo = 0x3000; hi = 0x3000; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* adlam *)
let _adlam = {
  r16 = [| |];
  r32 = [|
    { lo = 0x01e900; hi = 0x01e94b; stride = 1 };
    { lo = 0x01e950; hi = 0x01e959; stride = 1 };
    { lo = 0x01e95e; hi = 0x01e95f; stride = 1 };
  |];
  latin_offset = 0;
}

(* ahom *)
let _ahom = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011700; hi = 0x01171a; stride = 1 };
    { lo = 0x01171d; hi = 0x01172b; stride = 1 };
    { lo = 0x011730; hi = 0x011746; stride = 1 };
  |];
  latin_offset = 0;
}

(* anatolian_hieroglyphs *)
let _anatolian_hieroglyphs = {
  r16 = [| |];
  r32 = [|
    { lo = 0x014400; hi = 0x014646; stride = 1 };
  |];
  latin_offset = 0;
}

(* arabic *)
let _arabic = {
  r16 = [|
    { lo = 0x0600; hi = 0x0604; stride = 1 };
    { lo = 0x0606; hi = 0x060b; stride = 1 };
    { lo = 0x060d; hi = 0x061a; stride = 1 };
    { lo = 0x061c; hi = 0x061e; stride = 1 };
    { lo = 0x0620; hi = 0x063f; stride = 1 };
    { lo = 0x0641; hi = 0x064a; stride = 1 };
    { lo = 0x0656; hi = 0x066f; stride = 1 };
    { lo = 0x0671; hi = 0x06dc; stride = 1 };
    { lo = 0x06de; hi = 0x06ff; stride = 1 };
    { lo = 0x0750; hi = 0x077f; stride = 1 };
    { lo = 0x0870; hi = 0x088e; stride = 1 };
    { lo = 0x0890; hi = 0x0891; stride = 1 };
    { lo = 0x0898; hi = 0x08e1; stride = 1 };
    { lo = 0x08e3; hi = 0x08ff; stride = 1 };
    { lo = 0xfb50; hi = 0xfbc2; stride = 1 };
    { lo = 0xfbd3; hi = 0xfd3d; stride = 1 };
    { lo = 0xfd40; hi = 0xfd8f; stride = 1 };
    { lo = 0xfd92; hi = 0xfdc7; stride = 1 };
    { lo = 0xfdcf; hi = 0xfdf0; stride = 33 };
    { lo = 0xfdf1; hi = 0xfdff; stride = 1 };
    { lo = 0xfe70; hi = 0xfe74; stride = 1 };
    { lo = 0xfe76; hi = 0xfefc; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* armenian *)
let _armenian = {
  r16 = [|
    { lo = 0x0531; hi = 0x0556; stride = 1 };
    { lo = 0x0559; hi = 0x058a; stride = 1 };
    { lo = 0x058d; hi = 0x058f; stride = 1 };
    { lo = 0xfb13; hi = 0xfb17; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* avestan *)
let _avestan = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010b00; hi = 0x010b35; stride = 1 };
    { lo = 0x010b39; hi = 0x010b3f; stride = 1 };
  |];
  latin_offset = 0;
}

(* balinese *)
let _balinese = {
  r16 = [|
    { lo = 0x1b00; hi = 0x1b4c; stride = 1 };
    { lo = 0x1b50; hi = 0x1b7e; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* bamum *)
let _bamum = {
  r16 = [|
    { lo = 0xa6a0; hi = 0xa6f7; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* bassa_vah *)
let _bassa_vah = {
  r16 = [| |];
  r32 = [|
    { lo = 0x016ad0; hi = 0x016aed; stride = 1 };
    { lo = 0x016af0; hi = 0x016af5; stride = 1 };
  |];
  latin_offset = 0;
}

(* batak *)
let _batak = {
  r16 = [|
    { lo = 0x1bc0; hi = 0x1bf3; stride = 1 };
    { lo = 0x1bfc; hi = 0x1bff; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* bengali *)
let _bengali = {
  r16 = [|
    { lo = 0x0980; hi = 0x0983; stride = 1 };
    { lo = 0x0985; hi = 0x098c; stride = 1 };
    { lo = 0x098f; hi = 0x0990; stride = 1 };
    { lo = 0x0993; hi = 0x09a8; stride = 1 };
    { lo = 0x09aa; hi = 0x09b0; stride = 1 };
    { lo = 0x09b2; hi = 0x09b6; stride = 4 };
    { lo = 0x09b7; hi = 0x09b9; stride = 1 };
    { lo = 0x09bc; hi = 0x09c4; stride = 1 };
    { lo = 0x09c7; hi = 0x09c8; stride = 1 };
    { lo = 0x09cb; hi = 0x09ce; stride = 1 };
    { lo = 0x09d7; hi = 0x09dc; stride = 5 };
    { lo = 0x09dd; hi = 0x09df; stride = 2 };
    { lo = 0x09e0; hi = 0x09e3; stride = 1 };
    { lo = 0x09e6; hi = 0x09fe; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* bhaiksuki *)
let _bhaiksuki = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011c00; hi = 0x011c08; stride = 1 };
    { lo = 0x011c0a; hi = 0x011c36; stride = 1 };
    { lo = 0x011c38; hi = 0x011c45; stride = 1 };
    { lo = 0x011c50; hi = 0x011c6c; stride = 1 };
  |];
  latin_offset = 0;
}

(* bopomofo *)
let _bopomofo = {
  r16 = [|
    { lo = 0x02ea; hi = 0x02eb; stride = 1 };
    { lo = 0x3105; hi = 0x312f; stride = 1 };
    { lo = 0x31a0; hi = 0x31bf; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* brahmi *)
let _brahmi = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011000; hi = 0x01104d; stride = 1 };
    { lo = 0x011052; hi = 0x011075; stride = 1 };
    { lo = 0x01107f; hi = 0x01107f; stride = 1 };
  |];
  latin_offset = 0;
}

(* braille *)
let _braille = {
  r16 = [|
    { lo = 0x2800; hi = 0x28ff; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* buginese *)
let _buginese = {
  r16 = [|
    { lo = 0x1a00; hi = 0x1a1b; stride = 1 };
    { lo = 0x1a1e; hi = 0x1a1f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* buhid *)
let _buhid = {
  r16 = [|
    { lo = 0x1740; hi = 0x1753; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* canadian_aboriginal *)
let _canadian_aboriginal = {
  r16 = [|
    { lo = 0x1400; hi = 0x167f; stride = 1 };
    { lo = 0x18b0; hi = 0x18f5; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* carian *)
let _carian = {
  r16 = [| |];
  r32 = [|
    { lo = 0x0102a0; hi = 0x0102d0; stride = 1 };
  |];
  latin_offset = 0;
}

(* caucasian_albanian *)
let _caucasian_albanian = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010530; hi = 0x010563; stride = 1 };
    { lo = 0x01056f; hi = 0x01056f; stride = 1 };
  |];
  latin_offset = 0;
}

(* chakma *)
let _chakma = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011100; hi = 0x011134; stride = 1 };
    { lo = 0x011136; hi = 0x011147; stride = 1 };
  |];
  latin_offset = 0;
}

(* cham *)
let _cham = {
  r16 = [|
    { lo = 0xaa00; hi = 0xaa36; stride = 1 };
    { lo = 0xaa40; hi = 0xaa4d; stride = 1 };
    { lo = 0xaa50; hi = 0xaa59; stride = 1 };
    { lo = 0xaa5c; hi = 0xaa5f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* cherokee *)
let _cherokee = {
  r16 = [|
    { lo = 0x13a0; hi = 0x13f5; stride = 1 };
    { lo = 0x13f8; hi = 0x13fd; stride = 1 };
    { lo = 0xab70; hi = 0xabbf; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* chorasmian *)
let _chorasmian = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010fb0; hi = 0x010fcb; stride = 1 };
  |];
  latin_offset = 0;
}

(* common *)
let _common = {
  r16 = [|
    { lo = 0x0000; hi = 0x0040; stride = 1 };
    { lo = 0x005b; hi = 0x0060; stride = 1 };
    { lo = 0x007b; hi = 0x00a9; stride = 1 };
    { lo = 0x00ab; hi = 0x00b9; stride = 1 };
    { lo = 0x00bb; hi = 0x00bf; stride = 1 };
    { lo = 0x00d7; hi = 0x00f7; stride = 32 };
    { lo = 0x02b9; hi = 0x02df; stride = 1 };
    { lo = 0x02e5; hi = 0x02e9; stride = 1 };
    { lo = 0x02ec; hi = 0x02ff; stride = 1 };
    { lo = 0x0374; hi = 0x037e; stride = 10 };
    { lo = 0x0385; hi = 0x0387; stride = 2 };
    { lo = 0x0605; hi = 0x060c; stride = 7 };
    { lo = 0x061b; hi = 0x061f; stride = 4 };
    { lo = 0x0640; hi = 0x06dd; stride = 157 };
    { lo = 0x08e2; hi = 0x0964; stride = 130 };
    { lo = 0x0965; hi = 0x0e3f; stride = 1242 };
    { lo = 0x0fd5; hi = 0x0fd8; stride = 1 };
    { lo = 0x10fb; hi = 0x16eb; stride = 1520 };
    { lo = 0x16ec; hi = 0x16ed; stride = 1 };
    { lo = 0x1735; hi = 0x1736; stride = 1 };
    { lo = 0x1802; hi = 0x1803; stride = 1 };
    { lo = 0x1805; hi = 0x1cd3; stride = 1230 };
    { lo = 0x1ce1; hi = 0x1ce9; stride = 8 };
    { lo = 0x1cea; hi = 0x1cec; stride = 1 };
    { lo = 0x1cee; hi = 0x1cf3; stride = 1 };
    { lo = 0x1cf5; hi = 0x1cf7; stride = 1 };
    { lo = 0x1cfa; hi = 0x2000; stride = 774 };
    { lo = 0x2001; hi = 0x200b; stride = 1 };
    { lo = 0x200e; hi = 0x2064; stride = 1 };
    { lo = 0x2066; hi = 0x2070; stride = 1 };
    { lo = 0x2074; hi = 0x207e; stride = 1 };
    { lo = 0x2080; hi = 0x208e; stride = 1 };
    { lo = 0x20a0; hi = 0x20c0; stride = 1 };
    { lo = 0x2100; hi = 0x2125; stride = 1 };
    { lo = 0x2127; hi = 0x2129; stride = 1 };
    { lo = 0x212c; hi = 0x2131; stride = 1 };
    { lo = 0x2133; hi = 0x214d; stride = 1 };
    { lo = 0x214f; hi = 0x215f; stride = 1 };
    { lo = 0x2189; hi = 0x218b; stride = 1 };
    { lo = 0x2190; hi = 0x2426; stride = 1 };
    { lo = 0x2440; hi = 0x244a; stride = 1 };
    { lo = 0x2460; hi = 0x27ff; stride = 1 };
    { lo = 0x2900; hi = 0x2b73; stride = 1 };
    { lo = 0x2b76; hi = 0x2b95; stride = 1 };
    { lo = 0x2b97; hi = 0x2bff; stride = 1 };
    { lo = 0x2e00; hi = 0x2e5d; stride = 1 };
    { lo = 0x2ff0; hi = 0x2ffb; stride = 1 };
    { lo = 0x3000; hi = 0x3004; stride = 1 };
    { lo = 0x3006; hi = 0x3008; stride = 2 };
    { lo = 0x3009; hi = 0x3020; stride = 1 };
    { lo = 0x3030; hi = 0x3037; stride = 1 };
    { lo = 0x303c; hi = 0x303f; stride = 1 };
    { lo = 0x309b; hi = 0x309c; stride = 1 };
    { lo = 0x30a0; hi = 0x30fb; stride = 91 };
    { lo = 0x30fc; hi = 0x3190; stride = 148 };
    { lo = 0x3191; hi = 0x319f; stride = 1 };
    { lo = 0x31c0; hi = 0x31e3; stride = 1 };
    { lo = 0x3220; hi = 0x325f; stride = 1 };
    { lo = 0x327f; hi = 0x32cf; stride = 1 };
    { lo = 0x32ff; hi = 0x3358; stride = 89 };
    { lo = 0x3359; hi = 0x33ff; stride = 1 };
    { lo = 0x4dc0; hi = 0x4dff; stride = 1 };
    { lo = 0xa700; hi = 0xa721; stride = 1 };
    { lo = 0xa788; hi = 0xa78a; stride = 1 };
    { lo = 0xa830; hi = 0xa839; stride = 1 };
    { lo = 0xa92e; hi = 0xa9cf; stride = 161 };
    { lo = 0xab5b; hi = 0xab6a; stride = 15 };
    { lo = 0xab6b; hi = 0xfd3e; stride = 20947 };
    { lo = 0xfd3f; hi = 0xfe10; stride = 209 };
    { lo = 0xfe11; hi = 0xfe19; stride = 1 };
    { lo = 0xfe30; hi = 0xfe52; stride = 1 };
    { lo = 0xfe54; hi = 0xfe66; stride = 1 };
    { lo = 0xfe68; hi = 0xfe6b; stride = 1 };
    { lo = 0xfeff; hi = 0xff01; stride = 2 };
    { lo = 0xff02; hi = 0xff20; stride = 1 };
    { lo = 0xff3b; hi = 0xff40; stride = 1 };
    { lo = 0xff5b; hi = 0xff65; stride = 1 };
    { lo = 0xff70; hi = 0xff9e; stride = 46 };
    { lo = 0xff9f; hi = 0xffe0; stride = 65 };
    { lo = 0xffe1; hi = 0xffe6; stride = 1 };
    { lo = 0xffe8; hi = 0xffee; stride = 1 };
    { lo = 0xfff9; hi = 0xfffd; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* coptic *)
let _coptic = {
  r16 = [|
    { lo = 0x03e2; hi = 0x03ef; stride = 1 };
    { lo = 0x2c80; hi = 0x2cf3; stride = 1 };
    { lo = 0x2cf9; hi = 0x2cff; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* cuneiform *)
let _cuneiform = {
  r16 = [| |];
  r32 = [|
    { lo = 0x012000; hi = 0x012399; stride = 1 };
    { lo = 0x012400; hi = 0x01246e; stride = 1 };
    { lo = 0x012470; hi = 0x012474; stride = 1 };
    { lo = 0x012480; hi = 0x012543; stride = 1 };
  |];
  latin_offset = 0;
}

(* cypriot *)
let _cypriot = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010800; hi = 0x010805; stride = 1 };
    { lo = 0x010808; hi = 0x01080a; stride = 2 };
    { lo = 0x01080b; hi = 0x010835; stride = 1 };
    { lo = 0x010837; hi = 0x010838; stride = 1 };
    { lo = 0x01083c; hi = 0x01083f; stride = 3 };
  |];
  latin_offset = 0;
}

(* cypro_minoan *)
let _cypro_minoan = {
  r16 = [| |];
  r32 = [|
    { lo = 0x012f90; hi = 0x012ff2; stride = 1 };
  |];
  latin_offset = 0;
}

(* cyrillic *)
let _cyrillic = {
  r16 = [|
    { lo = 0x0400; hi = 0x0484; stride = 1 };
    { lo = 0x0487; hi = 0x052f; stride = 1 };
    { lo = 0x1c80; hi = 0x1c88; stride = 1 };
    { lo = 0x1d2b; hi = 0x1d78; stride = 77 };
    { lo = 0x2de0; hi = 0x2dff; stride = 1 };
    { lo = 0xa640; hi = 0xa69f; stride = 1 };
    { lo = 0xfe2e; hi = 0xfe2f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* deseret *)
let _deseret = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010400; hi = 0x01044f; stride = 1 };
  |];
  latin_offset = 0;
}

(* devanagari *)
let _devanagari = {
  r16 = [|
    { lo = 0x0900; hi = 0x0950; stride = 1 };
    { lo = 0x0955; hi = 0x0963; stride = 1 };
    { lo = 0x0966; hi = 0x097f; stride = 1 };
    { lo = 0xa8e0; hi = 0xa8ff; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* dives_akuru *)
let _dives_akuru = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011900; hi = 0x011906; stride = 1 };
    { lo = 0x011909; hi = 0x01190c; stride = 3 };
    { lo = 0x01190d; hi = 0x011913; stride = 1 };
    { lo = 0x011915; hi = 0x011916; stride = 1 };
    { lo = 0x011918; hi = 0x011935; stride = 1 };
    { lo = 0x011937; hi = 0x011938; stride = 1 };
    { lo = 0x01193b; hi = 0x011946; stride = 1 };
    { lo = 0x011950; hi = 0x011959; stride = 1 };
  |];
  latin_offset = 0;
}

(* dogra *)
let _dogra = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011800; hi = 0x01183b; stride = 1 };
  |];
  latin_offset = 0;
}

(* duployan *)
let _duployan = {
  r16 = [| |];
  r32 = [|
    { lo = 0x01bc00; hi = 0x01bc6a; stride = 1 };
    { lo = 0x01bc70; hi = 0x01bc7c; stride = 1 };
    { lo = 0x01bc80; hi = 0x01bc88; stride = 1 };
    { lo = 0x01bc90; hi = 0x01bc99; stride = 1 };
    { lo = 0x01bc9c; hi = 0x01bc9f; stride = 1 };
  |];
  latin_offset = 0;
}

(* egyptian_hieroglyphs *)
let _egyptian_hieroglyphs = {
  r16 = [| |];
  r32 = [|
    { lo = 0x013000; hi = 0x013455; stride = 1 };
  |];
  latin_offset = 0;
}

(* elbasan *)
let _elbasan = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010500; hi = 0x010527; stride = 1 };
  |];
  latin_offset = 0;
}

(* elymaic *)
let _elymaic = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010fe0; hi = 0x010ff6; stride = 1 };
  |];
  latin_offset = 0;
}

(* ethiopic *)
let _ethiopic = {
  r16 = [|
    { lo = 0x1200; hi = 0x1248; stride = 1 };
    { lo = 0x124a; hi = 0x124d; stride = 1 };
    { lo = 0x1250; hi = 0x1256; stride = 1 };
    { lo = 0x1258; hi = 0x125a; stride = 2 };
    { lo = 0x125b; hi = 0x125d; stride = 1 };
    { lo = 0x1260; hi = 0x1288; stride = 1 };
    { lo = 0x128a; hi = 0x128d; stride = 1 };
    { lo = 0x1290; hi = 0x12b0; stride = 1 };
    { lo = 0x12b2; hi = 0x12b5; stride = 1 };
    { lo = 0x12b8; hi = 0x12be; stride = 1 };
    { lo = 0x12c0; hi = 0x12c2; stride = 2 };
    { lo = 0x12c3; hi = 0x12c5; stride = 1 };
    { lo = 0x12c8; hi = 0x12d6; stride = 1 };
    { lo = 0x12d8; hi = 0x1310; stride = 1 };
    { lo = 0x1312; hi = 0x1315; stride = 1 };
    { lo = 0x1318; hi = 0x135a; stride = 1 };
    { lo = 0x135d; hi = 0x137c; stride = 1 };
    { lo = 0x1380; hi = 0x1399; stride = 1 };
    { lo = 0x2d80; hi = 0x2d96; stride = 1 };
    { lo = 0x2da0; hi = 0x2da6; stride = 1 };
    { lo = 0x2da8; hi = 0x2dae; stride = 1 };
    { lo = 0x2db0; hi = 0x2db6; stride = 1 };
    { lo = 0x2db8; hi = 0x2dbe; stride = 1 };
    { lo = 0x2dc0; hi = 0x2dc6; stride = 1 };
    { lo = 0x2dc8; hi = 0x2dce; stride = 1 };
    { lo = 0x2dd0; hi = 0x2dd6; stride = 1 };
    { lo = 0x2dd8; hi = 0x2dde; stride = 1 };
    { lo = 0xab01; hi = 0xab06; stride = 1 };
    { lo = 0xab09; hi = 0xab0e; stride = 1 };
    { lo = 0xab11; hi = 0xab16; stride = 1 };
    { lo = 0xab20; hi = 0xab26; stride = 1 };
    { lo = 0xab28; hi = 0xab2e; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* georgian *)
let _georgian = {
  r16 = [|
    { lo = 0x10a0; hi = 0x10c5; stride = 1 };
    { lo = 0x10c7; hi = 0x10cd; stride = 6 };
    { lo = 0x10d0; hi = 0x10fa; stride = 1 };
    { lo = 0x10fc; hi = 0x10ff; stride = 1 };
    { lo = 0x1c90; hi = 0x1cba; stride = 1 };
    { lo = 0x1cbd; hi = 0x1cbf; stride = 1 };
    { lo = 0x2d00; hi = 0x2d25; stride = 1 };
    { lo = 0x2d27; hi = 0x2d2d; stride = 6 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* glagolitic *)
let _glagolitic = {
  r16 = [|
    { lo = 0x2c00; hi = 0x2c5f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* gothic *)
let _gothic = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010330; hi = 0x01034a; stride = 1 };
  |];
  latin_offset = 0;
}

(* grantha *)
let _grantha = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011300; hi = 0x011303; stride = 1 };
    { lo = 0x011305; hi = 0x01130c; stride = 1 };
    { lo = 0x01130f; hi = 0x011310; stride = 1 };
    { lo = 0x011313; hi = 0x011328; stride = 1 };
    { lo = 0x01132a; hi = 0x011330; stride = 1 };
    { lo = 0x011332; hi = 0x011333; stride = 1 };
    { lo = 0x011335; hi = 0x011339; stride = 1 };
    { lo = 0x01133c; hi = 0x011344; stride = 1 };
    { lo = 0x011347; hi = 0x011348; stride = 1 };
    { lo = 0x01134b; hi = 0x01134d; stride = 1 };
    { lo = 0x011350; hi = 0x011357; stride = 7 };
    { lo = 0x01135d; hi = 0x011363; stride = 1 };
    { lo = 0x011366; hi = 0x01136c; stride = 1 };
    { lo = 0x011370; hi = 0x011374; stride = 1 };
  |];
  latin_offset = 0;
}

(* greek *)
let _greek = {
  r16 = [|
    { lo = 0x0370; hi = 0x0373; stride = 1 };
    { lo = 0x0375; hi = 0x0377; stride = 1 };
    { lo = 0x037a; hi = 0x037d; stride = 1 };
    { lo = 0x037f; hi = 0x0384; stride = 5 };
    { lo = 0x0386; hi = 0x0388; stride = 2 };
    { lo = 0x0389; hi = 0x038a; stride = 1 };
    { lo = 0x038c; hi = 0x038e; stride = 2 };
    { lo = 0x038f; hi = 0x03a1; stride = 1 };
    { lo = 0x03a3; hi = 0x03e1; stride = 1 };
    { lo = 0x03f0; hi = 0x03ff; stride = 1 };
    { lo = 0x1d26; hi = 0x1d2a; stride = 1 };
    { lo = 0x1d5d; hi = 0x1d61; stride = 1 };
    { lo = 0x1d66; hi = 0x1d6a; stride = 1 };
    { lo = 0x1dbf; hi = 0x1f00; stride = 321 };
    { lo = 0x1f01; hi = 0x1f15; stride = 1 };
    { lo = 0x1f18; hi = 0x1f1d; stride = 1 };
    { lo = 0x1f20; hi = 0x1f45; stride = 1 };
    { lo = 0x1f48; hi = 0x1f4d; stride = 1 };
    { lo = 0x1f50; hi = 0x1f57; stride = 1 };
    { lo = 0x1f59; hi = 0x1f5f; stride = 2 };
    { lo = 0x1f60; hi = 0x1f7d; stride = 1 };
    { lo = 0x1f80; hi = 0x1fb4; stride = 1 };
    { lo = 0x1fb6; hi = 0x1fc4; stride = 1 };
    { lo = 0x1fc6; hi = 0x1fd3; stride = 1 };
    { lo = 0x1fd6; hi = 0x1fdb; stride = 1 };
    { lo = 0x1fdd; hi = 0x1fef; stride = 1 };
    { lo = 0x1ff2; hi = 0x1ff4; stride = 1 };
    { lo = 0x1ff6; hi = 0x1ffe; stride = 1 };
    { lo = 0x2126; hi = 0xab65; stride = 35391 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* gujarati *)
let _gujarati = {
  r16 = [|
    { lo = 0x0a81; hi = 0x0a83; stride = 1 };
    { lo = 0x0a85; hi = 0x0a8d; stride = 1 };
    { lo = 0x0a8f; hi = 0x0a91; stride = 1 };
    { lo = 0x0a93; hi = 0x0aa8; stride = 1 };
    { lo = 0x0aaa; hi = 0x0ab0; stride = 1 };
    { lo = 0x0ab2; hi = 0x0ab3; stride = 1 };
    { lo = 0x0ab5; hi = 0x0ab9; stride = 1 };
    { lo = 0x0abc; hi = 0x0ac5; stride = 1 };
    { lo = 0x0ac7; hi = 0x0ac9; stride = 1 };
    { lo = 0x0acb; hi = 0x0acd; stride = 1 };
    { lo = 0x0ad0; hi = 0x0ae0; stride = 16 };
    { lo = 0x0ae1; hi = 0x0ae3; stride = 1 };
    { lo = 0x0ae6; hi = 0x0af1; stride = 1 };
    { lo = 0x0af9; hi = 0x0aff; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* gunjala_gondi *)
let _gunjala_gondi = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011d60; hi = 0x011d65; stride = 1 };
    { lo = 0x011d67; hi = 0x011d68; stride = 1 };
    { lo = 0x011d6a; hi = 0x011d8e; stride = 1 };
    { lo = 0x011d90; hi = 0x011d91; stride = 1 };
    { lo = 0x011d93; hi = 0x011d98; stride = 1 };
    { lo = 0x011da0; hi = 0x011da9; stride = 1 };
  |];
  latin_offset = 0;
}

(* gurmukhi *)
let _gurmukhi = {
  r16 = [|
    { lo = 0x0a01; hi = 0x0a03; stride = 1 };
    { lo = 0x0a05; hi = 0x0a0a; stride = 1 };
    { lo = 0x0a0f; hi = 0x0a10; stride = 1 };
    { lo = 0x0a13; hi = 0x0a28; stride = 1 };
    { lo = 0x0a2a; hi = 0x0a30; stride = 1 };
    { lo = 0x0a32; hi = 0x0a33; stride = 1 };
    { lo = 0x0a35; hi = 0x0a36; stride = 1 };
    { lo = 0x0a38; hi = 0x0a39; stride = 1 };
    { lo = 0x0a3c; hi = 0x0a3e; stride = 2 };
    { lo = 0x0a3f; hi = 0x0a42; stride = 1 };
    { lo = 0x0a47; hi = 0x0a48; stride = 1 };
    { lo = 0x0a4b; hi = 0x0a4d; stride = 1 };
    { lo = 0x0a51; hi = 0x0a59; stride = 8 };
    { lo = 0x0a5a; hi = 0x0a5c; stride = 1 };
    { lo = 0x0a5e; hi = 0x0a66; stride = 8 };
    { lo = 0x0a67; hi = 0x0a76; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* han *)
let _han = {
  r16 = [|
    { lo = 0x2e80; hi = 0x2e99; stride = 1 };
    { lo = 0x2e9b; hi = 0x2ef3; stride = 1 };
    { lo = 0x2f00; hi = 0x2fd5; stride = 1 };
    { lo = 0x3005; hi = 0x3007; stride = 2 };
    { lo = 0x3021; hi = 0x3029; stride = 1 };
    { lo = 0x3038; hi = 0x303b; stride = 1 };
    { lo = 0x3400; hi = 0x4dbf; stride = 1 };
    { lo = 0x4e00; hi = 0x9fff; stride = 1 };
    { lo = 0xf900; hi = 0xfa6d; stride = 1 };
    { lo = 0xfa70; hi = 0xfad9; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* hangul *)
let _hangul = {
  r16 = [|
    { lo = 0x1100; hi = 0x11ff; stride = 1 };
    { lo = 0x302e; hi = 0x302f; stride = 1 };
    { lo = 0x3131; hi = 0x318e; stride = 1 };
    { lo = 0x3200; hi = 0x321e; stride = 1 };
    { lo = 0x3260; hi = 0x327e; stride = 1 };
    { lo = 0xa960; hi = 0xa97c; stride = 1 };
    { lo = 0xac00; hi = 0xd7a3; stride = 1 };
    { lo = 0xd7b0; hi = 0xd7c6; stride = 1 };
    { lo = 0xd7cb; hi = 0xd7fb; stride = 1 };
    { lo = 0xffa0; hi = 0xffbe; stride = 1 };
    { lo = 0xffc2; hi = 0xffc7; stride = 1 };
    { lo = 0xffca; hi = 0xffcf; stride = 1 };
    { lo = 0xffd2; hi = 0xffd7; stride = 1 };
    { lo = 0xffda; hi = 0xffdc; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* hanifi_rohingya *)
let _hanifi_rohingya = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010d00; hi = 0x010d27; stride = 1 };
    { lo = 0x010d30; hi = 0x010d39; stride = 1 };
  |];
  latin_offset = 0;
}

(* hanunoo *)
let _hanunoo = {
  r16 = [|
    { lo = 0x1720; hi = 0x1734; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* hatran *)
let _hatran = {
  r16 = [| |];
  r32 = [|
    { lo = 0x0108e0; hi = 0x0108f2; stride = 1 };
    { lo = 0x0108f4; hi = 0x0108f5; stride = 1 };
    { lo = 0x0108fb; hi = 0x0108ff; stride = 1 };
  |];
  latin_offset = 0;
}

(* hebrew *)
let _hebrew = {
  r16 = [|
    { lo = 0x0591; hi = 0x05c7; stride = 1 };
    { lo = 0x05d0; hi = 0x05ea; stride = 1 };
    { lo = 0x05ef; hi = 0x05f4; stride = 1 };
    { lo = 0xfb1d; hi = 0xfb36; stride = 1 };
    { lo = 0xfb38; hi = 0xfb3c; stride = 1 };
    { lo = 0xfb3e; hi = 0xfb40; stride = 2 };
    { lo = 0xfb41; hi = 0xfb43; stride = 2 };
    { lo = 0xfb44; hi = 0xfb46; stride = 2 };
    { lo = 0xfb47; hi = 0xfb4f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* hiragana *)
let _hiragana = {
  r16 = [|
    { lo = 0x3041; hi = 0x3096; stride = 1 };
    { lo = 0x309d; hi = 0x309f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* imperial_aramaic *)
let _imperial_aramaic = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010840; hi = 0x010855; stride = 1 };
    { lo = 0x010857; hi = 0x01085f; stride = 1 };
  |];
  latin_offset = 0;
}

(* inherited *)
let _inherited = {
  r16 = [|
    { lo = 0x0300; hi = 0x036f; stride = 1 };
    { lo = 0x0485; hi = 0x0486; stride = 1 };
    { lo = 0x064b; hi = 0x0655; stride = 1 };
    { lo = 0x0670; hi = 0x0951; stride = 737 };
    { lo = 0x0952; hi = 0x0954; stride = 1 };
    { lo = 0x1ab0; hi = 0x1ace; stride = 1 };
    { lo = 0x1cd0; hi = 0x1cd2; stride = 1 };
    { lo = 0x1cd4; hi = 0x1ce0; stride = 1 };
    { lo = 0x1ce2; hi = 0x1ce8; stride = 1 };
    { lo = 0x1ced; hi = 0x1cf4; stride = 7 };
    { lo = 0x1cf8; hi = 0x1cf9; stride = 1 };
    { lo = 0x1dc0; hi = 0x1dff; stride = 1 };
    { lo = 0x200c; hi = 0x200d; stride = 1 };
    { lo = 0x20d0; hi = 0x20f0; stride = 1 };
    { lo = 0x302a; hi = 0x302d; stride = 1 };
    { lo = 0x3099; hi = 0x309a; stride = 1 };
    { lo = 0xfe00; hi = 0xfe0f; stride = 1 };
    { lo = 0xfe20; hi = 0xfe2d; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* inscriptional_pahlavi *)
let _inscriptional_pahlavi = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010b60; hi = 0x010b72; stride = 1 };
    { lo = 0x010b78; hi = 0x010b7f; stride = 1 };
  |];
  latin_offset = 0;
}

(* inscriptional_parthian *)
let _inscriptional_parthian = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010b40; hi = 0x010b55; stride = 1 };
    { lo = 0x010b58; hi = 0x010b5f; stride = 1 };
  |];
  latin_offset = 0;
}

(* javanese *)
let _javanese = {
  r16 = [|
    { lo = 0xa980; hi = 0xa9cd; stride = 1 };
    { lo = 0xa9d0; hi = 0xa9d9; stride = 1 };
    { lo = 0xa9de; hi = 0xa9df; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* kaithi *)
let _kaithi = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011080; hi = 0x0110c2; stride = 1 };
    { lo = 0x0110cd; hi = 0x0110cd; stride = 1 };
  |];
  latin_offset = 0;
}

(* kannada *)
let _kannada = {
  r16 = [|
    { lo = 0x0c80; hi = 0x0c8c; stride = 1 };
    { lo = 0x0c8e; hi = 0x0c90; stride = 1 };
    { lo = 0x0c92; hi = 0x0ca8; stride = 1 };
    { lo = 0x0caa; hi = 0x0cb3; stride = 1 };
    { lo = 0x0cb5; hi = 0x0cb9; stride = 1 };
    { lo = 0x0cbc; hi = 0x0cc4; stride = 1 };
    { lo = 0x0cc6; hi = 0x0cc8; stride = 1 };
    { lo = 0x0cca; hi = 0x0ccd; stride = 1 };
    { lo = 0x0cd5; hi = 0x0cd6; stride = 1 };
    { lo = 0x0cdd; hi = 0x0cde; stride = 1 };
    { lo = 0x0ce0; hi = 0x0ce3; stride = 1 };
    { lo = 0x0ce6; hi = 0x0cef; stride = 1 };
    { lo = 0x0cf1; hi = 0x0cf3; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* katakana *)
let _katakana = {
  r16 = [|
    { lo = 0x30a1; hi = 0x30fa; stride = 1 };
    { lo = 0x30fd; hi = 0x30ff; stride = 1 };
    { lo = 0x31f0; hi = 0x31ff; stride = 1 };
    { lo = 0x32d0; hi = 0x32fe; stride = 1 };
    { lo = 0x3300; hi = 0x3357; stride = 1 };
    { lo = 0xff66; hi = 0xff6f; stride = 1 };
    { lo = 0xff71; hi = 0xff9d; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* kawi *)
let _kawi = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011f00; hi = 0x011f10; stride = 1 };
    { lo = 0x011f12; hi = 0x011f3a; stride = 1 };
    { lo = 0x011f3e; hi = 0x011f59; stride = 1 };
  |];
  latin_offset = 0;
}

(* kayah_li *)
let _kayah_li = {
  r16 = [|
    { lo = 0xa900; hi = 0xa92d; stride = 1 };
    { lo = 0xa92f; hi = 0xa92f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* kharoshthi *)
let _kharoshthi = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010a00; hi = 0x010a03; stride = 1 };
    { lo = 0x010a05; hi = 0x010a06; stride = 1 };
    { lo = 0x010a0c; hi = 0x010a13; stride = 1 };
    { lo = 0x010a15; hi = 0x010a17; stride = 1 };
    { lo = 0x010a19; hi = 0x010a35; stride = 1 };
    { lo = 0x010a38; hi = 0x010a3a; stride = 1 };
    { lo = 0x010a3f; hi = 0x010a48; stride = 1 };
    { lo = 0x010a50; hi = 0x010a58; stride = 1 };
  |];
  latin_offset = 0;
}

(* khitan_small_script *)
let _khitan_small_script = {
  r16 = [| |];
  r32 = [|
    { lo = 0x016fe4; hi = 0x018b00; stride = 6940 };
    { lo = 0x018b01; hi = 0x018cd5; stride = 1 };
  |];
  latin_offset = 0;
}

(* khmer *)
let _khmer = {
  r16 = [|
    { lo = 0x1780; hi = 0x17dd; stride = 1 };
    { lo = 0x17e0; hi = 0x17e9; stride = 1 };
    { lo = 0x17f0; hi = 0x17f9; stride = 1 };
    { lo = 0x19e0; hi = 0x19ff; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* khojki *)
let _khojki = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011200; hi = 0x011211; stride = 1 };
    { lo = 0x011213; hi = 0x011241; stride = 1 };
  |];
  latin_offset = 0;
}

(* khudawadi *)
let _khudawadi = {
  r16 = [| |];
  r32 = [|
    { lo = 0x0112b0; hi = 0x0112ea; stride = 1 };
    { lo = 0x0112f0; hi = 0x0112f9; stride = 1 };
  |];
  latin_offset = 0;
}

(* lao *)
let _lao = {
  r16 = [|
    { lo = 0x0e81; hi = 0x0e82; stride = 1 };
    { lo = 0x0e84; hi = 0x0e86; stride = 2 };
    { lo = 0x0e87; hi = 0x0e8a; stride = 1 };
    { lo = 0x0e8c; hi = 0x0ea3; stride = 1 };
    { lo = 0x0ea5; hi = 0x0ea7; stride = 2 };
    { lo = 0x0ea8; hi = 0x0ebd; stride = 1 };
    { lo = 0x0ec0; hi = 0x0ec4; stride = 1 };
    { lo = 0x0ec6; hi = 0x0ec8; stride = 2 };
    { lo = 0x0ec9; hi = 0x0ece; stride = 1 };
    { lo = 0x0ed0; hi = 0x0ed9; stride = 1 };
    { lo = 0x0edc; hi = 0x0edf; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* latin *)
let _latin = {
  r16 = [|
    { lo = 0x0041; hi = 0x005a; stride = 1 };
    { lo = 0x0061; hi = 0x007a; stride = 1 };
    { lo = 0x00aa; hi = 0x00ba; stride = 16 };
    { lo = 0x00c0; hi = 0x00d6; stride = 1 };
    { lo = 0x00d8; hi = 0x00f6; stride = 1 };
    { lo = 0x00f8; hi = 0x02b8; stride = 1 };
    { lo = 0x02e0; hi = 0x02e4; stride = 1 };
    { lo = 0x1d00; hi = 0x1d25; stride = 1 };
    { lo = 0x1d2c; hi = 0x1d5c; stride = 1 };
    { lo = 0x1d62; hi = 0x1d65; stride = 1 };
    { lo = 0x1d6b; hi = 0x1d77; stride = 1 };
    { lo = 0x1d79; hi = 0x1dbe; stride = 1 };
    { lo = 0x1e00; hi = 0x1eff; stride = 1 };
    { lo = 0x2071; hi = 0x207f; stride = 14 };
    { lo = 0x2090; hi = 0x209c; stride = 1 };
    { lo = 0x212a; hi = 0x212b; stride = 1 };
    { lo = 0x2132; hi = 0x214e; stride = 28 };
    { lo = 0x2160; hi = 0x2188; stride = 1 };
    { lo = 0x2c60; hi = 0x2c7f; stride = 1 };
    { lo = 0xa722; hi = 0xa787; stride = 1 };
    { lo = 0xa78b; hi = 0xa7ca; stride = 1 };
    { lo = 0xa7d0; hi = 0xa7d1; stride = 1 };
    { lo = 0xa7d3; hi = 0xa7d5; stride = 2 };
    { lo = 0xa7d6; hi = 0xa7d9; stride = 1 };
    { lo = 0xa7f2; hi = 0xa7ff; stride = 1 };
    { lo = 0xab30; hi = 0xab5a; stride = 1 };
    { lo = 0xab5c; hi = 0xab64; stride = 1 };
    { lo = 0xab66; hi = 0xab69; stride = 1 };
    { lo = 0xfb00; hi = 0xfb06; stride = 1 };
    { lo = 0xff21; hi = 0xff3a; stride = 1 };
    { lo = 0xff41; hi = 0xff5a; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* lepcha *)
let _lepcha = {
  r16 = [|
    { lo = 0x1c00; hi = 0x1c37; stride = 1 };
    { lo = 0x1c3b; hi = 0x1c49; stride = 1 };
    { lo = 0x1c4d; hi = 0x1c4f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* limbu *)
let _limbu = {
  r16 = [|
    { lo = 0x1900; hi = 0x191e; stride = 1 };
    { lo = 0x1920; hi = 0x192b; stride = 1 };
    { lo = 0x1930; hi = 0x193b; stride = 1 };
    { lo = 0x1940; hi = 0x1944; stride = 4 };
    { lo = 0x1945; hi = 0x194f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* linear_a *)
let _linear_a = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010600; hi = 0x010736; stride = 1 };
    { lo = 0x010740; hi = 0x010755; stride = 1 };
    { lo = 0x010760; hi = 0x010767; stride = 1 };
  |];
  latin_offset = 0;
}

(* linear_b *)
let _linear_b = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010000; hi = 0x01000b; stride = 1 };
    { lo = 0x01000d; hi = 0x010026; stride = 1 };
    { lo = 0x010028; hi = 0x01003a; stride = 1 };
    { lo = 0x01003c; hi = 0x01003d; stride = 1 };
    { lo = 0x01003f; hi = 0x01004d; stride = 1 };
    { lo = 0x010050; hi = 0x01005d; stride = 1 };
    { lo = 0x010080; hi = 0x0100fa; stride = 1 };
  |];
  latin_offset = 0;
}

(* lisu *)
let _lisu = {
  r16 = [|
    { lo = 0xa4d0; hi = 0xa4ff; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* lycian *)
let _lycian = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010280; hi = 0x01029c; stride = 1 };
  |];
  latin_offset = 0;
}

(* lydian *)
let _lydian = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010920; hi = 0x010939; stride = 1 };
    { lo = 0x01093f; hi = 0x01093f; stride = 1 };
  |];
  latin_offset = 0;
}

(* mahajani *)
let _mahajani = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011150; hi = 0x011176; stride = 1 };
  |];
  latin_offset = 0;
}

(* makasar *)
let _makasar = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011ee0; hi = 0x011ef8; stride = 1 };
  |];
  latin_offset = 0;
}

(* malayalam *)
let _malayalam = {
  r16 = [|
    { lo = 0x0d00; hi = 0x0d0c; stride = 1 };
    { lo = 0x0d0e; hi = 0x0d10; stride = 1 };
    { lo = 0x0d12; hi = 0x0d44; stride = 1 };
    { lo = 0x0d46; hi = 0x0d48; stride = 1 };
    { lo = 0x0d4a; hi = 0x0d4f; stride = 1 };
    { lo = 0x0d54; hi = 0x0d63; stride = 1 };
    { lo = 0x0d66; hi = 0x0d7f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* mandaic *)
let _mandaic = {
  r16 = [|
    { lo = 0x0840; hi = 0x085b; stride = 1 };
    { lo = 0x085e; hi = 0x085e; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* manichaean *)
let _manichaean = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010ac0; hi = 0x010ae6; stride = 1 };
    { lo = 0x010aeb; hi = 0x010af6; stride = 1 };
  |];
  latin_offset = 0;
}

(* marchen *)
let _marchen = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011c70; hi = 0x011c8f; stride = 1 };
    { lo = 0x011c92; hi = 0x011ca7; stride = 1 };
    { lo = 0x011ca9; hi = 0x011cb6; stride = 1 };
  |];
  latin_offset = 0;
}

(* masaram_gondi *)
let _masaram_gondi = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011d00; hi = 0x011d06; stride = 1 };
    { lo = 0x011d08; hi = 0x011d09; stride = 1 };
    { lo = 0x011d0b; hi = 0x011d36; stride = 1 };
    { lo = 0x011d3a; hi = 0x011d3c; stride = 2 };
    { lo = 0x011d3d; hi = 0x011d3f; stride = 2 };
    { lo = 0x011d40; hi = 0x011d47; stride = 1 };
    { lo = 0x011d50; hi = 0x011d59; stride = 1 };
  |];
  latin_offset = 0;
}

(* medefaidrin *)
let _medefaidrin = {
  r16 = [| |];
  r32 = [|
    { lo = 0x016e40; hi = 0x016e9a; stride = 1 };
  |];
  latin_offset = 0;
}

(* meetei_mayek *)
let _meetei_mayek = {
  r16 = [|
    { lo = 0xaae0; hi = 0xaaf6; stride = 1 };
    { lo = 0xabc0; hi = 0xabed; stride = 1 };
    { lo = 0xabf0; hi = 0xabf9; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* mende_kikakui *)
let _mende_kikakui = {
  r16 = [| |];
  r32 = [|
    { lo = 0x01e800; hi = 0x01e8c4; stride = 1 };
    { lo = 0x01e8c7; hi = 0x01e8d6; stride = 1 };
  |];
  latin_offset = 0;
}

(* meroitic_cursive *)
let _meroitic_cursive = {
  r16 = [| |];
  r32 = [|
    { lo = 0x0109a0; hi = 0x0109b7; stride = 1 };
    { lo = 0x0109bc; hi = 0x0109cf; stride = 1 };
    { lo = 0x0109d2; hi = 0x0109ff; stride = 1 };
  |];
  latin_offset = 0;
}

(* meroitic_hieroglyphs *)
let _meroitic_hieroglyphs = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010980; hi = 0x01099f; stride = 1 };
  |];
  latin_offset = 0;
}

(* miao *)
let _miao = {
  r16 = [| |];
  r32 = [|
    { lo = 0x016f00; hi = 0x016f4a; stride = 1 };
    { lo = 0x016f4f; hi = 0x016f87; stride = 1 };
    { lo = 0x016f8f; hi = 0x016f9f; stride = 1 };
  |];
  latin_offset = 0;
}

(* modi *)
let _modi = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011600; hi = 0x011644; stride = 1 };
    { lo = 0x011650; hi = 0x011659; stride = 1 };
  |];
  latin_offset = 0;
}

(* mongolian *)
let _mongolian = {
  r16 = [|
    { lo = 0x1800; hi = 0x1801; stride = 1 };
    { lo = 0x1804; hi = 0x1806; stride = 2 };
    { lo = 0x1807; hi = 0x1819; stride = 1 };
    { lo = 0x1820; hi = 0x1878; stride = 1 };
    { lo = 0x1880; hi = 0x18aa; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* mro *)
let _mro = {
  r16 = [| |];
  r32 = [|
    { lo = 0x016a40; hi = 0x016a5e; stride = 1 };
    { lo = 0x016a60; hi = 0x016a69; stride = 1 };
    { lo = 0x016a6e; hi = 0x016a6f; stride = 1 };
  |];
  latin_offset = 0;
}

(* multani *)
let _multani = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011280; hi = 0x011286; stride = 1 };
    { lo = 0x011288; hi = 0x01128a; stride = 2 };
    { lo = 0x01128b; hi = 0x01128d; stride = 1 };
    { lo = 0x01128f; hi = 0x01129d; stride = 1 };
    { lo = 0x01129f; hi = 0x0112a9; stride = 1 };
  |];
  latin_offset = 0;
}

(* myanmar *)
let _myanmar = {
  r16 = [|
    { lo = 0x1000; hi = 0x109f; stride = 1 };
    { lo = 0xa9e0; hi = 0xa9fe; stride = 1 };
    { lo = 0xaa60; hi = 0xaa7f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* nabataean *)
let _nabataean = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010880; hi = 0x01089e; stride = 1 };
    { lo = 0x0108a7; hi = 0x0108af; stride = 1 };
  |];
  latin_offset = 0;
}

(* nag_mundari *)
let _nag_mundari = {
  r16 = [| |];
  r32 = [|
    { lo = 0x01e4d0; hi = 0x01e4f9; stride = 1 };
  |];
  latin_offset = 0;
}

(* nandinagari *)
let _nandinagari = {
  r16 = [| |];
  r32 = [|
    { lo = 0x0119a0; hi = 0x0119a7; stride = 1 };
    { lo = 0x0119aa; hi = 0x0119d7; stride = 1 };
    { lo = 0x0119da; hi = 0x0119e4; stride = 1 };
  |];
  latin_offset = 0;
}

(* new_tai_lue *)
let _new_tai_lue = {
  r16 = [|
    { lo = 0x1980; hi = 0x19ab; stride = 1 };
    { lo = 0x19b0; hi = 0x19c9; stride = 1 };
    { lo = 0x19d0; hi = 0x19da; stride = 1 };
    { lo = 0x19de; hi = 0x19df; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* newa *)
let _newa = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011400; hi = 0x01145b; stride = 1 };
    { lo = 0x01145d; hi = 0x011461; stride = 1 };
  |];
  latin_offset = 0;
}

(* nko *)
let _nko = {
  r16 = [|
    { lo = 0x07c0; hi = 0x07fa; stride = 1 };
    { lo = 0x07fd; hi = 0x07ff; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* nushu *)
let _nushu = {
  r16 = [| |];
  r32 = [|
    { lo = 0x016fe1; hi = 0x01b170; stride = 16783 };
    { lo = 0x01b171; hi = 0x01b2fb; stride = 1 };
  |];
  latin_offset = 0;
}

(* nyiakeng_puachue_hmong *)
let _nyiakeng_puachue_hmong = {
  r16 = [| |];
  r32 = [|
    { lo = 0x01e100; hi = 0x01e12c; stride = 1 };
    { lo = 0x01e130; hi = 0x01e13d; stride = 1 };
    { lo = 0x01e140; hi = 0x01e149; stride = 1 };
    { lo = 0x01e14e; hi = 0x01e14f; stride = 1 };
  |];
  latin_offset = 0;
}

(* ogham *)
let _ogham = {
  r16 = [|
    { lo = 0x1680; hi = 0x169c; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* ol_chiki *)
let _ol_chiki = {
  r16 = [|
    { lo = 0x1c50; hi = 0x1c7f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* old_hungarian *)
let _old_hungarian = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010c80; hi = 0x010cb2; stride = 1 };
    { lo = 0x010cc0; hi = 0x010cf2; stride = 1 };
    { lo = 0x010cfa; hi = 0x010cff; stride = 1 };
  |];
  latin_offset = 0;
}

(* old_italic *)
let _old_italic = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010300; hi = 0x010323; stride = 1 };
    { lo = 0x01032d; hi = 0x01032f; stride = 1 };
  |];
  latin_offset = 0;
}

(* old_north_arabian *)
let _old_north_arabian = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010a80; hi = 0x010a9f; stride = 1 };
  |];
  latin_offset = 0;
}

(* old_permic *)
let _old_permic = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010350; hi = 0x01037a; stride = 1 };
  |];
  latin_offset = 0;
}

(* old_persian *)
let _old_persian = {
  r16 = [| |];
  r32 = [|
    { lo = 0x0103a0; hi = 0x0103c3; stride = 1 };
    { lo = 0x0103c8; hi = 0x0103d5; stride = 1 };
  |];
  latin_offset = 0;
}

(* old_sogdian *)
let _old_sogdian = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010f00; hi = 0x010f27; stride = 1 };
  |];
  latin_offset = 0;
}

(* old_south_arabian *)
let _old_south_arabian = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010a60; hi = 0x010a7f; stride = 1 };
  |];
  latin_offset = 0;
}

(* old_turkic *)
let _old_turkic = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010c00; hi = 0x010c48; stride = 1 };
  |];
  latin_offset = 0;
}

(* old_uyghur *)
let _old_uyghur = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010f70; hi = 0x010f89; stride = 1 };
  |];
  latin_offset = 0;
}

(* oriya *)
let _oriya = {
  r16 = [|
    { lo = 0x0b01; hi = 0x0b03; stride = 1 };
    { lo = 0x0b05; hi = 0x0b0c; stride = 1 };
    { lo = 0x0b0f; hi = 0x0b10; stride = 1 };
    { lo = 0x0b13; hi = 0x0b28; stride = 1 };
    { lo = 0x0b2a; hi = 0x0b30; stride = 1 };
    { lo = 0x0b32; hi = 0x0b33; stride = 1 };
    { lo = 0x0b35; hi = 0x0b39; stride = 1 };
    { lo = 0x0b3c; hi = 0x0b44; stride = 1 };
    { lo = 0x0b47; hi = 0x0b48; stride = 1 };
    { lo = 0x0b4b; hi = 0x0b4d; stride = 1 };
    { lo = 0x0b55; hi = 0x0b57; stride = 1 };
    { lo = 0x0b5c; hi = 0x0b5d; stride = 1 };
    { lo = 0x0b5f; hi = 0x0b63; stride = 1 };
    { lo = 0x0b66; hi = 0x0b77; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* osage *)
let _osage = {
  r16 = [| |];
  r32 = [|
    { lo = 0x0104b0; hi = 0x0104d3; stride = 1 };
    { lo = 0x0104d8; hi = 0x0104fb; stride = 1 };
  |];
  latin_offset = 0;
}

(* osmanya *)
let _osmanya = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010480; hi = 0x01049d; stride = 1 };
    { lo = 0x0104a0; hi = 0x0104a9; stride = 1 };
  |];
  latin_offset = 0;
}

(* pahawh_hmong *)
let _pahawh_hmong = {
  r16 = [| |];
  r32 = [|
    { lo = 0x016b00; hi = 0x016b45; stride = 1 };
    { lo = 0x016b50; hi = 0x016b59; stride = 1 };
    { lo = 0x016b5b; hi = 0x016b61; stride = 1 };
    { lo = 0x016b63; hi = 0x016b77; stride = 1 };
    { lo = 0x016b7d; hi = 0x016b8f; stride = 1 };
  |];
  latin_offset = 0;
}

(* palmyrene *)
let _palmyrene = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010860; hi = 0x01087f; stride = 1 };
  |];
  latin_offset = 0;
}

(* pau_cin_hau *)
let _pau_cin_hau = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011ac0; hi = 0x011af8; stride = 1 };
  |];
  latin_offset = 0;
}

(* phags_pa *)
let _phags_pa = {
  r16 = [|
    { lo = 0xa840; hi = 0xa877; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* phoenician *)
let _phoenician = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010900; hi = 0x01091b; stride = 1 };
    { lo = 0x01091f; hi = 0x01091f; stride = 1 };
  |];
  latin_offset = 0;
}

(* psalter_pahlavi *)
let _psalter_pahlavi = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010b80; hi = 0x010b91; stride = 1 };
    { lo = 0x010b99; hi = 0x010b9c; stride = 1 };
    { lo = 0x010ba9; hi = 0x010baf; stride = 1 };
  |];
  latin_offset = 0;
}

(* rejang *)
let _rejang = {
  r16 = [|
    { lo = 0xa930; hi = 0xa953; stride = 1 };
    { lo = 0xa95f; hi = 0xa95f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* runic *)
let _runic = {
  r16 = [|
    { lo = 0x16a0; hi = 0x16ea; stride = 1 };
    { lo = 0x16ee; hi = 0x16f8; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* samaritan *)
let _samaritan = {
  r16 = [|
    { lo = 0x0800; hi = 0x082d; stride = 1 };
    { lo = 0x0830; hi = 0x083e; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* saurashtra *)
let _saurashtra = {
  r16 = [|
    { lo = 0xa880; hi = 0xa8c5; stride = 1 };
    { lo = 0xa8ce; hi = 0xa8d9; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* sharada *)
let _sharada = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011180; hi = 0x0111df; stride = 1 };
  |];
  latin_offset = 0;
}

(* shavian *)
let _shavian = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010450; hi = 0x01047f; stride = 1 };
  |];
  latin_offset = 0;
}

(* siddham *)
let _siddham = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011580; hi = 0x0115b5; stride = 1 };
    { lo = 0x0115b8; hi = 0x0115dd; stride = 1 };
  |];
  latin_offset = 0;
}

(* signwriting *)
let _signwriting = {
  r16 = [| |];
  r32 = [|
    { lo = 0x01d800; hi = 0x01da8b; stride = 1 };
    { lo = 0x01da9b; hi = 0x01da9f; stride = 1 };
    { lo = 0x01daa1; hi = 0x01daaf; stride = 1 };
  |];
  latin_offset = 0;
}

(* sinhala *)
let _sinhala = {
  r16 = [|
    { lo = 0x0d81; hi = 0x0d83; stride = 1 };
    { lo = 0x0d85; hi = 0x0d96; stride = 1 };
    { lo = 0x0d9a; hi = 0x0db1; stride = 1 };
    { lo = 0x0db3; hi = 0x0dbb; stride = 1 };
    { lo = 0x0dbd; hi = 0x0dc0; stride = 3 };
    { lo = 0x0dc1; hi = 0x0dc6; stride = 1 };
    { lo = 0x0dca; hi = 0x0dcf; stride = 5 };
    { lo = 0x0dd0; hi = 0x0dd4; stride = 1 };
    { lo = 0x0dd6; hi = 0x0dd8; stride = 2 };
    { lo = 0x0dd9; hi = 0x0ddf; stride = 1 };
    { lo = 0x0de6; hi = 0x0def; stride = 1 };
    { lo = 0x0df2; hi = 0x0df4; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* sogdian *)
let _sogdian = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010f30; hi = 0x010f59; stride = 1 };
  |];
  latin_offset = 0;
}

(* sora_sompeng *)
let _sora_sompeng = {
  r16 = [| |];
  r32 = [|
    { lo = 0x0110d0; hi = 0x0110e8; stride = 1 };
    { lo = 0x0110f0; hi = 0x0110f9; stride = 1 };
  |];
  latin_offset = 0;
}

(* soyombo *)
let _soyombo = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011a50; hi = 0x011aa2; stride = 1 };
  |];
  latin_offset = 0;
}

(* sundanese *)
let _sundanese = {
  r16 = [|
    { lo = 0x1b80; hi = 0x1bbf; stride = 1 };
    { lo = 0x1cc0; hi = 0x1cc7; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* syloti_nagri *)
let _syloti_nagri = {
  r16 = [|
    { lo = 0xa800; hi = 0xa82c; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* syriac *)
let _syriac = {
  r16 = [|
    { lo = 0x0700; hi = 0x070d; stride = 1 };
    { lo = 0x070f; hi = 0x074a; stride = 1 };
    { lo = 0x074d; hi = 0x074f; stride = 1 };
    { lo = 0x0860; hi = 0x086a; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* tagalog *)
let _tagalog = {
  r16 = [|
    { lo = 0x1700; hi = 0x1715; stride = 1 };
    { lo = 0x171f; hi = 0x171f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* tagbanwa *)
let _tagbanwa = {
  r16 = [|
    { lo = 0x1760; hi = 0x176c; stride = 1 };
    { lo = 0x176e; hi = 0x1770; stride = 1 };
    { lo = 0x1772; hi = 0x1773; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* tai_le *)
let _tai_le = {
  r16 = [|
    { lo = 0x1950; hi = 0x196d; stride = 1 };
    { lo = 0x1970; hi = 0x1974; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* tai_tham *)
let _tai_tham = {
  r16 = [|
    { lo = 0x1a20; hi = 0x1a5e; stride = 1 };
    { lo = 0x1a60; hi = 0x1a7c; stride = 1 };
    { lo = 0x1a7f; hi = 0x1a89; stride = 1 };
    { lo = 0x1a90; hi = 0x1a99; stride = 1 };
    { lo = 0x1aa0; hi = 0x1aad; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* tai_viet *)
let _tai_viet = {
  r16 = [|
    { lo = 0xaa80; hi = 0xaac2; stride = 1 };
    { lo = 0xaadb; hi = 0xaadf; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* takri *)
let _takri = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011680; hi = 0x0116b9; stride = 1 };
    { lo = 0x0116c0; hi = 0x0116c9; stride = 1 };
  |];
  latin_offset = 0;
}

(* tamil *)
let _tamil = {
  r16 = [|
    { lo = 0x0b82; hi = 0x0b83; stride = 1 };
    { lo = 0x0b85; hi = 0x0b8a; stride = 1 };
    { lo = 0x0b8e; hi = 0x0b90; stride = 1 };
    { lo = 0x0b92; hi = 0x0b95; stride = 1 };
    { lo = 0x0b99; hi = 0x0b9a; stride = 1 };
    { lo = 0x0b9c; hi = 0x0b9e; stride = 2 };
    { lo = 0x0b9f; hi = 0x0ba3; stride = 4 };
    { lo = 0x0ba4; hi = 0x0ba8; stride = 4 };
    { lo = 0x0ba9; hi = 0x0baa; stride = 1 };
    { lo = 0x0bae; hi = 0x0bb9; stride = 1 };
    { lo = 0x0bbe; hi = 0x0bc2; stride = 1 };
    { lo = 0x0bc6; hi = 0x0bc8; stride = 1 };
    { lo = 0x0bca; hi = 0x0bcd; stride = 1 };
    { lo = 0x0bd0; hi = 0x0bd7; stride = 7 };
    { lo = 0x0be6; hi = 0x0bfa; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* tangsa *)
let _tangsa = {
  r16 = [| |];
  r32 = [|
    { lo = 0x016a70; hi = 0x016abe; stride = 1 };
    { lo = 0x016ac0; hi = 0x016ac9; stride = 1 };
  |];
  latin_offset = 0;
}

(* tangut *)
let _tangut = {
  r16 = [| |];
  r32 = [|
    { lo = 0x016fe0; hi = 0x017000; stride = 32 };
    { lo = 0x017001; hi = 0x0187f7; stride = 1 };
    { lo = 0x018800; hi = 0x018aff; stride = 1 };
    { lo = 0x018d00; hi = 0x018d08; stride = 1 };
  |];
  latin_offset = 0;
}

(* telugu *)
let _telugu = {
  r16 = [|
    { lo = 0x0c00; hi = 0x0c0c; stride = 1 };
    { lo = 0x0c0e; hi = 0x0c10; stride = 1 };
    { lo = 0x0c12; hi = 0x0c28; stride = 1 };
    { lo = 0x0c2a; hi = 0x0c39; stride = 1 };
    { lo = 0x0c3c; hi = 0x0c44; stride = 1 };
    { lo = 0x0c46; hi = 0x0c48; stride = 1 };
    { lo = 0x0c4a; hi = 0x0c4d; stride = 1 };
    { lo = 0x0c55; hi = 0x0c56; stride = 1 };
    { lo = 0x0c58; hi = 0x0c5a; stride = 1 };
    { lo = 0x0c5d; hi = 0x0c60; stride = 3 };
    { lo = 0x0c61; hi = 0x0c63; stride = 1 };
    { lo = 0x0c66; hi = 0x0c6f; stride = 1 };
    { lo = 0x0c77; hi = 0x0c7f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* thaana *)
let _thaana = {
  r16 = [|
    { lo = 0x0780; hi = 0x07b1; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* thai *)
let _thai = {
  r16 = [|
    { lo = 0x0e01; hi = 0x0e3a; stride = 1 };
    { lo = 0x0e40; hi = 0x0e5b; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* tibetan *)
let _tibetan = {
  r16 = [|
    { lo = 0x0f00; hi = 0x0f47; stride = 1 };
    { lo = 0x0f49; hi = 0x0f6c; stride = 1 };
    { lo = 0x0f71; hi = 0x0f97; stride = 1 };
    { lo = 0x0f99; hi = 0x0fbc; stride = 1 };
    { lo = 0x0fbe; hi = 0x0fcc; stride = 1 };
    { lo = 0x0fce; hi = 0x0fd4; stride = 1 };
    { lo = 0x0fd9; hi = 0x0fda; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* tifinagh *)
let _tifinagh = {
  r16 = [|
    { lo = 0x2d30; hi = 0x2d67; stride = 1 };
    { lo = 0x2d6f; hi = 0x2d70; stride = 1 };
    { lo = 0x2d7f; hi = 0x2d7f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* tirhuta *)
let _tirhuta = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011480; hi = 0x0114c7; stride = 1 };
    { lo = 0x0114d0; hi = 0x0114d9; stride = 1 };
  |];
  latin_offset = 0;
}

(* toto *)
let _toto = {
  r16 = [| |];
  r32 = [|
    { lo = 0x01e290; hi = 0x01e2ae; stride = 1 };
  |];
  latin_offset = 0;
}

(* ugaritic *)
let _ugaritic = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010380; hi = 0x01039d; stride = 1 };
    { lo = 0x01039f; hi = 0x01039f; stride = 1 };
  |];
  latin_offset = 0;
}

(* vai *)
let _vai = {
  r16 = [|
    { lo = 0xa500; hi = 0xa62b; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* vithkuqi *)
let _vithkuqi = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010570; hi = 0x01057a; stride = 1 };
    { lo = 0x01057c; hi = 0x01058a; stride = 1 };
    { lo = 0x01058c; hi = 0x010592; stride = 1 };
    { lo = 0x010594; hi = 0x010595; stride = 1 };
    { lo = 0x010597; hi = 0x0105a1; stride = 1 };
    { lo = 0x0105a3; hi = 0x0105b1; stride = 1 };
    { lo = 0x0105b3; hi = 0x0105b9; stride = 1 };
    { lo = 0x0105bb; hi = 0x0105bc; stride = 1 };
  |];
  latin_offset = 0;
}

(* wancho *)
let _wancho = {
  r16 = [| |];
  r32 = [|
    { lo = 0x01e2c0; hi = 0x01e2f9; stride = 1 };
    { lo = 0x01e2ff; hi = 0x01e2ff; stride = 1 };
  |];
  latin_offset = 0;
}

(* warang_citi *)
let _warang_citi = {
  r16 = [| |];
  r32 = [|
    { lo = 0x0118a0; hi = 0x0118f2; stride = 1 };
    { lo = 0x0118ff; hi = 0x0118ff; stride = 1 };
  |];
  latin_offset = 0;
}

(* yezidi *)
let _yezidi = {
  r16 = [| |];
  r32 = [|
    { lo = 0x010e80; hi = 0x010ea9; stride = 1 };
    { lo = 0x010eab; hi = 0x010ead; stride = 1 };
    { lo = 0x010eb0; hi = 0x010eb1; stride = 1 };
  |];
  latin_offset = 0;
}

(* yi *)
let _yi = {
  r16 = [|
    { lo = 0xa000; hi = 0xa48c; stride = 1 };
    { lo = 0xa490; hi = 0xa4c6; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* zanabazar_square *)
let _zanabazar_square = {
  r16 = [| |];
  r32 = [|
    { lo = 0x011a00; hi = 0x011a47; stride = 1 };
  |];
  latin_offset = 0;
}

(* ascii_hex_digit *)
let _ascii_hex_digit = {
  r16 = [|
    { lo = 0x0030; hi = 0x0039; stride = 1 };
    { lo = 0x0041; hi = 0x0046; stride = 1 };
    { lo = 0x0061; hi = 0x0066; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* bidi_control *)
let _bidi_control = {
  r16 = [|
    { lo = 0x061c; hi = 0x200e; stride = 6642 };
    { lo = 0x200f; hi = 0x202a; stride = 27 };
    { lo = 0x202b; hi = 0x202e; stride = 1 };
    { lo = 0x2066; hi = 0x2069; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* dash *)
let _dash = {
  r16 = [|
    { lo = 0x002d; hi = 0x058a; stride = 1373 };
    { lo = 0x05be; hi = 0x1400; stride = 3650 };
    { lo = 0x1806; hi = 0x2010; stride = 2058 };
    { lo = 0x2011; hi = 0x2015; stride = 1 };
    { lo = 0x2053; hi = 0x207b; stride = 40 };
    { lo = 0x208b; hi = 0x2212; stride = 391 };
    { lo = 0x2e17; hi = 0x2e1a; stride = 3 };
    { lo = 0x2e3a; hi = 0x2e3b; stride = 1 };
    { lo = 0x2e40; hi = 0x2e5d; stride = 29 };
    { lo = 0x301c; hi = 0x3030; stride = 20 };
    { lo = 0x30a0; hi = 0xfe31; stride = 52625 };
    { lo = 0xfe32; hi = 0xfe58; stride = 38 };
    { lo = 0xfe63; hi = 0xff0d; stride = 170 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* deprecated *)
let _deprecated = {
  r16 = [|
    { lo = 0x0149; hi = 0x0673; stride = 1322 };
    { lo = 0x0f77; hi = 0x0f79; stride = 2 };
    { lo = 0x17a3; hi = 0x17a4; stride = 1 };
    { lo = 0x206a; hi = 0x206f; stride = 1 };
    { lo = 0x2329; hi = 0x232a; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* diacritic *)
let _diacritic = {
  r16 = [|
    { lo = 0x005e; hi = 0x0060; stride = 2 };
    { lo = 0x00a8; hi = 0x00af; stride = 7 };
    { lo = 0x00b4; hi = 0x00b7; stride = 3 };
    { lo = 0x00b8; hi = 0x02b0; stride = 504 };
    { lo = 0x02b1; hi = 0x034e; stride = 1 };
    { lo = 0x0350; hi = 0x0357; stride = 1 };
    { lo = 0x035d; hi = 0x0362; stride = 1 };
    { lo = 0x0374; hi = 0x0375; stride = 1 };
    { lo = 0x037a; hi = 0x0384; stride = 10 };
    { lo = 0x0385; hi = 0x0483; stride = 254 };
    { lo = 0x0484; hi = 0x0487; stride = 1 };
    { lo = 0x0559; hi = 0x0591; stride = 56 };
    { lo = 0x0592; hi = 0x05a1; stride = 1 };
    { lo = 0x05a3; hi = 0x05bd; stride = 1 };
    { lo = 0x05bf; hi = 0x05c1; stride = 2 };
    { lo = 0x05c2; hi = 0x05c4; stride = 2 };
    { lo = 0x064b; hi = 0x0652; stride = 1 };
    { lo = 0x0657; hi = 0x0658; stride = 1 };
    { lo = 0x06df; hi = 0x06e0; stride = 1 };
    { lo = 0x06e5; hi = 0x06e6; stride = 1 };
    { lo = 0x06ea; hi = 0x06ec; stride = 1 };
    { lo = 0x0730; hi = 0x074a; stride = 1 };
    { lo = 0x07a6; hi = 0x07b0; stride = 1 };
    { lo = 0x07eb; hi = 0x07f5; stride = 1 };
    { lo = 0x0818; hi = 0x0819; stride = 1 };
    { lo = 0x0898; hi = 0x089f; stride = 1 };
    { lo = 0x08c9; hi = 0x08d2; stride = 1 };
    { lo = 0x08e3; hi = 0x08fe; stride = 1 };
    { lo = 0x093c; hi = 0x094d; stride = 17 };
    { lo = 0x0951; hi = 0x0954; stride = 1 };
    { lo = 0x0971; hi = 0x09bc; stride = 75 };
    { lo = 0x09cd; hi = 0x0a3c; stride = 111 };
    { lo = 0x0a4d; hi = 0x0abc; stride = 111 };
    { lo = 0x0acd; hi = 0x0afd; stride = 48 };
    { lo = 0x0afe; hi = 0x0aff; stride = 1 };
    { lo = 0x0b3c; hi = 0x0b4d; stride = 17 };
    { lo = 0x0b55; hi = 0x0bcd; stride = 120 };
    { lo = 0x0c3c; hi = 0x0c4d; stride = 17 };
    { lo = 0x0cbc; hi = 0x0ccd; stride = 17 };
    { lo = 0x0d3b; hi = 0x0d3c; stride = 1 };
    { lo = 0x0d4d; hi = 0x0e47; stride = 125 };
    { lo = 0x0e48; hi = 0x0e4c; stride = 1 };
    { lo = 0x0e4e; hi = 0x0eba; stride = 108 };
    { lo = 0x0ec8; hi = 0x0ecc; stride = 1 };
    { lo = 0x0f18; hi = 0x0f19; stride = 1 };
    { lo = 0x0f35; hi = 0x0f39; stride = 2 };
    { lo = 0x0f3e; hi = 0x0f3f; stride = 1 };
    { lo = 0x0f82; hi = 0x0f84; stride = 1 };
    { lo = 0x0f86; hi = 0x0f87; stride = 1 };
    { lo = 0x0fc6; hi = 0x1037; stride = 113 };
    { lo = 0x1039; hi = 0x103a; stride = 1 };
    { lo = 0x1063; hi = 0x1064; stride = 1 };
    { lo = 0x1069; hi = 0x106d; stride = 1 };
    { lo = 0x1087; hi = 0x108d; stride = 1 };
    { lo = 0x108f; hi = 0x109a; stride = 11 };
    { lo = 0x109b; hi = 0x135d; stride = 706 };
    { lo = 0x135e; hi = 0x135f; stride = 1 };
    { lo = 0x1714; hi = 0x1715; stride = 1 };
    { lo = 0x17c9; hi = 0x17d3; stride = 1 };
    { lo = 0x17dd; hi = 0x1939; stride = 348 };
    { lo = 0x193a; hi = 0x193b; stride = 1 };
    { lo = 0x1a75; hi = 0x1a7c; stride = 1 };
    { lo = 0x1a7f; hi = 0x1ab0; stride = 49 };
    { lo = 0x1ab1; hi = 0x1abe; stride = 1 };
    { lo = 0x1ac1; hi = 0x1acb; stride = 1 };
    { lo = 0x1b34; hi = 0x1b44; stride = 16 };
    { lo = 0x1b6b; hi = 0x1b73; stride = 1 };
    { lo = 0x1baa; hi = 0x1bab; stride = 1 };
    { lo = 0x1c36; hi = 0x1c37; stride = 1 };
    { lo = 0x1c78; hi = 0x1c7d; stride = 1 };
    { lo = 0x1cd0; hi = 0x1ce8; stride = 1 };
    { lo = 0x1ced; hi = 0x1cf4; stride = 7 };
    { lo = 0x1cf7; hi = 0x1cf9; stride = 1 };
    { lo = 0x1d2c; hi = 0x1d6a; stride = 1 };
    { lo = 0x1dc4; hi = 0x1dcf; stride = 1 };
    { lo = 0x1df5; hi = 0x1dff; stride = 1 };
    { lo = 0x1fbd; hi = 0x1fbf; stride = 2 };
    { lo = 0x1fc0; hi = 0x1fc1; stride = 1 };
    { lo = 0x1fcd; hi = 0x1fcf; stride = 1 };
    { lo = 0x1fdd; hi = 0x1fdf; stride = 1 };
    { lo = 0x1fed; hi = 0x1fef; stride = 1 };
    { lo = 0x1ffd; hi = 0x1ffe; stride = 1 };
    { lo = 0x2cef; hi = 0x2cf1; stride = 1 };
    { lo = 0x2e2f; hi = 0x302a; stride = 507 };
    { lo = 0x302b; hi = 0x302f; stride = 1 };
    { lo = 0x3099; hi = 0x309c; stride = 1 };
    { lo = 0x30fc; hi = 0xa66f; stride = 30067 };
    { lo = 0xa67c; hi = 0xa67d; stride = 1 };
    { lo = 0xa67f; hi = 0xa69c; stride = 29 };
    { lo = 0xa69d; hi = 0xa6f0; stride = 83 };
    { lo = 0xa6f1; hi = 0xa700; stride = 15 };
    { lo = 0xa701; hi = 0xa721; stride = 1 };
    { lo = 0xa788; hi = 0xa78a; stride = 1 };
    { lo = 0xa7f8; hi = 0xa7f9; stride = 1 };
    { lo = 0xa8c4; hi = 0xa8e0; stride = 28 };
    { lo = 0xa8e1; hi = 0xa8f1; stride = 1 };
    { lo = 0xa92b; hi = 0xa92e; stride = 1 };
    { lo = 0xa953; hi = 0xa9b3; stride = 96 };
    { lo = 0xa9c0; hi = 0xa9e5; stride = 37 };
    { lo = 0xaa7b; hi = 0xaa7d; stride = 1 };
    { lo = 0xaabf; hi = 0xaac2; stride = 1 };
    { lo = 0xaaf6; hi = 0xab5b; stride = 101 };
    { lo = 0xab5c; hi = 0xab5f; stride = 1 };
    { lo = 0xab69; hi = 0xab6b; stride = 1 };
    { lo = 0xabec; hi = 0xabed; stride = 1 };
    { lo = 0xfb1e; hi = 0xfe20; stride = 770 };
    { lo = 0xfe21; hi = 0xfe2f; stride = 1 };
    { lo = 0xff3e; hi = 0xff40; stride = 2 };
    { lo = 0xff70; hi = 0xff9e; stride = 46 };
    { lo = 0xff9f; hi = 0xffe3; stride = 68 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* extender *)
let _extender = {
  r16 = [|
    { lo = 0x00b7; hi = 0x02d0; stride = 537 };
    { lo = 0x02d1; hi = 0x0640; stride = 879 };
    { lo = 0x07fa; hi = 0x0b55; stride = 859 };
    { lo = 0x0e46; hi = 0x0ec6; stride = 128 };
    { lo = 0x180a; hi = 0x1843; stride = 57 };
    { lo = 0x1aa7; hi = 0x1c36; stride = 399 };
    { lo = 0x1c7b; hi = 0x3005; stride = 5002 };
    { lo = 0x3031; hi = 0x3035; stride = 1 };
    { lo = 0x309d; hi = 0x309e; stride = 1 };
    { lo = 0x30fc; hi = 0x30fe; stride = 1 };
    { lo = 0xa015; hi = 0xa60c; stride = 1527 };
    { lo = 0xa9cf; hi = 0xa9e6; stride = 23 };
    { lo = 0xaa70; hi = 0xaadd; stride = 109 };
    { lo = 0xaaf3; hi = 0xaaf4; stride = 1 };
    { lo = 0xff70; hi = 0xff70; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* hex_digit *)
let _hex_digit = {
  r16 = [|
    { lo = 0x0030; hi = 0x0039; stride = 1 };
    { lo = 0x0041; hi = 0x0046; stride = 1 };
    { lo = 0x0061; hi = 0x0066; stride = 1 };
    { lo = 0xff10; hi = 0xff19; stride = 1 };
    { lo = 0xff21; hi = 0xff26; stride = 1 };
    { lo = 0xff41; hi = 0xff46; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* hyphen *)
let _hyphen = {
  r16 = [|
    { lo = 0x002d; hi = 0x00ad; stride = 128 };
    { lo = 0x058a; hi = 0x1806; stride = 4732 };
    { lo = 0x2010; hi = 0x2011; stride = 1 };
    { lo = 0x2e17; hi = 0x30fb; stride = 740 };
    { lo = 0xfe63; hi = 0xff0d; stride = 170 };
    { lo = 0xff65; hi = 0xff65; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* ids_binary_operator *)
let _ids_binary_operator = {
  r16 = [|
    { lo = 0x2ff0; hi = 0x2ff1; stride = 1 };
    { lo = 0x2ff4; hi = 0x2ffb; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* ids_trinary_operator *)
let _ids_trinary_operator = {
  r16 = [|
    { lo = 0x2ff2; hi = 0x2ff3; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* ideographic *)
let _ideographic = {
  r16 = [|
    { lo = 0x3006; hi = 0x3007; stride = 1 };
    { lo = 0x3021; hi = 0x3029; stride = 1 };
    { lo = 0x3038; hi = 0x303a; stride = 1 };
    { lo = 0x3400; hi = 0x4dbf; stride = 1 };
    { lo = 0x4e00; hi = 0x9fff; stride = 1 };
    { lo = 0xf900; hi = 0xfa6d; stride = 1 };
    { lo = 0xfa70; hi = 0xfad9; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* join_control *)
let _join_control = {
  r16 = [|
    { lo = 0x200c; hi = 0x200d; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* logical_order_exception *)
let _logical_order_exception = {
  r16 = [|
    { lo = 0x0e40; hi = 0x0e44; stride = 1 };
    { lo = 0x0ec0; hi = 0x0ec4; stride = 1 };
    { lo = 0x19b5; hi = 0x19b7; stride = 1 };
    { lo = 0x19ba; hi = 0xaab5; stride = 37115 };
    { lo = 0xaab6; hi = 0xaab9; stride = 3 };
    { lo = 0xaabb; hi = 0xaabc; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* noncharacter_code_point *)
let _noncharacter_code_point = {
  r16 = [|
    { lo = 0xfdd0; hi = 0xfdef; stride = 1 };
    { lo = 0xfffe; hi = 0xffff; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* other_alphabetic *)
let _other_alphabetic = {
  r16 = [|
    { lo = 0x0345; hi = 0x05b0; stride = 619 };
    { lo = 0x05b1; hi = 0x05bd; stride = 1 };
    { lo = 0x05bf; hi = 0x05c1; stride = 2 };
    { lo = 0x05c2; hi = 0x05c4; stride = 2 };
    { lo = 0x05c5; hi = 0x05c7; stride = 2 };
    { lo = 0x0610; hi = 0x061a; stride = 1 };
    { lo = 0x064b; hi = 0x0657; stride = 1 };
    { lo = 0x0659; hi = 0x065f; stride = 1 };
    { lo = 0x0670; hi = 0x06d6; stride = 102 };
    { lo = 0x06d7; hi = 0x06dc; stride = 1 };
    { lo = 0x06e1; hi = 0x06e4; stride = 1 };
    { lo = 0x06e7; hi = 0x06e8; stride = 1 };
    { lo = 0x06ed; hi = 0x0711; stride = 36 };
    { lo = 0x0730; hi = 0x073f; stride = 1 };
    { lo = 0x07a6; hi = 0x07b0; stride = 1 };
    { lo = 0x0816; hi = 0x0817; stride = 1 };
    { lo = 0x081b; hi = 0x0823; stride = 1 };
    { lo = 0x0825; hi = 0x0827; stride = 1 };
    { lo = 0x0829; hi = 0x082c; stride = 1 };
    { lo = 0x08d4; hi = 0x08df; stride = 1 };
    { lo = 0x08e3; hi = 0x08e9; stride = 1 };
    { lo = 0x08f0; hi = 0x0903; stride = 1 };
    { lo = 0x093a; hi = 0x093b; stride = 1 };
    { lo = 0x093e; hi = 0x094c; stride = 1 };
    { lo = 0x094e; hi = 0x094f; stride = 1 };
    { lo = 0x0955; hi = 0x0957; stride = 1 };
    { lo = 0x0962; hi = 0x0963; stride = 1 };
    { lo = 0x0981; hi = 0x0983; stride = 1 };
    { lo = 0x09be; hi = 0x09c4; stride = 1 };
    { lo = 0x09c7; hi = 0x09c8; stride = 1 };
    { lo = 0x09cb; hi = 0x09cc; stride = 1 };
    { lo = 0x09d7; hi = 0x09e2; stride = 11 };
    { lo = 0x09e3; hi = 0x0a01; stride = 30 };
    { lo = 0x0a02; hi = 0x0a03; stride = 1 };
    { lo = 0x0a3e; hi = 0x0a42; stride = 1 };
    { lo = 0x0a47; hi = 0x0a48; stride = 1 };
    { lo = 0x0a4b; hi = 0x0a4c; stride = 1 };
    { lo = 0x0a51; hi = 0x0a70; stride = 31 };
    { lo = 0x0a71; hi = 0x0a75; stride = 4 };
    { lo = 0x0a81; hi = 0x0a83; stride = 1 };
    { lo = 0x0abe; hi = 0x0ac5; stride = 1 };
    { lo = 0x0ac7; hi = 0x0ac9; stride = 1 };
    { lo = 0x0acb; hi = 0x0acc; stride = 1 };
    { lo = 0x0ae2; hi = 0x0ae3; stride = 1 };
    { lo = 0x0afa; hi = 0x0afc; stride = 1 };
    { lo = 0x0b01; hi = 0x0b03; stride = 1 };
    { lo = 0x0b3e; hi = 0x0b44; stride = 1 };
    { lo = 0x0b47; hi = 0x0b48; stride = 1 };
    { lo = 0x0b4b; hi = 0x0b4c; stride = 1 };
    { lo = 0x0b56; hi = 0x0b57; stride = 1 };
    { lo = 0x0b62; hi = 0x0b63; stride = 1 };
    { lo = 0x0b82; hi = 0x0bbe; stride = 60 };
    { lo = 0x0bbf; hi = 0x0bc2; stride = 1 };
    { lo = 0x0bc6; hi = 0x0bc8; stride = 1 };
    { lo = 0x0bca; hi = 0x0bcc; stride = 1 };
    { lo = 0x0bd7; hi = 0x0c00; stride = 41 };
    { lo = 0x0c01; hi = 0x0c04; stride = 1 };
    { lo = 0x0c3e; hi = 0x0c44; stride = 1 };
    { lo = 0x0c46; hi = 0x0c48; stride = 1 };
    { lo = 0x0c4a; hi = 0x0c4c; stride = 1 };
    { lo = 0x0c55; hi = 0x0c56; stride = 1 };
    { lo = 0x0c62; hi = 0x0c63; stride = 1 };
    { lo = 0x0c81; hi = 0x0c83; stride = 1 };
    { lo = 0x0cbe; hi = 0x0cc4; stride = 1 };
    { lo = 0x0cc6; hi = 0x0cc8; stride = 1 };
    { lo = 0x0cca; hi = 0x0ccc; stride = 1 };
    { lo = 0x0cd5; hi = 0x0cd6; stride = 1 };
    { lo = 0x0ce2; hi = 0x0ce3; stride = 1 };
    { lo = 0x0cf3; hi = 0x0d00; stride = 13 };
    { lo = 0x0d01; hi = 0x0d03; stride = 1 };
    { lo = 0x0d3e; hi = 0x0d44; stride = 1 };
    { lo = 0x0d46; hi = 0x0d48; stride = 1 };
    { lo = 0x0d4a; hi = 0x0d4c; stride = 1 };
    { lo = 0x0d57; hi = 0x0d62; stride = 11 };
    { lo = 0x0d63; hi = 0x0d81; stride = 30 };
    { lo = 0x0d82; hi = 0x0d83; stride = 1 };
    { lo = 0x0dcf; hi = 0x0dd4; stride = 1 };
    { lo = 0x0dd6; hi = 0x0dd8; stride = 2 };
    { lo = 0x0dd9; hi = 0x0ddf; stride = 1 };
    { lo = 0x0df2; hi = 0x0df3; stride = 1 };
    { lo = 0x0e31; hi = 0x0e34; stride = 3 };
    { lo = 0x0e35; hi = 0x0e3a; stride = 1 };
    { lo = 0x0e4d; hi = 0x0eb1; stride = 100 };
    { lo = 0x0eb4; hi = 0x0eb9; stride = 1 };
    { lo = 0x0ebb; hi = 0x0ebc; stride = 1 };
    { lo = 0x0ecd; hi = 0x0f71; stride = 164 };
    { lo = 0x0f72; hi = 0x0f83; stride = 1 };
    { lo = 0x0f8d; hi = 0x0f97; stride = 1 };
    { lo = 0x0f99; hi = 0x0fbc; stride = 1 };
    { lo = 0x102b; hi = 0x1036; stride = 1 };
    { lo = 0x1038; hi = 0x103b; stride = 3 };
    { lo = 0x103c; hi = 0x103e; stride = 1 };
    { lo = 0x1056; hi = 0x1059; stride = 1 };
    { lo = 0x105e; hi = 0x1060; stride = 1 };
    { lo = 0x1062; hi = 0x1064; stride = 1 };
    { lo = 0x1067; hi = 0x106d; stride = 1 };
    { lo = 0x1071; hi = 0x1074; stride = 1 };
    { lo = 0x1082; hi = 0x108d; stride = 1 };
    { lo = 0x108f; hi = 0x109a; stride = 11 };
    { lo = 0x109b; hi = 0x109d; stride = 1 };
    { lo = 0x1712; hi = 0x1713; stride = 1 };
    { lo = 0x1732; hi = 0x1733; stride = 1 };
    { lo = 0x1752; hi = 0x1753; stride = 1 };
    { lo = 0x1772; hi = 0x1773; stride = 1 };
    { lo = 0x17b6; hi = 0x17c8; stride = 1 };
    { lo = 0x1885; hi = 0x1886; stride = 1 };
    { lo = 0x18a9; hi = 0x1920; stride = 119 };
    { lo = 0x1921; hi = 0x192b; stride = 1 };
    { lo = 0x1930; hi = 0x1938; stride = 1 };
    { lo = 0x1a17; hi = 0x1a1b; stride = 1 };
    { lo = 0x1a55; hi = 0x1a5e; stride = 1 };
    { lo = 0x1a61; hi = 0x1a74; stride = 1 };
    { lo = 0x1abf; hi = 0x1ac0; stride = 1 };
    { lo = 0x1acc; hi = 0x1ace; stride = 1 };
    { lo = 0x1b00; hi = 0x1b04; stride = 1 };
    { lo = 0x1b35; hi = 0x1b43; stride = 1 };
    { lo = 0x1b80; hi = 0x1b82; stride = 1 };
    { lo = 0x1ba1; hi = 0x1ba9; stride = 1 };
    { lo = 0x1bac; hi = 0x1bad; stride = 1 };
    { lo = 0x1be7; hi = 0x1bf1; stride = 1 };
    { lo = 0x1c24; hi = 0x1c36; stride = 1 };
    { lo = 0x1de7; hi = 0x1df4; stride = 1 };
    { lo = 0x24b6; hi = 0x24e9; stride = 1 };
    { lo = 0x2de0; hi = 0x2dff; stride = 1 };
    { lo = 0xa674; hi = 0xa67b; stride = 1 };
    { lo = 0xa69e; hi = 0xa69f; stride = 1 };
    { lo = 0xa802; hi = 0xa80b; stride = 9 };
    { lo = 0xa823; hi = 0xa827; stride = 1 };
    { lo = 0xa880; hi = 0xa881; stride = 1 };
    { lo = 0xa8b4; hi = 0xa8c3; stride = 1 };
    { lo = 0xa8c5; hi = 0xa8ff; stride = 58 };
    { lo = 0xa926; hi = 0xa92a; stride = 1 };
    { lo = 0xa947; hi = 0xa952; stride = 1 };
    { lo = 0xa980; hi = 0xa983; stride = 1 };
    { lo = 0xa9b4; hi = 0xa9bf; stride = 1 };
    { lo = 0xa9e5; hi = 0xaa29; stride = 68 };
    { lo = 0xaa2a; hi = 0xaa36; stride = 1 };
    { lo = 0xaa43; hi = 0xaa4c; stride = 9 };
    { lo = 0xaa4d; hi = 0xaa7b; stride = 46 };
    { lo = 0xaa7c; hi = 0xaa7d; stride = 1 };
    { lo = 0xaab0; hi = 0xaab2; stride = 2 };
    { lo = 0xaab3; hi = 0xaab4; stride = 1 };
    { lo = 0xaab7; hi = 0xaab8; stride = 1 };
    { lo = 0xaabe; hi = 0xaaeb; stride = 45 };
    { lo = 0xaaec; hi = 0xaaef; stride = 1 };
    { lo = 0xaaf5; hi = 0xabe3; stride = 238 };
    { lo = 0xabe4; hi = 0xabea; stride = 1 };
    { lo = 0xfb1e; hi = 0xfb1e; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* other_default_ignorable_code_point *)
let _other_default_ignorable_code_point = {
  r16 = [|
    { lo = 0x034f; hi = 0x115f; stride = 3600 };
    { lo = 0x1160; hi = 0x17b4; stride = 1620 };
    { lo = 0x17b5; hi = 0x2065; stride = 2224 };
    { lo = 0x3164; hi = 0xffa0; stride = 52796 };
    { lo = 0xfff0; hi = 0xfff8; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* other_grapheme_extend *)
let _other_grapheme_extend = {
  r16 = [|
    { lo = 0x09be; hi = 0x09d7; stride = 25 };
    { lo = 0x0b3e; hi = 0x0b57; stride = 25 };
    { lo = 0x0bbe; hi = 0x0bd7; stride = 25 };
    { lo = 0x0cc2; hi = 0x0cd5; stride = 19 };
    { lo = 0x0cd6; hi = 0x0d3e; stride = 104 };
    { lo = 0x0d57; hi = 0x0dcf; stride = 120 };
    { lo = 0x0ddf; hi = 0x1b35; stride = 3414 };
    { lo = 0x200c; hi = 0x302e; stride = 4130 };
    { lo = 0x302f; hi = 0xff9e; stride = 53103 };
    { lo = 0xff9f; hi = 0xff9f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* other_id_continue *)
let _other_id_continue = {
  r16 = [|
    { lo = 0x00b7; hi = 0x0387; stride = 720 };
    { lo = 0x1369; hi = 0x1371; stride = 1 };
    { lo = 0x19da; hi = 0x19da; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* other_id_start *)
let _other_id_start = {
  r16 = [|
    { lo = 0x1885; hi = 0x1886; stride = 1 };
    { lo = 0x2118; hi = 0x212e; stride = 22 };
    { lo = 0x309b; hi = 0x309c; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* other_lowercase *)
let _other_lowercase = {
  r16 = [|
    { lo = 0x00aa; hi = 0x00ba; stride = 16 };
    { lo = 0x02b0; hi = 0x02b8; stride = 1 };
    { lo = 0x02c0; hi = 0x02c1; stride = 1 };
    { lo = 0x02e0; hi = 0x02e4; stride = 1 };
    { lo = 0x0345; hi = 0x037a; stride = 53 };
    { lo = 0x10fc; hi = 0x1d2c; stride = 3120 };
    { lo = 0x1d2d; hi = 0x1d6a; stride = 1 };
    { lo = 0x1d78; hi = 0x1d9b; stride = 35 };
    { lo = 0x1d9c; hi = 0x1dbf; stride = 1 };
    { lo = 0x2071; hi = 0x207f; stride = 14 };
    { lo = 0x2090; hi = 0x209c; stride = 1 };
    { lo = 0x2170; hi = 0x217f; stride = 1 };
    { lo = 0x24d0; hi = 0x24e9; stride = 1 };
    { lo = 0x2c7c; hi = 0x2c7d; stride = 1 };
    { lo = 0xa69c; hi = 0xa69d; stride = 1 };
    { lo = 0xa770; hi = 0xa7f2; stride = 130 };
    { lo = 0xa7f3; hi = 0xa7f4; stride = 1 };
    { lo = 0xa7f8; hi = 0xa7f9; stride = 1 };
    { lo = 0xab5c; hi = 0xab5f; stride = 1 };
    { lo = 0xab69; hi = 0xab69; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* other_math *)
let _other_math = {
  r16 = [|
    { lo = 0x005e; hi = 0x03d0; stride = 882 };
    { lo = 0x03d1; hi = 0x03d2; stride = 1 };
    { lo = 0x03d5; hi = 0x03f0; stride = 27 };
    { lo = 0x03f1; hi = 0x03f4; stride = 3 };
    { lo = 0x03f5; hi = 0x2016; stride = 7201 };
    { lo = 0x2032; hi = 0x2034; stride = 1 };
    { lo = 0x2040; hi = 0x2061; stride = 33 };
    { lo = 0x2062; hi = 0x2064; stride = 1 };
    { lo = 0x207d; hi = 0x207e; stride = 1 };
    { lo = 0x208d; hi = 0x208e; stride = 1 };
    { lo = 0x20d0; hi = 0x20dc; stride = 1 };
    { lo = 0x20e1; hi = 0x20e5; stride = 4 };
    { lo = 0x20e6; hi = 0x20eb; stride = 5 };
    { lo = 0x20ec; hi = 0x20ef; stride = 1 };
    { lo = 0x2102; hi = 0x2107; stride = 5 };
    { lo = 0x210a; hi = 0x2113; stride = 1 };
    { lo = 0x2115; hi = 0x2119; stride = 4 };
    { lo = 0x211a; hi = 0x211d; stride = 1 };
    { lo = 0x2124; hi = 0x2128; stride = 4 };
    { lo = 0x2129; hi = 0x212c; stride = 3 };
    { lo = 0x212d; hi = 0x212f; stride = 2 };
    { lo = 0x2130; hi = 0x2131; stride = 1 };
    { lo = 0x2133; hi = 0x2138; stride = 1 };
    { lo = 0x213c; hi = 0x213f; stride = 1 };
    { lo = 0x2145; hi = 0x2149; stride = 1 };
    { lo = 0x2195; hi = 0x2199; stride = 1 };
    { lo = 0x219c; hi = 0x219f; stride = 1 };
    { lo = 0x21a1; hi = 0x21a2; stride = 1 };
    { lo = 0x21a4; hi = 0x21a5; stride = 1 };
    { lo = 0x21a7; hi = 0x21a9; stride = 2 };
    { lo = 0x21aa; hi = 0x21ad; stride = 1 };
    { lo = 0x21b0; hi = 0x21b1; stride = 1 };
    { lo = 0x21b6; hi = 0x21b7; stride = 1 };
    { lo = 0x21bc; hi = 0x21cd; stride = 1 };
    { lo = 0x21d0; hi = 0x21d1; stride = 1 };
    { lo = 0x21d3; hi = 0x21d5; stride = 2 };
    { lo = 0x21d6; hi = 0x21db; stride = 1 };
    { lo = 0x21dd; hi = 0x21e4; stride = 7 };
    { lo = 0x21e5; hi = 0x2308; stride = 291 };
    { lo = 0x2309; hi = 0x230b; stride = 1 };
    { lo = 0x23b4; hi = 0x23b5; stride = 1 };
    { lo = 0x23b7; hi = 0x23d0; stride = 25 };
    { lo = 0x23e2; hi = 0x25a0; stride = 446 };
    { lo = 0x25a1; hi = 0x25ae; stride = 13 };
    { lo = 0x25af; hi = 0x25b6; stride = 1 };
    { lo = 0x25bc; hi = 0x25c0; stride = 1 };
    { lo = 0x25c6; hi = 0x25c7; stride = 1 };
    { lo = 0x25ca; hi = 0x25cb; stride = 1 };
    { lo = 0x25cf; hi = 0x25d3; stride = 1 };
    { lo = 0x25e2; hi = 0x25e4; stride = 2 };
    { lo = 0x25e7; hi = 0x25ec; stride = 1 };
    { lo = 0x2605; hi = 0x2606; stride = 1 };
    { lo = 0x2640; hi = 0x2642; stride = 2 };
    { lo = 0x2660; hi = 0x2663; stride = 1 };
    { lo = 0x266d; hi = 0x266e; stride = 1 };
    { lo = 0x27c5; hi = 0x27c6; stride = 1 };
    { lo = 0x27e6; hi = 0x27ef; stride = 1 };
    { lo = 0x2983; hi = 0x2998; stride = 1 };
    { lo = 0x29d8; hi = 0x29db; stride = 1 };
    { lo = 0x29fc; hi = 0x29fd; stride = 1 };
    { lo = 0xfe61; hi = 0xfe63; stride = 2 };
    { lo = 0xfe68; hi = 0xff3c; stride = 212 };
    { lo = 0xff3e; hi = 0xff3e; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* other_uppercase *)
let _other_uppercase = {
  r16 = [|
    { lo = 0x2160; hi = 0x216f; stride = 1 };
    { lo = 0x24b6; hi = 0x24cf; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* pattern_syntax *)
let _pattern_syntax = {
  r16 = [|
    { lo = 0x0021; hi = 0x002f; stride = 1 };
    { lo = 0x003a; hi = 0x0040; stride = 1 };
    { lo = 0x005b; hi = 0x005e; stride = 1 };
    { lo = 0x0060; hi = 0x007b; stride = 27 };
    { lo = 0x007c; hi = 0x007e; stride = 1 };
    { lo = 0x00a1; hi = 0x00a7; stride = 1 };
    { lo = 0x00a9; hi = 0x00ab; stride = 2 };
    { lo = 0x00ac; hi = 0x00b0; stride = 2 };
    { lo = 0x00b1; hi = 0x00bb; stride = 5 };
    { lo = 0x00bf; hi = 0x00d7; stride = 24 };
    { lo = 0x00f7; hi = 0x2010; stride = 7961 };
    { lo = 0x2011; hi = 0x2027; stride = 1 };
    { lo = 0x2030; hi = 0x203e; stride = 1 };
    { lo = 0x2041; hi = 0x2053; stride = 1 };
    { lo = 0x2055; hi = 0x205e; stride = 1 };
    { lo = 0x2190; hi = 0x245f; stride = 1 };
    { lo = 0x2500; hi = 0x2775; stride = 1 };
    { lo = 0x2794; hi = 0x2bff; stride = 1 };
    { lo = 0x2e00; hi = 0x2e7f; stride = 1 };
    { lo = 0x3001; hi = 0x3003; stride = 1 };
    { lo = 0x3008; hi = 0x3020; stride = 1 };
    { lo = 0x3030; hi = 0xfd3e; stride = 52494 };
    { lo = 0xfd3f; hi = 0xfe45; stride = 262 };
    { lo = 0xfe46; hi = 0xfe46; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* pattern_white_space *)
let _pattern_white_space = {
  r16 = [|
    { lo = 0x0009; hi = 0x000d; stride = 1 };
    { lo = 0x0020; hi = 0x0085; stride = 101 };
    { lo = 0x200e; hi = 0x200f; stride = 1 };
    { lo = 0x2028; hi = 0x2029; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* prepended_concatenation_mark *)
let _prepended_concatenation_mark = {
  r16 = [|
    { lo = 0x0600; hi = 0x0605; stride = 1 };
    { lo = 0x06dd; hi = 0x070f; stride = 50 };
    { lo = 0x0890; hi = 0x0891; stride = 1 };
    { lo = 0x08e2; hi = 0x08e2; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* quotation_mark *)
let _quotation_mark = {
  r16 = [|
    { lo = 0x0022; hi = 0x0027; stride = 5 };
    { lo = 0x00ab; hi = 0x00bb; stride = 16 };
    { lo = 0x2018; hi = 0x201f; stride = 1 };
    { lo = 0x2039; hi = 0x203a; stride = 1 };
    { lo = 0x2e42; hi = 0x300c; stride = 458 };
    { lo = 0x300d; hi = 0x300f; stride = 1 };
    { lo = 0x301d; hi = 0x301f; stride = 1 };
    { lo = 0xfe41; hi = 0xfe44; stride = 1 };
    { lo = 0xff02; hi = 0xff07; stride = 5 };
    { lo = 0xff62; hi = 0xff63; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* radical *)
let _radical = {
  r16 = [|
    { lo = 0x2e80; hi = 0x2e99; stride = 1 };
    { lo = 0x2e9b; hi = 0x2ef3; stride = 1 };
    { lo = 0x2f00; hi = 0x2fd5; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* regional_indicator *)
let _regional_indicator = {
  r16 = [| |];
  r32 = [|
    { lo = 0x01f1e6; hi = 0x01f1ff; stride = 1 };
  |];
  latin_offset = 0;
}

(* sentence_terminal *)
let _sentence_terminal = {
  r16 = [|
    { lo = 0x0021; hi = 0x002e; stride = 13 };
    { lo = 0x003f; hi = 0x0589; stride = 1354 };
    { lo = 0x061d; hi = 0x061f; stride = 1 };
    { lo = 0x06d4; hi = 0x0700; stride = 44 };
    { lo = 0x0701; hi = 0x0702; stride = 1 };
    { lo = 0x07f9; hi = 0x0837; stride = 62 };
    { lo = 0x0839; hi = 0x083d; stride = 4 };
    { lo = 0x083e; hi = 0x0964; stride = 294 };
    { lo = 0x0965; hi = 0x104a; stride = 1765 };
    { lo = 0x104b; hi = 0x1362; stride = 791 };
    { lo = 0x1367; hi = 0x1368; stride = 1 };
    { lo = 0x166e; hi = 0x1735; stride = 199 };
    { lo = 0x1736; hi = 0x1803; stride = 205 };
    { lo = 0x1809; hi = 0x1944; stride = 315 };
    { lo = 0x1945; hi = 0x1aa8; stride = 355 };
    { lo = 0x1aa9; hi = 0x1aab; stride = 1 };
    { lo = 0x1b5a; hi = 0x1b5b; stride = 1 };
    { lo = 0x1b5e; hi = 0x1b5f; stride = 1 };
    { lo = 0x1b7d; hi = 0x1b7e; stride = 1 };
    { lo = 0x1c3b; hi = 0x1c3c; stride = 1 };
    { lo = 0x1c7e; hi = 0x1c7f; stride = 1 };
    { lo = 0x203c; hi = 0x203d; stride = 1 };
    { lo = 0x2047; hi = 0x2049; stride = 1 };
    { lo = 0x2e2e; hi = 0x2e3c; stride = 14 };
    { lo = 0x2e53; hi = 0x2e54; stride = 1 };
    { lo = 0x3002; hi = 0xa4ff; stride = 29949 };
    { lo = 0xa60e; hi = 0xa60f; stride = 1 };
    { lo = 0xa6f3; hi = 0xa6f7; stride = 4 };
    { lo = 0xa876; hi = 0xa877; stride = 1 };
    { lo = 0xa8ce; hi = 0xa8cf; stride = 1 };
    { lo = 0xa92f; hi = 0xa9c8; stride = 153 };
    { lo = 0xa9c9; hi = 0xaa5d; stride = 148 };
    { lo = 0xaa5e; hi = 0xaa5f; stride = 1 };
    { lo = 0xaaf0; hi = 0xaaf1; stride = 1 };
    { lo = 0xabeb; hi = 0xfe52; stride = 21095 };
    { lo = 0xfe56; hi = 0xfe57; stride = 1 };
    { lo = 0xff01; hi = 0xff0e; stride = 13 };
    { lo = 0xff1f; hi = 0xff61; stride = 66 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* soft_dotted *)
let _soft_dotted = {
  r16 = [|
    { lo = 0x0069; hi = 0x006a; stride = 1 };
    { lo = 0x012f; hi = 0x0249; stride = 282 };
    { lo = 0x0268; hi = 0x029d; stride = 53 };
    { lo = 0x02b2; hi = 0x03f3; stride = 321 };
    { lo = 0x0456; hi = 0x0458; stride = 2 };
    { lo = 0x1d62; hi = 0x1d96; stride = 52 };
    { lo = 0x1da4; hi = 0x1da8; stride = 4 };
    { lo = 0x1e2d; hi = 0x1ecb; stride = 158 };
    { lo = 0x2071; hi = 0x2148; stride = 215 };
    { lo = 0x2149; hi = 0x2c7c; stride = 2867 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* terminal_punctuation *)
let _terminal_punctuation = {
  r16 = [|
    { lo = 0x0021; hi = 0x002c; stride = 11 };
    { lo = 0x002e; hi = 0x003a; stride = 12 };
    { lo = 0x003b; hi = 0x003f; stride = 4 };
    { lo = 0x037e; hi = 0x0387; stride = 9 };
    { lo = 0x0589; hi = 0x05c3; stride = 58 };
    { lo = 0x060c; hi = 0x061b; stride = 15 };
    { lo = 0x061d; hi = 0x061f; stride = 1 };
    { lo = 0x06d4; hi = 0x0700; stride = 44 };
    { lo = 0x0701; hi = 0x070a; stride = 1 };
    { lo = 0x070c; hi = 0x07f8; stride = 236 };
    { lo = 0x07f9; hi = 0x0830; stride = 55 };
    { lo = 0x0831; hi = 0x083e; stride = 1 };
    { lo = 0x085e; hi = 0x0964; stride = 262 };
    { lo = 0x0965; hi = 0x0e5a; stride = 1269 };
    { lo = 0x0e5b; hi = 0x0f08; stride = 173 };
    { lo = 0x0f0d; hi = 0x0f12; stride = 1 };
    { lo = 0x104a; hi = 0x104b; stride = 1 };
    { lo = 0x1361; hi = 0x1368; stride = 1 };
    { lo = 0x166e; hi = 0x16eb; stride = 125 };
    { lo = 0x16ec; hi = 0x16ed; stride = 1 };
    { lo = 0x1735; hi = 0x1736; stride = 1 };
    { lo = 0x17d4; hi = 0x17d6; stride = 1 };
    { lo = 0x17da; hi = 0x1802; stride = 40 };
    { lo = 0x1803; hi = 0x1805; stride = 1 };
    { lo = 0x1808; hi = 0x1809; stride = 1 };
    { lo = 0x1944; hi = 0x1945; stride = 1 };
    { lo = 0x1aa8; hi = 0x1aab; stride = 1 };
    { lo = 0x1b5a; hi = 0x1b5b; stride = 1 };
    { lo = 0x1b5d; hi = 0x1b5f; stride = 1 };
    { lo = 0x1b7d; hi = 0x1b7e; stride = 1 };
    { lo = 0x1c3b; hi = 0x1c3f; stride = 1 };
    { lo = 0x1c7e; hi = 0x1c7f; stride = 1 };
    { lo = 0x203c; hi = 0x203d; stride = 1 };
    { lo = 0x2047; hi = 0x2049; stride = 1 };
    { lo = 0x2e2e; hi = 0x2e3c; stride = 14 };
    { lo = 0x2e41; hi = 0x2e4c; stride = 11 };
    { lo = 0x2e4e; hi = 0x2e4f; stride = 1 };
    { lo = 0x2e53; hi = 0x2e54; stride = 1 };
    { lo = 0x3001; hi = 0x3002; stride = 1 };
    { lo = 0xa4fe; hi = 0xa4ff; stride = 1 };
    { lo = 0xa60d; hi = 0xa60f; stride = 1 };
    { lo = 0xa6f3; hi = 0xa6f7; stride = 1 };
    { lo = 0xa876; hi = 0xa877; stride = 1 };
    { lo = 0xa8ce; hi = 0xa8cf; stride = 1 };
    { lo = 0xa92f; hi = 0xa9c7; stride = 152 };
    { lo = 0xa9c8; hi = 0xa9c9; stride = 1 };
    { lo = 0xaa5d; hi = 0xaa5f; stride = 1 };
    { lo = 0xaadf; hi = 0xaaf0; stride = 17 };
    { lo = 0xaaf1; hi = 0xabeb; stride = 250 };
    { lo = 0xfe50; hi = 0xfe52; stride = 1 };
    { lo = 0xfe54; hi = 0xfe57; stride = 1 };
    { lo = 0xff01; hi = 0xff0c; stride = 11 };
    { lo = 0xff0e; hi = 0xff1a; stride = 12 };
    { lo = 0xff1b; hi = 0xff1f; stride = 4 };
    { lo = 0xff61; hi = 0xff64; stride = 3 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* unified_ideograph *)
let _unified_ideograph = {
  r16 = [|
    { lo = 0x3400; hi = 0x4dbf; stride = 1 };
    { lo = 0x4e00; hi = 0x9fff; stride = 1 };
    { lo = 0xfa0e; hi = 0xfa0f; stride = 1 };
    { lo = 0xfa11; hi = 0xfa13; stride = 2 };
    { lo = 0xfa14; hi = 0xfa1f; stride = 11 };
    { lo = 0xfa21; hi = 0xfa23; stride = 2 };
    { lo = 0xfa24; hi = 0xfa27; stride = 3 };
    { lo = 0xfa28; hi = 0xfa29; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* variation_selector *)
let _variation_selector = {
  r16 = [|
    { lo = 0x180b; hi = 0x180d; stride = 1 };
    { lo = 0x180f; hi = 0xfe00; stride = 58865 };
    { lo = 0xfe01; hi = 0xfe0f; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

(* white_space *)
let _white_space = {
  r16 = [|
    { lo = 0x0009; hi = 0x000d; stride = 1 };
    { lo = 0x0020; hi = 0x0085; stride = 101 };
    { lo = 0x00a0; hi = 0x1680; stride = 5600 };
    { lo = 0x2000; hi = 0x200a; stride = 1 };
    { lo = 0x2028; hi = 0x2029; stride = 1 };
    { lo = 0x202f; hi = 0x205f; stride = 48 };
    { lo = 0x3000; hi = 0x3000; stride = 1 };
  |];
  r32 = [| |];
  latin_offset = 0;
}

