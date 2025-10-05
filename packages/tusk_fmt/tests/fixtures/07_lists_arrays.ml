(* Test: Lists and arrays *)

(* List literals *)
let   empty   =   []
let   single   =   [1]
let   multiple   =   [1;   2;   3;   4;   5]

(* List construction *)
let   cons   =   1   ::   2   ::   3   ::   []

(* Nested lists *)
let   matrix   =   
  [
    [1;   2;   3];
    [4;   5;   6];
    [7;   8;   9]
  ]

(* List operations *)
let   result   =   
  Std.List.map   (fun   x   ->   x   *   2)
    (Std.List.filter   (fun   x   ->   x   mod   2   =   0)
      [1;   2;   3;   4;   5])

(* Array literals *)
let   arr_empty   =   [||]
let   arr_single   =   [|1|]
let   arr_multiple   =   [|1;   2;   3;   4;   5|]

(* Array access *)
let   first   =   arr_multiple.(0)
let   last   =   arr_multiple.(Std.Array.length   arr_multiple   -   1)

(* Array update *)
let   update_array   arr   =
  arr.(0)   <-   100;
  arr

(* String as array *)
let   char_at   s   i   =   s.[i]

(* List comprehension-like *)
let   squares   =
  Std.List.init   10   (fun   i   ->   i   *   i)

(* List with complex elements *)
let   people   =   [
  {   name   =   "Alice";   age   =   30   };
  {   name   =   "Bob";   age   =   25   };
  {   name   =   "Charlie";   age   =   35   }
]