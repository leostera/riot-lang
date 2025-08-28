open Sexplib.Std
open AndesCore
open AndesOCamlAst

type t = { tokentrees : TokenTrees.t } [@@deriving sexp]

let from_tokentrees tokentrees = { tokentrees }

let expect token t =
  match t with
  | TokenTrees.Token actual :: rest when Token.equal token actual -> rest
  | _ -> failwith "expected token not found"

let rec parse (t : t) : Item.t * TokenTrees.t =
  match t.tokentrees with
  | Token (Keyword Let) :: rest -> parse_top_level_let rest
  | _ -> failwith "top-level must start with a delimited token tree"

and parse_top_level_let t =
  let kind, t = parse_let ~expect_in:false t in
  (Item { kind = Let kind }, t)

and parse_let ?(expect_in = true) t =
  let ext, t = parse_extension t in
  let annot, t = parse_annotations t in
  let pat, t = parse_pattern t in
  let t = expect Eq t in
  let expr, t = parse_expression t in
  let t = if expect_in then expect (Keyword In) t else t in
  ({ ext; annot; pat; expr }, t)

and parse_extension t = (None, t)
and parse_annotations t = (None, t)

and parse_pattern t =
  match t with
  | TokenTrees.Token (Ident id) :: t -> (Bind id, t)
  | _ -> failwith "unexpected pattern"

and parse_expression t =
  match t with
  | Token (Ident id) :: t -> (Var id, t)
  | Token (Keyword Let) :: t ->
      let binding, t = parse_let t in
      (Let binding, t)
  | Token (Literal lit) :: t -> (parse_literal lit, t)
  | _ -> failwith "unexpected expression"

and parse_literal lit = match lit with String str -> Literal (String str)

module Tests = struct
  let parse str =
    let lexer = Lexer.from_string str in
    let tokentrees = TokenTrees.tokentrees lexer in
    let parser = from_tokentrees tokentrees in
    let items, _ = parse parser in
    items

  let%expect_test "parses top-level let" =
    let items = parse "let x = y" in
    Format.printf "%a" Item.pp items;
    [%expect
      {| (Item (kind (Let ((ext ()) (annot ()) (pat (Bind x)) (expr (Var y)))))) |}]

  let%expect_test "parses expression-level let" =
    let items = parse {ocaml|
let x = 
  let y = "what" in
  y
|ocaml} in
    Format.printf "%a" Item.pp items;
    [%expect
      {|
      (Item
        (kind
          (Let
            ((ext ()) (annot ()) (pat (Bind x))
              (expr
                (Let
                  ((ext ()) (annot ()) (pat (Bind y))
                    (expr (Literal (String what))))))))))
      |}]
end
