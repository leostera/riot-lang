open Sexplib.Std
open AndesOCamlAst

type token_tree =
  | Token of Token.t
  | Delimiter of Token.delimiter * token_tree list
[@@deriving sexp]

type t = token_tree list [@@deriving sexp]

let pp ppf token =
  let sexp = sexp_of_t token in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let rec tokentrees lexer acc =
  let token = Lexer.next_token lexer in
  match token with
  | EOF -> List.rev acc
  | OpenDelim delim ->
      let tree = handle_open_delim lexer delim in
      tokentrees lexer (tree :: acc)
  | CloseDelim _delim -> List.rev acc
  | _ -> tokentrees lexer (Token token :: acc)

and handle_open_delim lexer delim =
  let trees = tokentrees lexer [] in
  Delimiter (delim, trees)

let tokentrees lexer = tokentrees lexer []

(*************************************************************************************************)

module Tests = struct
  let%expect_test "small program" =
    let lexer =
      {code|

module A = struct
  let x = true
end

include A

module Test = struct
  let%test "captures comments" = 
    token1 = Token.Comment " hello world " [@doc "what"]
end
;;

  |code}
      |> Lexer.from_string
    in
    let trees = tokentrees lexer in
    Format.printf "%a" pp trees;
    [%expect
      {|
      ((Token (Keyword Module)) (Token (Ident A)) (Token Eq)
        (Delimiter (Keyword struct)
          ((Token (Keyword Let)) (Token (Ident x)) (Token Eq)
            (Token (Keyword True))))
        (Token (Keyword Include)) (Token (Ident A)) (Token (Keyword Module))
        (Token (Ident Test)) (Token Eq)
        (Delimiter (Keyword struct)
          ((Token (Keyword Let)) (Token Percent) (Token (Ident test))
            (Token (Literal (String "captures comments"))) (Token Eq)
            (Token (Ident token1)) (Token Eq) (Token (Ident Token)) (Token Dot)
            (Token (Ident Comment)) (Token (Literal (String " hello world ")))
            (Delimiter Bracket
              ((Token At) (Token (Ident doc)) (Token (Literal (String what)))))))
        (Token Semi) (Token Semi))
      |}]
end
