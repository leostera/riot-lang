open Std
open Std.Collections
module Slice = IO.IoVec.IoSlice

type t = {
  kind: Syntax_kind2.t;
  span: Ceibo.Span.t;
  legacy_kind: Token.token_kind;
}

type stream = {
  raw: t Vector.t;
  significant: int Vector.t;
}

let create_stream = fun () -> { raw = Vector.create (); significant = Vector.create () }

let create_stream_with_capacity = fun ~raw ~significant ->
  { raw = Vector.with_capacity ~size:raw; significant = Vector.with_capacity ~size:significant }

let push = fun stream token ->
  let index = Vector.length stream.raw in
  Vector.push stream.raw ~value:token;
  index

let push_significant = fun stream token ->
  let index = push stream token in
  Vector.push stream.significant ~value:index;
  index

let is_trivia = fun token -> Syntax_kind2.is_trivia token.kind

let is_significant = fun token -> not (is_trivia token)

let width = fun token -> token.span.Ceibo.Span.end_ - token.span.Ceibo.Span.start

let slice = fun ~source token ->
  let len = width token in
  if len <= 0 then
    Slice.empty
  else
    Slice.sub_unchecked source ~off:token.span.Ceibo.Span.start ~len

let text_slice = fun ~source token -> slice ~source token |> Slice.to_string

let contains_char = fun ~source token needle ->
  let slice = slice ~source token in
  let len = Slice.length slice in
  let rec loop index =
    if index >= len then
      false
    else if Slice.get_unchecked slice ~at:index = needle then
      true
    else
      loop (index + 1)
  in
  loop 0

let keyword_kind = function
  | Keyword.And -> Syntax_kind2.AND_KW
  | Keyword.As -> Syntax_kind2.AS_KW
  | Keyword.Asr -> Syntax_kind2.OPERATOR_KW
  | Keyword.Assert -> Syntax_kind2.ASSERT_KW
  | Keyword.Begin -> Syntax_kind2.BEGIN_KW
  | Keyword.Class -> Syntax_kind2.CLASS_KW
  | Keyword.Constraint -> Syntax_kind2.CONSTRAINT_KW
  | Keyword.Do -> Syntax_kind2.DO_KW
  | Keyword.Done -> Syntax_kind2.DONE_KW
  | Keyword.Downto -> Syntax_kind2.DOWNTO_KW
  | Keyword.Else -> Syntax_kind2.ELSE_KW
  | Keyword.End -> Syntax_kind2.END_KW
  | Keyword.Exception -> Syntax_kind2.EXCEPTION_KW
  | Keyword.External -> Syntax_kind2.EXTERNAL_KW
  | Keyword.False -> Syntax_kind2.FALSE_KW
  | Keyword.For -> Syntax_kind2.FOR_KW
  | Keyword.Fun -> Syntax_kind2.FUN_KW
  | Keyword.Function -> Syntax_kind2.FUNCTION_KW
  | Keyword.Functor -> Syntax_kind2.FUNCTOR_KW
  | Keyword.If -> Syntax_kind2.IF_KW
  | Keyword.In -> Syntax_kind2.IN_KW
  | Keyword.Include -> Syntax_kind2.INCLUDE_KW
  | Keyword.Inherit -> Syntax_kind2.INHERIT_KW
  | Keyword.Initializer -> Syntax_kind2.INITIALIZER_KW
  | Keyword.Land -> Syntax_kind2.OPERATOR_KW
  | Keyword.Lazy -> Syntax_kind2.LAZY_KW
  | Keyword.Let -> Syntax_kind2.LET_KW
  | Keyword.Lor -> Syntax_kind2.OPERATOR_KW
  | Keyword.Lsl -> Syntax_kind2.OPERATOR_KW
  | Keyword.Lsr -> Syntax_kind2.OPERATOR_KW
  | Keyword.Lxor -> Syntax_kind2.OPERATOR_KW
  | Keyword.Lnot -> Syntax_kind2.OPERATOR_KW
  | Keyword.Match -> Syntax_kind2.MATCH_KW
  | Keyword.Method -> Syntax_kind2.METHOD_KW
  | Keyword.Mod -> Syntax_kind2.OPERATOR_KW
  | Keyword.Module -> Syntax_kind2.MODULE_KW
  | Keyword.Mutable -> Syntax_kind2.MUTABLE_KW
  | Keyword.New -> Syntax_kind2.NEW_KW
  | Keyword.Nonrec -> Syntax_kind2.NONREC_KW
  | Keyword.Object -> Syntax_kind2.OBJECT_KW
  | Keyword.Of -> Syntax_kind2.OF_KW
  | Keyword.Open -> Syntax_kind2.OPEN_KW
  | Keyword.Or -> Syntax_kind2.OPERATOR_KW
  | Keyword.Private -> Syntax_kind2.PRIVATE_KW
  | Keyword.Rec -> Syntax_kind2.REC_KW
  | Keyword.Sig -> Syntax_kind2.SIG_KW
  | Keyword.Struct -> Syntax_kind2.STRUCT_KW
  | Keyword.Then -> Syntax_kind2.THEN_KW
  | Keyword.To -> Syntax_kind2.TO_KW
  | Keyword.True -> Syntax_kind2.TRUE_KW
  | Keyword.Try -> Syntax_kind2.TRY_KW
  | Keyword.Type -> Syntax_kind2.TYPE_KW
  | Keyword.Val -> Syntax_kind2.VAL_KW
  | Keyword.Virtual -> Syntax_kind2.VIRTUAL_KW
  | Keyword.When -> Syntax_kind2.WHEN_KW
  | Keyword.While -> Syntax_kind2.WHILE_KW
  | Keyword.With -> Syntax_kind2.WITH_KW

let open_delim_kind = function
  | Token.Paren -> Syntax_kind2.LPAREN
  | Token.Brace -> Syntax_kind2.LBRACE
  | Token.Bracket -> Syntax_kind2.LBRACKET
  | Token.Array -> Syntax_kind2.LBRACKET_BAR
  | Token.BeginEnd -> Syntax_kind2.BEGIN_KW
  | Token.StructEnd -> Syntax_kind2.STRUCT_KW
  | Token.SigEnd -> Syntax_kind2.SIG_KW
  | Token.ObjectEnd -> Syntax_kind2.OBJECT_KW

let close_delim_kind = function
  | Token.Paren -> Syntax_kind2.RPAREN
  | Token.Brace -> Syntax_kind2.RBRACE
  | Token.Bracket -> Syntax_kind2.RBRACKET
  | Token.Array -> Syntax_kind2.BAR_RBRACKET
  | Token.BeginEnd
  | Token.StructEnd
  | Token.SigEnd
  | Token.ObjectEnd -> Syntax_kind2.END_KW

let kind_of_token_kind = function
  | Token.Keyword keyword -> keyword_kind keyword
  | Token.Ident _ -> Syntax_kind2.IDENT
  | Token.Literal (Token.Int _) -> Syntax_kind2.INT
  | Token.Literal (Token.Float _) -> Syntax_kind2.FLOAT
  | Token.Literal (Token.String _) -> Syntax_kind2.STRING
  | Token.Literal (Token.Char _) -> Syntax_kind2.CHAR
  | Token.OpenDelim delimiter -> open_delim_kind delimiter
  | Token.CloseDelim delimiter -> close_delim_kind delimiter
  | Token.Comment _ -> Syntax_kind2.COMMENT
  | Token.Docstring _ -> Syntax_kind2.DOCSTRING
  | Token.Whitespace -> Syntax_kind2.WHITESPACE
  | Token.Plus -> Syntax_kind2.PLUS
  | Token.Minus -> Syntax_kind2.MINUS
  | Token.Star -> Syntax_kind2.STAR
  | Token.Slash -> Syntax_kind2.SLASH
  | Token.Percent -> Syntax_kind2.PERCENT
  | Token.Caret -> Syntax_kind2.CARET
  | Token.Eq -> Syntax_kind2.EQ
  | Token.Lt -> Syntax_kind2.LT
  | Token.Gt -> Syntax_kind2.GT
  | Token.LtEq -> Syntax_kind2.LTE
  | Token.GtEq -> Syntax_kind2.GTE
  | Token.Ne -> Syntax_kind2.NE
  | Token.Bang -> Syntax_kind2.BANG
  | Token.And -> Syntax_kind2.AMPAMP
  | Token.Or -> Syntax_kind2.BARBAR
  | Token.Colon -> Syntax_kind2.COLON
  | Token.Semi -> Syntax_kind2.SEMI
  | Token.Comma -> Syntax_kind2.COMMA
  | Token.Dot -> Syntax_kind2.DOT
  | Token.DotDot -> Syntax_kind2.DOTDOT
  | Token.Arrow -> Syntax_kind2.ARROW
  | Token.LeftArrow -> Syntax_kind2.LEFT_ARROW
  | Token.FatArrow -> Syntax_kind2.FAT_ARROW
  | Token.ColonColon -> Syntax_kind2.COLONCOLON
  | Token.ColonEq -> Syntax_kind2.COLONEQ
  | Token.Question -> Syntax_kind2.QUESTION
  | Token.At -> Syntax_kind2.AT
  | Token.Hash -> Syntax_kind2.HASH
  | Token.Tilde -> Syntax_kind2.TILDE
  | Token.Dollar -> Syntax_kind2.DOLLAR
  | Token.Pipe -> Syntax_kind2.PIPE
  | Token.Ampersand -> Syntax_kind2.AMPERSAND
  | Token.Underscore -> Syntax_kind2.UNDERSCORE
  | Token.Backtick -> Syntax_kind2.BACKTICK
  | Token.Quote -> Syntax_kind2.QUOTE
  | Token.StarStar -> Syntax_kind2.STARSTAR
  | Token.EqEq -> Syntax_kind2.EQEQ
  | Token.BangEq -> Syntax_kind2.BANGEQ
  | Token.AtAt -> Syntax_kind2.ATAT
  | Token.PipeGt -> Syntax_kind2.PIPEGT
  | Token.PercentGt -> Syntax_kind2.PERCENTGT
  | Token.LtPercent -> Syntax_kind2.LTPERCENT
  | Token.PlusDot -> Syntax_kind2.PLUSDOT
  | Token.MinusDot -> Syntax_kind2.MINUSDOT
  | Token.StarDot -> Syntax_kind2.STARDOT
  | Token.SlashDot -> Syntax_kind2.SLASHDOT
  | Token.EOF -> Syntax_kind2.EOF
  | Token.Unknown _ -> Syntax_kind2.UNKNOWN

let kind_of_trivia_kind = function
  | Token.WhitespaceTrivia -> Syntax_kind2.WHITESPACE
  | Token.CommentTrivia _ -> Syntax_kind2.COMMENT
  | Token.DocstringTrivia _ -> Syntax_kind2.DOCSTRING

let raw_of_trivia = fun (trivia: Token.trivia) ->
  {
    kind = kind_of_trivia_kind trivia.kind;
    span = trivia.span;
    legacy_kind = Token.token_kind_of_trivia_kind trivia.kind
  }

let raw_of_token = fun (token: Token.t) ->
  { kind = kind_of_token_kind token.kind; span = token.span; legacy_kind = token.kind }

let of_lexer_tokens = fun tokens ->
  let token_count = List.length tokens in
  let stream = create_stream_with_capacity ~raw:(token_count * 2) ~significant:token_count in
  List.for_each tokens
    ~fn:(fun token ->
      List.for_each
        token.Token.leading_trivia
        ~fn:(fun trivia -> ignore (push stream (raw_of_trivia trivia)));
      ignore (push_significant stream (raw_of_token token)));
  stream
