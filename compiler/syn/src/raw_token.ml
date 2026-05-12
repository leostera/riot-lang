open Std
open Std.Collections

module Slice = IO.IoVec.IoSlice

type t = {
  kind: Syntax_kind.t;
  span: Span.t;
  legacy_kind: Token.token_kind;
  has_newline: bool;
}

type stream = {
  raw: t Vector.t;
  significant: int Vector.t;
}

let create_stream = fun () -> { raw = Vector.create (); significant = Vector.create () }

let create_stream_with_capacity = fun ~raw ~significant -> {
  raw = Vector.with_capacity ~size:raw;
  significant = Vector.with_capacity ~size:significant;
}

let push = fun stream token ->
  let index = Vector.length stream.raw in
  Vector.push stream.raw ~value:token;
  index

let push_significant = fun stream token ->
  let index = push stream token in
  Vector.push stream.significant ~value:index;
  index

let is_trivia = fun token -> Syntax_kind.is_trivia token.kind

let is_significant = fun token -> not (is_trivia token)

let width = fun token -> Span.width token.span

let has_newline = fun token -> token.has_newline

let slice = fun ~source token ->
  let len = width token in
  if len <= 0 then
    Slice.empty
  else
    Slice.sub_unchecked source ~off:token.span.Span.start ~len

let text_slice = fun ~source token ->
  slice ~source token
  |> Slice.to_string

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

let span_contains_char = fun ~source span needle ->
  let start = span.Span.start in
  let end_ = span.Span.end_ in
  let rec loop index =
    if Int.(index >= end_) then
      false
    else if Char.equal (Slice.get_unchecked source ~at:index) needle then
      true
    else
      loop Int.(index + 1)
  in
  if Int.(start < 0 || end_ > Slice.length source || end_ <= start) then
    false
  else
    loop start

let keyword_kind = fun __tmp1 ->
  match __tmp1 with
  | Keyword.And -> Syntax_kind.AND_KW
  | Keyword.As -> Syntax_kind.AS_KW
  | Keyword.Asr -> Syntax_kind.OPERATOR_KW
  | Keyword.Begin -> Syntax_kind.BEGIN_KW
  | Keyword.Constraint -> Syntax_kind.CONSTRAINT_KW
  | Keyword.Do -> Syntax_kind.DO_KW
  | Keyword.Done -> Syntax_kind.DONE_KW
  | Keyword.Downto -> Syntax_kind.DOWNTO_KW
  | Keyword.Else -> Syntax_kind.ELSE_KW
  | Keyword.End -> Syntax_kind.END_KW
  | Keyword.Exception -> Syntax_kind.EXCEPTION_KW
  | Keyword.External -> Syntax_kind.EXTERNAL_KW
  | Keyword.False -> Syntax_kind.FALSE_KW
  | Keyword.For -> Syntax_kind.FOR_KW
  | Keyword.Fun -> Syntax_kind.FUN_KW
  | Keyword.Function -> Syntax_kind.FUNCTION_KW
  | Keyword.Functor -> Syntax_kind.FUNCTOR_KW
  | Keyword.If -> Syntax_kind.IF_KW
  | Keyword.In -> Syntax_kind.IN_KW
  | Keyword.Include -> Syntax_kind.INCLUDE_KW
  | Keyword.Land -> Syntax_kind.OPERATOR_KW
  | Keyword.Let -> Syntax_kind.LET_KW
  | Keyword.Lor -> Syntax_kind.OPERATOR_KW
  | Keyword.Lsl -> Syntax_kind.OPERATOR_KW
  | Keyword.Lsr -> Syntax_kind.OPERATOR_KW
  | Keyword.Lxor -> Syntax_kind.OPERATOR_KW
  | Keyword.Lnot -> Syntax_kind.OPERATOR_KW
  | Keyword.Match -> Syntax_kind.MATCH_KW
  | Keyword.Mod -> Syntax_kind.OPERATOR_KW
  | Keyword.Module -> Syntax_kind.MODULE_KW
  | Keyword.Mutable -> Syntax_kind.MUTABLE_KW
  | Keyword.Nonrec -> Syntax_kind.NONREC_KW
  | Keyword.Of -> Syntax_kind.OF_KW
  | Keyword.Open -> Syntax_kind.OPEN_KW
  | Keyword.Or -> Syntax_kind.OPERATOR_KW
  | Keyword.Private -> Syntax_kind.PRIVATE_KW
  | Keyword.Rec -> Syntax_kind.REC_KW
  | Keyword.Sig -> Syntax_kind.SIG_KW
  | Keyword.Struct -> Syntax_kind.STRUCT_KW
  | Keyword.Then -> Syntax_kind.THEN_KW
  | Keyword.To -> Syntax_kind.TO_KW
  | Keyword.True -> Syntax_kind.TRUE_KW
  | Keyword.Try -> Syntax_kind.TRY_KW
  | Keyword.Type -> Syntax_kind.TYPE_KW
  | Keyword.Val -> Syntax_kind.VAL_KW
  | Keyword.When -> Syntax_kind.WHEN_KW
  | Keyword.While -> Syntax_kind.WHILE_KW
  | Keyword.With -> Syntax_kind.WITH_KW

let open_delim_kind = fun __tmp1 ->
  match __tmp1 with
  | Token.Paren -> Syntax_kind.LPAREN
  | Token.Brace -> Syntax_kind.LBRACE
  | Token.Bracket -> Syntax_kind.LBRACKET
  | Token.Array -> Syntax_kind.LBRACKET_BAR
  | Token.BeginEnd -> Syntax_kind.BEGIN_KW
  | Token.StructEnd -> Syntax_kind.STRUCT_KW
  | Token.SigEnd -> Syntax_kind.SIG_KW

let close_delim_kind = fun __tmp1 ->
  match __tmp1 with
  | Token.Paren -> Syntax_kind.RPAREN
  | Token.Brace -> Syntax_kind.RBRACE
  | Token.Bracket -> Syntax_kind.RBRACKET
  | Token.Array -> Syntax_kind.BAR_RBRACKET
  | Token.BeginEnd
  | Token.StructEnd
  | Token.SigEnd -> Syntax_kind.END_KW

let kind_of_token_kind = fun __tmp1 ->
  match __tmp1 with
  | Token.Keyword keyword -> keyword_kind keyword
  | Token.Ident _ -> Syntax_kind.IDENT
  | Token.Literal (Token.Int _) -> Syntax_kind.INT
  | Token.Literal (Token.Float _) -> Syntax_kind.FLOAT
  | Token.Literal (Token.String _) -> Syntax_kind.STRING
  | Token.Literal (Token.Char _) -> Syntax_kind.CHAR
  | Token.OpenDelim delimiter -> open_delim_kind delimiter
  | Token.CloseDelim delimiter -> close_delim_kind delimiter
  | Token.Comment _ -> Syntax_kind.COMMENT
  | Token.Docstring _ -> Syntax_kind.DOCSTRING
  | Token.Whitespace -> Syntax_kind.WHITESPACE
  | Token.Plus -> Syntax_kind.PLUS
  | Token.Minus -> Syntax_kind.MINUS
  | Token.Star -> Syntax_kind.STAR
  | Token.Slash -> Syntax_kind.SLASH
  | Token.Percent -> Syntax_kind.PERCENT
  | Token.Caret -> Syntax_kind.CARET
  | Token.Eq -> Syntax_kind.EQ
  | Token.Lt -> Syntax_kind.LT
  | Token.Gt -> Syntax_kind.GT
  | Token.LtEq -> Syntax_kind.LTE
  | Token.GtEq -> Syntax_kind.GTE
  | Token.Ne -> Syntax_kind.NE
  | Token.Bang -> Syntax_kind.BANG
  | Token.And -> Syntax_kind.AMPAMP
  | Token.Or -> Syntax_kind.BARBAR
  | Token.Colon -> Syntax_kind.COLON
  | Token.Semi -> Syntax_kind.SEMI
  | Token.Comma -> Syntax_kind.COMMA
  | Token.Dot -> Syntax_kind.DOT
  | Token.DotDot -> Syntax_kind.DOTDOT
  | Token.Arrow -> Syntax_kind.ARROW
  | Token.LeftArrow -> Syntax_kind.LEFT_ARROW
  | Token.FatArrow -> Syntax_kind.FAT_ARROW
  | Token.ColonColon -> Syntax_kind.COLONCOLON
  | Token.ColonEq -> Syntax_kind.COLONEQ
  | Token.Question -> Syntax_kind.QUESTION
  | Token.At -> Syntax_kind.AT
  | Token.Hash -> Syntax_kind.HASH
  | Token.Tilde -> Syntax_kind.TILDE
  | Token.Dollar -> Syntax_kind.DOLLAR
  | Token.Pipe -> Syntax_kind.PIPE
  | Token.Ampersand -> Syntax_kind.AMPERSAND
  | Token.Underscore -> Syntax_kind.UNDERSCORE
  | Token.Backtick -> Syntax_kind.BACKTICK
  | Token.Quote -> Syntax_kind.QUOTE
  | Token.StarStar -> Syntax_kind.STARSTAR
  | Token.EqEq -> Syntax_kind.EQEQ
  | Token.BangEq -> Syntax_kind.BANGEQ
  | Token.AtAt -> Syntax_kind.ATAT
  | Token.PipeGt -> Syntax_kind.PIPEGT
  | Token.PercentGt -> Syntax_kind.PERCENTGT
  | Token.LtPercent -> Syntax_kind.LTPERCENT
  | Token.PlusDot -> Syntax_kind.PLUSDOT
  | Token.MinusDot -> Syntax_kind.MINUSDOT
  | Token.StarDot -> Syntax_kind.STARDOT
  | Token.SlashDot -> Syntax_kind.SLASHDOT
  | Token.EOF -> Syntax_kind.EOF
  | Token.Unknown _ -> Syntax_kind.UNKNOWN

let kind_of_trivia_kind = fun __tmp1 ->
  match __tmp1 with
  | Token.WhitespaceTrivia -> Syntax_kind.WHITESPACE
  | Token.CommentTrivia _ -> Syntax_kind.COMMENT
  | Token.DocstringTrivia _ -> Syntax_kind.DOCSTRING

let raw_of_trivia = fun (trivia: Token.trivia) ->
  {
    kind = kind_of_trivia_kind trivia.kind;
    span = trivia.span;
    legacy_kind = Token.token_kind_of_trivia_kind trivia.kind;
    has_newline = false;
  }

let raw_of_token = fun (token: Token.t) ->
  {
    kind = kind_of_token_kind token.kind;
    span = token.span;
    legacy_kind = token.kind;
    has_newline = false;
  }

let from_lexer_tokens = fun ~source tokens ->
  let token_count = List.length tokens in
  let stream = create_stream_with_capacity ~raw:(token_count * 2) ~significant:token_count in
  let raw_of_trivia (trivia: Token.trivia) = {
    kind = kind_of_trivia_kind trivia.kind;
    span = trivia.span;
    legacy_kind = Token.token_kind_of_trivia_kind trivia.kind;
    has_newline = span_contains_char ~source trivia.span '\n';
  }
  in
  let raw_of_token (token: Token.t) = {
    kind = kind_of_token_kind token.kind;
    span = token.span;
    legacy_kind = token.kind;
    has_newline = span_contains_char ~source token.span '\n';
  }
  in
  List.for_each
    tokens
    ~fn:(fun token ->
      List.for_each
        token.Token.leading_trivia
        ~fn:(fun trivia -> ignore (push stream (raw_of_trivia trivia)));
      ignore (push_significant stream (raw_of_token token)));
  stream
