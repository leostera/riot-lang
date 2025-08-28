open AndesOCamlAst

type delimiter = Parenthesis | Brace | Bracket

type t = token_tree list
and token_tree = Token of Token.t | Delimited of delimiter * t
