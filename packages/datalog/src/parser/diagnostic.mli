type t = { message : string; span : Ceibo.Span.t; help : string option }

val make : message:string -> span:Ceibo.Span.t -> help:string option -> t
val expected : expected:string -> found:Token.located -> span:Ceibo.Span.t -> t
val unexpected : found:Token.located -> span:Ceibo.Span.t -> t
val unterminated_string : span:Ceibo.Span.t -> t
val lowercase_variable : name:string -> span:Ceibo.Span.t -> t
val missing_rule_body : span:Ceibo.Span.t -> t
val unclosed_paren : span:Ceibo.Span.t -> t
val missing_statement_terminator : span:Ceibo.Span.t -> t
val missing_closing_paren : span:Ceibo.Span.t -> t
val to_string : t -> string
