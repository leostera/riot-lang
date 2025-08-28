
rustc_parse: source_file_to_stream
  lexer::lex_token_trees
    * creates a cursor
      * Cursor produces char
      * Cursor advance_token produces Token(Kind+Pos)
    * creates a string_reader (cursor)
    tokentrees::TokenTreesReader::lex_all_token_trees(string_reader)
    

