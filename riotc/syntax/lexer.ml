// Tokenization for the RiotML bootstrap subset.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use String.{length, get, slice}

type SourceSpan = SourceSpan {
  start_codepoint: i32,
  end_codepoint: i32
}

type TokenInfo = TokenInfo {
  lexeme: string,
  span: SourceSpan
}

type LexError =
  | UnterminatedString(SourceSpan)
  | UnterminatedEscape(SourceSpan)

type StringScanState = StringScanState {
  source: string,
  string_start: i32,
  codepoint_offset: i32
}

type Token =
  | IdentifierToken(TokenInfo)
  | IntegerToken(TokenInfo)
  | TextToken(TokenInfo)
  | SymbolToken(TokenInfo)

type ScanCursor = ScanCursor {
  source: string,
  offset: i32
}

type ScanState = ScanState {
  cursor: ScanCursor,
  reversed_tokens: List<Token>
}

type ChunkRequest = ChunkRequest {
  state: ScanState,
  character_budget: i32
}

type ScanCharacter = ScanCharacter {
  source: string,
  offset: i32,
  reversed_tokens: List<Token>,
  character_budget: i32,
  codepoint: i32
}

type ScanAdvance = ScanAdvance {
  source: string,
  offset: i32,
  reversed_tokens: List<Token>,
  character_budget: i32
}

type ScanChunk =
  | Continue(ScanState)
  | Complete(List<Token>)
  | Failed(LexError)

let is_space = fn code ->
  match code {
    | 9 -> true
    | 10 -> true
    | 13 -> true
    | 32 -> true
    | _ -> false
  }

let is_digit = fn code ->
  match (code >= 48) {
    | true -> code <= 57
    | false -> false
  }

let is_lower_or_underscore = fn code ->
  match (code == 95) {
    | true -> true
    | false -> match (code >= 97) {
      | true -> code <= 122
      | false -> false
    }
  }

let is_letter = fn code ->
  match (code >= 65) {
    | true -> match (code <= 90) {
      | true -> true
      | false -> is_lower_or_underscore code
    }
    | false -> is_lower_or_underscore code
  }

let is_word_character = fn code ->
  match (is_letter code) {
    | true -> true
    | false -> is_digit code
  }

let make_token_info = fn source -> fn start_codepoint -> fn end_codepoint -> TokenInfo {
  lexeme: slice source start_codepoint end_codepoint,
  span: SourceSpan {
    start_codepoint: start_codepoint,
    end_codepoint: end_codepoint
  }
}

let scan_digits = fn (ScanCursor { source, offset }) ->
  match (get source offset) {
    | Some(code) -> match (is_digit code) {
      | true -> scan_digits (ScanCursor { source: source, offset: offset + 1 })
      | false -> offset
    }
    | None -> offset
  }

let scan_word = fn (ScanCursor { source, offset }) ->
  match (get source offset) {
    | Some(code) -> match (is_word_character code) {
      | true -> scan_word (ScanCursor { source: source, offset: offset + 1 })
      | false -> offset
    }
    | None -> offset
  }

let scan_string = fn (StringScanState { source, string_start, codepoint_offset }) ->
  match (get source codepoint_offset) {
    | Some(34) -> Ok(codepoint_offset + 1)
    | Some(92) -> match (get source (codepoint_offset + 1)) {
      | Some(_) -> scan_string (StringScanState {
        source: source,
        string_start: string_start,
        codepoint_offset: codepoint_offset + 2
      })
      | None -> Err(UnterminatedEscape(SourceSpan {
        start_codepoint: string_start,
        end_codepoint: codepoint_offset + 1
      }))
    }
    | Some(_) -> scan_string (StringScanState {
      source: source,
      string_start: string_start,
      codepoint_offset: codepoint_offset + 1
    })
    | None -> Err(UnterminatedString(SourceSpan {
      start_codepoint: string_start,
      end_codepoint: codepoint_offset
    }))
  }

let skip_line = fn (ScanCursor { source, offset }) ->
  match (get source offset) {
    | Some(10) -> offset + 1
    | Some(_) -> skip_line (ScanCursor { source: source, offset: offset + 1 })
    | None -> offset
  }

let two_character_operator = fn (ScanCursor { source, offset }) ->
  match (get source offset) {
    | Some(45) -> match (get source (offset + 1)) {
      | Some(62) -> "->"
      | _ -> ""
    }
    | Some(61) -> match (get source (offset + 1)) {
      | Some(61) -> "=="
      | _ -> ""
    }
    | Some(33) -> match (get source (offset + 1)) {
      | Some(61) -> "!="
      | _ -> ""
    }
    | Some(60) -> match (get source (offset + 1)) {
      | Some(61) -> "<="
      | _ -> ""
    }
    | Some(62) -> match (get source (offset + 1)) {
      | Some(61) -> ">="
      | _ -> ""
    }
    | _ -> ""
  }

let reverse_token_accumulator = fn remaining -> fn reversed ->
  match remaining {
    | Nil -> reversed
    | Cons(token, rest) -> reverse_token_accumulator rest (Cons(token, reversed))
  }

let reverse_tokens = fn tokens -> reverse_token_accumulator tokens Nil

let scan_chunk = fn (ChunkRequest {
  state: ScanState {
    cursor: ScanCursor { source, offset },
    reversed_tokens
  },
  character_budget
}) ->
  match (character_budget <= 0) {
    | true -> Continue(ScanState {
      cursor: ScanCursor { source: source, offset: offset },
      reversed_tokens: reversed_tokens
    })
    | false -> match (get source offset) {
      | None -> Complete(reverse_tokens reversed_tokens)
      | Some(codepoint) -> scan_character (ScanCharacter {
        source: source,
        offset: offset,
        reversed_tokens: reversed_tokens,
        character_budget: character_budget,
        codepoint: codepoint
      })
    }
  }

let scan_character = fn (ScanCharacter {
  source,
  offset,
  reversed_tokens,
  character_budget,
  codepoint
}) ->
  match (is_space codepoint) {
    | true -> advance_scan (ScanAdvance {
      source: source,
      offset: offset + 1,
      reversed_tokens: reversed_tokens,
      character_budget: character_budget
    })
    | false -> match (codepoint == 34) {
      | true -> scan_text_token (ScanCharacter {
        source: source,
        offset: offset,
        reversed_tokens: reversed_tokens,
        character_budget: character_budget,
        codepoint: codepoint
      })
      | false -> match (is_digit codepoint) {
        | true -> scan_integer_token (ScanCharacter {
          source: source,
          offset: offset,
          reversed_tokens: reversed_tokens,
          character_budget: character_budget,
          codepoint: codepoint
        })
        | false -> match (is_letter codepoint) {
          | true -> scan_identifier_token (ScanCharacter {
            source: source,
            offset: offset,
            reversed_tokens: reversed_tokens,
            character_budget: character_budget,
            codepoint: codepoint
          })
          | false -> scan_symbol (ScanCharacter {
            source: source,
            offset: offset,
            reversed_tokens: reversed_tokens,
            character_budget: character_budget,
            codepoint: codepoint
          })
        }
      }
    }
  }

let advance_scan = fn (ScanAdvance {
  source,
  offset,
  reversed_tokens,
  character_budget
}) ->
  scan_chunk (ChunkRequest {
    state: ScanState {
      cursor: ScanCursor { source: source, offset: offset },
      reversed_tokens: reversed_tokens
    },
    character_budget: character_budget - 1
  })

let scan_text_token = fn (ScanCharacter {
  source,
  offset,
  reversed_tokens,
  character_budget,
  codepoint: _
}) ->
  match (scan_string (StringScanState {
    source: source,
    string_start: offset,
    codepoint_offset: offset + 1
  })) {
    | Err(error) -> Failed(error)
    | Ok(end_offset) -> {
      let token = TextToken(make_token_info source offset end_offset);
      advance_scan (ScanAdvance {
        source: source,
        offset: end_offset,
        reversed_tokens: Cons(token, reversed_tokens),
        character_budget: character_budget
      })
    }
  }

let scan_integer_token = fn (ScanCharacter {
  source,
  offset,
  reversed_tokens,
  character_budget,
  codepoint: _
}) -> {
  let end_offset = scan_digits (ScanCursor { source: source, offset: offset });
  let token = IntegerToken(make_token_info source offset end_offset);
  advance_scan (ScanAdvance {
    source: source,
    offset: end_offset,
    reversed_tokens: Cons(token, reversed_tokens),
    character_budget: character_budget
  })
}

let scan_identifier_token = fn (ScanCharacter {
  source,
  offset,
  reversed_tokens,
  character_budget,
  codepoint: _
}) -> {
  let end_offset = scan_word (ScanCursor { source: source, offset: offset });
  let token = IdentifierToken(make_token_info source offset end_offset);
  advance_scan (ScanAdvance {
    source: source,
    offset: end_offset,
    reversed_tokens: Cons(token, reversed_tokens),
    character_budget: character_budget
  })
}

let scan_symbol = fn (ScanCharacter {
  source,
  offset,
  reversed_tokens,
  character_budget,
  codepoint
}) ->
  match (codepoint == 47) {
    | true -> match (get source (offset + 1)) {
      | Some(47) -> advance_scan (ScanAdvance {
        source: source,
        offset: skip_line (ScanCursor { source: source, offset: offset + 2 }),
        reversed_tokens: reversed_tokens,
        character_budget: character_budget
      })
      | _ -> scan_operator_or_punctuation (ScanCharacter {
        source: source,
        offset: offset,
        reversed_tokens: reversed_tokens,
        character_budget: character_budget,
        codepoint: codepoint
      })
    }
    | false -> scan_operator_or_punctuation (ScanCharacter {
      source: source,
      offset: offset,
      reversed_tokens: reversed_tokens,
      character_budget: character_budget,
      codepoint: codepoint
    })
  }

let scan_operator_or_punctuation = fn (ScanCharacter {
  source,
  offset,
  reversed_tokens,
  character_budget,
  codepoint: _
}) -> {
  let cursor = ScanCursor { source: source, offset: offset };
  let operator = two_character_operator cursor;
  match (length operator > 0) {
    | true -> append_symbol (ScanCharacter {
      source: source,
      offset: offset,
      reversed_tokens: reversed_tokens,
      character_budget: character_budget,
      codepoint: 0
    }) (offset + 2)
    | false -> append_symbol (ScanCharacter {
      source: source,
      offset: offset,
      reversed_tokens: reversed_tokens,
      character_budget: character_budget,
      codepoint: 0
    }) (offset + 1)
  }
}

let append_symbol = fn (ScanCharacter {
  source,
  offset,
  reversed_tokens,
  character_budget,
  codepoint: _
}) -> fn next_offset -> {
  let token = SymbolToken(make_token_info source offset next_offset);
  advance_scan (ScanAdvance {
    source: source,
    offset: next_offset,
    reversed_tokens: Cons(token, reversed_tokens),
    character_budget: character_budget
  })
}

let scan_all = fn state ->
  match (scan_chunk (ChunkRequest { state: state, character_budget: 24 })) {
    | Complete(tokens) -> Ok(tokens)
    | Continue(next_state) -> scan_all next_state
    | Failed(error) -> Err(error)
  }

let lex = fn source ->
  scan_all (ScanState {
    cursor: ScanCursor { source: source, offset: 0 },
    reversed_tokens: Nil
  })
