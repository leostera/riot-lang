// Decoding helpers for source literals shared by expression and pattern parsing.
use Prelude.{List, Nil, Cons, Option, Some, None}
use String.{length, get, slice}

type StringDecodeState = StringDecodeState {
  contents: string,
  end_offset: i32,
  offset: i32,
  reversed_pieces: List<string>
}

let join_string_pieces = fn (reversed_pieces: List<string>) -> fn (accumulated: string) ->
  match reversed_pieces {
    | Nil -> accumulated
    | Cons(piece, remaining) -> join_string_pieces remaining (accumulated + piece)
  }

let decode_string = fn (StringDecodeState {
  contents,
  end_offset,
  offset,
  reversed_pieces
}) ->
  match (offset >= end_offset) {
    | true -> join_string_pieces reversed_pieces ""
    | false -> match (get contents offset) {
      | Some(92) -> match (get contents (offset + 1)) {
        | Some(110) -> decode_string (StringDecodeState {
          contents: contents,
          end_offset: end_offset,
          offset: offset + 2,
          reversed_pieces: Cons("\n", reversed_pieces)
        })
        | Some(114) -> decode_string (StringDecodeState {
          contents: contents,
          end_offset: end_offset,
          offset: offset + 2,
          reversed_pieces: Cons("\r", reversed_pieces)
        })
        | Some(116) -> decode_string (StringDecodeState {
          contents: contents,
          end_offset: end_offset,
          offset: offset + 2,
          reversed_pieces: Cons("\t", reversed_pieces)
        })
        | Some(34) -> decode_string (StringDecodeState {
          contents: contents,
          end_offset: end_offset,
          offset: offset + 2,
          reversed_pieces: Cons("\"", reversed_pieces)
        })
        | Some(92) -> decode_string (StringDecodeState {
          contents: contents,
          end_offset: end_offset,
          offset: offset + 2,
          reversed_pieces: Cons("\\", reversed_pieces)
        })
        | Some(48) -> decode_string (StringDecodeState {
          contents: contents,
          end_offset: end_offset,
          offset: offset + 2,
          reversed_pieces: Cons("\0", reversed_pieces)
        })
        | Some(_) -> decode_string (StringDecodeState {
          contents: contents,
          end_offset: end_offset,
          offset: offset + 2,
          reversed_pieces: Cons(slice contents offset (offset + 2), reversed_pieces)
        })
        | None -> decode_string (StringDecodeState {
          contents: contents,
          end_offset: end_offset,
          offset: offset + 1,
          reversed_pieces: Cons("\\", reversed_pieces)
        })
      }
      | Some(_) -> decode_string (StringDecodeState {
        contents: contents,
        end_offset: end_offset,
        offset: offset + 1,
        reversed_pieces: Cons(slice contents offset (offset + 1), reversed_pieces)
      })
      | None -> join_string_pieces reversed_pieces ""
    }
  }

let decode_string_literal = fn lexeme -> {
  let content_length = length lexeme - 1;
  decode_string (StringDecodeState {
    contents: slice lexeme 1 content_length,
    end_offset: content_length - 1,
    offset: 0,
    reversed_pieces: Nil
  })
}
