//! Source-position conversion helpers for LSP.
//!
//! TinyML parser/checker spans are byte offsets into UTF-8 source text. LSP
//! ranges use zero-based line numbers and UTF-16 code-unit columns, so every
//! diagnostic/hint/hover range passes through this module.

use crate::parser::lexer::Span;
use tower_lsp::lsp_types::{Position, Range};

pub fn span_to_range(source: &str, span: Span) -> Range {
    Range {
        start: byte_to_position(source, span.start),
        end: byte_to_position(source, span.end),
    }
}

pub fn byte_to_position(source: &str, byte: usize) -> Position {
    let byte = byte.min(source.len());
    let mut line = 0u32;
    let mut line_start = 0usize;

    for (idx, ch) in source.char_indices() {
        if idx >= byte {
            break;
        }
        if ch == '\n' {
            line += 1;
            line_start = idx + ch.len_utf8();
        }
    }

    let character = source[line_start..byte]
        .encode_utf16()
        .count()
        .try_into()
        .unwrap_or(u32::MAX);

    Position { line, character }
}

pub fn position_to_byte(source: &str, position: Position) -> usize {
    let mut line = 0u32;
    let mut line_start = 0usize;

    for (idx, ch) in source.char_indices() {
        if line == position.line {
            break;
        }
        if ch == '\n' {
            line += 1;
            line_start = idx + ch.len_utf8();
        }
    }

    if line != position.line {
        return source.len();
    }

    let mut utf16 = 0u32;
    for (rel, ch) in source[line_start..].char_indices() {
        if ch == '\n' || utf16 >= position.character {
            return line_start + rel;
        }
        utf16 += ch.len_utf16() as u32;
        if utf16 > position.character {
            return line_start + rel;
        }
    }

    source.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn converts_byte_spans_to_utf16_ranges() {
        let source = "let x = \"λ\"\nlet y = x";
        let start = source.find('y').expect("y");
        let range = span_to_range(source, Span::new(start, start + 1));
        assert_eq!(range.start.line, 1);
        assert_eq!(range.start.character, 4);
        assert_eq!(range.end.character, 5);
    }

    #[test]
    fn converts_lsp_position_to_byte_offset() {
        let source = "let λ = 1\nlet x = λ";
        let byte = position_to_byte(source, Position::new(1, 8));
        assert_eq!(&source[byte..byte + "λ".len()], "λ");
    }
}
