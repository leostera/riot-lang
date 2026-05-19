pub(super) fn parse_int_literal(raw: &str) -> Result<i64, String> {
    let normalized = raw.replace('_', "");
    if let Some(hex) = normalized
        .strip_prefix("0x")
        .or_else(|| normalized.strip_prefix("0X"))
    {
        i64::from_str_radix(hex, 16)
            .map_err(|error| format!("invalid hex integer literal: {error}"))
    } else if let Some(binary) = normalized
        .strip_prefix("0b")
        .or_else(|| normalized.strip_prefix("0B"))
    {
        i64::from_str_radix(binary, 2)
            .map_err(|error| format!("invalid binary integer literal: {error}"))
    } else {
        normalized
            .parse::<i64>()
            .map_err(|error| format!("invalid integer literal: {error}"))
    }
}

pub(super) fn parse_char_literal(raw: &str) -> Result<char, String> {
    let body = raw
        .strip_prefix('\'')
        .and_then(|body| body.strip_suffix('\''))
        .ok_or_else(|| "invalid character literal".to_owned())?;

    if let Some(escaped) = body.strip_prefix('\\') {
        match escaped {
            "\\" => Ok('\\'),
            "'" => Ok('\''),
            "n" => Ok('\n'),
            "r" => Ok('\r'),
            "t" => Ok('\t'),
            other => Err(format!("unsupported character escape: \\{other}")),
        }
    } else {
        let mut chars = body.chars();
        let Some(value) = chars.next() else {
            return Err("empty character literal".to_owned());
        };
        if chars.next().is_some() {
            return Err("character literal contains more than one character".to_owned());
        }
        Ok(value)
    }
}
