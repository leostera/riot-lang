// Minimal owned Printf surface for the current JS backend slices.

function isDigit(char) {
  return char >= "0" && char <= "9";
}

function decodeEscapes(text) {
  let output = "";

  for (let i = 0; i < text.length; i += 1) {
    const char = text[i];
    if (char !== "\\" || i + 1 >= text.length) {
      output += char;
      continue;
    }

    i += 1;
    switch (text[i]) {
      case "\\":
        output += "\\";
        break;
      case "n":
        output += "\n";
        break;
      case "r":
        output += "\r";
        break;
      case "t":
        output += "\t";
        break;
      case "\"":
        output += "\"";
        break;
      default:
        output += `\\${text[i]}`;
        break;
    }
  }

  return output;
}

function renderFormattedValue(specifier, precision, value) {
  switch (specifier) {
    case "b":
      return value ? "true" : "false";
    case "c":
      return String(value);
    case "d":
    case "i":
      return String(Math.trunc(Number(value)));
    case "f": {
      const number = Number(value);
      return precision === null ? String(number) : number.toFixed(precision);
    }
    case "s":
      return String(value);
    case "S":
      return JSON.stringify(String(value));
    case "u":
      return String(Math.trunc(Number(value)) >>> 0);
    default:
      throw new Error(`Unsupported Riot JS Printf specifier: %${specifier}`);
  }
}

function formatString(format, values) {
  const decodedFormat = decodeEscapes(format);
  let output = "";
  let argIndex = 0;

  for (let i = 0; i < decodedFormat.length; i += 1) {
    const char = decodedFormat[i];
    if (char !== "%") {
      output += char;
      continue;
    }

    i += 1;
    if (i >= decodedFormat.length) {
      throw new Error("Unterminated Riot JS Printf format string");
    }

    if (decodedFormat[i] === "%") {
      output += "%";
      continue;
    }

    while (i < decodedFormat.length && "-+ #0".includes(decodedFormat[i])) {
      i += 1;
    }

    while (i < decodedFormat.length && isDigit(decodedFormat[i])) {
      i += 1;
    }

    let precision = null;
    if (decodedFormat[i] === ".") {
      i += 1;
      const precision_start = i;
      while (i < decodedFormat.length && isDigit(decodedFormat[i])) {
        i += 1;
      }
      precision =
        precision_start === i
          ? 0
          : Number(decodedFormat.slice(precision_start, i));
    }

    const specifier = decodedFormat[i];
    if (specifier == null) {
      throw new Error("Unterminated Riot JS Printf format string");
    }

    if (argIndex >= values.length) {
      throw new Error(
        `Missing Riot JS Printf argument for %${specifier} in ${decodedFormat}`
      );
    }

    output += renderFormattedValue(specifier, precision, values[argIndex]);
    argIndex += 1;
  }

  return output;
}

function writeString(text) {
  if (
    typeof process !== "undefined" &&
    process != null &&
    process.stdout != null &&
    typeof process.stdout.write === "function"
  ) {
    process.stdout.write(text);
    return;
  }

  if (text.endsWith("\n")) {
    console.log(text.slice(0, -1));
    return;
  }

  console.log(text);
}

export function sprintf(format, ...values) {
  return formatString(format, values);
}

export function printf(format, ...values) {
  writeString(sprintf(format, ...values));
  return undefined;
}
