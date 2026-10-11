function requireString(value, operation) {
  if (typeof value !== "string") {
    throw new TypeError(`String.${operation} expects a string`);
  }
  return Array.from(value);
}

function integerIndex(value, operation) {
  if (typeof value !== "bigint") {
    throw new TypeError(`String.${operation} expects an i32 index`);
  }
  const number = Number(value);
  if (!Number.isSafeInteger(number)) {
    throw new RangeError(`String.${operation} index is outside the safe range`);
  }
  return number;
}

const concat = (left) => (right) => {
  if (typeof left !== "string" || typeof right !== "string") {
    throw new TypeError("String.concat expects strings");
  }
  return left + right;
};

const length = (value) => BigInt(requireString(value, "length").length);

const get = (value) => (index) => {
  const characters = requireString(value, "get");
  const position = integerIndex(index, "get");
  if (position < 0 || position >= characters.length) return { type: "None" };
  return { type: "Some", value: BigInt(characters[position].codePointAt(0)) };
};

const slice = (value) => (start) => (end) => {
  const characters = requireString(value, "slice");
  const first = Math.max(0, Math.min(characters.length, integerIndex(start, "slice")));
  const last = Math.max(first, Math.min(characters.length, integerIndex(end, "slice")));
  return characters.slice(first, last).join("");
};

const equal = (left) => (right) => {
  if (typeof left !== "string" || typeof right !== "string") {
    throw new TypeError("String.equal expects strings");
  }
  return left === right;
};

export { concat, length, get, slice, equal };
