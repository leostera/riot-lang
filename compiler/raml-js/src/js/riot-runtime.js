export function print_endline(value) {
  console.log(value);
}

export function print_newline() {
  console.log("");
}

export function print_int(value) {
  process.stdout.write(String(value));
}

export function print_string(value) {
  process.stdout.write(String(value));
}

export function print_char(value) {
  process.stdout.write(String(value));
}

export function makeCurried(fn, arity = fn.length) {
  function curried(...args) {
    if (args.length >= arity) {
      const result = fn(...args.slice(0, arity));
      if (args.length === arity) {
        return result;
      }
      if (typeof result !== "function") {
        throw new Error(
          `Cannot apply ${args.length} arguments to a Riot JS function of arity ${arity}`
        );
      }
      return makeCurried(result)(...args.slice(arity));
    }

    return makeCurried(fn.bind(null, ...args), arity - args.length);
  }

  return curried;
}

export function callPrimitive(name, ...args) {
  switch (name) {
    case "%addfloat":
      return args[0] + args[1];
    case "%subfloat":
      return args[0] - args[1];
    case "%mulfloat":
      return args[0] * args[1];
    case "%divfloat":
      return args[0] / args[1];
    case "%addint":
      return args[0] + args[1];
    case "%subint":
      return args[0] - args[1];
    case "%mulint":
      return args[0] * args[1];
    case "%divint":
      return args[0] / args[1];
    case "%modint":
      return args[0] % args[1];
    case "%concatstring":
      return String(args[0]) + String(args[1]);
    case "%string_of_int":
      return String(args[0]);
    case "%string_of_float":
      return String(args[0]);
    case "%int_of_string":
      if (typeof args[0] !== "string" || !/^[+-]?[0-9]+$/.test(args[0])) {
        throw new Error(`Unsupported Riot JS int_of_string input: ${args[0]}`);
      }
      return Number.parseInt(args[0], 10);
    case "%float_of_string":
      if (
        typeof args[0] !== "string" ||
        !/^[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?$/.test(args[0])
      ) {
        throw new Error(`Unsupported Riot JS float_of_string input: ${args[0]}`);
      }
      return Number.parseFloat(args[0]);
    case "%eq":
      return args[0] === args[1];
    case "%neq":
      return args[0] !== args[1];
    case "%lt":
      return args[0] < args[1];
    case "%le":
      return args[0] <= args[1];
    case "%gt":
      return args[0] > args[1];
    case "%ge":
      return args[0] >= args[1];
    case "%sqrtfloat":
      return Math.sqrt(args[0]);
    case "%tuple_make":
      return args;
    case "%tuple_get":
      return args[0][args[1]];
    case "%trace":
      console.log(args[0]);
      return undefined;
    default:
      throw new Error(`Unsupported Riot JS primitive: ${name}`);
  }
}
