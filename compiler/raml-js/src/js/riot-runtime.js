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
    case "add_float":
      return args[0] + args[1];
    case "subtract_float":
      return args[0] - args[1];
    case "multiply_float":
      return args[0] * args[1];
    case "divide_float":
      return args[0] / args[1];
    case "add_int":
      return args[0] + args[1];
    case "subtract_int":
      return args[0] - args[1];
    case "multiply_int":
      return args[0] * args[1];
    case "divide_int":
      return args[0] / args[1];
    case "modulo_int":
      return args[0] % args[1];
    case "concatenate_string":
      return String(args[0]) + String(args[1]);
    case "int_to_string":
      return String(args[0]);
    case "float_to_string":
      return String(args[0]);
    case "int_of_string":
      if (typeof args[0] !== "string" || !/^[+-]?[0-9]+$/.test(args[0])) {
        throw new Error(`Unsupported Riot JS int_of_string input: ${args[0]}`);
      }
      return Number.parseInt(args[0], 10);
    case "float_of_string":
      if (
        typeof args[0] !== "string" ||
        !/^[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?$/.test(args[0])
      ) {
        throw new Error(`Unsupported Riot JS float_of_string input: ${args[0]}`);
      }
      return Number.parseFloat(args[0]);
    case "equal":
      return args[0] === args[1];
    case "not_equal":
      return args[0] !== args[1];
    case "less_than":
      return args[0] < args[1];
    case "less_or_equal":
      return args[0] <= args[1];
    case "greater_than":
      return args[0] > args[1];
    case "greater_or_equal":
      return args[0] >= args[1];
    case "float_sqrt":
      return Math.sqrt(args[0]);
    case "tuple_make":
      return args;
    case "tuple_get":
      return args[0][args[1]];
    case "trace":
      console.log(args[0]);
      return undefined;
    default:
      throw new Error(`Unsupported Riot JS primitive: ${name}`);
  }
}
