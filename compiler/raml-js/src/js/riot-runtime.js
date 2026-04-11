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
    default:
      throw new Error(`Unsupported Riot JS primitive: ${name}`);
  }
}
