import { readFileSync, writeFileSync } from "node:fs";
import { spawnSync } from "node:child_process";

const nil = () => ({ type: "Nil" });
const cons = (head, tail) => ({ type: "Cons", value: [head, tail] });
const ok = (value) => ({ type: "Ok", value });
const err = (message) => ({ type: "Err", value: message });

function listFromArray(values) {
  let result = nil();
  for (let index = values.length - 1; index >= 0; index -= 1) {
    result = cons(values[index], result);
  }
  return result;
}

function parseStringList(list) {
  const values = [];
  let rest = list;
  while (rest && rest.type === "Cons") {
    if (
      !Array.isArray(rest.value) ||
      rest.value.length !== 2 ||
      typeof rest.value[0] !== "string"
    ) {
      return { ok: false, error: "invalid Host.run_process argument list" };
    }
    values.push(rest.value[0]);
    rest = rest.value[1];
  }
  if (!rest || rest.type !== "Nil") {
    return { ok: false, error: "invalid Host.run_process argument list" };
  }
  return { ok: true, values };
}

const args = () => listFromArray(process.argv.slice(2));

function read_file(path) {
  if (typeof path !== "string") return err("Host.read_file expects a string path");
  try {
    return ok(readFileSync(path, "utf8"));
  } catch (error) {
    return err(error instanceof Error ? error.message : String(error));
  }
}

function write_file(path, contents) {
  if (typeof path !== "string" || typeof contents !== "string") {
    return err("Host.write_file expects string path and contents");
  }
  try {
    writeFileSync(path, contents, "utf8");
    return ok(undefined);
  } catch (error) {
    return err(error instanceof Error ? error.message : String(error));
  }
}

function write_stdout(message) {
  if (typeof message !== "string") throw new TypeError("Host.write_stdout expects a string");
  process.stdout.write(message);
}

function write_stderr(message) {
  if (typeof message !== "string") throw new TypeError("Host.write_stderr expects a string");
  process.stderr.write(message);
}

function run_process(program, argumentsList) {
  if (typeof program !== "string") {
    return err("Host.run_process expects a string program name");
  }
  const parsedArguments = parseStringList(argumentsList);
  if (!parsedArguments.ok) return err(parsedArguments.error);

  try {
    const child = spawnSync(program, parsedArguments.values, { stdio: "inherit" });
    if (child.error) return err(child.error.message);
    if (child.status === null) {
      return err(`process terminated without an exit code${child.signal ? ` (${child.signal})` : ""}`);
    }
    return ok(BigInt(child.status));
  } catch (error) {
    return err(error instanceof Error ? error.message : String(error));
  }
}

export { args, read_file, write_file, write_stdout, write_stderr, run_process };
