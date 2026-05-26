/// How a command-line argument consumes input.
type Action =
  | Set
  | SetTrue

/// A small command-line argument definition.
type Arg =
  | Flag(String, String)
  | OptionArg(String, String)
  | Positional(String)

/// A command with a name and flat argument definitions.
type Command = Command(String, List<Arg>)

/// Parsed command-line arguments.
type Matches = Matches(Command, List<String>)

/// Argument parsing errors.
type Error =
  | MissingValue(String)

/// Build a command.
fn command(name: String, args: List<Arg>) -> Command {
  Command(name, args)
}

/// Build a boolean long flag.
fn flag(name: String, long: String) -> Arg {
  Flag(name, long)
}

/// Build an option that consumes the following argument as a value.
fn option(name: String, long: String) -> Arg {
  OptionArg(name, long)
}

/// Build a positional argument marker.
fn positional(name: String) -> Arg {
  Positional(name)
}

fn arg_name(arg: Arg) -> String {
  match arg {
    Flag(name, _) -> name,
    OptionArg(name, _) -> name,
    Positional(name) -> name
  }
}

fn arg_long(arg: Arg) -> Option<String> {
  match arg {
    Flag(_, long) -> Some(long),
    OptionArg(_, long) -> Some(long),
    Positional(_) -> None
  }
}

fn find_by_name(args: List<Arg>, name: String) -> Option<Arg> {
  match args {
    [] -> None,
    [arg, ..rest] ->
      if arg_name(arg) == name {
        Some(arg)
      } else {
        find_by_name(rest, name)
      }
  }
}

fn find_by_long(args: List<Arg>, long: String) -> Option<Arg> {
  match args {
    [] -> None,
    [arg, ..rest] ->
      match arg_long(arg) {
        Some(value) ->
          if value == long {
            Some(arg)
          } else {
            find_by_long(rest, long)
          },
        None -> find_by_long(rest, long)
      }
  }
}

fn validate_args(defs: List<Arg>, values: List<String>) -> Result<(), Error> {
  match values {
    [] -> Ok(()),
    [value, ..rest] ->
      match find_by_long(defs, value) {
        Some(arg) ->
          match arg {
            Flag(_, _) -> validate_args(defs, rest),
            OptionArg(_, long) ->
              match rest {
                [] -> Err(MissingValue(long)),
                [_, ..tail] -> validate_args(defs, tail)
              },
            Positional(_) -> validate_args(defs, rest)
          },
        None -> validate_args(defs, rest)
      }
  }
}

/// Parse raw process arguments against a command.
fn parse(cmd: Command, args: List<String>) -> Result<Matches, Error> {
  match cmd {
    Command(_, defs) ->
      match validate_args(defs, args) {
        Ok(_) -> Ok(Matches(cmd, args)),
        Err(error) -> Err(error)
      }
  }
}

fn contains(values: List<String>, needle: String) -> bool {
  match values {
    [] -> false,
    [value, ..rest] ->
      if value == needle {
        true
      } else {
        contains(rest, needle)
      }
  }
}

fn value_after_current(current: String, rest: List<String>, needle: String) -> Option<String> {
  match rest {
    [] -> None,
    [next, ..tail] ->
      if current == needle {
        Some(next)
      } else {
        value_after_current(next, tail, needle)
      }
  }
}

fn value_after(values: List<String>, needle: String) -> Option<String> {
  match values {
    [] -> None,
    [value, ..rest] -> value_after_current(value, rest, needle)
  }
}

/// Check whether a named flag was present.
fn get_flag(matches: Matches, name: String) -> bool {
  match matches {
    Matches(cmd, values) ->
      match cmd {
        Command(_, defs) ->
          match find_by_name(defs, name) {
            Some(arg) ->
              match arg {
                Flag(_, long) -> contains(values, long),
                OptionArg(_, _) -> false,
                Positional(_) -> false
              },
            None -> false
          }
      }
  }
}

/// Return the first value for a named option.
fn get_one(matches: Matches, name: String) -> Option<String> {
  match matches {
    Matches(cmd, values) ->
      match cmd {
        Command(_, defs) ->
          match find_by_name(defs, name) {
            Some(arg) ->
              match arg {
                OptionArg(_, long) -> value_after(values, long),
                Flag(_, _) -> None,
                Positional(_) -> None
              },
            None -> None
          }
      }
  }
}

/// Render a parser error.
fn error_message(error: Error) -> String {
  match error {
    MissingValue(name) -> string_concat("missing value for ", name)
  }
}
