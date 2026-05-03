(**
   Environment and system information.

   This module provides access to environment variables, command-line
   arguments, and system directories.

   ## Examples

   Working with environment:

   ```ocaml open Std

   (* Get command-line arguments *) let files = List.tl Env.args in (* Skip
   program name *)

   (* Read environment variables *) let port = Env.var Int ~name:"PORT" |>
   Option.unwrap_or ~default:8080 in

   (* Get system directories *) let home = Env.home_dir () |> Option.expect
   ~msg:"No home dir" in let cwd = Env.current_dir () |> Result.expect
   ~msg:"Cannot get cwd" ```

   ## Type-safe Environment Variables

   Unlike traditional string-based APIs, this module provides type-safe
   environment variable access with automatic parsing.
*)
val args: string list

(**
   Command-line arguments passed to the program.

   The first element is typically the program name, followed by user-provided
   arguments.

   ## Examples

   ```ocaml (* Get all arguments *) let all_args = Env.args in

   (* Skip program name *) let user_args = match Env.args with | [] -> [] |
   _::rest -> rest in

   (* Simple argument parsing *) let verbose = List.mem "--verbose" Env.args in
   let files = List.filter (fun s -> not (String.starts_with ~prefix:"-" s) )
   (List.tl Env.args) ```
*)
val current_dir: unit -> (Path.t, Path.error) Result.t

(**
   Returns the current working directory.

   ## Examples

   ```ocaml match Env.current_dir () with | Ok cwd -> println "Working in: %s"
   (Path.to_string cwd) | Error e -> println "Cannot get cwd: %s" (show_error
   e)

   (* Build relative paths *) let config_path = Env.current_dir () |>
   Result.map (fun cwd -> cwd / Path.v "config.toml") |> Result.expect
   ~msg:"Cannot determine config path" ```
*)
val set_current_dir: Path.t -> (unit, Path.error) Result.t

(**
   Changes the current working directory.

   ## Examples

   ```ocaml (* Change to project directory *) Env.set_current_dir (Path.v
   "/home/user/project") |> Result.expect ~msg:"Cannot change directory";

   (* Temporary directory change *) let with_dir path f = let original =
   Env.current_dir () in let _ = Env.set_current_dir path in let result = f ()
   in let _ = match original with | Ok dir -> Env.set_current_dir dir | Error _
   -> Ok () in result ```

   ## Errors

   Returns error if:
   - Directory doesn't exist
   - Permission denied
   - Path is not a directory
*)
val home_dir: unit -> Path.t option

(**
   Returns the user's home directory.

   ## Examples

   ```ocaml (* Get home directory *) let home = Env.home_dir () |>
   Option.expect ~msg:"Cannot determine home directory" in

   (* Build config paths *) let config_dir = Env.home_dir () |> Option.map (fun
   h -> h / Path.v ".config" / Path.v "myapp") |> Option.unwrap_or
   ~default:(Path.v "/tmp/myapp") ```

   ## Platform Behavior

   - Unix/Linux/macOS: Returns `$HOME`
   - Windows: Returns `%USERPROFILE%` or `%HOMEDRIVE%%HOMEPATH%`

   Returns [`None`] if the home directory cannot be determined.
*)

(** Type specifications for environment variable parsing *)
type 't var_type =
  | String: string var_type
  (** String values (no parsing) *)
  | Int: int var_type
  (** Integer values *)
  | Float: float var_type
  (** Floating point values *)
  | Bool: bool var_type
  (** Boolean values (true/false, 1/0, yes/no) *)
  | Char: char var_type

(**
   Reads and parses a typed environment variable.

   Returns [`None`] if the variable is not set or parsing fails.

   ## Examples

   ```ocaml
   (* Read different types *)
   let port = Env.var Int ~name:"PORT"
     |> Option.unwrap_or ~default:3000 in

   let debug = Env.var Bool ~name:"DEBUG"
     |> Option.unwrap_or ~default:false in

   let timeout = Env.var Float ~name:"TIMEOUT"
     |> Option.unwrap_or ~default:30.0 in

   let separator = Env.var Char ~name:"SEP"
     |> Option.unwrap_or ~default:',' in

   (* Configuration pattern *)
   type config = {
     host: string;
     port: int;
     debug: bool;
   }

   let load_config () = {
     host = Env.var String ~name:"HOST"
       |> Option.unwrap_or ~default:"localhost";
     port = Env.var Int ~name:"PORT"
       |> Option.unwrap_or ~default:8080;
     debug = Env.var Bool ~name:"DEBUG"
       |> Option.unwrap_or ~default:false;
   }
   ```

   ## Parsing Rules

   - `Int`: Parses decimal integers
   - `Float`: Parses floating point numbers
   - `Bool`: Accepts "true"/"false", "yes"/"no", "1"/"0"
   - `Char`: Takes first character of the string
   - `String`: Returns value as-is
*)
val get: 't var_type -> var:string -> 't option

(** Use `var kind ~name` as the conventional alias for `get kind ~var:name`. *)
val var: 't var_type -> name:string -> 't option

(**
   Sets an environment variable.

   Returns the previous value if it existed.

   ## Examples

   ```ocaml (* Set variables *) let _ = Env.set_var ~name:"DEBUG" ~value:"true"
   in let _ = Env.set_var ~name:"PORT" ~value:"8080" in

   (* Save and restore *) let old_path = Env.set_var ~name:"PATH"
   ~value:new_path in (* ... do work ... *) Option.iter (fun p -> Env.set_var
   ~name:"PATH" ~value:p |> ignore ) old_path ```
*)
val set: var:string -> value:string -> string option

(**
   Removes an environment variable.

   Returns the previous value if it existed.
*)
val remove: var:string -> string option

(**
   Returns all environment variables as key-value pairs.

   ## Examples

   ```ocaml (* List all variables *) Env.vars () |> List.iter (fun (name,
   value) -> Printf.printf "%s=%s\n" name value);

   (* Filter specific variables *) let path_vars = Env.vars () |> List.filter
   (fun (name, _) -> String.contains name "PATH")

   (* Check for required variables *) let check_required names = let env_names
   = Env.vars () |> List.map fst in List.filter (fun name -> not (List.mem name
   env_names) ) names ```
*)
val vars: unit -> (string * string) list
