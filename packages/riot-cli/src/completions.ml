open Std
open Riot_model

let command =
  let open ArgParser in
    let open ArgParser.Arg in
      command "completions" |> about "Generate shell completions or list completion data" |> args
        [
          option "shell" |> long "shell" |> possible_values [ "bash"; "zsh"; "fish" ] |> help "Generate completion script for shell (zsh, bash, fish)";
          flag "packages" |> long "packages" |> help "List available packages in current workspace";
          flag "binaries" |> long "binaries" |> help
            "List available binaries in current workspace (format: \
               binary:package)";
          flag "tests" |> long "tests" |> help
            "List available test binaries in current workspace (format: \
               test:package)";
          flag "benchmarks" |> long "benchmarks" |> help
            "List available benchmark binaries in current workspace (format: \
               bench:package)";
          flag "commands" |> long "commands" |> help "List available package commands in current workspace";
        ]

let detect_shell = fun () ->
  match Env.get Env.String ~var:"SHELL" with
  | None -> None
  | Some shell_path ->
      if String.ends_with ~suffix:"/zsh" shell_path || shell_path = "zsh" then
        Some Shell_completions.Zsh
      else if String.ends_with ~suffix:"/bash" shell_path || shell_path = "bash" then
        Some Shell_completions.Bash
      else if String.ends_with ~suffix:"/fish" shell_path || shell_path = "fish" then
        Some Shell_completions.Fish
      else
        None

let install_zsh_completions = fun () ->
  let home_dir = Env.home_dir () |> Option.expect ~msg:"Could not determine home directory" in
  let completions_dir = Path.(home_dir / Path.v ".zsh" / Path.v "completions") in
  let target = Path.(completions_dir / Path.v "_riot") in
  let script = Shell_completions.generate_script Shell_completions.Zsh in
  Fs.create_dir_all completions_dir |> Result.expect ~msg:"Failed to create ~/.zsh/completions";
  Fs.write script target |> Result.expect ~msg:"Failed to install completions";
  println ("Installed zsh completions to " ^ Path.to_string target);
  println "";
  println "If ~/.zsh/completions is not already in your fpath, add this to ~/.zshrc:";
  println "  fpath=(~/.zsh/completions $fpath)";
  println "";
  println "Then reload completions with:";
  println "  autoload -Uz compinit && compinit";
  println "Or restart your shell.";
  Ok ()

let run_install = fun matches ->
  let open ArgParser in
    match get_one matches "shell", detect_shell () with
    | Some shell_str, _ -> (
        match Shell_completions.shell_from_string shell_str with
        | None ->
            println ("Error: Unknown shell '" ^ shell_str ^ "'");
            println "Supported shells: bash, zsh, fish";
            Error (Failure ("unknown shell: " ^ shell_str))
        | Some Zsh ->
            install_zsh_completions ()
        | Some Bash ->
            println "Bash completion installation is not implemented yet.";
            println "Try: riot completions --shell bash";
            Error (Failure "bash completion installation not implemented")
        | Some Fish ->
            println "Fish completion installation is not implemented yet.";
            println "Try: riot completions --shell fish";
            Error (Failure "fish completion installation not implemented")
      )
    | None, None ->
        Error (Failure "shell detection failed")
    | None, Some Zsh ->
        install_zsh_completions ()
    | None, Some Bash ->
        println "Bash completion installation is not implemented yet.";
        println "Try: riot completions --shell bash";
        Error (Failure "bash completion installation not implemented")
    | None, Some Fish ->
        println "Fish completion installation is not implemented yet.";
        println "Try: riot completions --shell fish";
        Error (Failure "fish completion installation not implemented")

let install_command =
  let open ArgParser in
    let open ArgParser.Arg in command "install"
    |> about "Install shell completions for the current user"
    |> args
      [
        option "shell" |> long "shell" |> possible_values [ "bash"; "zsh"; "fish" ] |> help "Shell to install completions for";
      ]

let run_install_args = fun argv ->
  let open ArgParser in
    match get_matches install_command ("install" :: argv) with
    | Ok matches -> run_install matches
    | Error err ->
        print_error err;
        Error (Failure "Argument parsing failed")

let run = fun matches ->
  (* Save reference to command before opening ArgParser *)
  let completions_command = command in
  let open ArgParser in
    match get_one matches "shell" with
    | Some shell_str -> (
        match Shell_completions.shell_from_string shell_str with
        | None ->
            println ("Error: Unknown shell '" ^ shell_str ^ "'");
            println "Supported shells: bash, zsh, fish";
            Error (Failure ("unknown shell: " ^ shell_str))
        | Some shell ->
            let script = Shell_completions.generate_script shell in
            print script;
            (* Print installation instructions to stderr so they don't pollute the script *)
            eprintln "";
            eprintln "# Installation instructions (printed to stderr):";
            (
              match shell with
              | Zsh ->
                  eprintln "# For zsh, save the completion script to a directory in your $fpath.";
                  eprintln "# Check your fpath with: echo $fpath";
                  eprintln "#";
                  eprintln "# Option 1 - User completions directory:";
                  eprintln "#   mkdir -p ~/.zsh/completions";
                  eprintln "#   riot completions --shell zsh > ~/.zsh/completions/_riot";
                  eprintln "#   Add to ~/.zshrc: fpath=(~/.zsh/completions $fpath)";
                  eprintln "#   Then run: autoload -Uz compinit && compinit";
                  eprintln "#";
                  eprintln "# Option 2 - System/Homebrew directory (if writable):";
                  eprintln "#   riot completions --shell zsh > /opt/homebrew/share/zsh/site-functions/_riot";
                  eprintln "#   Then run: exec zsh";
              | Bash ->
                  eprintln "# Bash completions not yet implemented"
              | Fish ->
                  eprintln "# Fish completions not yet implemented"
            );
            Ok ()
      )
    | None ->
        (* Dynamic completion helpers - need workspace context *)
        let has_packages = get_flag matches "packages" in
        let has_binaries = get_flag matches "binaries" in
        let has_tests = get_flag matches "tests" in
        let has_benchmarks = get_flag matches "benchmarks" in
        let has_commands = get_flag matches "commands" in
        if not (has_packages || has_binaries || has_tests || has_benchmarks || has_commands) then
          let () = print_help completions_command in
          Ok ()
        else
          (* Try to load workspace, but fail silently if not in one *)
          let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get current directory" in
          let workspace_manager = Workspace_manager.create () in
          match Workspace_manager.scan workspace_manager cwd with
          | Error _ ->
              (* Not in a workspace or scan failed - silently succeed *)
              Ok ()
          | Ok (workspace, _load_errors) ->
              if has_packages then
                List.for_each
                  (Shell_completions.list_packages workspace)
                  ~fn:(fun pkg -> println pkg);
              if has_binaries then
                List.for_each
                  (Shell_completions.list_binaries workspace)
                  ~fn:(fun bin -> println bin);
              if has_tests then
                List.for_each (Shell_completions.list_tests workspace) ~fn:(fun test -> println test);
              if has_benchmarks then
                List.for_each
                  (Shell_completions.list_benchmarks workspace)
                  ~fn:(fun bench -> println bench);
              if has_commands then
                (
                  let command_lines = Shell_completions.list_commands workspace in
                  List.for_each command_lines ~fn:println
                );
              Ok ()
