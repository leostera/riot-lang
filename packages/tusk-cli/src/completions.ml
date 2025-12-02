open Std
open Tusk_model

let command =
  let open ArgParser in
  let open Arg in
  command "completions"
  |> about "Generate shell completions or list completion data"
  |> args
       [
         option "shell" |> long "shell"
         |> possible_values [ "bash"; "zsh"; "fish" ]
         |> help "Generate completion script for shell (zsh, bash, fish)";
         flag "packages" |> long "packages"
         |> help "List available packages in current workspace";
         flag "binaries" |> long "binaries"
         |> help
              "List available binaries in current workspace (format: \
               binary:package)";
         flag "tests" |> long "tests"
         |> help
              "List available test binaries in current workspace (format: \
               test:package)";
         flag "benchmarks" |> long "benchmarks"
         |> help
              "List available benchmark binaries in current workspace (format: \
               bench:package)";
         flag "commands" |> long "commands"
         |> help "List available package commands in current workspace";
       ]

let run matches =
  (* Save reference to command before opening ArgParser *)
  let completions_command = command in
  let open ArgParser in

  (* Check if generating full script *)
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
          (match shell with
          | Zsh ->
              eprintln "# For zsh, save the completion script to a directory in your $fpath.";
              eprintln "# Check your fpath with: echo $fpath";
              eprintln "#";
              eprintln "# Option 1 - User completions directory:";
              eprintln "#   mkdir -p ~/.zsh/completions";
              eprintln "#   tusk completions --shell zsh > ~/.zsh/completions/_tusk";
              eprintln "#   Add to ~/.zshrc: fpath=(~/.zsh/completions $fpath)";
              eprintln "#   Then run: autoload -Uz compinit && compinit";
              eprintln "#";
              eprintln "# Option 2 - System/Homebrew directory (if writable):";
              eprintln "#   tusk completions --shell zsh > /opt/homebrew/share/zsh/site-functions/_tusk";
              eprintln "#   Then run: exec zsh";
          | Bash ->
              eprintln "# Bash completions not yet implemented"
          | Fish ->
              eprintln "# Fish completions not yet implemented");
          Ok ())
  | None ->
      (* Dynamic completion helpers - need workspace context *)
      let has_packages = get_flag matches "packages" in
      let has_binaries = get_flag matches "binaries" in
      let has_tests = get_flag matches "tests" in
      let has_benchmarks = get_flag matches "benchmarks" in
      let has_commands = get_flag matches "commands" in

      if not (has_packages || has_binaries || has_tests || has_benchmarks || has_commands) then
        (* Use ArgParser to print help instead of manual *)
        let () = print_help completions_command in
        Ok ()
      else
        (* Try to load workspace, but fail silently if not in one *)
        let cwd =
          Env.current_dir ()
          |> Result.expect ~msg:"Failed to get current directory"
        in

        match Workspace_manager.scan cwd with
        | Error _ ->
            (* Not in a workspace or scan failed - silently succeed *)
            Ok ()
        | Ok (workspace, _load_errors) ->
            if has_packages then
              List.iter
                (fun pkg -> println pkg)
                (Shell_completions.list_packages workspace);

            if has_binaries then
              List.iter
                (fun bin -> println bin)
                (Shell_completions.list_binaries workspace);

            if has_tests then
              List.iter
                (fun test -> println test)
                (Shell_completions.list_tests workspace);

            if has_benchmarks then
              List.iter
                (fun bench -> println bench)
                (Shell_completions.list_benchmarks workspace);

            if has_commands then (
              let command_lines = Shell_completions.list_commands workspace in
              List.iter println command_lines);

            Ok ()
