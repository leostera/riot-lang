open Std

type shell = Zsh | Bash | Fish

let shell_to_string = function
  | Zsh -> "zsh"
  | Bash -> "bash"
  | Fish -> "fish"

let shell_from_string = function
  | "zsh" -> Some Zsh
  | "bash" -> Some Bash
  | "fish" -> Some Fish
  | _ -> None

let list_packages (workspace : Tusk_model.Workspace.t) =
  workspace.packages
  |> List.map (fun (pkg : Tusk_model.Package.t) -> pkg.name)
  |> List.sort String.compare

(** List binaries as "package:binary" for display in completions, excluding tests *)
let list_binaries (workspace : Tusk_model.Workspace.t) =
  workspace.packages
  |> List.filter Tusk_model.Package.is_workspace_member
  |> List.concat_map (fun (pkg : Tusk_model.Package.t) ->
      List.filter_map
        (fun (bin : Tusk_model.Package.binary) ->
          (* Filter out test binaries *)
          if
            String.ends_with ~suffix:"_tests" bin.name
            || String.ends_with ~suffix:"-tests" bin.name
          then None
          else Some (pkg.name ^ ":" ^ bin.name))
        pkg.binaries)
  |> List.sort_uniq String.compare

(** List package names, package wildcards, and test binaries for completions *)
let list_tests (workspace : Tusk_model.Workspace.t) =
  let test_packages =
    workspace.packages
    |> List.filter Tusk_model.Package.is_workspace_member
    |> List.filter_map (fun (pkg : Tusk_model.Package.t) ->
        let has_tests =
          List.exists
            (fun (bin : Tusk_model.Package.binary) ->
              String.ends_with ~suffix:"_tests" bin.name
              || String.ends_with ~suffix:"-tests" bin.name)
            pkg.binaries
        in
        if has_tests then Some pkg.name else None)
  in
  let individual_tests =
    workspace.packages
    |> List.filter Tusk_model.Package.is_workspace_member
    |> List.concat_map (fun (pkg : Tusk_model.Package.t) ->
        List.filter_map
          (fun (bin : Tusk_model.Package.binary) ->
            if
              String.ends_with ~suffix:"_tests" bin.name
              || String.ends_with ~suffix:"-tests" bin.name
            then Some (pkg.name ^ ":" ^ bin.name)
            else None)
          pkg.binaries)
  in
  (* Add pkg:... entries for packages with tests *)
  let package_wildcards = List.map (fun pkg_name -> pkg_name ^ ":...") test_packages in
  (test_packages @ package_wildcards @ individual_tests)
  |> List.sort_uniq String.compare

(** List benchmark binaries as "package:bench" for display in completions *)
let list_benchmarks (workspace : Tusk_model.Workspace.t) =
  let individual_benches =
    workspace.packages
    |> List.filter Tusk_model.Package.is_workspace_member
    |> List.concat_map (fun (pkg : Tusk_model.Package.t) ->
        List.filter_map
          (fun (bin : Tusk_model.Package.binary) ->
            if String.ends_with ~suffix:"_bench" bin.name
            then Some (pkg.name ^ ":" ^ bin.name)
            else None)
          pkg.binaries)
  in
  (* Add pkg:... entries for packages with benchmarks *)
  let package_wildcards =
    workspace.packages
    |> List.filter Tusk_model.Package.is_workspace_member
    |> List.filter_map (fun (pkg : Tusk_model.Package.t) ->
        let has_benches =
          List.exists
            (fun (bin : Tusk_model.Package.binary) ->
              String.ends_with ~suffix:"_bench" bin.name)
            pkg.binaries
        in
        if has_benches then Some (pkg.name ^ ":...") else None)
  in
  (package_wildcards @ individual_benches) |> List.sort_uniq String.compare

(** List package commands as "package:command\tdescription" (tab-separated) for display in completions *)
let list_commands (workspace : Tusk_model.Workspace.t) =
  Tusk_model.Workspace.discover_commands workspace
  |> List.map (fun (cmd : Tusk_model.Package_command.t) ->
      let name = cmd.package_name ^ ":" ^ cmd.name in
      (* Use help text from TOML, or provide fallback *)
      let desc = 
        if String.length cmd.description = 0 then 
          "Package command"
        else 
          cmd.description 
      in
      let tab = Char.chr 9 in  (* Explicit tab character *)
      name ^ String.make 1 tab ^ desc)
  |> List.sort_uniq String.compare

(** List package command descriptions matching the order of list_commands *)
let list_command_descriptions (workspace : Tusk_model.Workspace.t) =
  list_commands workspace
  |> List.map (fun line ->
      (* Extract description after tab *)
      match String.index_opt line '\t' with
      | Some idx -> String.sub line (idx + 1) (String.length line - idx - 1)
      | None -> "Package command")

let generate_zsh_script () =
  {|#compdef tusk

_tusk() {
    local -a builtin_commands package_commands all_commands

    builtin_commands=(
        'build:Build packages'
        'fix:Lint code and optionally apply safe fixes'
        'run:Run a binary'
        'test:Run tests'
        'bench:Run benchmarks'
        'clean:Clean build artifacts'
        'install:Install dependencies'
        'new:Create new package'
        'completions:Generate shell completions'
        'doc:Generate documentation'
        'lsp:Start LSP server'
        'version:Show version'
    )

    # Load package commands dynamically (format: "package:command\tdescription")
    local -a raw_lines package_commands package_descs
    raw_lines=(${(f)"$(tusk completions --commands 2>/dev/null)"})
    
    # Parse tab-separated name and description
    package_commands=()
    package_descs=()
    for line in $raw_lines; do
        local name="${line%%$'\t'*}"
        local desc="${line#*$'\t'}"
        package_commands+=("$name")
        package_descs+=("$desc")
    done

    local context state state_descr line
    typeset -A opt_args

    # Check if we've passed -- separator, if so use default file completion
    local i
    for i in {2..$CURRENT}; do
        if [[ "${words[$i]}" == "--" ]]; then
            _files
            return 0
        fi
    done

    # If we're completing the first argument (the command), show all commands
    if [[ $CURRENT -eq 2 ]]; then
        # Combine builtin and package commands for _describe
        local -a all_commands_with_descs
        all_commands_with_descs=($builtin_commands)
        
        # Add package commands in "name:description" format
        for i in {1..${#package_commands[@]}}; do
            # Escape colons in the command name for _describe
            local escaped_name="${package_commands[$i]//:/\\:}"
            all_commands_with_descs+=("$escaped_name:${package_descs[$i]}")
        done
        
        # Show all commands with descriptions, sorted by _describe
        _describe -t commands 'command' all_commands_with_descs
        return 0
    fi

    case "$words[2]" in
        run)
            # Check if we're completing the binary name (position 3)
            if [[ $CURRENT -eq 3 ]]; then
                local -a binaries
                binaries=(${(f)"$(tusk completions --binaries 2>/dev/null)"})
                compadd -a binaries
            else
                _arguments \
                    '(-p --package)'{-p,--package}'[Run binary from package]:package:->packages'

                case $state in
                    packages)
                        local -a packages
                        packages=(${(f)"$(tusk completions --packages 2>/dev/null)"})
                        _describe 'package' packages
                        ;;
                esac
            fi
            ;;
        build)
            _arguments \
                '(-x --target)'{-x,--target}'[Build for target architecture]:target:' \
                '--all-targets[Build for all configured targets]' \
                '*:package:->packages'

            case $state in
                packages)
                    local -a packages
                    packages=(${(f)"$(tusk completions --packages 2>/dev/null)"})
                    _describe 'package' packages
                    ;;
            esac
            ;;
        test)
            # Check if we're completing the test pattern (position 3)
            if [[ $CURRENT -eq 3 ]]; then
                local -a tests
                tests=(${(f)"$(tusk completions --tests 2>/dev/null)"})
                compadd -a tests
            else
                _arguments \
                    '(-p --package)'{-p,--package}'[Run tests from package]:package:->packages' \
                    '(-v --verbose)'{-v,--verbose}'[Verbose output]'
                
                case $state in
                    packages)
                        local -a packages
                        packages=(${(f)"$(tusk completions --packages 2>/dev/null)"})
                        _describe 'package' packages
                        ;;
                esac
            fi
            ;;
        bench)
            # Check if we're completing the benchmark pattern (position 3)
            if [[ $CURRENT -eq 3 ]]; then
                local -a benches
                benches=(${(f)"$(tusk completions --benchmarks 2>/dev/null)"})
                compadd -a benches
            else
                _arguments \
                    '(-p --package)'{-p,--package}'[Run benchmarks from package]:package:->packages' \
                    '(-v --verbose)'{-v,--verbose}'[Verbose output]'
                
                case $state in
                    packages)
                        local -a packages
                        packages=(${(f)"$(tusk completions --packages 2>/dev/null)"})
                        _describe 'package' packages
                        ;;
                esac
            fi
            ;;
        completions)
            if [[ $CURRENT -eq 3 ]]; then
                compadd install
            elif [[ "${words[3]}" == "install" ]]; then
                _arguments \
                    '--shell[Shell type]:shell:(bash zsh fish)'
            else
                _arguments \
                    '--shell[Shell type]:shell:(bash zsh fish)' \
                    '--packages[List packages]' \
                    '--binaries[List binaries]' \
                    '--tests[List tests]' \
                    '--benchmarks[List benchmarks]' \
                    '--commands[List commands]'
            fi
            ;;
        fix)
            _arguments \
                '--apply[Apply safe fixes to files]' \
                '--check[Check for issues without modifying files]' \
                '--json[Emit machine-readable JSON output]' \
                ':path:_files'
            ;;
        fmt)
            _arguments \
                '--check[Check if files need formatting]' \
                '--verify[Verify formatting would preserve syntax hashes]' \
                '--json[Emit machine-readable JSONL events]'
            ;;
        clean|install|new|doc|lsp|version)
            # These commands have their own completion logic
            # Can be extended later
            ;;
        *:*)
            # Package command (format: package:command)
            # Just complete files - users can use --help to learn about options
            _files
            ;;
        *)
            _describe 'command' all_commands
            ;;
    esac
}

_tusk "$@"
|}

let generate_bash_script () =
  (* Placeholder for future bash support *)
  "# Bash completions not yet implemented\n"

let generate_fish_script () =
  (* Placeholder for future fish support *)
  "# Fish completions not yet implemented\n"

let generate_script = function
  | Zsh -> generate_zsh_script ()
  | Bash -> generate_bash_script ()
  | Fish -> generate_fish_script ()
