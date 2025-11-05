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
  |> List.concat_map (fun (pkg : Tusk_model.Package.t) ->
      List.filter_map
        (fun (bin : Tusk_model.Package.binary) ->
          (* Filter out test binaries *)
          if
            String.ends_with ~suffix:"_tests" bin.name
            || String.ends_with ~suffix:"-tests" bin.name
          then None
          else Some (format "%s:%s" pkg.name bin.name))
        pkg.binaries)
  |> List.sort_uniq String.compare

(** List test binaries as "package:test" for display in completions *)
let list_tests (workspace : Tusk_model.Workspace.t) =
  workspace.packages
  |> List.concat_map (fun (pkg : Tusk_model.Package.t) ->
      List.filter_map
        (fun (bin : Tusk_model.Package.binary) ->
          if
            String.ends_with ~suffix:"_tests" bin.name
            || String.ends_with ~suffix:"-tests" bin.name
          then Some (format "%s:%s" pkg.name bin.name)
          else None)
        pkg.binaries)
  |> List.sort_uniq String.compare

let generate_zsh_script () =
  {|#compdef tusk

_tusk() {
    local -a subcommands

    subcommands=(
        'build:Build packages'
        'run:Run a binary'
        'test:Run tests'
        'clean:Clean build artifacts'
        'install:Install dependencies'
        'new:Create new package'
        'server:Manage tusk server'
        'rpc:Send RPC commands'
        'mcp:MCP server commands'
        'completions:Generate shell completions'
        'doc:Generate documentation'
        'lsp:Start LSP server'
        'version:Show version'
    )

    local context state state_descr line
    typeset -A opt_args

    case "$words[2]" in
        run)
            # Check if we're completing the binary name (position 3)
            if [[ $CURRENT -eq 3 ]]; then
                local -a binaries
                binaries=(${(f)"$(tusk completions --binaries 2>/dev/null)"})
                compadd -a binaries
            fi
            ;;
        build)
            _arguments \
                '(-p --package)'{-p,--package}'[Build specific package]:package:->packages' \
                '(-v --verbose)'{-v,--verbose}'[Verbose output]'
            
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
        completions)
            _arguments \
                '--shell[Shell type]:shell:(bash zsh fish)' \
                '--packages[List packages]' \
                '--binaries[List binaries]' \
                '--tests[List tests]'
            ;;
        clean|install|new|server|rpc|mcp|doc|lsp|version)
            # These commands have their own completion logic
            # Can be extended later
            ;;
        *)
            _describe 'command' subcommands
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
