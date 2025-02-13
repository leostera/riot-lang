{
  description = "serde-ml/toml: TOML suppport for serde.ml";

  outputs = inputs @ {
    self,
    nixpkgs,
    treefmt,
    nix-filter,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
      perSystem = {
        l,
        pkgs,
        config,
        ...
      }: let
        op = pkgs.ocamlPackages;
        filter = nix-filter.lib;
      in {
        packages.default = config.packages.serde_toml;
        packages.serde_toml = op.buildDunePackage {
          pname = "serde_toml";
          version = "dev-${self.lastModifiedDate}";

          duneVersion = "3";

          src = filter {
            root = ./.;
            include = [
              "lib"
              "test"

              ./dune-project
            ];
          };

          propagatedBuildInputs = l.attrValues {
            inherit (op) serde rio;
          };

          checkInputs = l.attrValues {
            inherit (op) spices qcheck serde_derive;
          };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [config.packages.default];
          packages = l.attrValues {
            inherit (op) ocaml-lsp ocamlformat;
          };
        };

        treefmt.config = {
          projectRootFile = ./dune-project;
          flakeFormatter = true;

          programs.alejandra.enable = true;
          programs.ocamlformat = {
            enable = true;
            configFile = ./.ocamlformat;
          };
        };
      };
      imports = [
        {
          perSystem = {
            lib,
            system,
            ...
          }: {
            _module.args.pkgs = import nixpkgs {
              inherit system;
              overlays = [(import ./nix/ocaml.overlay.nix)];
            };

            _module.args.l = lib // builtins;
          };
        }

        treefmt.flakeModule
      ];
    };

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nix-filter.url = "github:numtide/nix-filter";
    treefmt.url = "github:numtide/treefmt-nix";
  };
}
