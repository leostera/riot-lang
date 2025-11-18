#!/usr/bin/env python3
"""
Bootstrap script for tusk build system
"""

import os
import subprocess
import sys

OCAML_VERSION = os.getenv("OCAML_VERSION", "5.3.0+riot")

def run(cmd):
    """Execute a command and exit on failure"""
    print(f"$ {cmd}")
    result = subprocess.run(cmd, shell=True)
    if result.returncode != 0:
        print(f"Error: Command failed: {cmd}")
        sys.exit(1)

def ensure_toolchain(version):
    """Ensure OCaml toolchain is installed"""
    home = os.path.expanduser("~")
    toolchain_dir = os.path.join(home, ".tusk", "toolchains", version)
    ocamlc = os.path.join(toolchain_dir, "bin", "ocamlopt.opt")
    
    if os.path.exists(ocamlc):
        print(f"✓ OCaml {version} already installed")
        return toolchain_dir
    
    print(f"Installing OCaml {version}...")
    os.makedirs(toolchain_dir, exist_ok=True)
    
    # Download and build OCaml
    run(f"curl -L https://github.com/leostera/riot-ocaml/archive/{version}.tar.gz -o /tmp/ocaml-{version}.tar.gz")
    run(f"cd /tmp && tar xzf ocaml-{version}.tar.gz")
    run(f"cd /tmp/ocaml-{version} && ./configure --prefix={toolchain_dir}")
    run(f"cd /tmp/ocaml-{version} && make -j")
    run(f"cd /tmp/ocaml-{version} && make install")
    run(f"rm -rf /tmp/ocaml-{version} /tmp/ocaml-{version}.tar.gz")
    
    print(f"✓ OCaml {version} installed")
    return toolchain_dir

def main():
    print("=== Bootstrap: Building minitusk ===\n")
    
    # Ensure OCaml toolchain
    toolchain_dir = ensure_toolchain(OCAML_VERSION)
    ocamlc = f"{toolchain_dir}/bin/ocamlc"
    
    print("\nBuilding minitusk...\n")

    # Clean and create bootstrap directory
    run("rm -rf ./_build/bootstrap")
    os.makedirs("./_build/bootstrap/sandbox/minitusk", exist_ok=True)

    # Build minitusk (compile all modules in dependency order)
    print("=== Compiling minitusk ===")

    # Copy all source files to bootstrap directory
    # Build order based on dependencies
    source_files = [
        "const.ml",
        "io.ml",
        "ocaml_platform.ml",
        "toml.ml",
        "file_scanner.ml",
        "graph.ml",
        "package.ml",
        "dep_graph.ml",
        "action.ml",
        "main.ml"
    ]

    for file in source_files:
        run(f"cp packages/minitusk/src/{file} ./_build/bootstrap/sandbox/minitusk")

    # Compile in dependency order (as determined by ocamldep)
    run(f"cd ./_build/bootstrap/sandbox/minitusk && {ocamlc} -I +unix -o minitusk unix.cma " + " ".join(source_files))
    
    # Install
    run("rm -f ./minitusk")
    run("cp ./_build/bootstrap/sandbox/minitusk/minitusk ./minitusk")
    run("chmod +x ./minitusk")
    
    print("\n✓ Bootstrap complete! Minitusk executable at: ./minitusk")
    print("\nNow run: ./minitusk")

if __name__ == "__main__":
    main()
