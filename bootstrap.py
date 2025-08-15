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
    ocamlc = os.path.join(toolchain_dir, "bin", "ocamlc")
    
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
    run("rm -rf ./target/bootstrap")
    os.makedirs("./target/bootstrap", exist_ok=True)
    
    # Build minitusk (just needs Unix module)
    print("=== Compiling minitusk ===")
    run("cp packages/minitusk/src/main.ml ./target/bootstrap/minitusk.ml")
    run(f"cd ./target/bootstrap && {ocamlc} -I +unix -o minitusk unix.cma minitusk.ml")
    
    # Install
    run("rm -f ./minitusk")
    run("cp ./target/bootstrap/minitusk ./minitusk")
    run("chmod +x ./minitusk")
    
    print("\n✓ Bootstrap complete! Minitusk executable at: ./minitusk")
    print("\nNow run: ./minitusk")

if __name__ == "__main__":
    main()
