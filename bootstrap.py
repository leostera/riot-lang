#!/usr/bin/env python3
"""
Bootstrap script for tusk build system
"""

import os
import platform
import subprocess
import sys
import urllib.request
import tarfile
import tempfile

OCAML_VERSION = os.getenv("OCAML_VERSION", "5.5.0")
OCAML_CDN_BASE_URL = os.getenv("TUSK_OCAML_CDN_URL", "https://cdn.ocaml.ai/ocaml").rstrip("/")

def detect_libc():
    """Detect whether we're on glibc (gnu) or musl"""
    try:
        # Check if ldd is musl-based
        result = subprocess.run(["ldd", "--version"], 
                              capture_output=True, 
                              text=True, 
                              timeout=2)
        output = result.stdout.lower() + result.stderr.lower()
        if "musl" in output:
            return "musl"
        else:
            return "gnu"
    except:
        # Default to gnu (glibc) if detection fails
        return "gnu"

def get_host_triple():
    """Get the host triple for the current platform"""
    system = platform.system().lower()
    machine = platform.machine().lower()
    
    if system == "darwin":
        if machine in ["arm64", "aarch64"]:
            return "aarch64-apple-darwin"
        elif machine in ["x86_64", "amd64"]:
            return "x86_64-apple-darwin"
    elif system == "linux":
        libc = detect_libc()
        if machine in ["x86_64", "amd64"]:
            return f"x86_64-unknown-linux-{libc}"
        elif machine in ["arm64", "aarch64"]:
            return f"aarch64-unknown-linux-{libc}"
    
    # Fallback to a generic triple
    return f"{machine}-unknown-{system}"

def run(cmd):
    """Execute a command and exit on failure"""
    print(f"$ {cmd}")
    result = subprocess.run(cmd, shell=True)
    if result.returncode != 0:
        print(f"Error: Command failed: {cmd}")
        sys.exit(1)

def ensure_toolchain(version):
    """Ensure OCaml toolchain is installed"""
    host_triple = get_host_triple()
    home = os.path.expanduser("~")
    toolchain_dir = os.path.join(home, ".tusk", "toolchains", version, host_triple)
    ocamlc = os.path.join(toolchain_dir, "bin", "ocamlopt.opt")
    
    if os.path.exists(ocamlc):
        print(f"✓ OCaml {version} ({host_triple}) already installed")
        return toolchain_dir
    
    print(f"Installing OCaml {version} for {host_triple}...")
    os.makedirs(toolchain_dir, exist_ok=True)
    
    # Try to download prebuilt binary first
    binary_url = f"{OCAML_CDN_BASE_URL}/ocaml-{version}-{host_triple}.tar.gz"
    print(f"Attempting to download prebuilt binary from: {binary_url}")
    
    try:
        print("✓ Prebuilt binary found, downloading...")
        with tempfile.TemporaryDirectory() as temp_dir:
            tar_path = os.path.join(temp_dir, f"ocaml-{version}-{host_triple}.tar.gz")
            
            # Download the file
            urllib.request.urlretrieve(binary_url, tar_path)
            print(f"✓ Downloaded to {tar_path}")
            
            # Extract the tarball directly to toolchain directory
            with tarfile.open(tar_path, 'r:gz') as tar:
                tar.extractall(toolchain_dir)
            
            print(f"✓ OCaml {version} ({host_triple}) installed from prebuilt binary")
                
    except Exception as e:
        print(f"✗ Failed to install prebuilt binary: {e}")
        print("Falling back to source build...")
        vendored_ocaml = os.path.abspath("./vendor/ocaml")
        if os.path.exists(os.path.join(vendored_ocaml, "configure")):
            print(f"✓ Using vendored OCaml source at {vendored_ocaml}")
            run(
                f"./scripts/toolchain/build-vendored-ocaml.sh --prefix '{toolchain_dir}'"
            )
        else:
            # Download and build OCaml from source
            run(f"curl -L https://github.com/leostera/riot-ocaml/archive/{version}.tar.gz -o /tmp/ocaml-{version}.tar.gz")
            run(f"cd /tmp && tar xzf ocaml-{version}.tar.gz")
            run(f"cd /tmp/ocaml-{version} && ./configure --prefix={toolchain_dir}")
            run(f"cd /tmp/ocaml-{version} && make -j")
            run(f"cd /tmp/ocaml-{version} && make install")
            run(f"rm -rf /tmp/ocaml-{version} /tmp/ocaml-{version}.tar.gz")
        print(f"✓ OCaml {version} ({host_triple}) built from source")
    
    return toolchain_dir

def main():
    print("=== Bootstrap: Building minitusk ===\n")
    
    # Ensure OCaml toolchain
    toolchain_dir = ensure_toolchain(OCAML_VERSION)
    ocamlopt = f"{toolchain_dir}/bin/ocamlopt"
    
    print("\nBuilding minitusk...\n")

    # Clean and create bootstrap directory
    run("rm -rf ./_build/bootstrap")
    os.makedirs("./_build/bootstrap/sandbox/minitusk", exist_ok=True)

    # Build minitusk (compile all modules in dependency order)
    print("=== Compiling minitusk ===")

    # Generate const.ml with correct paths and architecture
    print("=== Generating const.ml ===")
    const_ml_content = f"""let aliases_suffix = "__aliases"
let c_ext = ".c"
let cma_ext = ".cma"
let cmi_ext = ".cmi"
let cmo_ext = ".cmo"
let current_dir = "."
let h_ext = ".h"
let ml_ext = ".ml"
let ml_gen_extension = ".ml.gen"
let mli_ext = ".mli"
let src_dir = "src"
let native_dir = "native"

(** Get the home directory *)
let home_dir = 
  try Sys.getenv "HOME" 
  with Not_found -> "/Users/ostera"

(** Get the host triple for the current platform *)
let get_host_triple () = "{get_host_triple()}"

(** Get the default OCaml version *)
let ocaml_version = "{OCAML_VERSION}"

(** Get the toolchain directory for a given version and target *)
let get_toolchain_dir ?(version = ocaml_version) ?(target = get_host_triple ()) () =
  Filename.concat home_dir (Filename.concat ".tusk/toolchains" (Filename.concat version target))

(** Get the bin directory for a given toolchain *)
let get_toolchain_bin_dir ?(version = ocaml_version) ?(target = get_host_triple ()) () =
  Filename.concat (get_toolchain_dir ~version ~target ()) "bin"

(** Get the lib directory for a given toolchain *)
let get_toolchain_lib_dir ?(version = ocaml_version) ?(target = get_host_triple ()) () =
  Filename.concat (get_toolchain_dir ~version ~target ()) "lib/ocaml"
"""
    
    with open("./_build/bootstrap/sandbox/minitusk/const.ml", "w") as f:
        f.write(const_ml_content)

# Copy all other source files to bootstrap directory
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

    for file in source_files[1:]:  # Skip const.ml since it's generated
        run(f"cp packages/minitusk/src/{file} ./_build/bootstrap/sandbox/minitusk")

    # Compile in dependency order (as determined by ocamldep)
    run(f"cd ./_build/bootstrap/sandbox/minitusk && {ocamlopt} -I +unix -o minitusk unix.cmxa " + " ".join(source_files))
    
    # Install
    run("rm -f ./minitusk")
    run("cp ./_build/bootstrap/sandbox/minitusk/minitusk ./minitusk")
    run("chmod +x ./minitusk")
    
    print("\n✓ Bootstrap complete! Minitusk executable at: ./minitusk")
    print("\nNow run: ./minitusk")

if __name__ == "__main__":
    main()
