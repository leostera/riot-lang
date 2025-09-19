#!/bin/bash
set -e
set -i

echo "Building tusk-depgraph..."

# Use a known std directory
STD_DIR="../../target/bootstrap/out/std"

echo "Using std from: $STD_DIR"

# Stay in the project directory, use src/ prefix for files

# Compile ocamldep wrapper
echo "Compiling ocamldep..."
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -I src -c src/ocamldep.ml

# Compile node_id
echo "Compiling node_id..."
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -I src -c src/node_id.mli
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -I src -c src/node_id.ml

# Compile module_registry
echo "Compiling module_registry..."
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -I src -c src/module_registry.mli
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -I src -c src/module_registry.ml

# Compile dep_graph2
echo "Compiling dep_graph2..."
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -I src -c src/dep_graph2.ml

# Compile main
echo "Compiling main..."
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -I src -c src/main.ml

# Link everything
echo "Linking..."
KERNEL_DIR="../../target/bootstrap/out/kernel"

cp ${KERNEL_DIR}/*.o .

# Need -custom and the .o files for C stubs to be available
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I +unix \
    unix.cma \
    -custom \
    $KERNEL_DIR/kernel.cma \
    $STD_DIR/std.cma \
    src/ocamldep.cmo \
    src/node_id.cmo \
    src/module_registry.cmo \
    src/dep_graph2.cmo \
    src/main.cmo \
    -o src/tusk-depgraph

echo "Build successful! Binary created at: src/tusk-depgraph"
echo ""
echo "Usage: ./src/tusk-depgraph <directory>"
echo "Example: ./src/tusk-depgraph ../miniriot"
