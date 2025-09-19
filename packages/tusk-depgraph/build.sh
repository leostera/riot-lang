#!/bin/bash
set -e
set -i

echo "Building tusk-depgraph..."

# Use a known std directory
STD_DIR="../../../target/bootstrap/out/std"

echo "Using std from: $STD_DIR"

cd src

# Compile ocamldep wrapper
echo "Compiling ocamldep..."
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -c ocamldep.ml

echo "Compiling file_scanner..."
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -c file_scanner.ml

# Compile node_id
echo "Compiling node_id..."
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -c node_id.mli
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -c node_id.ml

# Compile module_registry
echo "Compiling module_registry..."
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -c module_registry.mli
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -c module_registry.ml

# Compile dep_graph
echo "Compiling dep_graph..."
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -c dep_graph.mli
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -c dep_graph.ml

# Compile main
echo "Compiling main..."
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I $STD_DIR -c main.ml

# Link everything
echo "Linking..."
/Users/ostera/.tusk/toolchains/5.3.0/bin/ocamlc -I +unix -I +str \
    unix.cma \
    str.cma \
    $STD_DIR/std.cma \
    ocamldep.cmo \
    file_scanner.cmo \
    node_id.cmo \
    module_registry.cmo \
    dep_graph.cmo \
    main.cmo \
    -o tusk-depgraph

echo "Build successful! Binary created at: src/tusk-depgraph"
echo ""
echo "Usage: ./src/tusk-depgraph <directory>"
echo "Example: ./src/tusk-depgraph ../miniriot"
