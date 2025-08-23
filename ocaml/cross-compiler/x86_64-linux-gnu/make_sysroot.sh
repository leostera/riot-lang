#!/bin/bash
set -e

TRIPLET="x86_64-linux-gnu"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SYSROOT_DIR="$SCRIPT_DIR/sysroot"

echo "Creating sysroot for $TRIPLET..."

# Clean and create sysroot directory
rm -rf "$SYSROOT_DIR"
mkdir -p "$SYSROOT_DIR"

# Build the sysroot container
echo "Building sysroot container..."
docker build --platform linux/amd64 -t sysroot-$TRIPLET -f "$SCRIPT_DIR/Sysroot.dockerfile" "$SCRIPT_DIR"

# Extract sysroot files from container
echo "Extracting sysroot files..."
docker run --platform linux/amd64 --rm -v "$SYSROOT_DIR:/output" sysroot-$TRIPLET bash -c "
    # Copy headers
    cp -r /usr/include /output/
    
    # Copy architecture-specific headers
    cp -r /usr/include/$TRIPLET/* /output/include/
    
    # Create lib directory and copy libraries
    mkdir -p /output/lib
    cp -r /lib/$TRIPLET/* /output/lib/
    cp -r /usr/lib/$TRIPLET/* /output/lib/
    
    # Copy actual shared library files
    find /lib/$TRIPLET -name '*.so.*' -exec cp {} /output/lib/ \;
    find /usr/lib/$TRIPLET -name '*.so.*' -exec cp {} /output/lib/ \;
    
    # Copy startup files and GCC libraries
    cp /usr/lib/$TRIPLET/Scrt1.o /output/lib/
    cp /usr/lib/$TRIPLET/crt*.o /output/lib/
    cp /usr/lib/gcc/$TRIPLET/*/crt*.o /output/lib/
    cp /usr/lib/gcc/$TRIPLET/*/libgcc* /output/lib/
    
    # Fix linker scripts to use relative paths
    find /output/lib -name '*.so' -type f | while read -r file; do
        if [[ \$(file \"\$file\") == *'ASCII text'* ]]; then
            sed -i 's|/lib/$TRIPLET/|./|g' \"\$file\"
            sed -i 's|/usr/lib/$TRIPLET/|./|g' \"\$file\"
            sed -i 's|/lib64/|./|g' \"\$file\"
        fi
    done
    
    echo 'Sysroot extraction complete'
"

echo "Sysroot created at: $SYSROOT_DIR"
echo "Test with: clang --target=$TRIPLET -fuse-ld=lld --sysroot=$SYSROOT_DIR ..."