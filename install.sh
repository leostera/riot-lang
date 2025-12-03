#!/bin/sh
# Tusk installation script
# Usage: curl -sSL https://cdn.riot.ml/tusk/install.sh | sh

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Print with color
print_info() { printf "${GREEN}==>${NC} %s\n" "$1"; }
print_warn() { printf "${YELLOW}Warning:${NC} %s\n" "$1"; }
print_error() { printf "${RED}Error:${NC} %s\n" "$1" >&2; }

# Detect OS and architecture
detect_platform() {
    OS="$(uname -s)"
    ARCH="$(uname -m)"
    
    case "$OS" in
        Linux*)
            OS_TYPE="linux"
            ;;
        Darwin*)
            OS_TYPE="darwin"
            ;;
        *)
            print_error "Unsupported operating system: $OS"
            exit 1
            ;;
    esac
    
    case "$ARCH" in
        x86_64|amd64)
            ARCH_TYPE="x86_64"
            ;;
        aarch64|arm64)
            ARCH_TYPE="aarch64"
            ;;
        *)
            print_error "Unsupported architecture: $ARCH"
            exit 1
            ;;
    esac
    
    # Determine libc for Linux
    if [ "$OS_TYPE" = "linux" ]; then
        if ldd --version 2>&1 | grep -q musl; then
            LIBC="musl"
        else
            LIBC="gnu"
        fi
        PLATFORM="${ARCH_TYPE}-unknown-linux-${LIBC}"
    else
        # macOS
        PLATFORM="${ARCH_TYPE}-apple-darwin"
    fi
    
    print_info "Detected platform: $PLATFORM"
}

# Download and install tusk
install_tusk() {
    INSTALL_DIR="${HOME}/.tusk/bin"
    TUSK_REPO="leostera/riot"
    VERSION="${TUSK_VERSION:-latest}"
    
    print_info "Installing Tusk ($VERSION) for $PLATFORM..."
    
    # Create installation directory
    mkdir -p "$INSTALL_DIR"
    
    # Construct download URL
    if [ "$VERSION" = "latest" ]; then
        # Get the latest commit SHA from the latest release
        RELEASE_API="https://api.github.com/repos/${TUSK_REPO}/releases/tags/latest"
        print_info "Fetching latest development build..."
        
        # Try to get version from GitHub API
        if command -v curl >/dev/null 2>&1; then
            LATEST_VERSION=$(curl -sSL "$RELEASE_API" | grep '"name":' | head -1 | sed 's/.*"name": ".*(\(sha-[^)]*\))".*/\1/')
            if [ -z "$LATEST_VERSION" ]; then
                LATEST_VERSION="sha-48e8cec"  # Fallback to current commit
            fi
        else
            LATEST_VERSION="sha-48e8cec"  # Fallback
        fi
        
        DOWNLOAD_URL="https://github.com/${TUSK_REPO}/releases/download/latest/tusk-${LATEST_VERSION}-${PLATFORM}.tar.gz"
    else
        DOWNLOAD_URL="https://github.com/${TUSK_REPO}/releases/download/${VERSION}/tusk-${VERSION}-${PLATFORM}.tar.gz"
    fi
    
    print_info "Downloading from: $DOWNLOAD_URL"
    
    # Download and extract
    TMPDIR=$(mktemp -d)
    trap "rm -rf $TMPDIR" EXIT
    
    if command -v curl >/dev/null 2>&1; then
        curl -sSL "$DOWNLOAD_URL" | tar xz -C "$TMPDIR"
    elif command -v wget >/dev/null 2>&1; then
        wget -qO- "$DOWNLOAD_URL" | tar xz -C "$TMPDIR"
    else
        print_error "Neither curl nor wget found. Please install one of them."
        exit 1
    fi
    
    # Move binary to install directory
    mv "$TMPDIR/tusk" "$INSTALL_DIR/tusk"
    chmod +x "$INSTALL_DIR/tusk"
    
    print_info "Tusk installed to: $INSTALL_DIR/tusk"
}

# Add to PATH in shell config
add_to_path() {
    INSTALL_DIR="${HOME}/.tusk/bin"
    SHELL_NAME="$(basename "$SHELL")"
    
    case "$SHELL_NAME" in
        bash)
            SHELL_CONFIG="${HOME}/.bashrc"
            ;;
        zsh)
            SHELL_CONFIG="${HOME}/.zshrc"
            ;;
        fish)
            SHELL_CONFIG="${HOME}/.config/fish/config.fish"
            mkdir -p "$(dirname "$SHELL_CONFIG")"
            ;;
        *)
            print_warn "Unknown shell: $SHELL_NAME"
            print_warn "Please manually add $INSTALL_DIR to your PATH"
            return
            ;;
    esac
    
    # Check if already in config
    if [ -f "$SHELL_CONFIG" ] && grep -q ".tusk/bin" "$SHELL_CONFIG"; then
        print_info "PATH already configured in $SHELL_CONFIG"
        return
    fi
    
    # Add to config
    if [ "$SHELL_NAME" = "fish" ]; then
        echo "fish_add_path -g $INSTALL_DIR" >> "$SHELL_CONFIG"
    else
        echo "" >> "$SHELL_CONFIG"
        echo "# Tusk" >> "$SHELL_CONFIG"
        echo "export PATH=\"\$HOME/.tusk/bin:\$PATH\"" >> "$SHELL_CONFIG"
    fi
    
    print_info "Added $INSTALL_DIR to PATH in $SHELL_CONFIG"
    print_warn "Please restart your shell or run: source $SHELL_CONFIG"
}

# Verify installation
verify_installation() {
    INSTALL_DIR="${HOME}/.tusk/bin"
    
    if [ ! -f "$INSTALL_DIR/tusk" ]; then
        print_error "Installation failed: tusk binary not found"
        exit 1
    fi
    
    # Add to PATH temporarily for verification
    export PATH="$INSTALL_DIR:$PATH"
    
    if command -v tusk >/dev/null 2>&1; then
        VERSION_OUTPUT=$(tusk --version 2>&1 || echo "unknown")
        print_info "Tusk installed successfully!"
        print_info "Version: $VERSION_OUTPUT"
        echo ""
        print_info "To get started, run:"
        echo "  tusk --help"
    else
        print_error "Installation completed but tusk not found in PATH"
        exit 1
    fi
}

# Main installation flow
main() {
    print_info "Tusk Installation Script"
    echo ""
    
    detect_platform
    install_tusk
    add_to_path
    echo ""
    verify_installation
}

main
