#!/bin/sh
# Riot installation script
# Usage:
#   curl -sSL https://get.riot.ml | sh
#   curl -sSL https://get.riot.ml | sh -s -- -v 0.0.31
#   curl -sSL https://get.riot.ml | sh -s -- --riot-dir "$HOME/.local/share/riot"

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

usage() {
    cat <<'EOF'
Riot installer

Usage:
  curl -sSL https://get.riot.ml | sh
  curl -sSL https://get.riot.ml | sh -s -- -v 0.0.31
  curl -sSL https://get.riot.ml | sh -s -- --version 0.0.31
  curl -sSL https://get.riot.ml | sh -s -- --riot-dir "$HOME/.local/share/riot"

Options:
  -v, --version VERSION   Install a specific Riot version instead of latest.
  --riot-dir DIR          Install into DIR instead of $HOME/.riot.
  -h, --help              Show this help.

Environment:
  RIOT_VERSION            Install a specific Riot version.
  RIOT_DIR                Install into this directory instead of $HOME/.riot.
  RIOT_INSTALL_AGENT      Override the install telemetry agent header.
EOF
}

parse_args() {
    while [ "$#" -gt 0 ]; do
        case "$1" in
            -v|--version)
                if [ "$#" -lt 2 ] || [ -z "$2" ]; then
                    print_error "$1 requires a version argument"
                    exit 1
                fi
                RIOT_VERSION="$2"
                export RIOT_VERSION
                shift 2
                ;;
            --version=*)
                RIOT_VERSION="${1#--version=}"
                if [ -z "$RIOT_VERSION" ]; then
                    print_error "--version requires a version argument"
                    exit 1
                fi
                export RIOT_VERSION
                shift
                ;;
            --riot-dir)
                if [ "$#" -lt 2 ] || [ -z "$2" ]; then
                    print_error "$1 requires a directory argument"
                    exit 1
                fi
                RIOT_DIR="$2"
                export RIOT_DIR
                shift 2
                ;;
            --riot-dir=*)
                RIOT_DIR="${1#--riot-dir=}"
                if [ -z "$RIOT_DIR" ]; then
                    print_error "--riot-dir requires a directory argument"
                    exit 1
                fi
                export RIOT_DIR
                shift
                ;;
            -h|--help)
                usage
                exit 0
                ;;
            --)
                shift
                break
                ;;
            *)
                print_error "Unknown option: $1"
                echo ""
                usage
                exit 1
                ;;
        esac
    done

    if [ "$#" -gt 0 ]; then
        print_error "Unexpected argument: $1"
        echo ""
        usage
        exit 1
    fi
}

configure_paths() {
    RIOT_DIR="${RIOT_DIR:-${HOME}/.riot}"
    RIOT_BIN_DIR="${RIOT_DIR}/bin"
    export RIOT_DIR
    export RIOT_BIN_DIR
}

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

# Download and install riot
install_riot() {
    INSTALL_DIR="$RIOT_BIN_DIR"
    RIOT_HOME="$RIOT_DIR"
    RIOT_REPO="leostera/riot"
    VERSION="${RIOT_VERSION:-latest}"
    RIOT_INSTALL_AGENT="${RIOT_INSTALL_AGENT:-riot-install@1}"
    
    print_info "Installing Riot ($VERSION) for $PLATFORM..."
    
    # Create installation directory
    mkdir -p "$INSTALL_DIR"
    
    # Construct download URL from CDN
    S3_BASE_URL="https://cdn.pkgs.ml"
    
    if [ "$VERSION" = "latest" ]; then
        print_info "Fetching latest development build..."
        # For latest, try to get the current version from the API or use a known recent SHA
        # For now, users should specify a version or we default to latest
        VERSION="latest"
    fi
    
    DOWNLOAD_URL="${S3_BASE_URL}/riot/riot-${VERSION}-${PLATFORM}.tar.gz"
    
    print_info "Downloading from: $DOWNLOAD_URL"
    
    # Download and extract
    TMPDIR=$(mktemp -d)
    trap "rm -rf $TMPDIR" EXIT
    
    # Try to download
    if command -v curl >/dev/null 2>&1; then
        HTTP_CODE=$(curl -sSL -H "X-Riot-Agent: $RIOT_INSTALL_AGENT" -w "%{http_code}" -o "$TMPDIR/riot.tar.gz" "$DOWNLOAD_URL")
        if [ "$HTTP_CODE" != "200" ]; then
            handle_download_error "$HTTP_CODE"
        fi
        tar xzf "$TMPDIR/riot.tar.gz" -C "$TMPDIR"
    elif command -v wget >/dev/null 2>&1; then
        if ! wget -q --header="X-Riot-Agent: $RIOT_INSTALL_AGENT" -O "$TMPDIR/riot.tar.gz" "$DOWNLOAD_URL"; then
            handle_download_error "wget_failed"
        fi
        tar xzf "$TMPDIR/riot.tar.gz" -C "$TMPDIR"
    else
        print_error "Neither curl nor wget found. Please install one of them."
        exit 1
    fi
    
    # Move binary to install directory
    if [ ! -f "$TMPDIR/riot" ]; then
        print_error "Binary not found in downloaded archive"
        exit 1
    fi
    
    mv "$TMPDIR/riot" "$INSTALL_DIR/riot"
    chmod +x "$INSTALL_DIR/riot"

    if [ -f "$TMPDIR/release.json" ]; then
        mv "$TMPDIR/release.json" "$RIOT_HOME/release.json"
    fi
    
    print_info "Riot installed to: $INSTALL_DIR/riot"
}

# Handle download errors
handle_download_error() {
    HTTP_CODE="$1"
    
    echo ""
    print_error "Failed to download Riot binary for $PLATFORM"
    echo ""
    
    if [ "$HTTP_CODE" = "404" ]; then
        echo "The binary for your platform is not available yet."
        echo ""
        echo "Platform detected: $PLATFORM"
        echo ""
        echo "Currently supported platforms:"
        echo "  - x86_64-unknown-linux-gnu (Linux x86_64 with glibc)"
        echo "  - x86_64-unknown-linux-musl (Linux x86_64 with musl libc)"
        echo "  - aarch64-unknown-linux-gnu (Linux ARM64 with glibc)"
        echo "  - aarch64-apple-darwin (macOS Apple Silicon)"
        echo ""
        echo "If you believe this is an error, please check:"
        echo "  - The version you're trying to install exists"
        echo "  - Your platform is supported"
        echo "  - https://github.com/leostera/riot/issues"
    else
        echo "HTTP Status: $HTTP_CODE"
        echo ""
        echo "Please check:"
        echo "  - Your internet connection"
        echo "  - The CDN is accessible: https://cdn.pkgs.ml"
        echo "  - https://github.com/leostera/riot/issues"
    fi
    
    exit 1
}

# Add to PATH in shell config
add_to_path() {
    INSTALL_DIR="$RIOT_BIN_DIR"
    SHELL_NAME="$(basename "$SHELL")"
    PATH_CONFIGURED=false
    RIOT_DIR_CONFIGURED=false
    
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
            if [ "$RIOT_DIR" != "${HOME}/.riot" ]; then
                print_warn "Please manually set RIOT_DIR=$RIOT_DIR"
            fi
            return
            ;;
    esac
    
    # Check if already in config
    if [ -f "$SHELL_CONFIG" ]; then
        if grep -Fq "$INSTALL_DIR" "$SHELL_CONFIG"; then
            PATH_CONFIGURED=true
        fi

        if [ "$RIOT_DIR" = "${HOME}/.riot" ] && grep -q ".riot/bin" "$SHELL_CONFIG"; then
            PATH_CONFIGURED=true
        fi

        if grep -q "RIOT_DIR" "$SHELL_CONFIG"; then
            RIOT_DIR_CONFIGURED=true
        fi
    fi
    
    # Add to config
    if [ "$PATH_CONFIGURED" = "true" ]; then
        print_info "PATH already configured in $SHELL_CONFIG"
    elif [ "$SHELL_NAME" = "fish" ]; then
        echo "fish_add_path -g $INSTALL_DIR" >> "$SHELL_CONFIG"
        print_info "Added $INSTALL_DIR to PATH in $SHELL_CONFIG"
    else
        echo "" >> "$SHELL_CONFIG"
        echo "# Riot" >> "$SHELL_CONFIG"
        echo "export PATH=\"$INSTALL_DIR:\$PATH\"" >> "$SHELL_CONFIG"
        print_info "Added $INSTALL_DIR to PATH in $SHELL_CONFIG"
    fi

    if [ "$RIOT_DIR" != "${HOME}/.riot" ]; then
        if [ "$RIOT_DIR_CONFIGURED" = "true" ]; then
            print_info "RIOT_DIR already configured in $SHELL_CONFIG"
        elif [ "$SHELL_NAME" = "fish" ]; then
            echo "set -gx RIOT_DIR \"$RIOT_DIR\"" >> "$SHELL_CONFIG"
            print_info "Added RIOT_DIR to $SHELL_CONFIG"
        else
            echo "export RIOT_DIR=\"$RIOT_DIR\"" >> "$SHELL_CONFIG"
            print_info "Added RIOT_DIR to $SHELL_CONFIG"
        fi
    fi

    print_warn "Please restart your shell or run: source $SHELL_CONFIG"
}

# Verify installation
verify_installation() {
    INSTALL_DIR="$RIOT_BIN_DIR"
    
    if [ ! -f "$INSTALL_DIR/riot" ]; then
        print_error "Installation failed: riot binary not found"
        exit 1
    fi
    
    # Add to PATH temporarily for verification
    export PATH="$INSTALL_DIR:$PATH"
    
    if command -v riot >/dev/null 2>&1; then
        VERSION_OUTPUT=$(riot --version 2>&1 || echo "unknown")
        print_info "Riot installed successfully!"
        print_info "Version: $VERSION_OUTPUT"
        echo ""
        print_info "To get started, run:"
        echo "  riot --help"
    else
        print_error "Installation completed but riot not found in PATH"
        exit 1
    fi
}

# Main installation flow
main() {
    parse_args "$@"
    configure_paths

    print_info "Riot Installation Script"
    echo ""
    
    detect_platform
    install_riot
    add_to_path
    echo ""
    verify_installation
}

main "$@"
