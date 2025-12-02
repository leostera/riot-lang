#!/bin/bash
# Build script for Riot Docker images
set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default values
IMAGE_NAME="${IMAGE_NAME:-riot-builder}"
IMAGE_TAG="${IMAGE_TAG:-latest}"
DOCKERFILE="${DOCKERFILE:-docker/Dockerfile}"
PLATFORM="${PLATFORM:-}"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --platform)
            PLATFORM="$2"
            shift 2
            ;;
        --name)
            IMAGE_NAME="$2"
            shift 2
            ;;
        --tag)
            IMAGE_TAG="$2"
            shift 2
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --platform PLATFORM    Build for specific platform (e.g., linux/amd64, linux/arm64)"
            echo "  --name NAME           Image name (default: riot-builder)"
            echo "  --tag TAG             Image tag (default: latest)"
            echo "  --help                Show this help message"
            echo ""
            echo "Examples:"
            echo "  $0                                # Build for native platform"
            echo "  $0 --platform linux/amd64         # Build for x86_64 Linux"
            echo "  $0 --platform linux/arm64         # Build for ARM64 Linux"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

echo -e "${GREEN}=== Building Riot Docker Image ===${NC}"
echo "Image: ${IMAGE_NAME}:${IMAGE_TAG}"
echo "Dockerfile: ${DOCKERFILE}"
if [ -n "$PLATFORM" ]; then
    echo "Platform: ${PLATFORM}"
fi
echo ""

# Check if we're in the right directory
if [ ! -f "bootstrap.py" ]; then
    echo -e "${RED}Error: bootstrap.py not found!${NC}"
    echo "Please run this script from the root of the Riot repository."
    exit 1
fi

# Build the image
echo -e "${YELLOW}Building Docker image...${NC}"

# Build platform-specific command
BUILD_CMD="docker build"
if [ -n "$PLATFORM" ]; then
    BUILD_CMD="$BUILD_CMD --platform $PLATFORM"
fi
BUILD_CMD="$BUILD_CMD -t ${IMAGE_NAME}:${IMAGE_TAG} -f ${DOCKERFILE} ."

echo "Command: $BUILD_CMD"
echo ""
eval $BUILD_CMD

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Build successful!${NC}"
    echo ""
    echo "Image details:"
    docker images ${IMAGE_NAME}:${IMAGE_TAG}
    echo ""
    echo -e "${GREEN}Test the image with:${NC}"
    echo "  docker run --rm ${IMAGE_NAME}:${IMAGE_TAG} --help"
    echo ""
    echo -e "${GREEN}Use in your project:${NC}"
    echo "  docker run --rm -v \$(pwd):/app ${IMAGE_NAME}:${IMAGE_TAG} build"
else
    echo -e "${RED}✗ Build failed!${NC}"
    exit 1
fi
