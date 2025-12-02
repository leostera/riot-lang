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

echo -e "${GREEN}=== Building Riot Docker Image ===${NC}"
echo "Image: ${IMAGE_NAME}:${IMAGE_TAG}"
echo "Dockerfile: ${DOCKERFILE}"
echo ""

# Check if we're in the right directory
if [ ! -f "bootstrap.py" ]; then
    echo -e "${RED}Error: bootstrap.py not found!${NC}"
    echo "Please run this script from the root of the Riot repository."
    exit 1
fi

# Build the image
echo -e "${YELLOW}Building Docker image...${NC}"
docker build \
    -t ${IMAGE_NAME}:${IMAGE_TAG} \
    -f ${DOCKERFILE} \
    .

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
