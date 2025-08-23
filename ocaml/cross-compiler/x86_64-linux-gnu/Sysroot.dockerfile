FROM ubuntu:22.04

# Install development tools and libraries
RUN apt-get update && apt-get install -y \
    libc6-dev \
    build-essential \
    gcc \
    && rm -rf /var/lib/apt/lists/*

# This container is used to extract a complete sysroot
# The make_sysroot.sh script will copy files from this container