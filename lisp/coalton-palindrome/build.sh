#!/bin/bash

# Build script for Coalton palindrome binaries
# This script builds the Coalton binaries and copies them to the target-bin directory

set -e

echo "Building Coalton palindrome binaries..."

# Optional: run tests here in a separate step if needed. For production builds,
# we skip running tests to avoid mixing Coalton dev/release FASLs.

# Create target-bin directory if it doesn't exist
mkdir -p ../target-bin

# Build native executables via SBCL build files
echo "Building palprod-coalton-smallest binary..."
sbcl --dynamic-space-size 4096 --noinform --disable-debugger --load build-coalton-smallest.lisp --quit | cat

echo "Building palprod-coalton-largest binary..."
sbcl --dynamic-space-size 4096 --noinform --disable-debugger --load build-coalton-largest.lisp --quit | cat

echo "Coalton binaries built successfully!"
echo "Binaries available in target-bin/:"
echo "  - palprod-coalton-smallest"
echo "  - palprod-coalton-largest"
