#!/bin/bash

echo "Running Haskell palindrome tests..."

# Ensure we use ghcup GHC instead of system GHC
export PATH="$HOME/.ghcup/bin:$PATH"

# Check if ghc is available
if ! command -v ghc &> /dev/null; then
    echo "Error: GHC not found. Please install GHC to run tests."
    echo "On Ubuntu/Debian: sudo apt install ghc"
    echo "On macOS: brew install ghc"
    exit 1
fi

# Compile the Palindrome module first
echo "Compiling Palindrome module..."
ghc -O2 -funbox-strict-fields -fspec-constr-count=10 -c src/Palindrome.hs

# Compile and run tests
echo "Compiling test suite..."
ghc -O2 -funbox-strict-fields -fspec-constr-count=10 -isrc tests.hs -o tests

if [ $? -eq 0 ]; then
    echo "Running tests..."
    ./tests
    echo ""
    echo "Tests completed!"
    rm -f tests tests.hi tests.o src/Palindrome.o src/Palindrome.hi
else
    echo "Compilation failed!"
    exit 1
fi
