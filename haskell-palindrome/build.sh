#!/bin/bash

echo "Building Haskell palindrome binaries..."

# Ensure we use ghcup GHC instead of system GHC
export PATH="$HOME.ghcup/bin:$PATH"

# Run tests first
echo "Running testsources..."
cabal test

if [ $? -ne 0 ]; then
    echo "Tests failed!"
    exit 1
fi

# Build the library and executables
cabal build

if [ $? -eq 0 ]; then
    echo "Copying binaries to target-bin directory..."
    
    # Create target-bin directory if it doesn't exist
    mkdir -p ../target-bin
    
    # Copy the built executables
    cp dist-newstyle/build/*/ghc-*/haskell-palindrome-*/x/palprod-haskell-smallest/build/palprod-haskell-smallest/palprod-haskell-smallest ../target-bin/
    cp dist-newstyle/build/*/ghc-*/haskell-palindrome-*/x/palprod-haskell-largest/build/palprod-haskell-largest/palprod-haskell-largest ../target-bin/
    
    echo "Haskell binaries built successfully!"
    echo "Binaries available in target-bin/:"
    echo "  - palprod-haskell-smallest"
    echo "  - palprod-haskell-largest"
else
    echo "Build failed!"
    exit 1
fi
