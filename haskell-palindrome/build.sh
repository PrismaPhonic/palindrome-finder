#!/bin/bash

echo "Building Haskell palindrome binaries..."

# Run tests first
echo "Running tests..."
cabal test --ghc-options="-O2 -funbox-strict-fields -fspec-constr-count=10"

if [ $? -ne 0 ]; then
    echo "Tests failed!"
    exit 1
fi

# Build the library and executables
cabal build --ghc-options="-O2 -funbox-strict-fields -fspec-constr-count=10"

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
