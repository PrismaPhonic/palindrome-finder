#!/bin/bash

# Build script for Go palindrome binaries
# This script builds the Go binaries and copies them to the target-bin directory

set -euo pipefail

echo "Building Go palindrome binaries..."

# Build smallest binary
echo "Building palprod-go-smallest..."
go build -ldflags="-s -w" -gcflags="all=-B -l=4" -o palprod-go-smallest ./cmd/palprod-go-smallest

# Build largest binary  
echo "Building palprod-go-largest..."
go build -ldflags="-s -w" -gcflags="all=-B -l=4" -o palprod-go-largest ./cmd/palprod-go-largest

# Copy to target-bin directory
echo "Moving binaries to target-bin directory..."
mv palprod-go-smallest ../target-bin/
mv palprod-go-largest ../target-bin/

echo "Go binaries built successfully!"
echo "Binaries available in target-bin/:"
echo "  - palprod-go-smallest"
echo "  - palprod-go-largest"

cat <<'EOF'

Tip: Go 1.21+ supports PGO (Profile-Guided Optimization). Use build-pgo.sh to
generate a CPU profile via --server and build optimized binaries.
EOF
