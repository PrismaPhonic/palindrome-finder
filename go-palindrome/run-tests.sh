#!/usr/bin/env bash
set -euo pipefail

echo "Running Go palindrome tests..."

# Resolve paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "$SCRIPT_DIR"

# Check if go is available
if ! command -v go >/dev/null 2>&1; then
  echo "Error: Go not found. Please install Go to run tests."
  echo "Visit: https://golang.org/dl/"
  exit 1
fi

# Run tests with verbose output
echo "Running test suite..."
go test -v

echo ""
echo "Go tests completed!"
