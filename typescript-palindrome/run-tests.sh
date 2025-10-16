#!/bin/bash

echo "Running TypeScript palindrome tests..."

# Check if Deno is available
if ! command -v deno &> /dev/null; then
    echo "Error: Deno not found. Please install Deno to run tests."
    echo "Install from: https://deno.land/manual/getting_started/installation"
    exit 1
fi

# Check if Bun is available
if ! command -v bun &> /dev/null; then
    echo "Error: Bun not found. Please install Bun to run tests."
    echo "Install from: https://bun.sh/docs/installation"
    exit 1
fi

echo ""
echo "=== Running tests with Deno ==="
echo ""

# Run tests with Deno
if deno run --allow-read test.ts; then
    echo "✓ Deno tests passed"
else
    echo "✗ Deno tests failed"
    exit 1
fi

echo ""
echo "=== Running tests with Bun ==="
echo ""

# Run tests with Bun
if bun run test.ts; then
    echo "✓ Bun tests passed"
else
    echo "✗ Bun tests failed"
    exit 1
fi

echo ""
echo "All TypeScript tests passed with both Deno and Bun! 🎉"
