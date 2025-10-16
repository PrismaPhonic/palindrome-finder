#!/usr/bin/env zsh

echo "Building TypeScript palindrome solution..."

# Check if Deno is installed
if ! command -v deno &> /dev/null; then
    echo "Error: Deno is not installed"
    exit 1
fi

# Check if Bun is installed
if ! command -v bun &> /dev/null; then
    echo "Error: Bun is not installed"
    exit 1
fi

# Create target-bin directory if it doesn't exist
mkdir -p ../target-bin

echo "Creating executable scripts..."

# Get the absolute path to the typescript-palindrome directory
TYPESCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Create Deno executables - use Deno-specific file with V8 optimization flags including AVX2
cat > ../target-bin/palprod-deno-smallest << EOF
#!/bin/bash
deno run --allow-read --v8-flags="--invocation-count-for-early-optimization=5,--invocation-count-for-maglev-with-delay=50,--concurrent-turbofan-max-threads=8,--max-inlined-bytecode-size=2000,--turbo-loop-peeling,--turbo-loop-variable,--turbo-escape,--turbo-allocation-folding,--stress-inline,--enable-avx2,--enable-avx,--enable-sse4-2,--enable-sse4-1,--regexp-simd" "$TYPESCRIPT_DIR/palindrome-deno.ts" --server smallest
EOF

cat > ../target-bin/palprod-deno-largest << EOF
#!/bin/bash
deno run --allow-read --v8-flags="--invocation-count-for-early-optimization=5,--invocation-count-for-maglev-with-delay=50,--concurrent-turbofan-max-threads=8,--max-inlined-bytecode-size=2000,--turbo-loop-peeling,--turbo-loop-variable,--turbo-escape,--turbo-allocation-folding,--stress-inline,--enable-avx2,--enable-avx,--enable-sse4-2,--enable-sse4-1,--regexp-simd" "$TYPESCRIPT_DIR/palindrome-deno.ts" --server largest
EOF

# Create Bun executables - use Bun-specific file
cat > ../target-bin/palprod-bun-smallest << EOF
#!/bin/bash
bun run "$TYPESCRIPT_DIR/palindrome-bun.ts" --server smallest
EOF

cat > ../target-bin/palprod-bun-largest << EOF
#!/bin/bash
bun run "$TYPESCRIPT_DIR/palindrome-bun.ts" --server largest
EOF

# Make executables executable
chmod +x ../target-bin/palprod-deno-smallest
chmod +x ../target-bin/palprod-deno-largest
chmod +x ../target-bin/palprod-bun-smallest
chmod +x ../target-bin/palprod-bun-largest

echo "Build complete!"
echo "Executables created:"
echo "  - ../target-bin/palprod-deno-smallest (Deno optimized)"
echo "  - ../target-bin/palprod-deno-largest (Deno optimized)"
echo "  - ../target-bin/palprod-bun-smallest (Bun)"
echo "  - ../target-bin/palprod-bun-largest (Bun)"
