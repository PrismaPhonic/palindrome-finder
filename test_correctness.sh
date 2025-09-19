#!/bin/bash

echo "Testing correctness of all palindrome servers..."
echo "Range: 1-999, Iterations: 10000"
echo "=================================="

# Test parameters
MIN_VAL=1
MAX_VAL=999
ITERS=10000

# Function to test a server
test_server() {
    local server_name="$1"
    local server_path="$2"
    local test_type="$3"
    
    echo "Testing $server_name ($test_type)..." >&2
    
    # Send commands and capture output
    {
        echo "INIT $MIN_VAL $MAX_VAL"
        echo "WARMUP $ITERS"
        echo "RUN $ITERS"
        echo "QUIT"
    } | timeout 30 "$server_path" --server 2>/dev/null | grep "OK " | tail -1
}

# Test smallest servers
echo ""
echo "=== SMALLEST SERVERS ==="
echo "Server Name | Product | Accumulator"
echo "------------|---------|------------"

smallest_results=()

# Test Haskell smallest
result=$(test_server "Haskell" "./target-bin/palprod-haskell-smallest" "smallest")
echo "Haskell     | $result"
smallest_results+=("Haskell:$result")

# Test Go smallest
result=$(test_server "Go" "./target-bin/palprod-go-smallest" "smallest")
echo "Go          | $result"
smallest_results+=("Go:$result")

# Test Rust smallest
result=$(test_server "Rust" "./target-bin/palprod-rust-smallest" "smallest")
echo "Rust        | $result"
smallest_results+=("Rust:$result")

# Test Coalton smallest
result=$(test_server "Coalton" "./target-bin/palprod-coalton-smallest" "smallest")
echo "Coalton     | $result"
smallest_results+=("Coalton:$result")

# Test Fast smallest
result=$(test_server "Fast" "./target-bin/palprod-fast-smallest-inner" "smallest")
echo "Fast        | $result"
smallest_results+=("Fast:$result")

# Test largest servers
echo ""
echo "=== LARGEST SERVERS ==="
echo "Server Name | Product | Accumulator"
echo "------------|---------|------------"

largest_results=()

# Test Haskell largest
result=$(test_server "Haskell" "./target-bin/palprod-haskell-largest" "largest")
echo "Haskell     | $result"
largest_results+=("Haskell:$result")

# Test Go largest
result=$(test_server "Go" "./target-bin/palprod-go-largest" "largest")
echo "Go          | $result"
largest_results+=("Go:$result")

# Test Rust largest
result=$(test_server "Rust" "./target-bin/palprod-rust-largest" "largest")
echo "Rust        | $result"
largest_results+=("Rust:$result")

# Test Coalton largest
result=$(test_server "Coalton" "./target-bin/palprod-coalton-largest" "largest")
echo "Coalton     | $result"
largest_results+=("Coalton:$result")

# Test Fast largest
result=$(test_server "Fast" "./target-bin/palprod-fast-largest-inner" "largest")
echo "Fast        | $result"
largest_results+=("Fast:$result")

# Analyze results
echo ""
echo "=== CORRECTNESS ANALYSIS ==="

# Check smallest results
echo "Smallest servers:"
smallest_products=()
smallest_accumulators=()

for result in "${smallest_results[@]}"; do
    IFS=':' read -r name data <<< "$result"
    # Parse "OK product accumulator" format
    if [[ "$data" =~ ^OK[[:space:]]+([0-9]+)[[:space:]]+([0-9]+)$ ]]; then
        product="${BASH_REMATCH[1]}"
        accumulator="${BASH_REMATCH[2]}"
        smallest_products+=("$product")
        smallest_accumulators+=("$accumulator")
        echo "  $name: product=$product, accumulator=$accumulator"
    else
        echo "  $name: ERROR parsing result: $data"
    fi
done

# Check if all products match
smallest_product_consistent=true
first_product="${smallest_products[0]}"
for product in "${smallest_products[@]}"; do
    if [ "$product" != "$first_product" ]; then
        smallest_product_consistent=false
        break
    fi
done

# Check if all accumulators match
smallest_accumulator_consistent=true
first_accumulator="${smallest_accumulators[0]}"
for accumulator in "${smallest_accumulators[@]}"; do
    if [ "$accumulator" != "$first_accumulator" ]; then
        smallest_accumulator_consistent=false
        break
    fi
done

echo ""
echo "Largest servers:"
largest_products=()
largest_accumulators=()

for result in "${largest_results[@]}"; do
    IFS=':' read -r name data <<< "$result"
    # Parse "OK product accumulator" format
    if [[ "$data" =~ ^OK[[:space:]]+([0-9]+)[[:space:]]+([0-9]+)$ ]]; then
        product="${BASH_REMATCH[1]}"
        accumulator="${BASH_REMATCH[2]}"
        largest_products+=("$product")
        largest_accumulators+=("$accumulator")
        echo "  $name: product=$product, accumulator=$accumulator"
    else
        echo "  $name: ERROR parsing result: $data"
    fi
done

# Check if all products match
largest_product_consistent=true
first_product="${largest_products[0]}"
for product in "${largest_products[@]}"; do
    if [ "$product" != "$first_product" ]; then
        largest_product_consistent=false
        break
    fi
done

# Check if all accumulators match
largest_accumulator_consistent=true
first_accumulator="${largest_accumulators[0]}"
for accumulator in "${largest_accumulators[@]}"; do
    if [ "$accumulator" != "$first_accumulator" ]; then
        largest_accumulator_consistent=false
        break
    fi
done

echo ""
echo "=== FINAL RESULTS ==="
if [ "$smallest_product_consistent" = true ] && [ "$smallest_accumulator_consistent" = true ]; then
    echo "✅ Smallest servers: ALL CONSISTENT"
    echo "   Product: $first_product"
    echo "   Accumulator: $first_accumulator"
else
    echo "❌ Smallest servers: INCONSISTENT"
    if [ "$smallest_product_consistent" = false ]; then
        echo "   Products don't match!"
    fi
    if [ "$smallest_accumulator_consistent" = false ]; then
        echo "   Accumulators don't match!"
    fi
fi

if [ "$largest_product_consistent" = true ] && [ "$largest_accumulator_consistent" = true ]; then
    echo "✅ Largest servers: ALL CONSISTENT"
    echo "   Product: $first_product"
    echo "   Accumulator: $first_accumulator"
else
    echo "❌ Largest servers: INCONSISTENT"
    if [ "$largest_product_consistent" = false ]; then
        echo "   Products don't match!"
    fi
    if [ "$largest_accumulator_consistent" = false ]; then
        echo "   Accumulators don't match!"
    fi
fi

echo ""
echo "Test completed!"
