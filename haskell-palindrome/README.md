
# Haskell Palindrome Implementation

This is a high-performance Haskell implementation of the palindrome finding algorithm, designed to match the performance characteristics of the Common Lisp and Rust implementations.

## Algorithmic Choices

The implementation mirrors the same optimizations used in the CL and Rust versions:

1. **Half-reverse palindrome detection** - No string conversion, minimal arithmetic
2. **Even-digit optimization** - Skip even-digit numbers not divisible by 11
3. **Tight pruning bounds** - Outer and inner pruning to minimize palindrome checks
4. **Unboxed arrays** - Use `UArray` for factor pair storage to avoid heap allocation
5. **Strict evaluation** - Bang patterns and strict fields for performance
6. **Fast integer square root** - Newton's method implementation

## Build Requirements

- GHC 9.4+
- Cabal 3.0+

## Build Instructions

```bash
# Install dependencies (if needed)
cabal update

# Run tests first
cabal test --ghc-options="-O2 -funbox-strict-fields -fspec-constr-count=10"

# Build the project
cabal build --ghc-options="-O2 -funbox-strict-fields -fspec-constr-count=10"

# Or use the build script (includes tests)
./build.sh

# Or run tests manually
./run-tests.sh
```

## Testing

The implementation includes comprehensive tests ported from the Rust version:

- **Palindrome detection tests**: Single digit, even/odd length, trailing zeros
- **Factor pair tests**: All ranges from 1-9 up to 1000-9999
- **Edge case tests**: No palindromes in range, invalid ranges
- **Benchmark case tests**: Specific cases used in performance testing

Run tests with:
```bash
./run-tests.sh
# or
cabal test
```

## Performance Optimizations

- **Unboxed arrays**: `UArray Int Word64` for factor pairs
- **Strict evaluation**: Bang patterns (`!`) on all hot path variables
- **Speculative construction**: `-fspec-constr-count=10` for aggressive inlining
- **Unboxed strict fields**: `-funbox-strict-fields` for better memory layout
- **High optimization level**: `-O2` for maximum performance

## Benchmark Runner Implementation

The benchmark runners use a sophisticated iteration strategy to prevent compiler elision:

### Range-Based Iteration
Instead of running the same range repeatedly, the runners iterate across different ranges:
- **Smallest**: Varies `min` from `minVal` to `maxVal` while keeping `max` constant
- **Largest**: Varies `max` from `maxVal` to `minVal` while keeping `min` constant

### Accumulator Anti-Elision Trick
To ensure the compiler doesn't optimize away the work, each iteration:
1. **Computes the palindrome result** for the current range
2. **Extracts the product and factor pairs** from the result
3. **Sums the factor pairs** into a running accumulator
4. **Adds the product, factor sum, and iteration counter** to the accumulator
5. **Returns both the final product and accumulator** to prove work was done

This approach ensures that:
- Each iteration processes a different range (preventing memoization)
- The accumulator value changes on every iteration (preventing elision)
- The final accumulator value is returned and printed (proving work was done)

### Mathematical Distribution
For `N` total iterations across `R` ranges:
- Each range runs `N ÷ R` times (base iterations)
- The first `N mod R` ranges run one additional time (remainder iterations)
- Total iterations = `(N ÷ R) × R + (N mod R) = N` (exactly)

## Usage

```bash
# Run smallest palindrome finder
./palprod-haskell-smallest 100 999 1

# Run largest palindrome finder  
./palprod-haskell-largest 100 999 1

# Server mode for benchmarking
./palprod-haskell-smallest --server
./palprod-haskell-largest --server
```
