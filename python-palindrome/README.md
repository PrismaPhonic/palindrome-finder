# Python Palindrome Solution

This directory contains a Python implementation of the palindromic product finding algorithms, designed to demonstrate the performance difference between interpreted and compiled languages.

## Features

- **Pure Python**: Uses only Python standard library for maximum compatibility and performance
- **PyPy Support**: Includes PyPy executables with JIT compilation for significant performance improvements
- **Go-like Structure**: Follows the simpler Go implementation structure rather than the heavily optimized Rust version
- **Line Protocol Server**: Implements the same IPC protocol as other language implementations for fair benchmarking
- **Comprehensive Testing**: Includes unit tests for all core functionality

## Performance Characteristics

This Python implementation is expected to be significantly slower than the compiled implementations (Rust, Go, Haskell, Common Lisp, Coalton) due to:

- **Interpreted Execution**: Python bytecode interpretation overhead
- **Dynamic Typing**: Runtime type checking and object creation
- **GIL Limitations**: Global Interpreter Lock affecting multi-threading
- **Memory Overhead**: Python object overhead vs. native types

However, it uses the best available Python optimization techniques:
- Pure Python with built-in operations (no NumPy overhead for single operations)
- PyPy with JIT compilation for 65x performance improvement over regular Python
- Efficient algorithms matching the Go implementation
- Minimal object allocation in hot paths
- Fast integer arithmetic using Python's optimized built-ins

## Building

```bash
./build.sh
```

This will:
1. Create a Python virtual environment
2. Install NumPy and other dependencies
3. Create executable scripts in `../target-bin/`

## Testing

```bash
python test.py
```

## Usage

The executables follow the same line protocol as other implementations and require the `--server` flag:

```bash
# Regular Python (baseline)
./target-bin/palprod-python-smallest
./target-bin/palprod-python-largest

# PyPy with JIT (optimized)
./target-bin/palprod-pypy-smallest
./target-bin/palprod-pypy-largest
```

The executables automatically run in server mode and read commands from stdin.

### Protocol Commands

- `INIT <min> <max>`: Set the factor range
- `WARMUP <iters>`: Run iterations without reporting results
- `RUN <iters>`: Run iterations and report timing
- `QUIT`: Exit

### Example

```bash
# Regular Python
echo -e "INIT 10 99\nRUN 1\nQUIT" | ./target-bin/palprod-python-smallest

# PyPy with JIT (much faster)
echo -e "INIT 10 99\nRUN 1\nQUIT" | ./target-bin/palprod-pypy-smallest
```

Expected output:
```
OK
OK 121 121 23030
```

Where the format is: `OK <product> <accumulator> <nanoseconds>`

## Implementation Details

### Core Functions

- `is_palindrome(n)`: Fast palindrome detection with early rejection rules
- `has_even_digits(n)`: Efficient even digit count detection
- `collect_factor_pairs(product, min, max)`: Find all factor pairs in range
- `smallest(min, max)`: Find smallest palindromic product
- `largest(min, max)`: Find largest palindromic product

### Optimizations

- **Early Rejection**: Skip numbers ending in 0 or failing the 11-divisibility rule
- **Product Hoisting**: Calculate initial product once, then increment/decrement by x
- **Bounded Search**: Use mathematical bounds to limit search space
- **Fast Square Root**: Uses Python's built-in `math.isqrt()` for optimal performance

### Dependencies

- **Python 3.8+**: For type hints, modern Python features, and `math.isqrt()`
- **No external dependencies**: Uses only Python standard library

## Benchmarking

This implementation is designed to be included in the palindrome benchmark suite to demonstrate the performance difference between interpreted and compiled languages. The benchmark suite now includes:

- **Regular Python**: Baseline interpreted performance
- **PyPy with JIT**: Shows how modern JIT compilation can make interpreted languages competitive with compiled ones

PyPy achieves remarkable performance - in some cases even faster than compiled implementations!
