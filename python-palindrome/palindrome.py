#!/usr/bin/env python3
"""
Python implementation of palindromic product finding algorithms.
"""

import sys
import time
import math
from typing import Optional, List, Tuple



def has_even_digits(n: int) -> bool:
    """
    Returns True if n has an even number of decimal digits.
    Preconditions: n >= 11. Callers should filter out n < 10 and trailing-zero cases.
    We use this to apply the rule "even-length palindromes must be divisible by 11".
    """
    cmp_a = int(n >= 100)
    cmp_b = int(n >= 1_000)
    cmp_c = int(n >= 10_000)
    cmp_d = int(n >= 100_000)
    t0 = cmp_a ^ cmp_b
    t1 = cmp_c ^ cmp_d
    parity = 1 ^ (t0 ^ t1)
    return parity == 1



def is_palindrome(n: int) -> bool:
    """
    Returns True if n is a decimal palindrome (numeric half-reversal).
    """
    if n < 10:
        return True
    
    # Non-zero numbers ending in 0 cannot be palindromes.
    if n % 10 == 0 and n != 0:
        return False
    
    # Even-length palindromes must be divisible by 11.
    if has_even_digits(n) and n % 11 != 0:
        return False

    # Half-reverse
    m = n
    rev = 0
    while m > rev:
        rev = rev * 10 + m % 10
        m //= 10

    return m == rev or m == rev // 10



def collect_factor_pairs(product: int, min_val: int, max_val: int) -> List[int]:
    """
    Collects ordered factor pairs (x, y) (with x <= y) such that
    x * y == product and both factors lie in [min_val..max_val].
    Returns a flat array [x0, y0, x1, y1, ...]
    Uses a tight divisor window:
        x in [ ceil(product / max_val) .. min(max_val, isqrt(product)) ]
    """
    # Tight window: x in [ceil(product/max_val) .. min(max_val, isqrt(product))]
    low = max(min_val, (product + max_val - 1) // max_val)  # ceil(product/max_val)
    high = min(max_val, math.isqrt(product))

    # Use list to collect pairs (up to 4 elements = 2 pairs)
    pairs = []
    for x in range(low, high + 1):
        if product % x == 0:
            y = product // x
            pairs.extend([x, y])
            if len(pairs) == 4:  # Limit to 2 pairs
                break

    return pairs



def smallest(min_val: int, max_val: int) -> Optional[Tuple[int, List[int]]]:
    """
    Finds the smallest palindromic product in [min_val..max_val] and its factor pairs.
    Returns (product, pairs) or None if either the range is invalid or no palindrome exists.
    The factor pairs are returned as a flat array [x0, y0, x1, y1, ...].
    Algorithm:
      - x ascends from min_val to max_val.
      - Outer prune: if x*x >= best, later rows cannot improve best.
      - For a fixed x, we only need y up to y_upper = min(max_val, (best-1)/x).
        If y_upper < x, there is no work in that row.
      - Iterate y from x to y_upper; the first palindrome in that row is
        the row minimum; update best and continue.
    """
    best = 2**32 - 1  # equivalent to u32::MAX in Rust

    for x in range(min_val, max_val + 1):
        if x * x >= best:
            break  # outer prune

        # Row cap: only products < best matter.
        y_upper = min(max_val, (best - 1) // x)
        if y_upper < x:
            continue  # no valid y in this row

        # Hoist initial product calculation and increment by x
        y, prod = x, x * x
        while True:
            if is_palindrome(prod):
                best = prod
                break
            if y == y_upper:
                break
            y += 1
            prod += x

    if best == 2**32 - 1:
        return None
    else:
        pairs = collect_factor_pairs(best, min_val, max_val)
        return (best, pairs)



def largest(min_val: int, max_val: int) -> Optional[Tuple[int, List[int]]]:
    """
    Finds the largest palindromic product in [min_val..max_val] and its factor pairs.
    Returns (product, pairs) or None if either the range is invalid or no palindrome exists.
    The factor pairs are returned as a flat array [x0, y0, x1, y1, ...].
    Algorithm:
      - x descends from max_val to min_val.
      - Outer prune: if x*max_val <= best, earlier rows cannot improve best.
      - For a fixed x, only products > best matter. That means
        y >= floor(best/x) + 1. We also require y >= x to keep x <= y.
        Let yLower = max(x, floor(best/x)+1).
      - Iterate y from max_val down to yLower; the first palindrome in that row
        is the row maximum; update best and continue.
    """
    best = 0

    # x descends
    for x in range(max_val, min_val - 1, -1):
        # Outer prune: once x*max_val <= best, smaller x cannot improve.
        if x * max_val <= best:
            break

        # y lower bound: only products > best matter; also enforce y >= x.
        y_lower = max(x, (best // x) + 1)

        if y_lower > max_val:
            continue  # no work for this row

        # Hoist initial product calculation and decrement by x
        y, prod = max_val, x * max_val
        while True:
            if is_palindrome(prod):
                best = prod
                break
            if y == y_lower:
                break
            y -= 1
            prod -= x  # decrement by x instead of recalculating x * y

    if best > 0:
        pairs = collect_factor_pairs(best, min_val, max_val)
        return (best, pairs)
    else:
        return None



def run_server(do_iters_func):
    """
    Implements the generic line-protocol server used by the benchmark harness.
    Protocol (one command per line):
      - INIT <min> <max>: set the factor range (must be called first).
      - WARMUP <iters>: run iters iterations without reporting a result.
      - RUN <iters>: run iters iterations and print OK <product> with the
        last product found.
      - QUIT: exit.
    """
    min_val = 0
    max_val = 0

    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue

        parts = line.split()
        if not parts:
            continue

        command = parts[0]

        if command == "INIT":
            min_val = int(parts[1])
            max_val = int(parts[2])
            print("OK")
            sys.stdout.flush()

        elif command == "WARMUP":
            iters = int(parts[1])
            do_iters_func(min_val, max_val, iters)
            print("OK")
            sys.stdout.flush()

        elif command == "RUN":
            iters = int(parts[1])
            prod, acc, nanos = do_iters_func(min_val, max_val, iters)
            print(f"OK {prod} {acc} {nanos}")
            sys.stdout.flush()

        elif command == "QUIT":
            break



def do_iters_smallest(min_val: int, max_val: int, iters: int) -> Tuple[int, int, int]:
    """Run iterations for smallest palindrome finding."""
    acc = 0
    counter = 0
    current = min_val
    start_time = time.perf_counter_ns()
    
    for _ in range(iters):
        result = smallest(current, max_val)
        if result:
            product, pairs = result
            # Match Rust's accumulation: product + counter + sum of all factor pairs
            acc += product + counter + sum(pairs)
            counter += 1
        
        # Increment current, wrapping back to min_val when reaching max_val
        current = min_val if current >= max_val else current + 1
    
    end_time = time.perf_counter_ns()
    nanos = end_time - start_time
    
    # Return the last product found (or 0 if none)
    last_result = smallest(min_val, max_val)
    last_prod = last_result[0] if last_result else 0
    
    return last_prod, acc, nanos



def do_iters_largest(min_val: int, max_val: int, iters: int) -> Tuple[int, int, int]:
    """Run iterations for largest palindrome finding."""
    acc = 0
    counter = 0
    current = max_val
    start_time = time.perf_counter_ns()
    
    for _ in range(iters):
        result = largest(min_val, current)
        if result:
            product, pairs = result
            # Match Rust's accumulation: product + counter + sum of all factor pairs
            acc += product + counter + sum(pairs)
            counter += 1
        
        # Decrement current, wrapping back to max_val when reaching min_val
        current = max_val if current <= min_val else current - 1
    
    end_time = time.perf_counter_ns()
    nanos = end_time - start_time
    
    # Return the last product found (or 0 if none)
    last_result = largest(min_val, max_val)
    last_prod = last_result[0] if last_result else 0
    
    return last_prod, acc, nanos



if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python palindrome.py <smallest|largest> --server [--jit]", file=sys.stderr)
        sys.exit(1)
    
    mode = sys.argv[1]
    flag = sys.argv[2]
    use_jit = len(sys.argv) > 3 and sys.argv[3] == "--jit"
    
    if flag != "--server":
        print("Must use --server flag", file=sys.stderr)
        sys.exit(1)
    
    # PyPy automatically JIT compiles hot functions
    if use_jit:
        print("PyPy will automatically JIT compile hot functions", file=sys.stderr)
    
    if mode == "smallest":
        run_server(do_iters_smallest)
    elif mode == "largest":
        run_server(do_iters_largest)
    else:
        print("Mode must be 'smallest' or 'largest'", file=sys.stderr)
        sys.exit(1)
