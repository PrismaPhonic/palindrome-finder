// Package palindrome implements palindromic product finding algorithms
// that closely mirror the Rust implementation for fair benchmarking.
package palindrome

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

// hasEvenDigits returns true if n has an even number of decimal digits.
// Preconditions: n >= 11. Callers should filter out n < 10 and trailing-zero cases.
// We use this to apply the rule "even-length palindromes must be divisible by 11".
//
//go:inline
func hasEvenDigits(n uint32) bool {
	// Use comparisons to count digits efficiently (capped to 10 digits for uint32)
	if n < 100 {
		return true // 2 digits
	} else if n < 1_000 {
		return false // 3 digits
	} else if n < 10_000 {
		return true // 4 digits
	} else if n < 100_000 {
		return false // 5 digits
	} else if n < 1_000_000 {
		return true // 6 digits
	} else if n < 10_000_000 {
		return false // 7 digits
	} else if n < 100_000_000 {
		return true // 8 digits
	} else if n < 1_000_000_000 {
		return false // 9 digits
	} else {
		return true // 10 digits (max for uint32)
	}
}

// IsPal returns true if n is a decimal palindrome (numeric half-reversal).
// Fast-paths:
// - n < 10 is palindrome
// - n % 10 == 0 (and n != 0) cannot be palindrome
// - even-length palindromes must be divisible by 11; if not, reject
// Core:
// - Build rev by taking right digits of m until rev >= m.
// - Then return m == rev || m == rev/10.
//
//go:inline
func IsPal(n uint32) bool {
	if n < 10 {
		return true
	}
	// Non-zero numbers ending in 0 cannot be palindromes.
	if n%10 == 0 && n != 0 {
		return false
	}
	// Even-length palindromes must be divisible by 11.
	if hasEvenDigits(n) && n%11 != 0 {
		return false
	}

	// Half-reverse
	m := n
	var rev uint32 = 0
	for m > rev {
		rev = rev*10 + m%10
		m /= 10
	}

	return m == rev || m == rev/10
}

// CollectFactorPairs collects ordered factor pairs (x, y) (with x <= y) such that
// x * y == product and both factors lie in [min..max].
// Returns a flat array [x0, y0, x1, y1, ...] to match the Rust approach.
// Uses a tight divisor window:
//
//	x in [ ceil(product / max) .. min(max, isqrt(product)) ]
//
//go:inline
func CollectFactorPairs(product, min, max uint32) []uint32 {
	// Tight window: x in [ceil(product/max) .. min(max, isqrt(product))]
	low := maxUint32(min, (product+max-1)/max) // ceil(product/max)
	high := minUint32(max, fastIsqrt(product))

	// Use fixed-size buffer with running count (4 elements = 2 pairs)
	var buffer [4]uint32
	count := 0
	for x := low; x <= high && count < 4; x++ {
		if product%x == 0 {
			y := product / x
			buffer[count] = x
			count++
			buffer[count] = y
			count++
		}
	}

	return buffer[:count]
}

// Smallest finds the smallest palindromic product in [min..max] and its factor pairs.
// Returns (product, pairs, true) or (0, nil, false) if either the range is invalid or
// no palindrome exists.
// The factor pairs are returned as a flat array [x0, y0, x1, y1, ...].
// Algorithm:
//   - x ascends from min to max.
//   - Outer prune: if x*x >= best, later rows cannot improve best.
//   - For a fixed x, we only need y up to y_upper = min(max, (best-1)/x).
//     If y_upper < x, there is no work in that row.
//   - Iterate y from x to y_upper; the first palindrome in that row is
//     the row minimum; update best and continue.
//
//go:inline
func Smallest(min, max uint32) *struct {
	Product uint32
	Pairs   []uint32
} {

	best := ^uint32(0) // equivalent to u32::MAX in Rust

	for x := min; x <= max; x++ {
		if x*x >= best {
			break // outer prune
		}

		// Row cap: only products < best matter.
		yUpper := minUint32(max, (best-1)/x)
		if yUpper < x {
			continue // no valid y in this row
		}

		for y := x; y <= yUpper; y++ {
			// No prod >= best check needed; yUpper already enforces it.
			prod := x * y // guaranteed < best via yUpper
			if IsPal(prod) {
				best = prod
				break // row minimum found; move to next x
			}
		}
	}

	if best == ^uint32(0) {
		return nil
	} else {
		pairs := CollectFactorPairs(best, min, max)
		return &struct {
			Product uint32
			Pairs   []uint32
		}{Product: best, Pairs: pairs}
	}
}

// Largest finds the largest palindromic product in [min..max] and its factor pairs.
// Returns (product, pairs, true) or (0, nil, false) if either the range is invalid or
// no palindrome exists.
// The factor pairs are returned as a flat array [x0, y0, x1, y1, ...].
// Algorithm:
//   - x descends from max to min.
//   - Outer prune: if x*max <= best, earlier rows cannot improve best.
//   - For a fixed x, only products > best matter. That means
//     y >= floor(best/x) + 1. We also require y >= x to keep x <= y.
//     Let yLower = max(x, floor(best/x)+1).
//   - Iterate y from max down to yLower; the first palindrome in that row
//     is the row maximum; update best and continue.
//
//go:inline
func Largest(min, max uint32) *struct {
	Product uint32
	Pairs   []uint32
} {

	var best uint32 = 0

	// x descends
	for x := max; x >= min; x-- {
		// Outer prune: once x*max <= best, smaller x cannot improve.
		if x*max <= best {
			break
		}

		// y lower bound: only products > best matter; also enforce y >= x.
		yLower := maxUint32(x, (best/x)+1)

		if yLower > max {
			continue // no work for this row
		}

		for y := max; y >= yLower; y-- {
			p := x * y // guaranteed > best by bounds of yLower
			if IsPal(p) {
				best = p // row maximum found
				break    // move to next x
			}
		}
	}

	if best > 0 {
		pairs := CollectFactorPairs(best, min, max)
		return &struct {
			Product uint32
			Pairs   []uint32
		}{Product: best, Pairs: pairs}
	} else {
		return nil
	}
}

// RunServer implements the generic line-protocol server used by the benchmark harness.
// Protocol (one command per line):
//   - INIT <min> <max>: set the factor range (must be called first).
//   - WARMUP <iters>: run iters iterations without reporting a result.
//   - RUN <iters>: run iters iterations and print OK <product> with the
//     last product found.
//   - QUIT: exit.
//
// The doIters function must do the full work (including
// factor pair building) and return the final product as (uint32, uint32).
func RunServer(doIters func(uint32, uint32, uint32) (uint32, uint32)) {
	reader := bufio.NewReader(os.Stdin)
	writer := bufio.NewWriter(os.Stdout)
	defer writer.Flush()

	var min, max *uint32

	for {
		line, err := reader.ReadString('\n')
		if err == io.EOF {
			break
		}
		if err != nil {
			panic(err)
		}

		parts := strings.Fields(strings.TrimSpace(line))
		if len(parts) == 0 {
			continue
		}

		cmd := strings.ToUpper(parts[0])
		switch cmd {
		case "INIT":
			if len(parts) != 3 {
				fmt.Fprintln(writer, "ERR BADARGS")
				writer.Flush()
				continue
			}
			a64, err1 := strconv.ParseUint(parts[1], 10, 32)
			b64, err2 := strconv.ParseUint(parts[2], 10, 32)
			if err1 != nil || err2 != nil {
				fmt.Fprintln(writer, "ERR BADARGS")
				writer.Flush()
				continue
			}
			a := uint32(a64)
			b := uint32(b64)
			min = &a
			max = &b
			fmt.Fprintln(writer, "OK")
			writer.Flush()

		case "WARMUP":
			if len(parts) != 2 {
				fmt.Fprintln(writer, "ERR BADARGS")
				writer.Flush()
				continue
			}
			iters64, err := strconv.ParseUint(parts[1], 10, 32)
			if err != nil {
				fmt.Fprintln(writer, "ERR BADARGS")
				writer.Flush()
				continue
			}
			iters := uint32(iters64)
			if min != nil && max != nil {
				_, _ = doIters(*min, *max, iters)
			}
			fmt.Fprintln(writer, "OK")
			writer.Flush()

		case "RUN":
			if len(parts) != 2 {
				fmt.Fprintln(writer, "ERR BADARGS")
				writer.Flush()
				continue
			}
			iters64, err := strconv.ParseUint(parts[1], 10, 32)
			if err != nil {
				fmt.Fprintln(writer, "ERR BADARGS")
				writer.Flush()
				continue
			}
			iters := uint32(iters64)
			if min != nil && max != nil {
				prod, acc := doIters(*min, *max, iters)
				fmt.Fprintf(writer, "OK %d %d\n", prod, acc)
			} else {
				fmt.Fprintln(writer, "ERR NOTINIT")
			}
			writer.Flush()

		case "QUIT":
			return

		default:
			fmt.Fprintln(writer, "ERR BADCMD")
			writer.Flush()
		}
	}
}

// fastIsqrt computes the integer square root using Newton's method with bit manipulation
//
//go:inline
func fastIsqrt(n uint32) uint32 {
	if n < 2 {
		return n
	}

	// Initial guess using bit manipulation
	x := n
	y := (x + 1) >> 1 // equivalent to (x + 1) / 2

	// Newton's method: x = (x + n/x) / 2
	for y < x {
		x = y
		y = (x + n/x) >> 1
	}

	return x
}

// Helper functions for min/max since Go doesn't have built-in ones for uint32
//
//go:inline
func minUint32(a, b uint32) uint32 {
	if a < b {
		return a
	}

	return b
}

//
//go:inline
func maxUint32(a, b uint32) uint32 {
	if a > b {
		return a
	}

	return b
}
