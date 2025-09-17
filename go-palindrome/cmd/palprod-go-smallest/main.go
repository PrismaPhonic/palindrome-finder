package main

import (
	"fmt"
	"os"
	"strconv"

	"palindrome"
)

func doIters(min, max uint64, iters uint64) (uint64, uint64) {
	rangeCount := max - min + 1
	itersPerRange := iters / rangeCount
	remainder := iters % rangeCount
	var acc uint64 = 0
	var counter uint64 = 0

	// Base iterations: run itersPerRange times for each range
	for idx := uint64(0); idx < rangeCount; idx++ {
		currentMin := min + idx
		for j := uint64(0); j < itersPerRange; j++ {
			if res := palindrome.Smallest(currentMin, max); res != nil {
				prod := res.Product
				fpairs := res.Pairs
				var sPairs uint64
				for k := 0; k+1 < len(fpairs); k += 2 {
					sPairs += fpairs[k] + fpairs[k+1]
				}
				acc += prod + sPairs + counter
				counter++
			}
		}
	}

	// Remainder iterations: run 1 additional time for first remainder ranges
	for idx := uint64(0); idx < remainder; idx++ {
		currentMin := min + idx
		if res := palindrome.Smallest(currentMin, max); res != nil {
			prod := res.Product
			fpairs := res.Pairs
			var sPairs uint64
			for k := 0; k+1 < len(fpairs); k += 2 {
				sPairs += fpairs[k] + fpairs[k+1]
			}
			acc += prod + sPairs + counter
			counter++
		}
	}

	// Return the result for the original range
	result := palindrome.Smallest(min, max)
	var prod uint64
	if result != nil {
		prod = result.Product
	}
	return prod, acc
}

func main() {
	args := os.Args
	if len(args) > 1 && args[1] == "--server" {
		palindrome.RunServer(doIters)
		return
	}
	if len(args) != 4 {
		fmt.Fprintf(os.Stderr, "usage: %s <min> <max> <iters>  |  %s --server\n", args[0], args[0])
		os.Exit(2)
	}
	min, err1 := strconv.ParseUint(args[1], 10, 64)
	max, err2 := strconv.ParseUint(args[2], 10, 64)
	iters, err3 := strconv.ParseUint(args[3], 10, 64)
	if err1 != nil || err2 != nil || err3 != nil {
		fmt.Fprintf(os.Stderr, "error parsing arguments\n")
		os.Exit(2)
	}
	if iters == 0 {
		iters = 1
	}
	prod, _ := doIters(min, max, iters)
	fmt.Printf("%d\n", prod)
}
