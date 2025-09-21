package main

import (
	"fmt"
	"os"
	"strconv"

	"palindrome"
)

func doIters(min, max uint32, iters uint32) (uint32, uint32) {
	var acc uint32 = 0
	var counter uint32 = 0
	currentMax := max

	for n := uint32(0); n < iters; n++ {
		if res := palindrome.Largest(min, currentMax); res != nil {
			prod := res.Product
			fpairs := res.Pairs
			var sPairs uint32
			for k := 0; k+1 < len(fpairs); k += 2 {
				sPairs += fpairs[k] + fpairs[k+1]
			}
			acc += prod + sPairs + counter
			counter++
		}
		if currentMax <= min {
			currentMax = max
		} else {
			currentMax--
		}
	}

	// Return the result for the original range
	result := palindrome.Largest(min, max)
	var prod uint32
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
	min64, err1 := strconv.ParseUint(args[1], 10, 32)
	max64, err2 := strconv.ParseUint(args[2], 10, 32)
	iters64, err3 := strconv.ParseUint(args[3], 10, 32)
	if err1 != nil || err2 != nil || err3 != nil {
		fmt.Fprintf(os.Stderr, "error parsing arguments\n")
		os.Exit(2)
	}
	min := uint32(min64)
	max := uint32(max64)
	iters := uint32(iters64)
	if iters == 0 {
		iters = 1
	}
	prod, _ := doIters(min, max, iters)
	fmt.Printf("%d\n", prod)
}
