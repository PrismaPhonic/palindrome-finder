package main

import (
	"fmt"
	"os"
	"strconv"

	"palindrome"
)

func doIters(min, max uint64, iters uint64) (uint64, bool) {
	for i := uint64(0); i < iters; i++ {
		_ = palindrome.Smallest(min, max)
	}
	result := palindrome.Smallest(min, max)
	if result == nil {
		return 0, false
	}
	return result.Product, true
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
