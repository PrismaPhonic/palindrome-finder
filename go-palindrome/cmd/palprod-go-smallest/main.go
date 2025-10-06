package main

import (
	"fmt"
	"os"
	"runtime/pprof"
	"strconv"
	"time"

	"palindrome"
)

func doIters(min, max uint32, iters uint32) (uint32, uint64, uint64) {
	var acc uint64 = 0
	var counter uint64 = 0
	currentMin := min
	start := time.Now()

	for n := uint32(0); n < iters; n++ {
		if res := palindrome.Smallest(currentMin, max); res != nil {
			prod := res.Product
			fpairs := res.Pairs
			var sPairs uint32
			for k := 0; k+1 < len(fpairs); k += 2 {
				sPairs += fpairs[k] + fpairs[k+1]
			}
			acc += uint64(prod) + uint64(sPairs) + counter
			counter++
		}
		if currentMin >= max {
			currentMin = min
		} else {
			currentMin++
		}
	}

	// Return the result for the original range
	result := palindrome.Smallest(min, max)
	var prod uint32
	if result != nil {
		prod = result.Product
	}
	elapsed := uint64(time.Since(start).Nanoseconds())
	return prod, acc, elapsed
}

func main() {
	args := os.Args

	// Detect optional CPU profile flag (only used with --server)
	cpuProfilePath := ""
	serverMode := false
	for i := 1; i < len(args); i++ {
		if args[i] == "--server" {
			serverMode = true
		}
		if args[i] == "--cpu-profile" && i+1 < len(args) {
			cpuProfilePath = args[i+1]
			i++
		}
	}

	if serverMode {
		var f *os.File
		if cpuProfilePath != "" {
			var err error
			f, err = os.Create(cpuProfilePath)
			if err != nil {
				fmt.Fprintf(os.Stderr, "failed to create cpu profile file: %v\n", err)
				os.Exit(2)
			}
			if err := pprof.StartCPUProfile(f); err != nil {
				fmt.Fprintf(os.Stderr, "failed to start cpu profiler: %v\n", err)
				os.Exit(2)
			}
			defer func() {
				pprof.StopCPUProfile()
				_ = f.Close()
			}()
		}
		palindrome.RunServer(doIters)
		return
	}
	// Assume correct input: three positional args
	min64, _ := strconv.ParseUint(args[1], 10, 32)
	max64, _ := strconv.ParseUint(args[2], 10, 32)
	iters64, _ := strconv.ParseUint(args[3], 10, 32)
	min := uint32(min64)
	max := uint32(max64)
	iters := uint32(iters64)
	if iters == 0 {
		iters = 1
	}
	prod, _, _ := doIters(min, max, iters)
	fmt.Printf("%d\n", prod)
}
