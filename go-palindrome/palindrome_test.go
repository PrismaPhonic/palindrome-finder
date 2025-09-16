package palindrome

import (
	"reflect"
	"sort"
	"testing"
)

// Helper function to normalize factor pairs for comparison (matches Rust norm function)
func norm(pairs [][2]uint64) [][2]uint64 {
	result := make([][2]uint64, len(pairs))
	copy(result, pairs)
	sort.Slice(result, func(i, j int) bool {
		if result[i][0] == result[j][0] {
			return result[i][1] < result[j][1]
		}
		return result[i][0] < result[j][0]
	})
	return result
}

// Helper function to convert flat array to pairs and compare (matches Rust assert_some_eq)
func assertSomeEq(t *testing.T, got func() *struct {
	Product uint64
	Pairs   []uint64
}, expectP uint64, expectFactors [][2]uint64) {
	result := got()
	if result == nil {
		t.Errorf("expected Some(..), got None")
		return
	}
	if result.Product != expectP {
		t.Errorf("product mismatch: got %d, expected %d", result.Product, expectP)
	}

	// Convert flat array to pairs for comparison
	var resultPairs [][2]uint64
	// Find the actual length by looking for the first zero pair
	for i := 0; i < len(result.Pairs); i += 2 {
		if i+1 < len(result.Pairs) && result.Pairs[i] != 0 && result.Pairs[i+1] != 0 {
			resultPairs = append(resultPairs, [2]uint64{result.Pairs[i], result.Pairs[i+1]})
		} else {
			break
		}
	}

	normalizedResult := norm(resultPairs)
	normalizedExpected := norm(expectFactors)
	if !reflect.DeepEqual(normalizedResult, normalizedExpected) {
		t.Errorf("factors mismatch: got %v, expected %v", normalizedResult, normalizedExpected)
	}
}

func TestSmallest(t *testing.T) {
	result := Smallest(910, 999)
	if result == nil {
		t.Errorf("expected Some(..), got None")
		return
	}
	if result.Product != 861168 {
		t.Errorf("expected product 861168, got %d", result.Product)
	}
	if len(result.Pairs) < 2 || result.Pairs[0] != 924 || result.Pairs[1] != 932 {
		t.Errorf("expected factors [924, 932], got %v", result.Pairs)
	}
}

func TestLargest910_999(t *testing.T) {
	result := Largest(910, 999)
	if result == nil {
		t.Errorf("expected Some(..), got None")
		return
	}
	if result.Product != 906609 {
		t.Errorf("expected product 906609, got %d", result.Product)
	}
	// Check if (913, 993) is in the flat array
	foundPair := false
	for i := 0; i < len(result.Pairs); i += 2 {
		if i+1 < len(result.Pairs) && result.Pairs[i] == 913 && result.Pairs[i+1] == 993 {
			foundPair = true
			break
		}
	}
	if !foundPair {
		t.Errorf("expected factor pair (913, 993) not found in %v", result.Pairs)
	}
}

func TestLargest100_999(t *testing.T) {
	result := Largest(100, 999)
	if result == nil {
		t.Errorf("expected Some(..), got None")
		return
	}
	if result.Product != 906609 {
		t.Errorf("expected product 906609, got %d", result.Product)
	}
	// Check if (913, 993) is in the flat array
	foundPair := false
	for i := 0; i < len(result.Pairs); i += 2 {
		if i+1 < len(result.Pairs) && result.Pairs[i] == 913 && result.Pairs[i+1] == 993 {
			foundPair = true
			break
		}
	}
	if !foundPair {
		t.Errorf("expected factor pair (913, 993) not found in %v", result.Pairs)
	}
}

func TestSingleDigitPal(t *testing.T) {
	if !IsPal(9) {
		t.Errorf("expected is_pal(9) to be true")
	}
}

func TestEvenSixPal(t *testing.T) {
	if !IsPal(906609) {
		t.Errorf("expected is_pal(906609) to be true")
	}
}

func TestTrailingZeroPal(t *testing.T) {
	if IsPal(40) {
		t.Errorf("expected is_pal(40) to be false")
	}
}

func TestEvenNotDiv11(t *testing.T) {
	// even digits that don't % 11
	if IsPal(123456) {
		t.Errorf("expected is_pal(123456) to be false")
	}
}

func TestOddLengthPal(t *testing.T) {
	if !IsPal(10988901) {
		t.Errorf("expected is_pal(10988901) to be true")
	}
}

func TestFindTheSmallestPalindromeFromSingleDigitFactors(t *testing.T) {
	minFactor, maxFactor := uint64(1), uint64(9)
	palindrome := uint64(1)
	factors := [][2]uint64{{1, 1}}
	assertSomeEq(t, func() *struct {
		Product uint64
		Pairs   []uint64
	} {
		return Smallest(minFactor, maxFactor)
	}, palindrome, factors)
}

func TestFindTheLargestPalindromeFromSingleDigitFactors(t *testing.T) {
	minFactor, maxFactor := uint64(1), uint64(9)
	palindrome := uint64(9)
	factors := [][2]uint64{{1, 9}, {3, 3}}
	assertSomeEq(t, func() *struct {
		Product uint64
		Pairs   []uint64
	} {
		return Largest(minFactor, maxFactor)
	}, palindrome, factors)
}

func TestFindTheSmallestPalindromeFromDoubleDigitFactors(t *testing.T) {
	minFactor, maxFactor := uint64(10), uint64(99)
	palindrome := uint64(121)
	factors := [][2]uint64{{11, 11}}
	assertSomeEq(t, func() *struct {
		Product uint64
		Pairs   []uint64
	} {
		return Smallest(minFactor, maxFactor)
	}, palindrome, factors)
}

func TestFindTheLargestPalindromeFromDoubleDigitFactors(t *testing.T) {
	minFactor, maxFactor := uint64(10), uint64(99)
	palindrome := uint64(9009)
	factors := [][2]uint64{{91, 99}}
	assertSomeEq(t, func() *struct {
		Product uint64
		Pairs   []uint64
	} {
		return Largest(minFactor, maxFactor)
	}, palindrome, factors)
}

func TestFindTheSmallestPalindromeFromTripleDigitFactors(t *testing.T) {
	minFactor, maxFactor := uint64(100), uint64(999)
	palindrome := uint64(10201)
	factors := [][2]uint64{{101, 101}}
	assertSomeEq(t, func() *struct {
		Product uint64
		Pairs   []uint64
	} {
		return Smallest(minFactor, maxFactor)
	}, palindrome, factors)
}

func TestFindTheLargestPalindromeFromTripleDigitFactors(t *testing.T) {
	minFactor, maxFactor := uint64(100), uint64(999)
	palindrome := uint64(906609)
	factors := [][2]uint64{{913, 993}}
	assertSomeEq(t, func() *struct {
		Product uint64
		Pairs   []uint64
	} {
		return Largest(minFactor, maxFactor)
	}, palindrome, factors)
}

func TestFindTheSmallestPalindromeFromFourDigitFactors(t *testing.T) {
	minFactor, maxFactor := uint64(1000), uint64(9999)
	palindrome := uint64(1002001)
	factors := [][2]uint64{{1001, 1001}}
	assertSomeEq(t, func() *struct {
		Product uint64
		Pairs   []uint64
	} {
		return Smallest(minFactor, maxFactor)
	}, palindrome, factors)
}

func TestFindTheLargestPalindromeFromFourDigitFactors(t *testing.T) {
	minFactor, maxFactor := uint64(1000), uint64(9999)
	palindrome := uint64(99000099)
	factors := [][2]uint64{{9901, 9999}}
	assertSomeEq(t, func() *struct {
		Product uint64
		Pairs   []uint64
	} {
		return Largest(minFactor, maxFactor)
	}, palindrome, factors)
}

func TestEmptyResultForSmallestIfNoPalindromeInTheRange(t *testing.T) {
	minFactor, maxFactor := uint64(1002), uint64(1003)
	result := Smallest(minFactor, maxFactor)
	if result != nil {
		t.Errorf("expected smallest(1002, 1003) to return None")
	}
}

func TestEmptyResultForLargestIfNoPalindromeInTheRange(t *testing.T) {
	minFactor, maxFactor := uint64(15), uint64(15)
	result := Largest(minFactor, maxFactor)
	if result != nil {
		t.Errorf("expected largest(15, 15) to return None")
	}
}

func TestErrorResultForSmallestIfMinIsMoreThanMax(t *testing.T) {
	minFactor, maxFactor := uint64(10000), uint64(1)
	result := Smallest(minFactor, maxFactor)
	if result != nil {
		t.Errorf("expected smallest(10000, 1) to return None")
	}
}

func TestErrorResultForLargestIfMinIsMoreThanMax(t *testing.T) {
	minFactor, maxFactor := uint64(2), uint64(1)
	result := Largest(minFactor, maxFactor)
	if result != nil {
		t.Errorf("expected largest(2, 1) to return None")
	}
}

func TestSmallestProductDoesNotUseTheSmallestFactor(t *testing.T) {
	minFactor, maxFactor := uint64(3215), uint64(4000)
	palindrome := uint64(10988901)
	factors := [][2]uint64{{3297, 3333}}
	assertSomeEq(t, func() *struct {
		Product uint64
		Pairs   []uint64
	} {
		return Smallest(minFactor, maxFactor)
	}, palindrome, factors)
}
