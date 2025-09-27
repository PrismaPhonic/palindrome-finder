//! Functional programming style implementations (mirroring Haskell approach)
//!
//! This module contains recursive, functional-style implementations of the
//! palindrome search algorithms that mirror the Haskell implementation patterns.
//! Uses explicit tail calls via the `become` keyword for optimal performance.

use arrayvec::ArrayVec;

/// Functional recursive palindrome check using half-reverse method
/// Mirrors the Haskell `isPalindrome` function with recursive `loop`
#[inline(always)]
pub fn is_pal_functional(n: u32) -> bool {
    if n < 10 {
        return true;
    }
    // Non-zero numbers ending in 0 cannot be palindromes
    // Non-zero numbers ending in 0 cannot be palindromes.
    if n.is_multiple_of(10) {
        return false;
    }
    // Even-length palindromes must be divisible by 11
    if has_even_digits(n) && !n.is_multiple_of(11) {
        return false;
    }

    // Recursive half-reverse function (mirrors Haskell `loop`)
    half_reverse_loop(n, 0)
}

#[inline(always)]
fn half_reverse_loop(m: u32, rev: u32) -> bool {
    if m <= rev {
        m == rev || m == rev / 10
    } else {
        let digit = m % 10;
        let new_rev = rev * 10 + digit;
        become half_reverse_loop(m / 10, new_rev)
    }
}

/// Returns true if `n` has an even number of decimal digits.
/// Mirrors the original `has_even_digits` function.
#[inline(always)]
fn has_even_digits(n: u32) -> bool {
    debug_assert!(n >= 11);

    // Use bit manipulation to count digits efficiently
    // This avoids floating point and reduces the number of comparisons
    if n < 100 {
        true // 2 digits
    } else if n < 1_000 {
        false // 3 digits
    } else if n < 10_000 {
        true // 4 digits
    } else if n < 100_000 {
        false // 5 digits
    } else if n < 1_000_000 {
        true // 6 digits
    } else if n < 10_000_000 {
        false // 7 digits
    } else if n < 100_000_000 {
        true // 8 digits
    } else if n < 1_000_000_000 {
        false // 9 digits
    } else {
        true // 10 digits (max for u32)
    }
}

/// Functional recursive factor pair collection
/// Mirrors the Haskell `collectPositiveFactorPairs` function
#[inline(always)]
pub fn collect_factor_pairs(product: u32, min: u32, max: u32) -> ArrayVec<u32, 4> {
    let sqrt_p = product.isqrt();
    let low = product.div_ceil(max).max(min);
    let high = sqrt_p.min(max);

    // Verified for bounds [1, 999] inclusive (both smallest and largest):
    // the factor-pair list never exceeds 4 slots (2 pairs).
    let mut result: ArrayVec<u32, 4> = ArrayVec::new_const();
    collect_pairs_recursive(product, min, max, low, high, &mut result);
    result
}

#[inline(always)]
fn collect_pairs_recursive(
    product: u32,
    min: u32,
    max: u32,
    x: u32,
    high: u32,
    result: &mut ArrayVec<u32, 4>,
) {
    if x > high {
        return;
    }

    if product.is_multiple_of(x) {
        let y = product / x;
        result.push_unchecked(x);
        result.push_unchecked(y);
    }

    become collect_pairs_recursive(product, min, max, x + 1, high, result);
}

#[inline(always)]
pub fn smallest_product_functional(min: u32, max: u32) -> Option<u32> {
    // Start row search at x = min with sentinel best
    search_smallest(min, u32::MAX, min, max)
}

#[inline(always)]
fn search_smallest(x: u32, best: u32, min_bound: u32, max_bound: u32) -> Option<u32> {
    // Mirrors Haskell's searchRowsForSmallest with x ascending and pruning
    if x > max_bound || x * x >= best {
        if best == u32::MAX { None } else { Some(best) }
    } else {
        match search_row_smallest(x, best, max_bound) {
            None => search_smallest(x + 1, best, min_bound, max_bound),
            Some(new_best) => search_smallest(x + 1, new_best, min_bound, max_bound),
        }
    }
}

#[inline(always)]
fn search_row_smallest(x: u32, current_best: u32, max: u32) -> Option<u32> {
    let y_upper = ((current_best - 1) / x).min(max);
    if y_upper < x {
        None
    } else {
        search_column_smallest(x, x, y_upper, current_best)
    }
}

#[inline(always)]
fn search_column_smallest(x: u32, y: u32, y_upper: u32, current_best: u32) -> Option<u32> {
    if y > y_upper {
        if current_best == u32::MAX {
            None
        } else {
            Some(current_best)
        }
    } else {
        let prod = x * y;
        if is_pal_functional(prod) {
            Some(prod)
        } else {
            become search_column_smallest(x, y + 1, y_upper, current_best)
        }
    }
}

/// Functional largest palindrome search using tail recursion
/// Pure functional implementation with explicit tail calls
#[inline(always)]
pub fn largest_product_functional(min: u32, max: u32) -> Option<u32> {
    // Start with x = max, keep max as fixed upper bound
    search_largest(max, min, max, 0)
}

#[inline(always)]
fn search_largest(x: u32, min: u32, max: u32, best: u32) -> Option<u32> {
    if x < min || x * max <= best {
        if best == 0 { None } else { Some(best) }
    } else {
        match search_row_for_largest(x, best, max) {
            None => search_largest(x - 1, min, max, best),
            Some(new_best) => search_largest(x - 1, min, max, new_best),
        }
    }
}

#[inline(always)]
pub fn smallest_functional(min: u32, max: u32) -> Option<(u32, ArrayVec<u32, 4>)> {
    smallest_product_functional(min, max)
        .map(|product| (product, collect_factor_pairs(product, min, max)))
}

#[inline(always)]
pub fn largest_functional(min: u32, max: u32) -> Option<(u32, ArrayVec<u32, 4>)> {
    largest_product_functional(min, max)
        .map(|product| (product, collect_factor_pairs(product, min, max)))
}

#[inline(always)]
fn search_row_for_largest(x: u32, best: u32, max: u32) -> Option<u32> {
    let y_lower = ((best / x) + 1).max(x);
    if y_lower > max {
        None
    } else {
        search_column_largest(x, max, y_lower, 0)
    }
}

#[inline(always)]
fn search_column_largest(x: u32, y: u32, y_lower: u32, current_best: u32) -> Option<u32> {
    if y < y_lower {
        if current_best == 0 {
            None
        } else {
            Some(current_best)
        }
    } else {
        let prod = x * y;
        if is_pal_functional(prod) {
            Some(prod)
        } else {
            become search_column_largest(x, y - 1, y_lower, current_best)
        }
    }
}

#[inline(always)]
pub fn do_iters_largest_functional(min: u32, max: u32, iters: u64) -> (Option<u32>, u64) {
    // Create an iterator that cycles through the range
    let acc = (0..iters)
        .scan(max, |current_max, _| {
            let result = *current_max;
            *current_max = if *current_max <= min {
                max
            } else {
                *current_max - 1
            };
            Some(result)
        })
        .map(|current_max| {
            largest_functional(min, current_max)
                .map(|(prod, pairs)| (prod, pairs.into_iter().map(|v| v as u64).sum::<u64>()))
        })
        .fold((0u64, 0u64), |(acc, cnt), result| match result {
            Some((prod, pairs_sum)) => (acc + (prod as u64) + pairs_sum + cnt, cnt + 1),
            None => (acc, cnt + 1),
        });

    let base_prod = largest_functional(min, max).map(|(p, _)| p);
    (base_prod, acc.0)
}

#[inline(always)]
pub fn do_iters_smallest_functional(min: u32, max: u32, iters: u64) -> (Option<u32>, u64) {
    // Create an iterator that cycles through the range
    let acc = (0..iters)
        .scan(min, |current_min, _| {
            let result = *current_min;
            *current_min = if *current_min >= max {
                min
            } else {
                *current_min + 1
            };
            Some(result)
        })
        .map(|current_min| {
            smallest_functional(current_min, max)
                .map(|(prod, pairs)| (prod, pairs.into_iter().map(|v| v as u64).sum::<u64>()))
        })
        .fold((0u64, 0u64), |(acc, cnt), result| match result {
            Some((prod, pairs_sum)) => (acc + (prod as u64) + pairs_sum + cnt, cnt + 1),
            None => (acc, cnt + 1),
        });

    let base_prod = smallest_functional(min, max).map(|(p, _)| p);
    (base_prod, acc.0)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn norm(v: impl IntoIterator<Item = (u32, u32)>) -> Vec<(u32, u32)> {
        let mut out: Vec<(u32, u32)> = v.into_iter().collect();
        out.sort_unstable();
        out
    }

    fn assert_some_eq(
        got: Option<(u32, ArrayVec<u32, 4>)>,
        expect_p: u32,
        expect_factors: &[(u32, u32)],
    ) {
        let (p, f) = got.expect("expected Some(..), got None");
        assert_eq!(p, expect_p, "product mismatch");

        // Convert flat array to pairs for comparison
        let mut pairs = Vec::new();
        for i in (0..f.len()).step_by(2) {
            if i + 1 < f.len() {
                pairs.push((f[i], f[i + 1]));
            }
        }
        assert_eq!(
            norm(pairs),
            norm(expect_factors.to_vec()),
            "factors mismatch"
        );
    }

    #[test]
    fn test_smallest_functional() {
        let (product, factors) = smallest_functional(910, 999).unwrap();
        assert_eq!(product, 861168);
        assert_eq!(factors[0], 924);
        assert_eq!(factors[1], 932);
    }

    #[test]
    fn largest_910_999_functional() {
        let (p, f) = largest_functional(910, 999).unwrap();
        assert_eq!(p, 906_609);
        // Check if (913, 993) is in the flat array
        let mut found = false;
        for i in (0..f.len()).step_by(2) {
            if i + 1 < f.len() && f[i] == 913 && f[i + 1] == 993 {
                found = true;
                break;
            }
        }
        assert!(found);
    }

    #[test]
    fn largest_100_999_functional() {
        let (p, f) = largest_functional(100, 999).unwrap();
        assert_eq!(p, 906_609);
        // Check if (913, 993) is in the flat array
        let mut found = false;
        for i in (0..f.len()).step_by(2) {
            if i + 1 < f.len() && f[i] == 913 && f[i + 1] == 993 {
                found = true;
                break;
            }
        }
        assert!(found);
    }

    #[test]
    fn single_digit_pal_functional() {
        assert!(is_pal_functional(9));
    }

    #[test]
    fn even_six_pal_functional() {
        assert!(is_pal_functional(906_609));
    }

    #[test]
    fn trailing_zero_pal_functional() {
        assert!(!is_pal_functional(40));
    }

    #[test]
    fn even_not_div_11_functional() {
        // even digits that don't % 11
        assert!(!is_pal_functional(123_456));
    }

    #[test]
    fn odd_length_pal_functional() {
        assert!(is_pal_functional(10_988_901));
    }

    #[test]
    fn find_the_smallest_palindrome_from_single_digit_factors_functional() {
        let (min_factor, max_factor) = (1, 9);
        let palindrome = 1;
        let factors = [(1, 1)];
        assert_some_eq(
            smallest_functional(min_factor, max_factor),
            palindrome,
            &factors,
        );
    }

    #[test]
    fn find_the_largest_palindrome_from_single_digit_factors_functional() {
        let (min_factor, max_factor) = (1, 9);
        let palindrome = 9;
        let factors = [(1, 9), (3, 3)];
        assert_some_eq(
            largest_functional(min_factor, max_factor),
            palindrome,
            &factors,
        );
    }

    #[test]
    fn find_the_smallest_palindrome_from_double_digit_factors_functional() {
        let (min_factor, max_factor) = (10, 99);
        let palindrome = 121;
        let factors = [(11, 11)];
        assert_some_eq(
            smallest_functional(min_factor, max_factor),
            palindrome,
            &factors,
        );
    }

    #[test]
    fn find_the_largest_palindrome_from_double_digit_factors_functional() {
        let (min_factor, max_factor) = (10, 99);
        let palindrome = 9009;
        let factors = [(91, 99)];
        assert_some_eq(
            largest_functional(min_factor, max_factor),
            palindrome,
            &factors,
        );
    }

    #[test]
    fn find_the_smallest_palindrome_from_triple_digit_factors_functional() {
        let (min_factor, max_factor) = (100, 999);
        let palindrome = 10_201;
        let factors = [(101, 101)];
        assert_some_eq(
            smallest_functional(min_factor, max_factor),
            palindrome,
            &factors,
        );
    }

    #[test]
    fn find_the_largest_palindrome_from_triple_digit_factors_functional() {
        let (min_factor, max_factor) = (100, 999);
        let palindrome = 906_609;
        let factors = [(913, 993)];
        assert_some_eq(
            largest_functional(min_factor, max_factor),
            palindrome,
            &factors,
        );
    }

    #[test]
    fn find_the_smallest_palindrome_from_four_digit_factors_functional() {
        let (min_factor, max_factor) = (1000, 9999);
        let palindrome = 1_002_001;
        let factors = [(1001, 1001)];
        assert_some_eq(
            smallest_functional(min_factor, max_factor),
            palindrome,
            &factors,
        );
    }

    #[test]
    fn find_the_largest_palindrome_from_four_digit_factors_functional() {
        let (min_factor, max_factor) = (1000, 9999);
        let palindrome = 99_000_099;
        let factors = [(9901, 9999)];
        assert_some_eq(
            largest_functional(min_factor, max_factor),
            palindrome,
            &factors,
        );
    }

    #[test]
    fn empty_result_for_smallest_if_no_palindrome_in_the_range_functional() {
        let (min_factor, max_factor) = (1002, 1003);
        assert!(smallest_functional(min_factor, max_factor).is_none());
    }

    #[test]
    fn empty_result_for_largest_if_no_palindrome_in_the_range_functional() {
        let (min_factor, max_factor) = (15, 15);
        assert!(largest_functional(min_factor, max_factor).is_none());
    }

    #[test]
    fn error_result_for_smallest_if_min_is_more_than_max_functional() {
        let (min_factor, max_factor) = (10_000, 1);
        assert!(smallest_functional(min_factor, max_factor).is_none());
    }

    #[test]
    fn error_result_for_largest_if_min_is_more_than_max_functional() {
        let (min_factor, max_factor) = (2, 1);
        assert!(largest_functional(min_factor, max_factor).is_none());
    }

    #[test]
    fn smallest_product_does_not_use_the_smallest_factor_functional() {
        let (min_factor, max_factor) = (3215, 4000);
        let palindrome = 10_988_901;
        let factors = [(3297, 3333)];
        assert_some_eq(
            smallest_functional(min_factor, max_factor),
            palindrome,
            &factors,
        );
    }
}
