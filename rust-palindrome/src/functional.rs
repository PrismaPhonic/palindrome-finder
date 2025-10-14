//! Functional programming style implementations (mirroring Haskell approach)
//!
//! This module contains recursive, functional-style implementations of the
//! palindrome search algorithms that mirror the Haskell implementation patterns.
//! Uses explicit tail calls via the `become` keyword for optimal performance.

use std::num::NonZeroU32;

use crate::{divrem_u32_magic, has_even_digits};
use arrayvec::ArrayVec;

/// Functional recursive palindrome check using half-reverse method
/// Mirrors the Haskell `isPalindrome` function with recursive `loop`
#[inline(always)]
pub fn is_pal_functional(n: u32) -> bool {
    if n < 10 {
        return true;
    }
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
    collect_pairs_recursive(product, low, high, &mut result);
    result
}

#[inline(always)]
fn collect_pairs_recursive(product: u32, x: u32, high: u32, result: &mut ArrayVec<u32, 4>) {
    if x > high {
        return;
    }

    let (y, r) = divrem_u32_magic(product, x);
    if r == 0 {
        result.push(x);
        result.push(y);
    }

    become collect_pairs_recursive(product, x + 1, high, result);
}

#[inline(always)]
pub fn smallest_product_functional(min: u32, max: u32) -> Option<u32> {
    // Start row search at x = min with sentinel best
    search_smallest(min, u32::MAX, max)
}

#[inline(always)]
fn search_smallest(x: u32, best: u32, max_bound: u32) -> Option<u32> {
    // Mirrors Haskell's searchRowsForSmallest with x ascending and pruning
    if x > max_bound || x * x >= best {
        if best == u32::MAX { None } else { Some(best) }
    } else {
        match search_row_smallest(x, best, max_bound) {
            None => search_smallest(x + 1, best, max_bound),
            Some(new_best) => search_smallest(x + 1, new_best, max_bound),
        }
    }
}

#[inline(always)]
fn search_row_smallest(x: u32, current_best: u32, max: u32) -> Option<u32> {
    let x_nz = unsafe { NonZeroU32::new_unchecked(x) };
    let (q, _) = divrem_u32_magic(current_best - 1, x_nz.get());
    let y_upper = (q).min(max);
    if y_upper < x {
        None
    } else {
        search_column_smallest(x, x, x * x, y_upper, current_best)
    }
}

#[inline(always)]
fn search_column_smallest(
    x: u32,
    y: u32,
    prod: u32,
    y_upper: u32,
    current_best: u32,
) -> Option<u32> {
    if y > y_upper {
        if current_best == u32::MAX {
            None
        } else {
            Some(current_best)
        }
    } else if is_pal_functional(prod) {
        Some(prod)
    } else {
        become search_column_smallest(x, y + 1, prod + x, y_upper, current_best)
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
    let x_nz = unsafe { NonZeroU32::new_unchecked(x) };
    let (q, _) = divrem_u32_magic(best, x_nz.get());
    let y_lower = (q + 1).max(x);
    if y_lower > max {
        None
    } else {
        search_column_largest(x, max, x * max, y_lower, 0)
    }
}

#[inline(always)]
fn search_column_largest(
    x: u32,
    y: u32,
    prod: u32,
    y_lower: u32,
    current_best: u32,
) -> Option<u32> {
    if y < y_lower {
        if current_best == 0 {
            None
        } else {
            Some(current_best)
        }
    } else {
        if is_pal_functional(prod) {
            Some(prod)
        } else {
            become search_column_largest(x, y - 1, prod - x, y_lower, current_best)
        }
    }
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
    fn empty_result_for_largest_if_no_palindrome_in_the_range_functional() {
        let (min_factor, max_factor) = (15, 15);
        assert!(largest_functional(min_factor, max_factor).is_none());
    }
}
