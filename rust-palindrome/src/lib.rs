//! Palindromic Products
//! =========================================
//!
//! Problem
//! -------
//! Given an inclusive range [min..max], find:
//!   1) the smallest palindromic product of two factors in that range
//!   2) the largest palindromic product of two factors in that range
//! and also return all factor pairs (x, y) with min <= x <= y <= max that
//! produce that product.
//!
//! Approach (mirrors the SBCL/Common Lisp version)
//! -----------------------------------------------
//! 1) Palindrome test (numeric, no strings):
//!    - Early outs: single-digit numbers are palindromes; numbers ending in 0
//!      (and not 0 itself) are not palindromes.
//!    - Even-length palindromes must be divisible by 11 (number theory fact).
//!      We fast-reject even-length numbers not divisible by 11.
//!    - Half-reverse: build the reverse of the low half of the digits until
//!      rev >= m, then compare (m == rev) or (m == rev/10). This avoids a full
//!      reversal and reduces divisions.
//!
//! 2) Search structure with strong pruning:
//!    - For the smallest search:
//!        * x ascends (min -> max).
//!        * Outer prune: if x * x >= best, later rows cannot improve best.
//!        * For each x, cap y at y_upper = min(max, floor((best-1)/x)).
//!          That row then only tries x..=y_upper. First palindrome found in
//!          that row is the smallest for that row; update best and continue.
//!    - For the largest search:
//!        * x descends (max -> min).
//!        * Outer prune: if x * max <= best, earlier rows cannot improve best.
//!        * For each x, start y at max and stop when y_lower = max(x, floor(best/x)+1).
//!          The first palindrome found in that row is the largest for that row;
//!          update best and continue.
//!
//! 3) Factor-pair enumeration (for the final product):
//!    - Use a tight divisor window:
//!         x in [ ceil(product / max) .. min(max, isqrt(product)) ]
//!      For each x in that window, if product % x == 0, push (x, product/x)
//!      if y is in range and y >= x.
//!
//! Performance notes
//! -----------------
//! - The half-reverse palindrome + 11 rule reduces calls to the palindrome test.
//! - The tight y bounds in both searches greatly shrink the nested loops.
//! - The factor enumeration window keeps divisibility checks minimal.
//! - Build with release settings (opt-level=3, lto=thin, codegen-units=1,
//!   panic=abort). Consider RUSTFLAGS="-C target-cpu=native" for local runs.
//!
//! Correctness notes
//! -----------------
//! - The searches return None if min > max or if no palindrome exists.
//! - Factor collection uses ordered pairs (x <= y) within [min..max] only.
//!
//! This module is documented to match the Common Lisp file so readers can
//! compare the designs side-by-side.

use std::io::{BufRead, BufReader, Write};
use std::str::FromStr;

//
// Palindrome helpers
//

/// Returns true if `n` has an even number of decimal digits.
///
/// Preconditions:
/// - Call only with `n >= 11`. Callers should filter out `n < 10` and trailing-zero cases.
///
/// Rationale:
/// We use this to apply the rule "even-length palindromes must be divisible by 11".
#[inline(always)]
fn has_even_digits(n: u64) -> bool {
    debug_assert!(n >= 11);

    match n {
        11..=99 => true,                                           // 2 digits
        100..=999 => false,                                        // 3
        1_000..=9_999 => true,                                     // 4
        10_000..=99_999 => false,                                  // 5
        100_000..=999_999 => true,                                 // 6
        1_000_000..=9_999_999 => false,                            // 7
        10_000_000..=99_999_999 => true,                           // 8
        100_000_000..=999_999_999 => false,                        // 9
        1_000_000_000..=9_999_999_999 => true,                     // 10
        10_000_000_000..=99_999_999_999 => false,                  // 11
        100_000_000_000..=999_999_999_999 => true,                 // 12
        1_000_000_000_000..=9_999_999_999_999 => false,            // 13
        10_000_000_000_000..=99_999_999_999_999 => true,           // 14
        100_000_000_000_000..=999_999_999_999_999 => false,        // 15
        1_000_000_000_000_000..=9_999_999_999_999_999 => true,     // 16
        10_000_000_000_000_000..=99_999_999_999_999_999 => false,  // 17
        100_000_000_000_000_000..=999_999_999_999_999_999 => true, // 18
        // For the rest of u64 range (19+ digits), treat as odd length to match
        // the current Common Lisp mapping. Adjust if you ever extend CL bounds.
        _ => false,
    }
}

/// Return true if `n` is a decimal palindrome (numeric half-reversal).
///
/// Fast-paths:
/// - `n < 10` is palindrome
/// - `n % 10 == 0` (and `n != 0`) cannot be palindrome
/// - even-length palindromes must be divisible by 11; if not, reject
///
/// Core:
/// - Build `rev` by taking right digits of `m` until `rev >= m`.
/// - Then return `m == rev || m == rev/10`.
#[inline]
pub fn is_pal(n: u64) -> bool {
    if n < 10 {
        return true;
    }
    // Non-zero numbers ending in 0 cannot be palindromes.
    if n % 10 == 0 {
        return false;
    }
    // Even-length palindromes must be divisible by 11.
    if has_even_digits(n) && n % 11 != 0 {
        return false;
    }

    // Half-reverse
    let mut m = n;
    let mut rev: u64 = 0;
    while m > rev {
        rev = rev * 10 + m % 10;
        m /= 10;
    }

    m == rev || m == rev / 10
}

//
// Factor pair collection
//

/// When `product == 0` and `0` is in range, valid ordered pairs are `(0, y)`
/// for all `y in [min..max]`. Otherwise the set is empty.
#[inline]
fn collect_zero_factor_pairs(min: u64, max: u64) -> Vec<(u64, u64)> {
    if min == 0 {
        (min..=max).map(|y| (0, y)).collect()
    } else {
        Vec::new()
    }
}

/// Collect ordered factor pairs `(x, y)` (with `x <= y`) such that
/// `x * y == product` and both factors lie in `[min..max]`.
///
/// Uses a tight divisor window:
///   x in [ ceil(product / max) .. min(max, isqrt(product)) ]
#[inline]
pub fn collect_factor_pairs(product: u64, min: u64, max: u64) -> Vec<(u64, u64)> {
    if product == 0 {
        return collect_zero_factor_pairs(min, max);
    }

    // Tight window: x in [ceil(product/max) .. min(max, isqrt(product))]
    let low = product.div_ceil(max).max(min);
    let high = product.isqrt().min(max);

    let mut out = Vec::new();
    for x in low..=high {
        if product % x == 0 {
            let y = product / x;
            // y is automatically >= x because x <= isqrt(product)
            if y >= min && y <= max {
                out.push((x, y));
            }
        }
    }

    out
}

//
// Smallest / largest searches with pruning
//

/// Find the smallest palindromic product in `[min..max]` and its factor pairs.
///
/// Returns `Some((product, pairs))` or `None` if either the range is invalid or
/// no palindrome exists.
///
/// Algorithm:
/// - `x` ascends from `min` to `max`.
/// - Outer prune: if `x*x >= best`, later rows cannot improve `best`.
/// - For a fixed `x`, we only need `y` up to `y_upper = min(max, (best-1)/x)`.
///   If `y_upper < x`, there is no work in that row.
/// - Iterate `y` from `x` to `y_upper`; the first palindrome in that row is
///   the row minimum; update `best` and continue.
#[inline]
pub fn smallest(min: u64, max: u64) -> Option<(u64, Vec<(u64, u64)>)> {
    if min > max {
        return None;
    }

    let mut best = u64::MAX;

    for x in min..=max {
        let xx = x.saturating_mul(x);
        if xx >= best {
            break; // outer prune
        }

        // Row cap: only products < best matter.
        let y_upper = ((best - 1) / x).min(max);
        if y_upper < x {
            continue; // no valid y in this row
        }

        for y in x..=y_upper {
            // No prod >= best check needed; y_upper already enforces it.
            let prod = x * y;
            if is_pal(prod) {
                best = prod;
                break; // row minimum found; move to next x
            }
        }
    }

    if best == u64::MAX {
        None
    } else {
        Some((best, collect_factor_pairs(best, min, max)))
    }
}

/// Find the largest palindromic product in `[min..max]` and its factor pairs.
///
/// Returns `Some((product, pairs))` or `None` if either the range is invalid or
/// no palindrome exists.
///
/// Algorithm:
/// - `x` descends from `max` to `min`.
/// - Outer prune: if `x*max <= best`, earlier rows cannot improve `best`.
/// - For a fixed `x`, only products `> best` matter. That means
///   `y >= floor(best/x) + 1`. We also require `y >= x` to keep `x <= y`.
///   Let `y_lower = max(x, floor(best/x)+1)`.
/// - Iterate `y` from `max` down to `y_lower`; the first palindrome in that row
///   is the row maximum; update `best` and continue.
#[inline]
pub fn largest(min: u64, max: u64) -> Option<(u64, Vec<(u64, u64)>)> {
    if min > max {
        return None;
    }

    let mut best: u64 = 0;

    // x descends
    for x in (min..=max).rev() {
        // Outer prune: once x*max <= best, smaller x cannot improve.
        if x.saturating_mul(max) <= best {
            break;
        }

        // y lower bound: only products > best matter; also enforce y >= x.
        // For x == 0, this row cannot beat any positive best; skip by forcing an empty range.
        let y_lower = if x > 0 {
            let floor_best_div_x = best / x;
            (floor_best_div_x.saturating_add(1)).max(x)
        } else {
            max.saturating_add(1)
        };

        if y_lower > max {
            continue; // no work for this row
        }

        for y in (y_lower..=max).rev() {
            let p = x.saturating_mul(y);
            if is_pal(p) {
                best = p; // row maximum found
                break; // move to next x
            }
        }
    }

    if best > 0 || (min == 0 && max == 0) {
        Some((best, collect_factor_pairs(best, min, max)))
    } else {
        None
    }
}

//
// Simple line-protocol server used by the Criterion harness
//

/// Generic line-protocol server used by the benchmark harness.
///
/// Protocol (one command per line):
/// - `INIT <min> <max>`: set the factor range (must be called first).
/// - `WARMUP <iters>`: run `iters` iterations without reporting a result.
/// - `RUN <iters>`: run `iters` iterations and print `OK <product>` with the
///   last product found.
/// - `QUIT`: exit.
///
/// The `do_iters(min, max, iters)` closure must do the full work (including
/// factor pair building) and return the final product as `Option<u64>`.
pub fn run_server<F>(mut do_iters: F)
where
    F: FnMut(u64, u64, u64) -> Option<u64>,
{
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let mut reader = BufReader::new(stdin.lock());
    let mut writer = std::io::BufWriter::new(stdout.lock());

    let mut line = String::new();
    let mut min: Option<u64> = None;
    let mut max: Option<u64> = None;

    loop {
        line.clear();
        if reader.read_line(&mut line).unwrap_or(0) == 0 {
            break; // EOF
        }
        let mut parts = line.split_whitespace();
        let cmd = parts.next().unwrap_or("").to_ascii_uppercase();
        match cmd.as_str() {
            "INIT" => {
                let a = u64::from_str(parts.next().unwrap_or("")).unwrap();
                let b = u64::from_str(parts.next().unwrap_or("")).unwrap();
                min = Some(a);
                max = Some(b);
                writeln!(writer, "OK").unwrap();
                writer.flush().unwrap();
            }
            "WARMUP" => {
                let iters = u64::from_str(parts.next().unwrap_or("")).unwrap();
                if let (Some(a), Some(b)) = (min, max) {
                    let _ = do_iters(a, b, iters);
                }
                writeln!(writer, "OK").unwrap();
                writer.flush().unwrap();
            }
            "RUN" => {
                let iters = u64::from_str(parts.next().unwrap_or("")).unwrap();
                if let (Some(a), Some(b)) = (min, max) {
                    let prod = do_iters(a, b, iters).unwrap_or_default();
                    writeln!(writer, "OK {prod}").unwrap();
                } else {
                    writeln!(writer, "ERR NOTINIT").unwrap();
                }
                writer.flush().unwrap();
            }
            "QUIT" => break,
            _ => {
                writeln!(writer, "ERR BADCMD").unwrap();
                writer.flush().unwrap();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn norm(mut v: Vec<(u64, u64)>) -> Vec<(u64, u64)> {
        v.sort_unstable();
        v
    }

    fn assert_some_eq(
        got: Option<(u64, Vec<(u64, u64)>)>,
        expect_p: u64,
        expect_factors: &[(u64, u64)],
    ) {
        let (p, f) = got.expect("expected Some(..), got None");
        assert_eq!(p, expect_p, "product mismatch");
        assert_eq!(norm(f), norm(expect_factors.to_vec()), "factors mismatch");
    }

    #[test]
    fn test_smallest() {
        let (product, factors) = smallest(910, 999).unwrap();
        assert_eq!(product, 861168);
        assert_eq!(factors[0], (924, 932));
    }

    #[test]
    fn largest_910_999() {
        let (p, mut f) = largest(910, 999).unwrap();
        f.sort();
        assert_eq!(p, 906_609);
        assert!(f.contains(&(913, 993)));
    }

    #[test]
    fn largest_100_999() {
        let (p, mut f) = largest(100, 999).unwrap();
        f.sort();
        assert_eq!(p, 906_609);
        assert!(f.contains(&(913, 993)));
    }

    #[test]
    fn single_digit_pal() {
        assert!(is_pal(9));
    }

    #[test]
    fn even_six_pal() {
        assert!(is_pal(906_609));
    }

    #[test]
    fn trailing_zero_pal() {
        assert!(!is_pal(40));
    }

    #[test]
    fn even_not_div_11() {
        // even digits that don't % 11
        assert!(!is_pal(123_456));
    }

    #[test]
    fn odd_length_pal() {
        assert!(is_pal(10_988_901));
    }

    #[test]
    fn find_the_smallest_palindrome_from_single_digit_factors() {
        let (min_factor, max_factor) = (1, 9);
        let palindrome = 1;
        let factors = [(1, 1)];
        assert_some_eq(smallest(min_factor, max_factor), palindrome, &factors);
    }

    #[test]
    fn find_the_largest_palindrome_from_single_digit_factors() {
        let (min_factor, max_factor) = (1, 9);
        let palindrome = 9;
        let factors = [(1, 9), (3, 3)];
        assert_some_eq(largest(min_factor, max_factor), palindrome, &factors);
    }

    #[test]
    fn find_the_smallest_palindrome_from_double_digit_factors() {
        let (min_factor, max_factor) = (10, 99);
        let palindrome = 121;
        let factors = [(11, 11)];
        assert_some_eq(smallest(min_factor, max_factor), palindrome, &factors);
    }

    #[test]
    fn find_the_largest_palindrome_from_double_digit_factors() {
        let (min_factor, max_factor) = (10, 99);
        let palindrome = 9009;
        let factors = [(91, 99)];
        assert_some_eq(largest(min_factor, max_factor), palindrome, &factors);
    }

    #[test]
    fn find_the_smallest_palindrome_from_triple_digit_factors() {
        let (min_factor, max_factor) = (100, 999);
        let palindrome = 10_201;
        let factors = [(101, 101)];
        assert_some_eq(smallest(min_factor, max_factor), palindrome, &factors);
    }

    #[test]
    fn find_the_largest_palindrome_from_triple_digit_factors() {
        let (min_factor, max_factor) = (100, 999);
        let palindrome = 906_609;
        let factors = [(913, 993)];
        assert_some_eq(largest(min_factor, max_factor), palindrome, &factors);
    }

    #[test]
    fn find_the_smallest_palindrome_from_four_digit_factors() {
        let (min_factor, max_factor) = (1000, 9999);
        let palindrome = 1_002_001;
        let factors = [(1001, 1001)];
        assert_some_eq(smallest(min_factor, max_factor), palindrome, &factors);
    }

    #[test]
    fn find_the_largest_palindrome_from_four_digit_factors() {
        let (min_factor, max_factor) = (1000, 9999);
        let palindrome = 99_000_099;
        let factors = [(9901, 9999)];
        assert_some_eq(largest(min_factor, max_factor), palindrome, &factors);
    }

    #[test]
    fn empty_result_for_smallest_if_no_palindrome_in_the_range() {
        let (min_factor, max_factor) = (1002, 1003);
        assert!(smallest(min_factor, max_factor).is_none());
    }

    #[test]
    fn empty_result_for_largest_if_no_palindrome_in_the_range() {
        let (min_factor, max_factor) = (15, 15);
        assert!(largest(min_factor, max_factor).is_none());
    }

    #[test]
    fn error_result_for_smallest_if_min_is_more_than_max() {
        let (min_factor, max_factor) = (10_000, 1);
        assert!(smallest(min_factor, max_factor).is_none());
    }

    #[test]
    fn error_result_for_largest_if_min_is_more_than_max() {
        let (min_factor, max_factor) = (2, 1);
        assert!(largest(min_factor, max_factor).is_none());
    }

    #[test]
    fn smallest_product_does_not_use_the_smallest_factor() {
        let (min_factor, max_factor) = (3215, 4000);
        let palindrome = 10_988_901;
        let factors = [(3297, 3333)];
        assert_some_eq(smallest(min_factor, max_factor), palindrome, &factors);
    }
}
