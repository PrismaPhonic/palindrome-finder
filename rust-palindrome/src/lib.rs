use std::io::{BufRead, BufReader, Write};
use std::str::FromStr;

/// Returns true if `n` has an even number of decimal digits.
///
/// # Preconditions
/// - Must only be called with `n >= 11`.
/// - Callers are responsible for filtering out `n < 10` and trailing-zero cases.
///
/// Used by `is_pal` to apply the "even-length palindromes must be divisible by 11" rule.
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
        // TODO: Add more logic for rest of u64 range. For now this matches CL solution.
        _ => false, // 19+ digits (up to u64::MAX) => odd length
    }
}

#[inline]
pub fn is_pal(n: u64) -> bool {
    if n < 10 {
        return true;
    }

    if n % 10 == 0 {
        return false;
    }

    // Even-length palindromes must be divisible by 11.
    if has_even_digits(n) && n % 11 != 0 {
        return false;
    }

    let mut m = n;
    let mut rev: u64 = 0;
    while m > rev {
        rev = rev * 10 + m % 10;
        m /= 10;
    }

    m == rev || m == rev / 10
}

#[inline]
fn collect_zero_factor_pairs(min: u64, max: u64) -> Vec<(u64, u64)> {
    // When product == 0 and 0 is in range, valid ordered pairs are (0, y) for all y in [min, max].
    if min == 0 {
        (min..=max).map(|y| (0, y)).collect()
    } else {
        Vec::new()
    }
}

#[inline]
pub fn collect_factor_pairs(product: u64, min: u64, max: u64) -> Vec<(u64, u64)> {
    if product == 0 {
        return collect_zero_factor_pairs(min, max);
    }
    let mut out = Vec::new();
    for x in min..=max {
        if x == 0 {
            continue;
        } // product != 0 ⇒ skip div-by-zero
        if product % x != 0 {
            continue;
        } // exact divisor?
        let q = product / x;
        if x <= q && q <= max {
            out.push((x, q)); // ordered: x ≤ q
        }
    }
    out
}

#[inline]
pub fn largest(min: u64, max: u64) -> Option<(u64, Vec<(u64, u64)>)> {
    let mut best: u64 = 0;
    // x: max -> min with pruning
    for x in (min..=max).rev() {
        if x.saturating_mul(max) <= best {
            break;
        } // outer prune
        for y in (x..=max).rev() {
            let p = x.saturating_mul(y);
            if p <= best {
                break;
            } // inner prune
            if is_pal(p) {
                best = p; // row’s largest
                break;
            }
        }
    }
    if best > 0 || (min == 0 && max == 0) {
        Some((best, collect_factor_pairs(best, min, max)))
    } else {
        None
    }
}

/// Return (smallest_palindromic_product, factor_pairs)
/// where factor_pairs are all (x,y) with min <= x <= y <= max and x * y == product.
#[inline]
pub fn smallest(min: u64, max: u64) -> Option<(u64, Vec<(u64, u64)>)> {
    if min > max {
        return None;
    }

    // Start with "infinite" best; we’ll minimize.
    let mut best = u64::MAX;

    // x: ascend (min -> max)
    for x in min..=max {
        // Outer prune: if x * x >= best then later rows (larger x) cannot produce a smaller product.
        // Guard against overflow (won’t happen for our ranges, but be tidy).
        let xx = x.saturating_mul(x);
        if xx >= best {
            break;
        }

        // y: ascend (x -> max). As y grows, x * y grows.
        // Inner prune: if product >= best, the rest of the row can’t help.
        for y in x..=max {
            let prod = x * y;
            if prod >= best {
                break; // prune the rest of this row
            }
            if is_pal(prod) {
                best = prod;
                break; // first palindrome in this row is the smallest for this row
            }
        }
    }

    if best == u64::MAX {
        return None;
    }

    // Collect factor pairs for `best` (x <= y) within bounds.
    let mut pairs = Vec::new();
    if best == 0 {
        // If 0 is in range, every y in [min..max] forms (0,y).
        if min == 0 {
            for y in min..=max {
                pairs.push((0, y));
            }
        }
    } else {
        // x ranges from ceil(best/max) up to floor(sqrt(best)), inclusive.
        let sqrtb = (best as f64).sqrt() as u64;
        let low = best.div_ceil(max).max(min); // ceil division then clamp
        let high = sqrtb.min(max);
        for x in low..=high {
            if best % x == 0 {
                let y = best / x;
                if x <= y && y >= min && y <= max {
                    pairs.push((x, y));
                }
            }
        }
    }

    Some((best, pairs))
}

/// Generic line-protocol server. `do_iters(min,max,iters)` must do the full work
/// (including factor vector build) and return the final product.
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

    /// Normalize factors: ensure each pair is (min,max), then sort the whole list.
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
