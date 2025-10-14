#![feature(explicit_tail_calls, portable_simd)]

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
use std::num::NonZeroU32;
use std::time::Instant;

use arrayvec::ArrayVec;

pub mod functional;
pub mod simd;

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
fn has_even_digits(n: u32) -> bool {
    debug_assert!(n >= 11);
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
#[inline(always)]
pub fn is_pal(n: u32) -> bool {
    if n < 10 {
        return true;
    }
    // Non-zero numbers ending in 0 cannot be palindromes.
    if n.is_multiple_of(10) {
        return false;
    }

    // Even-length palindromes must be divisible by 11.
    if has_even_digits(n) && !n.is_multiple_of(11) {
        return false;
    }

    // Half-reverse
    let mut m = n;
    let mut rev: u32 = 0;
    while m > rev {
        rev = rev * 10 + m % 10;
        m /= 10;
    }

    m == rev || m == rev / 10
}

#[inline(always)]
const fn recip64(d: u32) -> u64 {
    // ceil(2^64 / d) = floor((2^64 + d - 1) / d)
    // This is the standard 64-bit reciprocal for 32-bit divisors.
    (1u128 << 64).div_ceil(d as u128) as u64
}

const fn build_recip_1k() -> [u64; 1000] {
    let mut table = [0u64; 1000];
    let mut i = 1usize;
    while i <= 999 {
        table[i] = recip64(i as u32);
        i += 1;
    }
    table
}

#[repr(align(64))] // start on a cache line (optional)
pub struct RecipTable([u64; 1000]);

pub const RECIP_1K: RecipTable = RecipTable(build_recip_1k());

#[inline(always)]
pub fn divrem_u32_magic(n: u32, d: u32) -> (u32, u32) {
    debug_assert!((1..=999).contains(&d));

    // Fast path handles 2^64 overflow case.
    if d == 1 {
        return (n, 0);
    }

    // unchecked to avoid bounds checks in hot code (debug_assert guards it)
    let m = unsafe { *RECIP_1K.0.get_unchecked(d as usize) };
    let mut q = (((n as u128) * (m as u128)) >> 64) as u32;
    let mut r = n.wrapping_sub(q.wrapping_mul(d));
    if r >= d {
        q = q.wrapping_add(1);
        r = r.wrapping_sub(d);
    }
    (q, r)
}

#[inline(always)]
pub fn is_multiple_of_magic(n: u32, d: u32) -> bool {
    divrem_u32_magic(n, d).1 == 0
}

//
// Factor pair collection
//

#[inline]
pub fn collect_factor_pairs_bounded_largest(
    product: NonZeroU32,
    min: NonZeroU32,
    max: NonZeroU32,
    // search anything with x <= search_max (typically min(known_x, known_y) - 1)
    search_max: NonZeroU32,
    out: &mut ArrayVec<u32, 4>,
) {
    if out.len() >= 4 {
        return;
    }

    let product = product.get();
    let min = min.get();
    let max = max.get();

    // x in [ceil(product/max) .. min(isqrt(product), search_max)]
    let low = product.div_ceil(max).max(min);
    let high = product.isqrt().min(search_max.get());
    if low > high {
        return;
    }

    let mut x = low;
    // raw write path to minimize bounds checks
    let ptr = out.as_mut_ptr();
    let mut len = out.len();

    unsafe {
        while x <= high {
            let (y, r) = divrem_u32_magic(product, x);
            if r == 0 {
                *ptr.add(len) = x;
                *ptr.add(len + 1) = y;
                len += 2;
                if len >= 4 {
                    break;
                }
            }
            x += 1;
        }
        out.set_len(len);
    }
}

#[inline]
pub fn collect_factor_pairs_bounded_smallest(
    product: NonZeroU32,
    min: NonZeroU32,
    max: NonZeroU32,
    // start strictly above the known smaller factor to avoid duplicating it
    // (typically known_x.min(known_y) + 1)
    start_above: NonZeroU32,
    out: &mut ArrayVec<u32, 4>,
) {
    if out.len() >= 4 {
        return;
    }

    let product = product.get();
    let min = min.get();
    let max = max.get();

    // x in [ceil(product/max) .. isqrt(product)], but begin at max(low, start_above)
    let low = product.div_ceil(max).max(min);
    let base = low.max(start_above.get());
    let high = product.isqrt();
    if base > high {
        return;
    }

    let mut x = base;
    let ptr = out.as_mut_ptr();
    let mut len = out.len();

    unsafe {
        while x <= high {
            let (y, r) = divrem_u32_magic(product, x);
            if r == 0 {
                *ptr.add(len) = x;
                *ptr.add(len + 1) = y;
                len += 2;
                if len >= 4 {
                    break;
                }
            }
            x += 1;
        }
        out.set_len(len);
    }
}

#[inline]
pub fn collect_factor_pairs_range(product: u32, min: u32, max: u32) -> ArrayVec<u32, 4> {
    // Tight window: x in [ceil(product/max) .. min(max, isqrt(product))]
    let low = product.div_ceil(max).max(min);
    let high = product.isqrt().min(max);

    // Verified for bounds [1, 999] inclusive (both smallest and largest):
    // the factor-pair list never exceeds 4 slots (2 pairs). Using capacity=4
    // improves cache usage and reduces stack footprint for this benchmark scope.
    let mut out: ArrayVec<u32, 4> = ArrayVec::new_const();
    for x in low..=high {
        if product.is_multiple_of(x) {
            // y is automatically >= x because x <= isqrt(product)
            let y = product / x;
            out.push(x);
            out.push(y);
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
/// The factor pairs are returned as a flat array `[x0, y0, x1, y1, ...]`.
///
/// Algorithm:
/// - `x` ascends from `min` to `max`.
/// - Outer prune: if `x*x >= best`, later rows cannot improve `best`.
/// - For a fixed `x`, we only need `y` up to `y_upper = min(max, (best-1)/x)`.
///   If `y_upper < x`, there is no work in that row.
/// - Iterate `y` from `x` to `y_upper`; the first palindrome in that row is
///   the row minimum; update `best` and continue.
#[inline]
pub fn smallest_product(
    min: NonZeroU32,
    max: NonZeroU32,
) -> Option<(NonZeroU32, NonZeroU32, NonZeroU32)> {
    let min = min.get();
    let max = max.get();

    let mut best: u32 = u32::MAX;
    let mut pair: (u32, u32) = (0, 0);
    let start = min.max(1);

    if start > max {
        return None;
    }

    let mut x = start;
    while x <= max {
        if x * x >= best {
            break;
        }

        let x_nz = unsafe { NonZeroU32::new_unchecked(x) };
        let (q, _) = divrem_u32_magic(best - 1, x_nz.get());
        let y_upper = (q).min(max);

        if y_upper >= x {
            let mut y = x;
            loop {
                let prod = x * y;
                if is_pal(prod) {
                    best = prod;
                    pair = (x, y);
                    break;
                }

                if y == y_upper {
                    break;
                }
                y += 1;
            }
        }

        x += 1;
    }

    if best == u32::MAX {
        None
    } else {
        Some((
            unsafe { NonZeroU32::new_unchecked(best) },
            unsafe { NonZeroU32::new_unchecked(pair.0) },
            unsafe { NonZeroU32::new_unchecked(pair.1) },
        ))
    }
}

#[inline]
pub fn smallest(min: u32, max: u32) -> Option<(u32, ArrayVec<u32, 4>)> {
    let min = unsafe { NonZeroU32::new_unchecked(min) };
    let max = unsafe { NonZeroU32::new_unchecked(max) };
    smallest_product(min, max).map(|(product, x, y)| {
        let x: u32 = x.into();
        let y: u32 = y.into();
        let mut factor_pairs = ArrayVec::new_const();
        factor_pairs.push(x);
        factor_pairs.push(y);
        let start_above = unsafe { NonZeroU32::new_unchecked(x.min(y) + 1) };
        collect_factor_pairs_bounded_smallest(product, min, max, start_above, &mut factor_pairs);

        (product.into(), factor_pairs)
    })
}

/// Find the largest palindromic product in `[min..max]` and its factor pairs.
///
/// Returns `Some((product, pairs))` or `None` if either the range is invalid or
/// no palindrome exists.
///
/// The factor pairs are returned as a flat array [x0, y0, x1, y1, ...].
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
pub fn largest_product(
    min: NonZeroU32,
    max: NonZeroU32,
) -> Option<(NonZeroU32, NonZeroU32, NonZeroU32)> {
    let min = min.get();
    let max = max.get();

    let mut best: u32 = 0;
    let mut pair: (u32, u32) = (0, 0);
    let start = min.max(1);

    if start > max {
        return None;
    }

    let mut x = max;
    while x >= start {
        if x * max <= best {
            break;
        }

        let x_nz = unsafe { NonZeroU32::new_unchecked(x) };
        let (q, _) = divrem_u32_magic(best, x_nz.get());
        let y_lower = (q + 1).max(x);

        if y_lower <= max {
            let mut y = max;
            loop {
                let p = x * y;
                if is_pal(p) {
                    best = p;
                    pair = (x, y);
                    break;
                }

                if y == y_lower {
                    break;
                }
                y -= 1;
            }
        }

        x -= 1;
    }

    if best > 0 {
        Some((
            unsafe { NonZeroU32::new_unchecked(best) },
            unsafe { NonZeroU32::new_unchecked(pair.0) },
            unsafe { NonZeroU32::new_unchecked(pair.1) },
        ))
    } else {
        None
    }
}

#[inline]
pub fn largest(min: u32, max: u32) -> Option<(u32, ArrayVec<u32, 4>)> {
    let min = unsafe { NonZeroU32::new_unchecked(min) };
    let max = unsafe { NonZeroU32::new_unchecked(max) };
    largest_product(min, max).map(|(product, x, y)| {
        let x: u32 = x.into();
        let y: u32 = y.into();
        let search_max = NonZeroU32::new(x.min(y).saturating_sub(1));
        let mut factor_pairs = ArrayVec::new_const();
        factor_pairs.push(x);
        factor_pairs.push(y);
        if let Some(search_max) = search_max {
            collect_factor_pairs_bounded_largest(product, min, max, search_max, &mut factor_pairs);
        };

        (product.into(), factor_pairs)
    })
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
    F: FnMut(u32, u32, u64) -> (Option<u32>, u64, u64),
{
    #[inline(always)]
    fn next_field<'a>(buf: &'a [u8], i: &mut usize) -> &'a [u8] {
        let s = *i;
        while *i < buf.len() && buf[*i] != b' ' && buf[*i] != b'\n' {
            *i += 1;
        }
        let field = &buf[s..*i];
        if *i < buf.len() && buf[*i] == b' ' {
            *i += 1;
        }
        field
    }

    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let mut reader = BufReader::new(stdin.lock());
    let mut writer = std::io::BufWriter::new(stdout.lock());

    let mut buf: Vec<u8> = Vec::with_capacity(256);
    let mut min: u32 = 0;
    let mut max: u32 = 0;

    loop {
        buf.clear();
        if reader
            .read_until(b'\n', &mut buf)
            .expect("This command parser very narrowly expects correct input")
            == 0
        {
            // EOF
            break;
        }

        // Command dispatch by first byte; jump directly to first integer
        let cmd0 = buf[0];

        if cmd0 == b'I' {
            // INIT <min> <max>
            let mut i = 5; // after "INIT "
            let a_bytes = next_field(&buf, &mut i);
            let b_bytes = next_field(&buf, &mut i);
            min = parse_u32(a_bytes);
            max = parse_u32(b_bytes);
            writer.write_all(b"OK\n").unwrap();
            writer.flush().unwrap();
        } else if cmd0 == b'W' {
            // WARMUP <iters>
            let mut i = 7; // after "WARMUP "
            let it_bytes = next_field(&buf, &mut i);
            let iters = parse_u64(it_bytes);
            let _ = do_iters(min, max, iters);
            writer.write_all(b"OK\n").unwrap();
            writer.flush().unwrap();
        } else if cmd0 == b'R' {
            // RUN <iters>
            let mut i = 4; // after "RUN "
            let it_bytes = next_field(&buf, &mut i);
            let iters = parse_u64(it_bytes);
            let (prod_opt, acc, nanos) = do_iters(min, max, iters);
            let prod = prod_opt.unwrap_or(0);
            let mut out = [0u8; 96];
            let mut p = 0usize;
            out[p..p + 3].copy_from_slice(b"OK ");
            p += 3;
            p += append_u32(&mut out[p..], prod);
            out[p] = b' ';
            p += 1;
            p += append_u64(&mut out[p..], acc);
            out[p] = b' ';
            p += 1;
            p += append_u64(&mut out[p..], nanos);
            out[p] = b'\n';
            p += 1;
            writer.write_all(&out[..p]).unwrap();
            writer.flush().unwrap();
        } else if cmd0 == b'Q' {
            // QUIT
            break;
        }
    }
}

#[inline(always)]
fn accumulate_result(acc: &mut u64, counter: &mut u64, result: Option<(u32, ArrayVec<u32, 4>)>) {
    if let Some((prod, pairs)) = result {
        *acc += prod as u64 + *counter + pairs.into_iter().map(|value| value as u64).sum::<u64>();
        *counter += 1;
    }
}

#[inline(always)]
pub fn run_iters_desc<F>(min: u32, max: u32, iters: u64, finder: F) -> (Option<u32>, u64, u64)
where
    F: Fn(u32, u32) -> Option<(u32, ArrayVec<u32, 4>)>,
{
    let base_prod = finder(min, max).map(|(product, _)| product);

    let mut acc: u64 = 0;
    let mut counter: u64 = 0;
    let mut current = max;
    let start = Instant::now();

    for _ in 0..iters {
        accumulate_result(&mut acc, &mut counter, finder(min, current));

        current = if current <= min { max } else { current - 1 };
    }

    let nanos = start.elapsed().as_nanos();
    let elapsed_ns = if nanos > u64::MAX as u128 {
        u64::MAX
    } else {
        nanos as u64
    };

    (base_prod, acc, elapsed_ns)
}

#[inline(always)]
pub fn run_iters_asc<F>(min: u32, max: u32, iters: u64, finder: F) -> (Option<u32>, u64, u64)
where
    F: Fn(u32, u32) -> Option<(u32, ArrayVec<u32, 4>)>,
{
    let base_prod = finder(min, max).map(|(product, _)| product);

    let mut acc: u64 = 0;
    let mut counter: u64 = 0;
    let mut current = min;
    let start = Instant::now();

    for _ in 0..iters {
        accumulate_result(&mut acc, &mut counter, finder(current, max));

        current = if current >= max { min } else { current + 1 };
    }

    let nanos = start.elapsed().as_nanos();
    let elapsed_ns = if nanos > u64::MAX as u128 {
        u64::MAX
    } else {
        nanos as u64
    };

    (base_prod, acc, elapsed_ns)
}

#[inline]
fn parse_u32(bytes: &[u8]) -> u32 {
    let mut v: u32 = 0;
    for &c in bytes {
        v = v.wrapping_mul(10).wrapping_add((c - b'0') as u32);
    }
    v
}

#[inline]
fn parse_u64(bytes: &[u8]) -> u64 {
    let mut v: u64 = 0;
    for &c in bytes {
        v = v.wrapping_mul(10).wrapping_add((c - b'0') as u64);
    }
    v
}

// Append decimal without allocation; returns bytes written
#[inline]
fn append_u32(dst: &mut [u8], mut v: u32) -> usize {
    if v == 0 {
        dst[0] = b'0';
        return 1;
    }
    let mut tmp = [0u8; 10];
    let mut i = 0;
    while v > 0 {
        tmp[i] = b'0' + (v % 10) as u8;
        v /= 10;
        i += 1;
    }
    // reverse into dst
    for j in 0..i {
        dst[j] = tmp[i - 1 - j];
    }
    i
}

#[inline]
fn append_u64(dst: &mut [u8], mut v: u64) -> usize {
    if v == 0 {
        dst[0] = b'0';
        return 1;
    }
    let mut tmp = [0u8; 20];
    let mut i = 0;
    while v > 0 {
        tmp[i] = b'0' + (v % 10) as u8;
        v /= 10;
        i += 1;
    }
    for j in 0..i {
        dst[j] = tmp[i - 1 - j];
    }
    i
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
    fn test_smallest() {
        let (product, factors) = smallest(910, 999).unwrap();
        assert_eq!(product, 861168);
        assert_eq!(factors[0], 924);
        assert_eq!(factors[1], 932);
    }

    #[test]
    fn largest_910_999() {
        let (p, f) = largest(910, 999).unwrap();
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
    fn largest_100_999() {
        let (p, f) = largest(100, 999).unwrap();
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
    fn empty_result_for_largest_if_no_palindrome_in_the_range() {
        let (min_factor, max_factor) = (15, 15);
        assert!(largest(min_factor, max_factor).is_none());
    }

    #[test]
    fn error_result_for_smallest_if_min_is_more_than_max() {
        let (min_factor, max_factor) = (10_000, 1);
        assert!(smallest(min_factor, max_factor).is_none());
    }

    // Measurement tests for factor-pair buffer sizing at 999
    // Run filtered: cargo test --release -- measure_iter -- --nocapture
    #[test]
    fn measure_iter_max_pairs_smallest_999() {
        let limit = 999u32;
        let max_len = (1..=limit)
            .filter_map(|current_min| {
                smallest(current_min, limit).map(|(product, pairs)| {
                    let len = pairs.len();
                    if len == 4 {
                        println!(
                            "current_min: {current_min} product: {product}; pairs: {:?}",
                            pairs
                        );
                    }
                    len
                })
            })
            .max()
            .unwrap_or(0);
        assert_eq!(max_len, 4, "observed max_len = {}", max_len);
    }

    #[test]
    fn measure_iter_max_pairs_largest_999() {
        let limit = 999u32;
        let max_len = (1..=limit)
            .rev()
            .filter_map(|current_max| largest(1u32, current_max).map(|(_, pairs)| pairs.len()))
            .max()
            .unwrap_or(0);
        assert_eq!(max_len, 4, "observed max_len = {}", max_len);
    }
}
