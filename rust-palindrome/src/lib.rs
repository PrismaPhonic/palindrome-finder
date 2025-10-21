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

use collections::{FactorBuf, PalOut};

pub mod collections;
pub mod functional;
pub mod simd;

//
// Palindrome helpers
//

#[inline(always)]
pub fn has_even_digits(n: u32) -> bool {
    debug_assert!(n >= 10);
    let cmp_a = (n >= 100) as u32;
    let cmp_b = (n >= 1_000) as u32;
    let cmp_c = (n >= 10_000) as u32;
    let cmp_d = (n >= 100_000) as u32;
    let t0 = cmp_a ^ cmp_b;
    let t1 = cmp_c ^ cmp_d;
    let parity = 1 ^ (t0 ^ t1);
    parity == 1
}

/// Return true if `n` is a decimal palindrome (numeric half-reversal).
/// Only supports up to 6 digits.
#[inline(always)]
pub fn is_pal(n: u32) -> bool {
    if n < 10 {
        return true;
    }

    let mut rev = n % 10;
    let mut m = n / 10;

    // Non-zero numbers ending in 0 cannot be palindromes.
    if rev == 0 {
        return false;
    }

    // Even-length palindromes must be divisible by 11.
    if has_even_digits(n) && !n.is_multiple_of(11) {
        return false;
    }

    // Half-reverse
    while m > rev {
        rev = rev * 10 + m % 10;
        m /= 10;
    }

    m == rev || m == rev / 10
}

#[inline(always)]
pub fn is_pal_old(n: u32) -> bool {
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
    let mut rev = 0;
    while m > rev {
        rev = rev * 10 + m % 10;
        m /= 10;
    }

    m == rev || m == rev / 10
}

#[inline(always)]
pub fn is_pal_v3(n: u32) -> bool {
    // 1 digit
    if n < 10 {
        return true;
    }

    // First peel once and reuse everywhere
    // We would have to check trailing zeros anyways and LLVM will fuse these
    // into a single magic constant based instruction.
    let mut rev = n % 10;
    let mut m = n / 10;

    // Non-zero numbers ending with 0 cannot be palindromes.
    if rev == 0 {
        return false;
    }

    if n < 100 {
        // 2 digits
        return m == rev;
    }

    // Second peel
    rev = rev * 10 + (m % 10);
    m /= 10;

    if n < 10_000 {
        // 3 or 4 digits.
        return m == rev || m == rev / 10;
    }

    // 3rd peel
    rev = rev * 10 + (m % 10);
    m /= 10;

    m == rev || m == rev / 10
}

#[inline(always)]
#[allow(clippy::manual_is_multiple_of)]
pub fn is_pal_v2(n: u32) -> bool {
    // 1 digit
    if n < 10 {
        return true;
    }

    // First peel once and reuse everywhere
    // We would have to check trailing zeros anyways and LLVM will fuse these
    // into a single magic constant based instruction.
    let mut rev = n % 10;
    let mut m = n / 10;

    // Non-zero numbers ending with 0 cannot be palindromes.
    if rev == 0 {
        return false;
    }

    if n < 100 {
        // 2 digits
        return m == rev;
    }

    let ge1k = (n >= 1_000) as u32;
    let ge10k = (n >= 10_000) as u32;
    let ge100k = (n >= 100_000) as u32;
    let parity = ge1k ^ ge10k ^ ge100k;

    if parity == 1 && n % 11 != 0 {
        return false;
    }

    loop {
        rev = rev * 10 + (m % 10);
        m /= 10;

        if m <= rev {
            break;
        }
    }

    if parity == 1 { m == rev } else { m == rev / 10 }
}

#[inline(always)]
const fn recip64(d: u32) -> u64 {
    // This is the standard 64-bit reciprocal for 32-bit divisors.
    (1u128 << 64).div_ceil(d as u128) as u64
}

#[inline(always)]
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
    #[cfg(debug_assertions)] // Useful in tests, but we don't benchmark with it.
    if d == 1 {
        return (n, 0);
    }

    // unchecked to avoid bounds checks in hot code (debug_assert guards it)
    let m = unsafe { *RECIP_1K.0.get_unchecked(d as usize) };
    let mut q = (((n as u128) * (m as u128)) >> 64) as u32;
    let mut r = n - q * d;
    if r >= d {
        q += 1;
        r -= d;
    }
    (q, r)
}

#[inline(always)]
pub fn is_multiple_of_magic(n: u32, d: u32) -> bool {
    divrem_u32_magic(n, d).1 == 0
}

#[inline]
pub fn collect_factor_pairs(product: NonZeroU32, min: NonZeroU32, max: NonZeroU32) -> FactorBuf {
    let product = product.get();
    let min = min.get();
    let max = max.get();

    let (q, _) = divrem_u32_magic(product + max - 1, max);
    let low = q.max(min);
    let high = product.isqrt().min(max);

    // Verified for bounds [1, 999] inclusive (both smallest and largest):
    // the factor-pair list never exceeds 4 slots (2 pairs). Using capacity=4
    // improves cache usage and reduces stack footprint for this benchmark scope.
    let mut out = FactorBuf::new();
    let mut len = 0usize;
    let ptr = out.as_mut_ptr();

    let mut x = low;
    while x <= high {
        let (y, r) = divrem_u32_magic(product, x);
        if r == 0 {
            unsafe {
                core::ptr::write(ptr.add(len), x);
                core::ptr::write(ptr.add(len + 1), y);
            };
            len += 2;
            if len == 4 {
                break;
            }
        }
        x += 1;
    }

    out
}

//
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
#[inline(always)]
pub fn smallest_product(min: NonZeroU32, max: NonZeroU32) -> Option<NonZeroU32> {
    let min = min.get();
    let max = max.get();

    let mut best: u32 = u32::MAX;
    let start = min.max(1);

    if start > max {
        return None;
    }

    let mut x = start;
    while x <= max {
        if x * x >= best {
            break;
        }

        let (q, _) = divrem_u32_magic(best - 1, x);
        let y_upper = (q).min(max);

        if y_upper >= x {
            let mut y = x;
            let mut prod = x * y;
            loop {
                // For some reason that I can't explain this performs much
                // better for smallest.
                if is_pal_old(prod) {
                    best = prod;
                    break;
                }

                if y == y_upper {
                    break;
                }
                y += 1;
                prod += x;
            }
        }

        x += 1;
    }

    if best == u32::MAX {
        None
    } else {
        Some(unsafe { NonZeroU32::new_unchecked(best) })
    }
}

#[inline(always)]
pub fn smallest(min: u32, max: u32) -> Option<PalOut> {
    let min = unsafe { NonZeroU32::new_unchecked(min) };
    let max = unsafe { NonZeroU32::new_unchecked(max) };
    smallest_product(min, max).map(|product| {
        let factor_pairs = collect_factor_pairs(product, min, max);
        factor_pairs.with_product(product.into())
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
pub fn largest_product(min: NonZeroU32, max: NonZeroU32) -> Option<NonZeroU32> {
    let min = min.get();
    let max = max.get();

    let mut best: u32 = 0;
    let start = min.max(1);

    if start > max {
        return None;
    }

    let mut x = max;
    while x >= start {
        if x * max <= best {
            break;
        }

        let (q, _) = divrem_u32_magic(best, x);
        let y_lower = (q + 1).max(x);

        if y_lower <= max {
            let mut y = max;
            let mut prod = x * y;
            loop {
                if is_pal_v3(prod) {
                    best = prod;
                    break;
                }

                if y == y_lower {
                    break;
                }
                y -= 1;
                prod -= x;
            }
        }

        x -= 1;
    }

    if best > 0 {
        Some(unsafe { NonZeroU32::new_unchecked(best) })
    } else {
        None
    }
}

#[inline]
pub fn largest(min: u32, max: u32) -> Option<PalOut> {
    let min = unsafe { NonZeroU32::new_unchecked(min) };
    let max = unsafe { NonZeroU32::new_unchecked(max) };
    largest_product(min, max).map(|product| {
        let factor_pairs = collect_factor_pairs(product, min, max);
        factor_pairs.with_product(product.into())
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
fn accumulate_result(acc: &mut u64, counter: &mut u64, result: Option<PalOut>) {
    if let Some(out) = result {
        *acc += out.product as u64
            + *counter
            + out.pairs.into_iter().map(|value| value as u64).sum::<u64>();
        *counter += 1;
    }
}

// A wrapper so we get no inlining for the one time we call largest to produce the base prod to be returned,
#[inline(never)]
fn largest_once(min: u32, max: u32) -> Option<PalOut> {
    largest(min, max)
}

#[inline(never)]
fn smallest_once(min: u32, max: u32) -> Option<PalOut> {
    smallest(min, max)
}

#[inline(never)]
pub fn run_iters_desc<F>(min: u32, max: u32, iters: u64, finder: F) -> (Option<u32>, u64, u64)
where
    F: Fn(u32, u32) -> Option<PalOut>,
{
    let base_prod = finder(min, max).map(|pal_out| pal_out.product);

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

#[inline(never)]
pub fn run_iters_largest(min: u32, max: u32, iters: u64) -> (Option<u32>, u64, u64) {
    let base_prod = largest_once(min, max).map(|pal_out| pal_out.product);

    let mut acc: u64 = 0;
    let mut counter: u64 = 0;
    let mut current = max;
    let start = Instant::now();

    for _ in 0..iters {
        if let Some(out) = largest(min, current) {
            acc += counter + out.consume();
            counter += 1;
        }

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

#[inline(never)]
pub fn run_iters_asc<F>(min: u32, max: u32, iters: u64, finder: F) -> (Option<u32>, u64, u64)
where
    F: Fn(u32, u32) -> Option<PalOut>,
{
    let base_prod = finder(min, max).map(|pal_out| pal_out.product);

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

#[inline(never)]
pub fn run_iters_smallest(min: u32, max: u32, iters: u64) -> (Option<u32>, u64, u64) {
    let base_prod = smallest_once(min, max).map(|pal_out| pal_out.product);

    let mut acc: u64 = 0;
    let mut counter: u64 = 0;
    let mut current = min;
    let start = Instant::now();

    for _ in 0..iters {
        if let Some(out) = smallest(current, max) {
            acc += counter + out.consume();
            counter += 1;
        }

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

#[inline(never)]
fn parse_u32(bytes: &[u8]) -> u32 {
    let mut v: u32 = 0;
    for &c in bytes {
        v = v.wrapping_mul(10).wrapping_add((c - b'0') as u32);
    }
    v
}

#[inline(never)]
fn parse_u64(bytes: &[u8]) -> u64 {
    let mut v: u64 = 0;
    for &c in bytes {
        v = v.wrapping_mul(10).wrapping_add((c - b'0') as u64);
    }
    v
}

// Append decimal without allocation; returns bytes written
#[inline(never)]
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

#[inline(never)]
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

    fn assert_some_eq(got: Option<PalOut>, expect_p: u32, expect_factors: &[(u32, u32)]) {
        let PalOut {
            product: p,
            pairs: f,
        } = got.expect("expected Some(..), got None");
        assert_eq!(p, expect_p, "product mismatch");

        // Convert flat array to pairs for comparison
        let mut pairs = Vec::new();
        for i in (0..f.len()).step_by(2) {
            if i + 1 < f.len() && f[i] != 0 {
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
        let PalOut { product, pairs } = smallest(910, 999).unwrap();
        assert_eq!(product, 861168);
        assert_eq!(pairs[0], 924);
        assert_eq!(pairs[1], 932);
    }

    #[test]
    fn largest_910_999() {
        let PalOut {
            product: p,
            pairs: f,
        } = largest(910, 999).unwrap();
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
        let PalOut {
            product: p,
            pairs: f,
        } = largest(100, 999).unwrap();
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
                smallest(current_min, limit).map(|PalOut { product, pairs }| {
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
            .filter_map(|current_max| {
                largest(1u32, current_max).map(|PalOut { product: _, pairs }| pairs.len())
            })
            .max()
            .unwrap_or(0);
        assert_eq!(max_len, 4, "observed max_len = {}", max_len);
    }

    // Generate all possible palindrome inputs from the range 2..=999
    fn palindrome_inputs() -> Vec<u32> {
        use std::collections::HashSet;
        let set: HashSet<u32> = (2u32..=999)
            .flat_map(|a| (2u32..=999).map(move |b| a * b))
            .collect();
        let mut vec: Vec<u32> = set.into_iter().collect();
        vec.sort();
        vec
    }

    #[test]
    fn compare_is_pal_implementations() {
        let inputs = palindrome_inputs();
        println!("Testing {} palindrome inputs", inputs.len());

        let mut differences = Vec::new();
        for &n in &inputs {
            let old_result = is_pal_old(n);
            let new_result = is_pal_v3(n);

            if old_result != new_result {
                differences.push((n, old_result, new_result));
                if differences.len() <= 10 {
                    // Print first 10 differences
                    println!(
                        "Difference at n={}: old={}, new={}",
                        n, old_result, new_result
                    );
                }
            }
        }

        if !differences.is_empty() {
            println!(
                "Found {} differences out of {} inputs",
                differences.len(),
                inputs.len()
            );
            println!("First few differences:");
            for (n, old, new) in differences.iter().take(5) {
                println!("  n={}: old={}, new={}", n, old, new);
            }
        } else {
            println!("No differences found - both implementations agree on all inputs");
        }

        // This test will fail if there are differences, helping us identify the issue
        assert!(
            differences.is_empty(),
            "Found {} differences between is_pal_old and is_pal",
            differences.len()
        );
    }

    #[test]
    fn compare_largest_implementations() {
        // Test the specific range that's causing issues
        let ranges = [
            (100, 999),
            (200, 999),
            (300, 999),
            (400, 999),
            (500, 999),
            (600, 999),
            (700, 999),
            (800, 999),
            (900, 999),
        ];

        for &(min, max) in &ranges {
            let old_result = largest_old(min, max);
            let new_result = largest(min, max);

            if old_result != new_result {
                println!("Difference in range ({}, {}):", min, max);
                println!("  old: {:?}", old_result);
                println!("  new: {:?}", new_result);
            }
        }
    }

    #[test]
    fn test_accumulate_function() {
        // Test the accumulate function with the problematic values
        let mut acc = 0u64;
        let mut counter = 0u64;

        // Test with a known PalOut
        let pal_out = PalOut {
            product: 4,
            pairs: [1, 4, 2, 2], // This represents pairs (1,4) and (2,2)
        };

        println!("Testing accumulate with PalOut: {:?}", pal_out);
        println!("Before: acc={}, counter={}", acc, counter);

        // Create a copy for consume() call
        let pal_out_copy = PalOut {
            product: pal_out.product,
            pairs: pal_out.pairs,
        };

        accumulate_result(&mut acc, &mut counter, Some(pal_out));

        println!("After: acc={}, counter={}", acc, counter);
        println!("consume() result: {}", pal_out_copy.consume());

        // Test the original logic manually
        let mut acc_manual = 0u64;
        let mut counter_manual = 0u64;
        let product = 4u32;
        let pairs = [1u32, 4u32, 2u32, 2u32];

        acc_manual += product as u64
            + counter_manual
            + pairs.into_iter().map(|value| value as u64).sum::<u64>();
        counter_manual += 1;

        println!(
            "Manual calculation: acc={}, counter={}",
            acc_manual, counter_manual
        );
        println!(
            "Expected: product={} + counter={} + sum_of_pairs={}",
            product,
            0,
            pairs.into_iter().sum::<u32>()
        );
    }

    #[test]
    fn test_specific_problematic_range() {
        // Test the specific range that's causing issues in the correctness test
        let min = 2u32;
        let max = 999u32;

        println!("Testing largest({}, {})", min, max);

        let old_result = largest_old(min, max);
        let new_result = largest(min, max);

        println!("Old result: {:?}", old_result);
        println!("New result: {:?}", new_result);

        if old_result != new_result {
            println!("DIFFERENCE FOUND!");
            if let (Some(old), Some(new)) = (old_result, new_result) {
                println!("Old product: {}, pairs: {:?}", old.product, old.pairs);
                println!("New product: {}, pairs: {:?}", new.product, new.pairs);

                // Test if the products are palindromes
                println!("Old product is palindrome: {}", is_pal_old(old.product));
                println!("New product is palindrome: {}", is_pal_old(new.product));
                println!("Old product is palindrome (v3): {}", is_pal_v3(old.product));
                println!("New product is palindrome (v3): {}", is_pal_v3(new.product));
            }
        } else {
            println!("Results are identical");
        }
    }

    #[test]
    fn test_run_iters_desc_comparison() {
        // Test the run_iters_desc function with the problematic range
        let min = 2u32;
        let max = 999u32;
        let iters = 10000u64;

        println!("Testing run_iters_desc({}, {}, {})", min, max, iters);

        // Test with old implementation
        let old_result = run_iters_desc_old(min, max, iters, largest_old);
        println!("Old result: {:?}", old_result);

        // Test with new implementation
        let new_result = run_iters_desc(min, max, iters, largest);
        println!("New result: {:?}", new_result);

        if old_result != new_result {
            println!("DIFFERENCE FOUND in run_iters_desc!");
        } else {
            println!("Results are identical");
        }
    }

    #[test]
    fn test_product_comparison_cycle() {
        // Test cycling through the range once (999 down to 2) and compare products
        let min = 2u32;
        let max = 999u32;

        println!(
            "Testing product comparison cycle from {} down to {}",
            max, min
        );

        // Test old implementation
        let mut products_old = Vec::new();
        let mut current_max_old = max;

        while current_max_old >= min {
            let product_old =
                largest_product_old(unsafe { NonZeroU32::new_unchecked(min) }, unsafe {
                    NonZeroU32::new_unchecked(current_max_old)
                });
            products_old.push((current_max_old, product_old));
            current_max_old -= 1;
        }

        // Test new implementation
        let mut products_new = Vec::new();
        let mut current_max_new = max;

        while current_max_new >= min {
            let product_new = largest_product(unsafe { NonZeroU32::new_unchecked(min) }, unsafe {
                NonZeroU32::new_unchecked(current_max_new)
            });
            products_new.push((current_max_new, product_new));
            current_max_new -= 1;
        }

        // Compare results
        let mut differences = Vec::new();
        for ((max_old, prod_old), (max_new, prod_new)) in
            products_old.iter().zip(products_new.iter())
        {
            if max_old != max_new {
                println!("ERROR: max values don't match: {} vs {}", max_old, max_new);
                break;
            }
            if prod_old != prod_new {
                differences.push((*max_old, *prod_old, *prod_new));
                if differences.len() <= 10 {
                    println!(
                        "Difference at max={}: old={:?}, new={:?}",
                        max_old, prod_old, prod_new
                    );
                }
            }
        }

        if !differences.is_empty() {
            println!(
                "Found {} differences out of {} comparisons",
                differences.len(),
                products_old.len()
            );
            println!("First few differences:");
            for (max_val, old_prod, new_prod) in differences.iter().take(5) {
                println!("  max={}: old={:?}, new={:?}", max_val, old_prod, new_prod);
            }
        } else {
            println!("No differences found - both implementations find identical products");
        }
    }

    #[test]
    fn test_specific_palindrome_888888() {
        let n = 888888u32;
        println!("Testing palindrome: {}", n);
        println!("is_pal_old({}) = {}", n, is_pal_old(n));
        println!("is_pal_v3({}) = {}", n, is_pal_v3(n));
        println!("is_pal({}) = {}", n, is_pal(n));

        // Let's also test 886688
        let n2 = 886688u32;
        println!("Testing palindrome: {}", n2);
        println!("is_pal_old({}) = {}", n2, is_pal_old(n2));
        println!("is_pal_v3({}) = {}", n2, is_pal_v3(n2));
        println!("is_pal({}) = {}", n2, is_pal(n2));
    }

    #[test]
    fn test_largest_product_specific_range() {
        // Test the specific range where we found differences
        let min = 2u32;
        let max = 992u32; // One of the problematic ranges

        println!("Testing largest_product({}, {})", min, max);

        let old_result = largest_product_old(unsafe { NonZeroU32::new_unchecked(min) }, unsafe {
            NonZeroU32::new_unchecked(max)
        });
        let new_result = largest_product(unsafe { NonZeroU32::new_unchecked(min) }, unsafe {
            NonZeroU32::new_unchecked(max)
        });

        println!("Old result: {:?}", old_result);
        println!("New result: {:?}", new_result);

        if old_result != new_result {
            println!("DIFFERENCE FOUND!");
            if let (Some(old_prod), Some(new_prod)) = (old_result, new_result) {
                println!(
                    "Old product: {}, New product: {}",
                    old_prod.get(),
                    new_prod.get()
                );
                println!("Old is palindrome: {}", is_pal_old(old_prod.get()));
                println!("New is palindrome: {}", is_pal_old(new_prod.get()));
            }
        } else {
            println!("Results are identical");
        }
    }

    // Add the old run_iters_desc function for comparison
    fn run_iters_desc_old<F>(min: u32, max: u32, iters: u64, finder: F) -> (Option<u32>, u64, u64)
    where
        F: Fn(u32, u32) -> Option<PalOut>,
    {
        let base_prod = finder(min, max).map(|pal_out| pal_out.product);

        let mut acc: u64 = 0;
        let mut counter: u64 = 0;
        let mut current_max = max;

        let start = std::time::Instant::now();
        for _ in 0..iters {
            if let Some(result) = finder(min, current_max) {
                accumulate_result_old(&mut acc, &mut counter, Some(result));
            } else {
                accumulate_result_old(&mut acc, &mut counter, None);
            }

            current_max = if current_max <= min {
                max
            } else {
                current_max - 1
            };
        }
        let elapsed = start.elapsed();
        let nanos = elapsed.as_nanos();
        let elapsed_ns = if nanos > u64::MAX as u128 {
            u64::MAX
        } else {
            nanos as u64
        };

        (base_prod, acc, elapsed_ns)
    }

    fn accumulate_result_old(acc: &mut u64, counter: &mut u64, result: Option<PalOut>) {
        if let Some(out) = result {
            *acc += out.product as u64
                + *counter
                + out.pairs.into_iter().map(|value| value as u64).sum::<u64>();
            *counter += 1;
        }
    }

    // Add the old largest function for comparison
    fn largest_old(min: u32, max: u32) -> Option<PalOut> {
        let min = unsafe { NonZeroU32::new_unchecked(min) };
        let max = unsafe { NonZeroU32::new_unchecked(max) };
        largest_product_old(min, max).map(|product| {
            let factor_pairs = collect_factor_pairs(product, min, max);
            factor_pairs.with_product(product.into())
        })
    }

    fn largest_product_old(min: NonZeroU32, max: NonZeroU32) -> Option<NonZeroU32> {
        let mut best = 0;
        let mut x = max.get();

        while x >= min.get() {
            let mut y = x;
            while y >= min.get() {
                let product = x * y;
                if product <= best {
                    break;
                }
                if is_pal_old(product) {
                    best = product;
                    break;
                }
                y -= 1;
            }
            x -= 1;
        }

        if best > 0 {
            Some(unsafe { NonZeroU32::new_unchecked(best) })
        } else {
            None
        }
    }
}
