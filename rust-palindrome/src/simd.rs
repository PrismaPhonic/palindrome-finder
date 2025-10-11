use std::io::{BufRead, BufReader, Write};
use std::num::NonZeroU32;
use std::time::Instant;

use arrayvec::ArrayVec;
use std::simd::cmp::{SimdPartialEq, SimdPartialOrd};
use std::simd::num::SimdUint;
use std::simd::{LaneCount, Mask, Simd, SupportedLaneCount};

use crate::{collect_factor_pairs, has_even_digits, is_pal};

const SIMD_WIDTH: usize = 8;
const SIMD_OFFSETS: Simd<u32, SIMD_WIDTH> = Simd::from_array([0, 1, 2, 3, 4, 5, 6, 7]);
const HALF_SIMD_OFFSETS: Simd<u32, 4> = Simd::from_array([0, 1, 2, 3]);
const QUARTER_SIMD_OFFSETS: Simd<u32, 2> = Simd::from_array([0, 1]);
const SCRATCH_CAPACITY: usize = SIMD_WIDTH * 2;

type SimdMask<const LANES: usize> = Mask<i32, LANES>;
type SimdU32<const LANES: usize> = Simd<u32, LANES>;

#[inline(always)]
fn div_mod_u32_const_portable<const LANES: usize>(
    v: SimdU32<LANES>,
    divisor: u32,
    reciprocal: u64,
) -> (SimdU32<LANES>, SimdU32<LANES>)
where
    LaneCount<LANES>: SupportedLaneCount,
{
    let wide: Simd<u64, LANES> = v.cast();
    let q = ((wide * Simd::splat(reciprocal)) >> Simd::splat(32u64)).cast::<u32>();
    let r = v - q * Simd::splat(divisor);
    let adjust = r.simd_ge(Simd::splat(divisor));
    let q_adj = q + adjust.select(Simd::splat(1u32), Simd::splat(0u32));
    let r_adj = r - adjust.select(Simd::splat(divisor), Simd::splat(0u32));
    (q_adj, r_adj)
}

#[inline(always)]
fn div_mod_10<const LANES: usize>(v: SimdU32<LANES>) -> (SimdU32<LANES>, SimdU32<LANES>)
where
    LaneCount<LANES>: SupportedLaneCount,
{
    div_mod_u32_const_portable(v, 10, 0x1999_999Au64)
}

#[inline(always)]
fn div_mod_11<const LANES: usize>(v: SimdU32<LANES>) -> (SimdU32<LANES>, SimdU32<LANES>)
where
    LaneCount<LANES>: SupportedLaneCount,
{
    div_mod_u32_const_portable(v, 11, 0x1745_D175u64)
}

#[inline(always)]
const fn hundred_threshold<const LANES: usize>() -> SimdU32<LANES>
where
    LaneCount<LANES>: SupportedLaneCount,
{
    Simd::splat(100)
}

#[inline(always)]
const fn thousand_threshold<const LANES: usize>() -> SimdU32<LANES>
where
    LaneCount<LANES>: SupportedLaneCount,
{
    Simd::splat(1_000)
}

#[inline(always)]
const fn ten_thousand_threshold<const LANES: usize>() -> SimdU32<LANES>
where
    LaneCount<LANES>: SupportedLaneCount,
{
    Simd::splat(10_000)
}

#[inline(always)]
const fn hundred_thousand_threshold<const LANES: usize>() -> SimdU32<LANES>
where
    LaneCount<LANES>: SupportedLaneCount,
{
    Simd::splat(100_000)
}

#[inline(always)]
const fn zero<const LANES: usize>() -> SimdU32<LANES>
where
    LaneCount<LANES>: SupportedLaneCount,
{
    Simd::splat(0)
}

/// Vectorised parity of decimal digit-count. Returns mask flagging even-digit lanes.
#[inline(always)]
fn has_even_digits_mask<const LANES: usize>(n: SimdU32<LANES>) -> SimdMask<LANES>
where
    LaneCount<LANES>: SupportedLaneCount,
{
    // We only need to check up to 6 digits, because 999 x 999 is 998,001 which
    // is only 6 digits.
    // We also don't need to check < 10 because we already bail out early on that.

    let mut parity = SimdMask::splat(true);
    let cmp_a = n.simd_ge(hundred_threshold());
    let cmp_b = n.simd_ge(thousand_threshold());
    let cmp_c = n.simd_ge(ten_thousand_threshold());
    let cmp_d = n.simd_ge(hundred_thousand_threshold());
    parity ^= cmp_a;
    parity ^= cmp_b;
    parity ^= cmp_c;
    parity ^= cmp_d;
    parity
}

#[inline(always)]
fn is_pal_simd_mask_generic<const LANES: usize>(n: SimdU32<LANES>) -> SimdMask<LANES>
where
    LaneCount<LANES>: SupportedLaneCount,
{
    let ten = Simd::splat(10);

    let mut m = n;
    let mut rev = Simd::splat(0u32);
    let mut active = m.simd_gt(rev);

    while active.any() {
        let (m_next, digits) = div_mod_10(m);
        let rev_next = rev * ten + digits;
        rev = active.select(rev_next, rev);
        m = active.select(m_next, m);
        active = m.simd_gt(rev);
    }

    let eq_full = m.simd_eq(rev);
    let (rev_div10, _) = div_mod_10(rev);
    let eq_half = rev_div10.simd_eq(m);
    eq_full | eq_half
}

#[inline(always)]
pub fn is_pal_simd_mask<const LANES: usize>(n: SimdU32<LANES>) -> SimdMask<LANES>
where
    LaneCount<LANES>: SupportedLaneCount,
{
    is_pal_simd_mask_generic(n)
}

// Bails out early for trailing zero or even fail modulo 11. We already bail
// before this for < 10 as a safety check, so no need to call it here.
#[inline(always)]
fn simd_early_bailout<const LANES: usize>(n: SimdU32<LANES>) -> SimdMask<LANES>
where
    LaneCount<LANES>: SupportedLaneCount,
{
    let (_n_div10, n_mod10) = div_mod_10(n);
    let trailing_zero = n_mod10.simd_eq(zero()) & n.simd_ne(zero());
    let (_n_div11, n_mod11) = div_mod_11(n);
    let even_fail = has_even_digits_mask(n) & n_mod11.simd_ne(zero());

    !trailing_zero & !even_fail
}

//
// Smallest / largest searches with pruning
//

#[inline]
pub fn smallest_product(min: u32, max: u32) -> Option<u32> {
    let mut best: u32 = u32::MAX;
    let start = min.max(1);

    let ten = Simd::splat(10);
    let mut x = start;
    let mut scratch: ArrayVec<u32, SCRATCH_CAPACITY> = ArrayVec::new();
    while x <= max {
        if x * x >= best {
            break;
        }

        let x_nz = unsafe { NonZeroU32::new_unchecked(x) };
        let y_upper = ((best - 1) / x_nz).min(max);

        if y_upper <= x {
            if x == max {
                break;
            }
            x += 1;
            continue;
        }

        let mut row_best: Option<u32> = None;
        let lane_span = SIMD_WIDTH as u32;
        let chunk_ceiling = y_upper.saturating_sub(lane_span - 1);

        if chunk_ceiling >= x {
            let x_vec = Simd::splat(x);
            let mut y_base = x;
            let mut y_vec = Simd::splat(y_base) + SIMD_OFFSETS;
            let lane_step = Simd::splat(lane_span);
            let prod_step = x_vec * lane_step;
            let mut prod_vec = x_vec * y_vec;

            loop {
                let lt10 = prod_vec.simd_lt(ten);
                if lt10.any() {
                    let lane = lt10.to_bitmask().trailing_zeros() as usize;
                    row_best = Some(prod_vec[lane]);
                    break;
                }

                let mask = simd_early_bailout(prod_vec);
                let bits = mask.to_bitmask();
                if bits != 0 {
                    for lane in 0..SIMD_WIDTH {
                        if (bits & (1 << lane)) != 0 {
                            scratch.push(prod_vec[lane]);
                        }
                    }
                }

                if scratch.len() >= SIMD_WIDTH
                    && let Some(found) = process_compact_full_lanes(&mut scratch)
                {
                    row_best = Some(found);
                    break;
                }

                let next_base = y_base.saturating_add(lane_span);
                if next_base > chunk_ceiling {
                    if let Some(found) = scan_smallest_tail(x, next_base, y_upper, &mut scratch) {
                        row_best = Some(found);
                    }
                    scratch.clear();
                    break;
                }

                y_base = next_base;
                y_vec += lane_step;
                prod_vec += prod_step;
            }
        } else if let Some(found) = scan_smallest_tail(x, x, y_upper, &mut scratch) {
            row_best = Some(found);
        }

        if let Some(prod) = row_best
            && prod < best
        {
            best = prod;
        }

        x += 1;
    }

    if best == u32::MAX { None } else { Some(best) }
}

#[inline]
pub fn smallest(min: u32, max: u32) -> Option<(u32, ArrayVec<u32, 4>)> {
    smallest_product(min, max).map(|product| (product, collect_factor_pairs(product, min, max)))
}

#[inline(always)]
fn process_palindrome_candidates<const LANES: usize>(
    prod_vec: SimdU32<LANES>,
    ten: SimdU32<LANES>,
    scratch: &mut ArrayVec<u32, SCRATCH_CAPACITY>,
) -> Option<u32>
where
    LaneCount<LANES>: SupportedLaneCount,
{
    let lt10 = prod_vec.simd_lt(ten);
    if lt10.any() {
        let lane = lt10.to_bitmask().trailing_zeros() as usize;
        return Some(prod_vec[lane]);
    }

    let mask = simd_early_bailout(prod_vec);
    let bits = mask.to_bitmask();
    if bits != 0 {
        for lane in 0..SIMD_WIDTH {
            if (bits & (1 << lane)) != 0 {
                scratch.push(prod_vec[lane]);
            }
        }
    }

    None
}

// Returns row best, if found, and where we left at on y_head.
#[inline(always)]
fn process_largest_palindrome_candidates<const LANES: usize>(
    x_vec: SimdU32<LANES>,
    mut y_vec: SimdU32<LANES>,
    mut y_head: u32,
    chunk_floor: u32,
    lane_step: SimdU32<LANES>,
    ten: SimdU32<LANES>,
    scratch: &mut ArrayVec<u32, SCRATCH_CAPACITY>,
) -> (Option<u32>, u32)
where
    LaneCount<LANES>: SupportedLaneCount,
{
    let prod_step = x_vec * lane_step;
    let mut prod_vec = x_vec * y_vec;

    loop {
        if let Some(best) = process_palindrome_candidates(prod_vec, ten, scratch) {
            return (Some(best), y_head);
        }

        if scratch.len() >= SIMD_WIDTH
            && let Some(found) = process_compact_full_lanes(scratch)
        {
            return (Some(found), y_head);
        }

        y_head = y_head.saturating_sub(LANES as u32);
        if y_head < chunk_floor {
            break;
        }

        y_vec -= lane_step;
        prod_vec -= prod_step;
    }

    (None, y_head)
}

#[inline]
pub fn largest_product(min: u32, max: u32) -> Option<u32> {
    let mut best: u32 = 0;
    let start = min.max(1);

    let ten = Simd::splat(10);
    let mut scratch: ArrayVec<u32, SCRATCH_CAPACITY> = ArrayVec::new();
    let mut x = max;
    while x >= start {
        if x * max <= best {
            break;
        }

        let x_nz = unsafe { NonZeroU32::new_unchecked(x) };
        let y_lower = ((best / x_nz) + 1).max(x);

        let mut row_best: Option<u32> = None;
        let lane_span = SIMD_WIDTH as u32;
        let full_chunk_floor = y_lower.saturating_add(lane_span - 1);
        let half_chunk_floor = y_lower.saturating_add((lane_span / 2) - 1);
        let quarter_chunk_floor = y_lower.saturating_add((lane_span / 4) - 1);
        let mut y_head = max;

        while y_head >= y_lower {
            if y_head >= full_chunk_floor {
                match process_largest_palindrome_candidates(
                    Simd::splat(x),
                    Simd::splat(y_head) - SIMD_OFFSETS,
                    y_head,
                    full_chunk_floor,
                    Simd::splat(lane_span),
                    ten,
                    &mut scratch,
                ) {
                    (Some(best), _) => {
                        row_best = Some(best);
                        break;
                    }
                    (_, next_head) => y_head = next_head,
                }
            } else if y_head >= half_chunk_floor {
                match process_largest_palindrome_candidates(
                    Simd::splat(x),
                    Simd::splat(y_head) - HALF_SIMD_OFFSETS,
                    y_head,
                    half_chunk_floor,
                    Simd::splat(4),
                    Simd::splat(10),
                    &mut scratch,
                ) {
                    (Some(best), _) => {
                        row_best = Some(best);
                        break;
                    }
                    (_, next_head) => y_head = next_head,
                }
            } else if y_head >= quarter_chunk_floor {
                match process_largest_palindrome_candidates(
                    Simd::splat(x),
                    Simd::splat(y_head) - QUARTER_SIMD_OFFSETS,
                    y_head,
                    quarter_chunk_floor,
                    Simd::splat(2),
                    Simd::splat(10),
                    &mut scratch,
                ) {
                    (Some(best), _) => {
                        row_best = Some(best);
                        break;
                    }
                    (_, next_head) => y_head = next_head,
                }
            } else {
                let prod = x * max;
                if let Some(best) = process_compact_until_done(&scratch) {
                    row_best = Some(best);
                } else if is_pal(prod) {
                    row_best = Some(prod);
                }

                scratch.clear();
                // Out of options at this point anyways, so break
                break;
            }
        }

        if let Some(prod) = row_best
            && prod > best
        {
            best = prod;
        }

        x -= 1;
    }

    if best > 0 { Some(best) } else { None }
}

#[inline]
pub fn largest(min: u32, max: u32) -> Option<(u32, ArrayVec<u32, 4>)> {
    largest_product(min, max).map(|product| (product, collect_factor_pairs(product, min, max)))
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

#[inline(always)]
pub fn is_pal_half_reverse(n: u32) -> bool {
    let mut m = n;
    let mut rev: u32 = 0;
    while m > rev {
        rev = rev * 10 + m % 10;
        m /= 10;
    }

    m == rev || m == rev / 10
}

#[inline(always)]
fn process_compact_until_done(values: &[u32]) -> Option<u32> {
    let mut offset = 0;
    let len = values.len();

    while offset < len {
        let remaining = len - offset;
        if remaining >= 8 {
            let vec = Simd::<u32, 8>::from_slice(&values[offset..offset + 8]);
            let mask = is_pal_simd_mask_generic(vec);
            if mask.any() {
                let lane = mask.to_bitmask().trailing_zeros() as usize;
                return Some(vec[lane]);
            }
            offset += 8;
        } else if remaining >= 4 {
            let vec = Simd::<u32, 4>::from_slice(&values[offset..offset + 4]);
            let mask = is_pal_simd_mask_generic(vec);
            if mask.any() {
                let lane = mask.to_bitmask().trailing_zeros() as usize;
                return Some(vec[lane]);
            }
            offset += 4;
        } else if remaining >= 2 {
            let vec = Simd::<u32, 2>::from_slice(&values[offset..offset + 2]);
            let mask = is_pal_simd_mask_generic(vec);
            if mask.any() {
                let lane = mask.to_bitmask().trailing_zeros() as usize;
                return Some(vec[lane]);
            }
            offset += 2;
        } else {
            let value = values[offset];
            if is_pal_half_reverse(value) {
                return Some(value);
            }
            offset += 1;
        }
    }

    None
}

#[inline]
fn scan_smallest_tail(
    x: u32,
    start: u32,
    y_upper: u32,
    scratch: &mut ArrayVec<u32, SCRATCH_CAPACITY>,
) -> Option<u32> {
    if start > y_upper {
        return process_compact_until_done(scratch.as_slice());
    }

    let mut current = start;
    while current <= y_upper {
        let prod = x * current;
        if prod < 10 {
            return Some(prod);
        }
        if prod.is_multiple_of(10) || (has_even_digits(prod) && !prod.is_multiple_of(11)) {
            if current == y_upper {
                break;
            }
            current += 1;
            continue;
        }

        scratch.push(prod);
        if current == y_upper {
            break;
        }
        current += 1;
    }

    process_compact_until_done(scratch.as_slice())
}

#[inline]
fn scan_largest_tail(
    x: u32,
    y_lower: u32,
    start: u32,
    scratch: &mut ArrayVec<u32, SCRATCH_CAPACITY>,
) -> Option<u32> {
    if start < y_lower {
        return process_compact_until_done(scratch.as_slice());
    }

    let mut current = start;
    while current >= y_lower {
        let prod = x * current;
        if prod < 10 {
            return Some(prod);
        }
        if prod.is_multiple_of(10) || (has_even_digits(prod) && !prod.is_multiple_of(11)) {
            if current == y_lower {
                break;
            }
            current -= 1;
            continue;
        }

        scratch.push(prod);
        if current == y_lower {
            break;
        }
        current -= 1;
    }

    let res = process_compact_until_done(scratch.as_slice());
    scratch.clear();
    res
}

#[inline(never)]
fn process_compact_full_lanes(values: &mut ArrayVec<u32, SCRATCH_CAPACITY>) -> Option<u32> {
    let len = values.len();
    if len == 0 {
        return None;
    }
    let mut offset = 0;

    while len - offset >= SIMD_WIDTH {
        let vec = Simd::<u32, SIMD_WIDTH>::from_slice(&values[offset..offset + SIMD_WIDTH]);
        let mask = is_pal_simd_mask_generic(vec);
        if mask.any() {
            let lane = mask.to_bitmask().trailing_zeros() as usize;
            values.drain(0..offset + SIMD_WIDTH);
            return Some(vec[lane]);
        }
        offset += SIMD_WIDTH;
    }

    values.drain(0..offset);
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::is_pal;

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
