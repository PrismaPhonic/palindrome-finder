use std::num::NonZeroU32;
use std::ptr;

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
    let (_, n_mod10) = div_mod_10(n);
    let trailing_zero = n_mod10.simd_eq(zero());
    let (_, n_mod11) = div_mod_11(n);
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

        if y_upper < x {
            x += 1;
            continue;
        }

        let lane_span = SIMD_WIDTH as u32;
        let full_chunk_ceiling = y_upper.saturating_sub(lane_span - 1);
        let half_chunk_ceiling = y_upper.saturating_sub((lane_span / 2) - 1);
        let quarter_chunk_ceiling = y_upper.saturating_sub((lane_span / 4) - 1);
        let mut y_base = x;

        while y_base <= y_upper {
            if y_base <= full_chunk_ceiling {
                match process_smallest_palindrome_candidates(
                    Simd::splat(x),
                    Simd::splat(y_base) + SIMD_OFFSETS,
                    y_base,
                    full_chunk_ceiling,
                    ten,
                    &mut scratch,
                ) {
                    (Some(row_best), _) => {
                        if row_best < best {
                            best = row_best;
                        }
                        break;
                    }
                    (_, next_base) => y_base = next_base,
                }
            } else if y_base <= half_chunk_ceiling {
                let prod_vec = Simd::splat(x) * (Simd::splat(y_base) + HALF_SIMD_OFFSETS);
                if let Some(row_best) =
                    process_palindrome_candidates(prod_vec, Simd::splat(10), &mut scratch)
                {
                    if row_best < best {
                        best = row_best;
                    }
                    break;
                }
                y_base += 4;
            } else if y_base <= quarter_chunk_ceiling {
                let prod_vec = Simd::splat(x) * (Simd::splat(y_base) + QUARTER_SIMD_OFFSETS);
                if let Some(row_best) =
                    process_palindrome_candidates(prod_vec, Simd::splat(10), &mut scratch)
                {
                    if row_best < best {
                        best = row_best;
                    }
                    break;
                }
                y_base += 2;
            } else {
                let prod = x * y_upper;
                if let Some(row_best) = process_compact_until_done_ptr(&scratch)
                    && row_best < best
                {
                    best = row_best;
                } else if is_pal(prod) && prod < best {
                    best = prod;
                }

                // Out of options at this point anyways, so break
                break;
            }
        }

        unsafe {
            scratch.set_len(0);
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
        // While it seems that we probably should process the scratch first
        // before returning. we can logically work through all single digit
        // cases for both smallest and largest.
        //
        // For smallest if we have single digit palindromes, they would all be
        // processed first anyways meaning we wouldn't have scratch to process.
        // In the case of largest, that's true too. Imagine we have a tight
        // upper bound of 10, our first valid candidate is 9 (the next is 121,
        // from 11 x 11) so all valid candidates start at single digits And if
        // we were starting at 11, we would have bailed already even with large
        // lanes.
        let lane = lt10.to_bitmask().trailing_zeros() as usize;
        return Some(prod_vec[lane]);
    }

    let mask = simd_early_bailout(prod_vec);
    let bits = mask.to_bitmask();
    pack_scratch_from_bitmask(bits, prod_vec, scratch);

    process_compact_full_lane_ptr(scratch)
}

// Very efficient packing of scratch based on bitmask, which removes bounds
// checking, which in turn allows llvm to better optimize this with automatic
// loop unrolling.
#[inline(always)]
fn pack_scratch_from_bitmask<const LANES: usize>(
    bits: u64,
    prod: SimdU32<LANES>,
    scratch: &mut ArrayVec<u32, SCRATCH_CAPACITY>,
) where
    LaneCount<LANES>: SupportedLaneCount,
{
    let mut m = bits;
    let mut len = scratch.len();
    let need = m.count_ones() as usize;

    // Improves performance by removing bounds checking in the loop.
    assert!(len + need <= scratch.capacity());

    let ptr = scratch.as_mut_ptr();

    while m != 0 {
        let lane = m.trailing_zeros() as usize;
        m &= m - 1;
        unsafe {
            *ptr.add(len) = prod[lane];
            len += 1;
        }
    }

    // single header write at exit
    unsafe {
        scratch.set_len(len);
    }
}

// Returns row best, if found, and where we left at on y_head.
#[inline(always)]
fn process_largest_palindrome_candidates<const LANES: usize>(
    x_vec: SimdU32<LANES>,
    y_vec: SimdU32<LANES>,
    mut y_head: u32,
    chunk_floor: u32,
    ten: SimdU32<LANES>,
    scratch: &mut ArrayVec<u32, SCRATCH_CAPACITY>,
) -> (Option<u32>, u32)
where
    LaneCount<LANES>: SupportedLaneCount,
{
    let lanes = LANES as u32;
    let lane_step = Simd::splat(lanes);
    let prod_step = x_vec * lane_step;
    let mut prod_vec = x_vec * y_vec;
    // We use a cutoff so we can save on the cost of saturating_sub use.
    let cutoff = chunk_floor + lanes;

    loop {
        if let Some(best) = process_palindrome_candidates(prod_vec, ten, scratch) {
            // next-head ignored by caller on hit, so we don't need to add an
            // extra instruction to compute correct next head.
            return (Some(best), y_head);
        }

        y_head -= lanes;
        if y_head < cutoff {
            break;
        }

        prod_vec -= prod_step;
    }

    (None, y_head)
}

// Returns row best, if found, and where we left at on y_base.
#[inline(always)]
fn process_smallest_palindrome_candidates<const LANES: usize>(
    x_vec: SimdU32<LANES>,
    y_vec: SimdU32<LANES>,
    mut y_base: u32,
    chunk_ceiling: u32,
    ten: SimdU32<LANES>,
    scratch: &mut ArrayVec<u32, SCRATCH_CAPACITY>,
) -> (Option<u32>, u32)
where
    LaneCount<LANES>: SupportedLaneCount,
{
    let lanes = LANES as u32;
    let lane_step = Simd::splat(lanes);
    let prod_step = x_vec * lane_step;
    let mut prod_vec = x_vec * y_vec;
    let cutoff = chunk_ceiling.saturating_sub(lanes);

    loop {
        if let Some(best) = process_palindrome_candidates(prod_vec, ten, scratch) {
            return (Some(best), y_base);
        }

        y_base += lanes;

        if y_base > cutoff {
            break;
        }

        prod_vec += prod_step;
    }

    (None, y_base)
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

        if y_lower > max {
            x += 1;
            continue;
        }

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
                    ten,
                    &mut scratch,
                ) {
                    (Some(row_best), _) => {
                        if row_best > best {
                            best = row_best;
                        }
                        break;
                    }
                    (_, next_head) => y_head = next_head,
                }
            } else if y_head >= half_chunk_floor {
                let prod_vec = Simd::splat(x) * (Simd::splat(y_head) - HALF_SIMD_OFFSETS);
                if let Some(row_best) =
                    process_palindrome_candidates(prod_vec, Simd::splat(10), &mut scratch)
                {
                    if row_best > best {
                        best = row_best;
                    }
                    break;
                }
                y_head = y_head.saturating_sub(4);
            } else if y_head >= quarter_chunk_floor {
                let prod_vec = Simd::splat(x) * (Simd::splat(y_head) - QUARTER_SIMD_OFFSETS);
                if let Some(row_best) =
                    process_palindrome_candidates(prod_vec, Simd::splat(10), &mut scratch)
                {
                    if row_best > best {
                        best = row_best;
                    }
                    break;
                }
                y_head = y_head.saturating_sub(4);
            } else {
                let prod = x * max;
                if let Some(row_best) = process_compact_until_done_ptr(&scratch)
                    && row_best > best
                {
                    best = row_best;
                } else if is_pal(prod) && prod > best {
                    best = prod;
                }

                // Out of options at this point anyways, so break
                break;
            }
        }

        unsafe {
            scratch.set_len(0);
        }

        x -= 1;
    }

    if best > 0 { Some(best) } else { None }
}

#[inline]
pub fn largest(min: u32, max: u32) -> Option<(u32, ArrayVec<u32, 4>)> {
    largest_product(min, max).map(|product| (product, collect_factor_pairs(product, min, max)))
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

#[inline(always)]
pub fn process_compact_until_done_ptr(values: &[u32]) -> Option<u32> {
    let mut off = 0;
    let len = values.len();

    if len - off >= 8 {
        unsafe {
            let v = Simd::<u32, 8>::from_array(ptr::read_unaligned(
                values.as_ptr().add(off) as *const [u32; 8]
            ));
            let mask = is_pal_simd_mask_generic(v);
            if mask.any() {
                let lane = mask.to_bitmask().trailing_zeros() as usize;
                return Some(ptr::read_unaligned(values.as_ptr().add(off + lane)));
            }
            off += 8;
        }
    }
    if len - off >= 4 {
        unsafe {
            let v = Simd::<u32, 4>::from_array(ptr::read_unaligned(
                values.as_ptr().add(off) as *const [u32; 4]
            ));
            let mask = is_pal_simd_mask_generic(v);
            if mask.any() {
                let lane = mask.to_bitmask().trailing_zeros() as usize;
                return Some(ptr::read_unaligned(values.as_ptr().add(off + lane)));
            }
            off += 4;
        }
    }
    if len - off >= 2 {
        unsafe {
            let v = Simd::<u32, 2>::from_array(ptr::read_unaligned(
                values.as_ptr().add(off) as *const [u32; 2]
            ));
            let mask = is_pal_simd_mask_generic(v);
            if mask.any() {
                let lane = mask.to_bitmask().trailing_zeros() as usize;
                return Some(ptr::read_unaligned(values.as_ptr().add(off + lane)));
            }
            off += 2;
        }
    }
    if off < len {
        let x = unsafe { ptr::read_unaligned(values.as_ptr().add(off)) };
        if is_pal_half_reverse(x) {
            return Some(x);
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

/// Processes one full simd lane, removing it from the supplied scratch.
/// Returns the first found palindrome.
/// We don't care about later palindromes because this is always processing for one "row"
/// Within the context of each row the first palindrome found will always be the
/// highest, so we can avoid processing the rest.
#[inline(always)]
fn process_compact_full_lane(values: &mut ArrayVec<u32, SCRATCH_CAPACITY>) -> Option<u32> {
    if values.len() < SIMD_WIDTH {
        return None; // caller can invoke unconditionally
    }
    let vec = Simd::<u32, SIMD_WIDTH>::from_slice(&values[0..SIMD_WIDTH]);
    let mask = is_pal_simd_mask_generic(vec);
    if mask.any() {
        let lane = mask.to_bitmask().trailing_zeros() as usize;
        values.drain(0..SIMD_WIDTH);
        return Some(vec[lane]);
    }

    values.drain(0..SIMD_WIDTH);
    None
}

#[inline(always)]
fn process_compact_full_lane_ptr(values: &mut ArrayVec<u32, SCRATCH_CAPACITY>) -> Option<u32> {
    let len = values.len();
    if len < SIMD_WIDTH {
        return None; // caller can invoke unconditionally
    }

    // Load first full lane without creating a [..8] slice.
    let v = unsafe {
        let p = values.as_ptr() as *const [u32; SIMD_WIDTH];
        Simd::<u32, SIMD_WIDTH>::from_array(ptr::read_unaligned(p))
    };

    let mask = is_pal_simd_mask_generic(v);

    // Grab the winning scalar from the backing buffer (avoid vec[lane] bounds check).
    let found = if mask.any() {
        let lane = mask.to_bitmask().trailing_zeros() as usize;
        Some(unsafe { *values.as_ptr().add(lane) })
    } else {
        None
    };

    // Consume the lane with a single move.
    unsafe {
        let p = values.as_mut_ptr();
        ptr::copy(p.add(SIMD_WIDTH), p, len - SIMD_WIDTH);
        values.set_len(len - SIMD_WIDTH);
    }

    found
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

    #[test]
    fn debug_panic_case_29_999_simd() {
        // Focus on just the x=33 case that's causing the issue
        let x = 33u32;
        let y_upper = 77u32;
        let best = 2552u32; // from previous iteration

        println!("Testing x={}, y_upper={}, best={}", x, y_upper, best);

        let ten = Simd::splat(10);
        let mut scratch: ArrayVec<u32, SCRATCH_CAPACITY> = ArrayVec::new();

        let lane_span = SIMD_WIDTH as u32;
        let full_chunk_ceiling = y_upper.saturating_sub(lane_span - 1);
        let mut y_base = x;

        println!("  full_chunk_ceiling={}", full_chunk_ceiling);

        if y_base <= full_chunk_ceiling {
            println!("  Processing full chunk at y_base={}", y_base);
            match process_smallest_palindrome_candidates(
                Simd::splat(x),
                Simd::splat(y_base) + SIMD_OFFSETS,
                y_base,
                full_chunk_ceiling,
                ten,
                &mut scratch,
            ) {
                (Some(row_best), _) => {
                    println!("  Found palindrome {} in full chunk", row_best);
                }
                (_, next_base) => {
                    println!("  No palindrome found, next_base={}", next_base);
                }
            }
        }
    }
}
