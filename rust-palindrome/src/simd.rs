use std::num::NonZeroU32;
use std::ptr;

use arrayvec::ArrayVec;
use std::simd::cmp::{SimdPartialEq, SimdPartialOrd};
use std::simd::num::SimdUint;
use std::simd::{LaneCount, Mask, Simd, SupportedLaneCount};

use crate::{
    collect_factor_pairs_bounded_largest, collect_factor_pairs_bounded_smallest, divrem_u32_magic,
    is_pal,
};

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
    let t0 = cmp_a ^ cmp_b;
    let t1 = cmp_c ^ cmp_d;
    parity ^= t0 ^ t1;
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
pub fn smallest_product(min: u32, max: u32) -> Option<(u32, u32, u32)> {
    let mut best: u32 = u32::MAX;
    let mut best_x: u32 = 0;
    let start = min.max(1);

    let ten = Simd::splat(10);
    let mut x = start;
    let mut scratch: ArrayVec<u32, SCRATCH_CAPACITY> = ArrayVec::new();
    while x <= max {
        if x * x >= best {
            break;
        }

        let x_nz = unsafe { NonZeroU32::new_unchecked(x) };
        let (q, _) = divrem_u32_magic(best - 1, x_nz.get());
        let y_upper = q.min(max);

        if y_upper < x {
            x += 1;
            continue;
        }

        let lane_span = SIMD_WIDTH as u32;
        let full_chunk_ceiling = y_upper - (lane_span - 1);
        let half_chunk_ceiling = y_upper - ((lane_span / 2) - 1);
        let quarter_chunk_ceiling = y_upper - ((lane_span / 4) - 1);
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
                        best = row_best;
                        best_x = x;
                        break;
                    }
                    (_, next_base) => y_base = next_base,
                }
            } else if y_base <= half_chunk_ceiling {
                let prod_vec = Simd::splat(x) * (Simd::splat(y_base) + HALF_SIMD_OFFSETS);
                if let Some(row_best) =
                    process_palindrome_candidates(prod_vec, Simd::splat(10), &mut scratch)
                {
                    best = row_best;
                    best_x = x;
                    break;
                }
                y_base += 4;
            } else if y_base <= quarter_chunk_ceiling {
                let prod_vec = Simd::splat(x) * (Simd::splat(y_base) + QUARTER_SIMD_OFFSETS);
                if let Some(row_best) =
                    process_palindrome_candidates(prod_vec, Simd::splat(10), &mut scratch)
                {
                    best = row_best;
                    best_x = x;
                    break;
                }
                y_base += 2;
            } else {
                let prod = x * y_upper;
                if let Some(row_best) = process_compact_until_done_ptr(&scratch) {
                    best = row_best;
                    best_x = x;
                } else if is_pal(prod) {
                    best = prod;
                    best_x = x;
                }

                // Out of options at this point anyways, so break
                break;
            }
        }

        // Fastest scratch clear possible. Safe because u32 impls Copy.
        unsafe {
            scratch.set_len(0);
        }
        x += 1;
    }

    if best == u32::MAX {
        None
    } else {
        let (y, _) = divrem_u32_magic(best, best_x);
        Some((best, best_x, y))
    }
}

#[inline]
pub fn smallest(min: u32, max: u32) -> Option<(u32, ArrayVec<u32, 4>)> {
    smallest_product(min, max).map(|(product, x, y)| {
        let mut factor_pairs = ArrayVec::new_const();
        factor_pairs.push(x);
        factor_pairs.push(y);
        let start_above = unsafe { NonZeroU32::new_unchecked(x.min(y) + 1) };
        collect_factor_pairs_bounded_smallest(
            unsafe { NonZeroU32::new_unchecked(product) },
            unsafe { NonZeroU32::new_unchecked(min) },
            unsafe { NonZeroU32::new_unchecked(max) },
            start_above,
            &mut factor_pairs,
        );
        (product, factor_pairs)
    })
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

    loop {
        if let Some(best) = process_palindrome_candidates(prod_vec, ten, scratch) {
            // next-head ignored by caller on hit, so we don't need to add an
            // extra instruction to compute correct next head.
            return (Some(best), y_head);
        }

        y_head -= lanes;
        if y_head < chunk_floor {
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

    loop {
        if let Some(best) = process_palindrome_candidates(prod_vec, ten, scratch) {
            return (Some(best), y_base);
        }

        y_base += lanes;

        if y_base > chunk_ceiling {
            break;
        }

        prod_vec += prod_step;
    }

    (None, y_base)
}

#[inline]
pub fn largest_product(min: u32, max: u32) -> Option<(u32, u32, u32)> {
    let mut best: u32 = 0;
    let mut best_x: u32 = 0;
    let start = min.max(1);

    let ten = Simd::splat(10);
    let mut scratch: ArrayVec<u32, SCRATCH_CAPACITY> = ArrayVec::new();
    let mut x = max;
    while x >= start {
        if x * max <= best {
            break;
        }

        let x_nz = unsafe { NonZeroU32::new_unchecked(x) };
        let (q, _) = divrem_u32_magic(best, x_nz.get());
        let y_lower = (q + 1).max(x);

        if y_lower > max {
            x += 1;
            continue;
        }

        let lane_span = SIMD_WIDTH as u32;
        let full_chunk_floor = y_lower + (lane_span - 1);
        let half_chunk_floor = y_lower + ((lane_span / 2) - 1);
        let quarter_chunk_floor = y_lower + ((lane_span / 4) - 1);
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
                        best = row_best;
                        best_x = x;
                        break;
                    }
                    (_, next_head) => y_head = next_head,
                }
            } else if y_head >= half_chunk_floor {
                let prod_vec = Simd::splat(x) * (Simd::splat(y_head) - HALF_SIMD_OFFSETS);
                if let Some(row_best) =
                    process_palindrome_candidates(prod_vec, Simd::splat(10), &mut scratch)
                {
                    best = row_best;
                    best_x = x;
                    break;
                }
                y_head -= 4;
            } else if y_head >= quarter_chunk_floor {
                let prod_vec = Simd::splat(x) * (Simd::splat(y_head) - QUARTER_SIMD_OFFSETS);
                if let Some(row_best) =
                    process_palindrome_candidates(prod_vec, Simd::splat(10), &mut scratch)
                {
                    best = row_best;
                    best_x = x;
                    break;
                }
                y_head -= 2;
            } else {
                let prod = x * y_lower;
                if let Some(row_best) = process_compact_until_done_ptr(&scratch) {
                    best = row_best;
                    best_x = x;
                } else if is_pal(prod) {
                    best = prod;
                    best_x = x;
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

    if best > 0 {
        let (y, _) = divrem_u32_magic(best, best_x);
        Some((best, best_x, y))
    } else {
        None
    }
}

#[inline(never)]
pub fn largest(min: u32, max: u32) -> Option<(u32, ArrayVec<u32, 4>)> {
    largest_product(min, max).map(|(product, x, y)| {
        let search_max = NonZeroU32::new(x.min(y).saturating_sub(1));
        let mut factor_pairs = ArrayVec::new_const();
        factor_pairs.push(x);
        factor_pairs.push(y);
        if let Some(search_max) = search_max {
            collect_factor_pairs_bounded_largest(
                unsafe { NonZeroU32::new_unchecked(product) },
                unsafe { NonZeroU32::new_unchecked(min) },
                unsafe { NonZeroU32::new_unchecked(max) },
                search_max,
                &mut factor_pairs,
            );
        }
        (product, factor_pairs)
    })
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
    fn empty_result_for_largest_if_no_palindrome_in_the_range() {
        let (min_factor, max_factor) = (15, 15);
        assert!(largest(min_factor, max_factor).is_none());
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
        let y_base = x;

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
