// Cargo.toml
// [dependencies]
// arrayvec = "0.7"

use arrayvec::ArrayVec;
use core::mem::MaybeUninit;
use core::ptr;

pub type PairList = ArrayVec<u32, 4>;
pub type Answer = Option<(u32, PairList)>;

const N: usize = 1000; // we map 1..=999 -> 0..=998

// ---------- Monotone memo tables (ArrayVec-style) ----------

struct PrefixMemo<T, const CAP: usize> {
    buf: [MaybeUninit<T>; CAP],
    init: [bool; CAP],
}
impl<T, const CAP: usize> PrefixMemo<T, CAP> {
    fn new() -> Self {
        Self {
            buf: unsafe { MaybeUninit::<[MaybeUninit<T>; CAP]>::uninit().assume_init() },
            init: [false; CAP],
        }
    }

    fn clear(&mut self) {
        for (slot, flag) in self.buf.iter_mut().zip(self.init.iter_mut()) {
            if *flag {
                unsafe { ptr::drop_in_place(slot.as_mut_ptr()) };
                *flag = false;
            }
        }
    }

    #[inline]
    fn get_or_init_with(&mut self, idx: usize, mut make: impl FnMut(usize) -> T) -> &T {
        debug_assert!(idx < CAP);
        if !self.init[idx] {
            self.buf[idx].write(make(idx));
            self.init[idx] = true;
        }
        unsafe { &*self.buf[idx].as_ptr() }
    }
}

impl<T, const CAP: usize> Drop for PrefixMemo<T, CAP> {
    fn drop(&mut self) {
        self.clear();
    }
}

struct SuffixMemo<T, const CAP: usize> {
    buf: [MaybeUninit<T>; CAP],
    init: [bool; CAP],
}
impl<T, const CAP: usize> SuffixMemo<T, CAP> {
    fn new() -> Self {
        Self {
            buf: unsafe { MaybeUninit::<[MaybeUninit<T>; CAP]>::uninit().assume_init() },
            init: [false; CAP],
        }
    }

    fn clear(&mut self) {
        for (slot, flag) in self.buf.iter_mut().zip(self.init.iter_mut()) {
            if *flag {
                unsafe { ptr::drop_in_place(slot.as_mut_ptr()) };
                *flag = false;
            }
        }
    }

    #[inline]
    fn get_or_init_with(&mut self, idx: usize, mut make: impl FnMut(usize) -> T) -> &T {
        debug_assert!(idx < CAP);
        if !self.init[idx] {
            self.buf[idx].write(make(idx));
            self.init[idx] = true;
        }
        unsafe { &*self.buf[idx].as_ptr() }
    }
}

impl<T, const CAP: usize> Drop for SuffixMemo<T, CAP> {
    fn drop(&mut self) {
        self.clear();
    }
}

// ---------- Independent memoizers ----------

pub struct SmallestMemo<F>
where
    F: Fn(u32, u32) -> Answer + Copy,
{
    table: PrefixMemo<Answer, N>, // key by min; min increases
    compute: F,                   // your smallest(min, max) implementation
}

impl<F> SmallestMemo<F>
where
    F: Fn(u32, u32) -> Answer + Copy,
{
    pub fn new(compute: F) -> Self {
        Self {
            table: PrefixMemo::new(),
            compute,
        }
    }

    /// Hot path: smallest(min, 999) with min increasing (direct-mapped).
    /// If max != 999, we just compute directly.
    #[inline]
    pub fn smallest(&mut self, min: u32, max: u32) -> Answer {
        debug_assert!((1..=999).contains(&min) && (1..=999).contains(&max) && min <= max);
        let idx = (min as usize) - 1;
        self.table
            .get_or_init_with(idx, |i| (self.compute)((i + 1) as u32, max))
            .clone()
    }
}

pub struct LargestMemo<G>
where
    G: Fn(u32, u32) -> Answer + Copy,
{
    table: SuffixMemo<Answer, N>, // key by max; max decreases
    compute: G,                   // your largest(min, max) implementation
}

impl<G> LargestMemo<G>
where
    G: Fn(u32, u32) -> Answer + Copy,
{
    pub fn new(compute: G) -> Self {
        Self {
            table: SuffixMemo::new(),
            compute,
        }
    }

    /// Hot path: largest(1, max) with max decreasing (direct-mapped).
    /// If min != 1, compute directly.
    #[inline]
    pub fn largest(&mut self, min: u32, max: u32) -> Answer {
        debug_assert!((1..=999).contains(&min) && (1..=999).contains(&max) && min <= max);
        let idx = (max as usize) - 1;
        self.table
            .get_or_init_with(idx, |i| (self.compute)(min, (i + 1) as u32))
            .clone()
    }
}
