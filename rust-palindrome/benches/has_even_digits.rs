#![allow(clippy::identity_op)]
#![feature(portable_simd)]

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use palprod_rust::{
    functional::{
        is_pal_functional, largest_functional, largest_product_functional, smallest_functional,
        smallest_product_functional,
    },
    has_even_digits, is_pal, largest, largest_product,
    simd::{
        is_pal_simd_mask, largest as largest_simd, largest_product as largest_product_simd,
        smallest as smallest_simd, smallest_product as smallest_product_simd,
    },
    smallest, smallest_product,
};
use std::num::NonZeroU32;
use std::simd::Simd;
use std::{env, time::Duration};

const SIMD_BATCH: usize = 8;

macro_rules! pal_step {
    ($n:ident, $rev:ident) => {{
        let digit = $n % 10;
        $rev = $rev * 10 + digit;
        $n /= 10;
    }};
}

macro_rules! pal_steps {
    ($n:ident, $rev:ident, 0) => {};
    ($n:ident, $rev:ident, 1) => {
        pal_step!($n, $rev);
    };
    ($n:ident, $rev:ident, 2) => {
        pal_step!($n, $rev);
        pal_steps!($n, $rev, 1);
    };
    ($n:ident, $rev:ident, 3) => {
        pal_step!($n, $rev);
        pal_steps!($n, $rev, 2);
    };
    ($n:ident, $rev:ident, 4) => {
        pal_step!($n, $rev);
        pal_steps!($n, $rev, 3);
    };
    ($n:ident, $rev:ident, 5) => {
        pal_step!($n, $rev);
        pal_steps!($n, $rev, 4);
    };
}

macro_rules! define_pal_helper_even {
    ($name:ident, $steps:tt) => {
        #[inline(always)]
        fn $name(mut n: u32) -> bool {
            let mut rev: u32 = 0;
            pal_steps!(n, rev, $steps);
            n == rev
        }
    };
}

macro_rules! define_pal_helper_odd {
    ($name:ident, $steps:tt) => {
        #[inline(always)]
        fn $name(mut n: u32) -> bool {
            let mut rev: u32 = 0;
            pal_steps!(n, rev, $steps);
            n /= 10;
            n == rev
        }
    };
}

define_pal_helper_even!(pal_helper_2_digit, 1);
define_pal_helper_odd!(pal_helper_3_digit, 1);
define_pal_helper_even!(pal_helper_4_digit, 2);
define_pal_helper_odd!(pal_helper_5_digit, 2);
define_pal_helper_even!(pal_helper_6_digit, 3);

#[inline(always)]
fn digits_length(n: u32) -> u32 {
    debug_assert!(n >= 10);
    let cmp_a = (n >= 100) as u32;
    let cmp_b = (n >= 1_000) as u32;
    let cmp_c = (n >= 10_000) as u32;
    let cmp_d = (n >= 100_000) as u32;
    let t0 = cmp_a + cmp_b;
    let t1 = cmp_c + cmp_d;
    2 + t0 + t1
}

#[inline(always)]
fn is_pal_digit_dispatch(n: u32) -> bool {
    if n < 10 {
        return true;
    }
    if n.is_multiple_of(10) {
        return false;
    }

    let digits = digits_length(n);
    if (digits & 1) == 0 && !n.is_multiple_of(11) {
        return false;
    }

    match digits {
        2 => pal_helper_2_digit(n),
        3 => pal_helper_3_digit(n),
        4 => pal_helper_4_digit(n),
        5 => pal_helper_5_digit(n),
        6 => pal_helper_6_digit(n),
        _ => false,
    }
}

#[inline(always)]
fn is_pal_loop_unrolled(n: u32) -> bool {
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

    pal_half_reverse_unrolled(n)
}

#[inline(always)]
pub fn pal_half_reverse_unrolled(n: u32) -> bool {
    let mut m = n / 10;
    let mut rev = n % 10;
    if m <= rev {
        return m == rev || m == rev / 10;
    }

    rev = rev * 10 + (m % 10);
    m /= 10;
    if m <= rev {
        return m == rev || m == rev / 10;
    }

    rev = rev * 10 + (m % 10);
    m /= 10;

    m == rev || m == rev / 10
}

#[inline(always)]
fn has_even_digits_branch(n: u32) -> bool {
    debug_assert!(n >= 10);
    if n < 100 {
        true
    } else if n < 1_000 {
        false
    } else if n < 10_000 {
        true
    } else if n < 100_000 {
        false
    } else {
        true
    }
}

#[inline(always)]
fn has_even_digits_ilog10(n: u32) -> bool {
    debug_assert!(n >= 10);
    n.ilog10() & 1 == 1
}

#[inline(always)]
fn has_even_digits_parity(n: u32) -> bool {
    debug_assert!(n >= 10);
    let mut parity: u32 = 1;
    let cmp_a = (n >= 100) as u32;
    let cmp_b = (n >= 1_000) as u32;
    let cmp_c = (n >= 10_000) as u32;
    let cmp_d = (n >= 100_000) as u32;
    parity ^= cmp_a;
    parity ^= cmp_b;
    parity ^= cmp_c;
    parity ^= cmp_d;
    parity == 1
}

#[inline(always)]
pub fn has_even_digits_parity_new(n: u32) -> bool {
    // each cmp_* can execute in parallel; we combine once
    let cmp_a = (n >= 100) as u32;
    let cmp_b = (n >= 1_000) as u32;
    let cmp_c = (n >= 10_000) as u32;
    let cmp_d = (n >= 100_000) as u32;
    let t0 = cmp_a ^ cmp_b;
    let t1 = cmp_c ^ cmp_d;
    let parity = 1 ^ (t0 ^ t1);
    parity == 1
}

fn input_numbers() -> Vec<u32> {
    // Cover 1-digit through 9-digit numbers (inclusive) with uniform density.
    // 999_999 numbers keep the benchmark realistic without being too slow.
    (1..=999_999).collect()
}

fn palindrome_inputs() -> Vec<u32> {
    // Sample palindromes and non-palindromes across the same range.
    // This covers our entire input from range of 2..999
    (4..=998_001).collect()
}

fn nz(n: u32) -> NonZeroU32 {
    NonZeroU32::new(n).unwrap()
}

fn factor_pair_inputs() -> Vec<(NonZeroU32, NonZeroU32, NonZeroU32)> {
    let ranges = [
        (1, 9),
        (10, 99),
        (100, 199),
        (200, 299),
        (300, 399),
        (400, 499),
        (500, 599),
        (600, 699),
        (700, 799),
        (800, 899),
        (900, 999),
    ];

    let mut samples = Vec::new();
    for &(min, max) in &ranges {
        if let Some((prod, _)) = largest(min, max) {
            samples.push((prod, min, max));
        }
        if let Some((prod, _)) = smallest(min, max) {
            samples.push((prod, min, max));
        }
    }

    if samples.is_empty() {
        return Vec::new();
    }

    let base = samples.clone();
    while samples.len() < 4_096 {
        samples.extend_from_slice(&base);
    }

    samples
        .into_iter()
        .map(|(a, b, c)| {
            (
                NonZeroU32::new(a).unwrap(),
                NonZeroU32::new(b).unwrap(),
                NonZeroU32::new(c).unwrap(),
            )
        })
        .collect()
}

fn bench_has_even_digits(c: &mut Criterion) {
    let inputs = input_numbers();

    if should_run("has_even_digits_branch") {
        c.bench_function("has_even_digits_branch", |b| {
            b.iter(|| {
                let mut acc = 0u32;
                for &n in &inputs {
                    acc += has_even_digits_branch(black_box(n)) as u32;
                }
                black_box(acc)
            });
        });
    }

    if should_run("has_even_digits_ilog10") {
        c.bench_function("has_even_digits_ilog10", |b| {
            b.iter(|| {
                let mut acc = 0u32;
                for &n in &inputs {
                    acc += has_even_digits_ilog10(black_box(n)) as u32;
                }
                black_box(acc)
            });
        });
    }

    if should_run("has_even_digits_parity") {
        c.bench_function("has_even_digits_parity", |b| {
            b.iter(|| {
                let mut acc = 0u32;
                for &n in &inputs {
                    acc += has_even_digits_parity(black_box(n)) as u32;
                }
                black_box(acc)
            });
        });
    }

    if should_run("has_even_digits_parity_new") {
        c.bench_function("has_even_digits_parity_new", |b| {
            b.iter(|| {
                let mut acc = 0u32;
                for &n in &inputs {
                    acc += has_even_digits_parity_new(black_box(n)) as u32;
                }
                black_box(acc)
            });
        });
    }
}

fn bench_palindrome_checks(c: &mut Criterion) {
    let inputs = palindrome_inputs();

    if should_run("is_pal_iterative") {
        c.bench_function("is_pal_iterative", |b| {
            b.iter(|| {
                let mut acc = 0u32;
                for &n in &inputs {
                    acc += is_pal(black_box(n)) as u32;
                }
                black_box(acc)
            });
        });
    }

    if should_run("is_pal_functional") {
        c.bench_function("is_pal_functional", |b| {
            b.iter(|| {
                let mut acc = 0u32;
                for &n in &inputs {
                    acc += is_pal_functional(black_box(n)) as u32;
                }
                black_box(acc)
            });
        });
    }

    if should_run("is_pal_simd") {
        c.bench_function("is_pal_simd", |b| {
            b.iter(|| {
                let mut acc = 0u32;
                let mut buf = [0u32; SIMD_BATCH];
                for chunk in inputs.chunks(SIMD_BATCH) {
                    buf.fill(0);
                    buf[..chunk.len()].copy_from_slice(chunk);
                    let vec = Simd::from_array(buf);
                    acc += is_pal_simd_mask(vec).to_bitmask().count_ones();
                }
                black_box(acc)
            });
        });
    }

    if should_run("is_pal_digit_dispatch") {
        c.bench_function("is_pal_digit_dispatch", |b| {
            b.iter(|| {
                let mut acc = 0u32;
                for &n in &inputs {
                    acc += is_pal_digit_dispatch(black_box(n)) as u32;
                }
                black_box(acc)
            });
        });
    }

    if should_run("is_pal_loop_unrolled") {
        c.bench_function("is_pal_digit_loop_unrolled", |b| {
            b.iter(|| {
                let mut acc = 0u32;
                for &n in &inputs {
                    acc += is_pal_loop_unrolled(black_box(n)) as u32;
                }
                black_box(acc)
            });
        });
    }
}

fn bench_largest_search_products(c: &mut Criterion) {
    let expected = NonZeroU32::new(906_609u32).unwrap();

    if should_run("largest_product_iterative") {
        c.bench_function("largest_product_iterative_100_999", |b| {
            b.iter(|| {
                let res = largest_product(black_box(nz(100)), black_box(nz(999)));
                debug_assert_eq!(res.unwrap().0, expected);
                black_box(res)
            });
        });
    }

    if should_run("largest_product_functional") {
        c.bench_function("largest_product_functional_100_999", |b| {
            b.iter(|| {
                let res = largest_product_functional(black_box(100), black_box(999));
                debug_assert_eq!(res.unwrap(), expected.get());
                black_box(res)
            });
        });
    }

    if should_run("largest_product_simd") {
        c.bench_function("largest_product_simd_100_999", |b| {
            b.iter(|| {
                let res = largest_product_simd(black_box(100), black_box(999));
                debug_assert_eq!(res.unwrap().0, expected.get());
                black_box(res)
            });
        });
    }
}

fn bench_largest(c: &mut Criterion) {
    if should_run("largest_iterative") {
        c.bench_function("largest_iterative_100_999", |b| {
            b.iter(|| {
                let res = largest(black_box(100), black_box(999));
                black_box(res)
            });
        });
    }

    if should_run("largest_functional") {
        c.bench_function("largest_functional_100_999", |b| {
            b.iter(|| {
                let res = largest_functional(black_box(100), black_box(999));
                black_box(res)
            });
        });
    }

    if should_run("largest_simd") {
        c.bench_function("largest_simd_100_999", |b| {
            b.iter(|| {
                let res = largest_simd(black_box(100), black_box(999));
                black_box(res)
            });
        });
    }
}

fn bench_smallest(c: &mut Criterion) {
    if should_run("smallest_iterative") {
        c.bench_function("smallest_iterative_910_999", |b| {
            b.iter(|| {
                let res = smallest(black_box(910), black_box(999));
                black_box(res)
            });
        });
    }

    if should_run("smallest_functional") {
        c.bench_function("smallest_functional_910_999", |b| {
            b.iter(|| {
                let res = smallest_functional(black_box(910), black_box(999));
                black_box(res)
            });
        });
    }

    if should_run("smallest_simd") {
        c.bench_function("smallest_simd_910_999", |b| {
            b.iter(|| {
                let res = smallest_simd(black_box(910), black_box(999));
                black_box(res)
            });
        });
    }
}

fn bench_smallest_search_products(c: &mut Criterion) {
    let expected = Some(121u32);

    if should_run("smallest_product_iterative") {
        c.bench_function("smallest_product_iterative_910_999", |b| {
            b.iter(|| {
                let res = smallest_product(black_box(nz(910)), black_box(nz(999)));
                debug_assert_eq!(res.unwrap().0, nz(expected.unwrap()));
                black_box(res)
            });
        });
    }

    if should_run("smallest_product_functional") {
        c.bench_function("smallest_product_functional_910_999", |b| {
            b.iter(|| {
                let res = smallest_product_functional(black_box(910), black_box(999));
                debug_assert_eq!(res, expected);
                black_box(res)
            });
        });
    }

    if should_run("smallest_product_simd") {
        c.bench_function("smallest_product_simd_910_999", |b| {
            b.iter(|| {
                let res = smallest_product_simd(black_box(910), black_box(999));
                debug_assert_eq!(res.unwrap().0, expected.unwrap());
                black_box(res)
            });
        });
    }
}

fn bench_smallest_product(c: &mut Criterion) {
    let filter = std::env::args().skip(1).collect::<Vec<_>>();
    if !filter.is_empty() && !filter.iter().any(|arg| arg.contains("smallest_product")) {
        return;
    }

    let mut group = c.benchmark_group("smallest_product");
    group.bench_function("smallest_product_scalar", |b| {
        b.iter(|| smallest_product(black_box(nz(100u32)), black_box(nz(999u32))))
    });
    group.bench_function("smallest_product_simd", |b| {
        b.iter(|| smallest_product_simd(black_box(100u32), black_box(999u32)))
    });
    group.finish();
}

fn should_run(name: &str) -> bool {
    match env::var("PAL_BENCH_FILTER") {
        Ok(filter) if filter.is_empty() => true,
        Ok(filter) => name.contains(&filter),
        Err(_) => true,
    }
}

fn do_iters_iterative_stub(min: u32, max: u32, iters: u64) -> (Option<u32>, u64) {
    let mut acc: u64 = 0;
    let mut counter: u64 = 0;
    let mut current_max = max;

    for _ in 0..iters {
        acc = acc.wrapping_add(current_max as u64).wrapping_add(counter);
        counter = counter.wrapping_add(1);
        current_max = if current_max <= min {
            max
        } else {
            current_max - 1
        };
    }

    (Some(max), acc)
}

fn do_iters_functional_stub(min: u32, max: u32, iters: u64) -> (Option<u32>, u64) {
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
        .map(|current_max| Some((max, current_max as u64)))
        .fold((0u64, 0u64), |(acc, cnt), result| match result {
            Some((prod, pairs_sum)) => (acc + prod as u64 + pairs_sum + cnt, cnt + 1),
            None => (acc, cnt + 1),
        });

    (Some(max), acc.0)
}

fn bench_do_iters_overhead(c: &mut Criterion) {
    let min = 2u32;
    let max = 999u32;
    let iters = 5_000_000u64;

    if should_run("do_iters_iterative_stub") {
        c.bench_function("do_iters_iterative_stub", |b| {
            b.iter(|| {
                let res = do_iters_iterative_stub(black_box(min), black_box(max), black_box(iters));
                black_box(res)
            });
        });
    }

    if should_run("do_iters_functional_stub") {
        c.bench_function("do_iters_functional_stub", |b| {
            b.iter(|| {
                let res =
                    do_iters_functional_stub(black_box(min), black_box(max), black_box(iters));
                black_box(res)
            });
        });
    }
}

criterion_group! {
    name = benches;
    config = Criterion::default()
        .sample_size(1000)
        .warm_up_time(Duration::from_millis(500))
        .measurement_time(Duration::from_secs(30));
    targets = bench_has_even_digits, bench_palindrome_checks, bench_largest_search_products, bench_smallest_search_products, bench_smallest, bench_largest, bench_do_iters_overhead, bench_smallest_product
}

criterion_main!(benches);
