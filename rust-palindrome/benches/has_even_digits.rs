use criterion::{Criterion, black_box, criterion_group, criterion_main};
use palprod_rust::{
    collect_factor_pairs,
    functional::{
        collect_factor_pairs as collect_factor_pairs_functional, is_pal_functional,
        largest_product_functional, smallest_product_functional,
    },
    is_pal, largest, largest_product, smallest, smallest_product,
};
use std::{env, time::Duration};

#[inline]
fn has_even_digits_branch(n: u32) -> bool {
    debug_assert!(n > 0);
    if n < 10 {
        false
    } else if n < 100 {
        true
    } else if n < 1_000 {
        false
    } else if n < 10_000 {
        true
    } else if n < 100_000 {
        false
    } else if n < 1_000_000 {
        true
    } else if n < 10_000_000 {
        false
    } else if n < 100_000_000 {
        true
    } else if n < 1_000_000_000 {
        false
    } else {
        true
    }
}

#[inline]
fn has_even_digits_ilog10(n: u32) -> bool {
    debug_assert!(n > 0);
    n.ilog10() & 1 == 1
}

#[inline]
fn has_even_digits_parity(n: u32) -> bool {
    debug_assert!(n > 0);
    let mut parity: u32 = 0;
    parity ^= (n >= 10) as u32;
    parity ^= (n >= 100) as u32;
    parity ^= (n >= 1_000) as u32;
    parity ^= (n >= 10_000) as u32;
    parity ^= (n >= 100_000) as u32;
    parity ^= (n >= 1_000_000) as u32;
    parity ^= (n >= 10_000_000) as u32;
    parity ^= (n >= 100_000_000) as u32;
    parity ^= (n >= 1_000_000_000) as u32;
    parity == 1
}

fn input_numbers() -> Vec<u32> {
    // Cover 1-digit through 9-digit numbers (inclusive) with uniform density.
    // 999_999 numbers keep the benchmark realistic without being too slow.
    (1..=999_999).collect()
}

fn palindrome_inputs() -> Vec<u32> {
    // Sample palindromes and non-palindromes across the same range.
    // Focus on the hot path: odd/even digit counts and trailing-zero rejection.
    (11..=999_999).collect()
}

fn factor_pair_inputs() -> Vec<(u32, u32, u32)> {
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
        return samples;
    }

    let base = samples.clone();
    while samples.len() < 4_096 {
        samples.extend_from_slice(&base);
    }

    samples
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
}

fn bench_largest_search_products(c: &mut Criterion) {
    let expected = Some(906_609u32);

    if should_run("largest_product_iterative") {
        c.bench_function("largest_product_iterative_100_999", |b| {
            b.iter(|| {
                let res = largest_product(black_box(100), black_box(999));
                debug_assert_eq!(res, expected);
                black_box(res)
            });
        });
    }

    if should_run("largest_product_functional") {
        c.bench_function("largest_product_functional_100_999", |b| {
            b.iter(|| {
                let res = largest_product_functional(black_box(100), black_box(999));
                debug_assert_eq!(res, expected);
                black_box(res)
            });
        });
    }
}

fn bench_smallest_search_products(c: &mut Criterion) {
    let expected = Some(121u32);

    if should_run("smallest_product_iterative") {
        c.bench_function("smallest_product_iterative_10_99", |b| {
            b.iter(|| {
                let res = smallest_product(black_box(10), black_box(99));
                debug_assert_eq!(res, expected);
                black_box(res)
            });
        });
    }

    if should_run("smallest_product_functional") {
        c.bench_function("smallest_product_functional_10_99", |b| {
            b.iter(|| {
                let res = smallest_product_functional(black_box(10), black_box(99));
                debug_assert_eq!(res, expected);
                black_box(res)
            });
        });
    }
}

fn should_run(name: &str) -> bool {
    match env::var("PAL_BENCH_FILTER") {
        Ok(filter) if filter.is_empty() => true,
        Ok(filter) => name.contains(&filter),
        Err(_) => true,
    }
}

fn bench_factor_pair_collection(c: &mut Criterion) {
    let inputs = factor_pair_inputs();
    assert!(
        !inputs.is_empty(),
        "factor pair input list should not be empty"
    );

    if should_run("collect_factor_pairs_iterative") {
        c.bench_function("collect_factor_pairs_iterative", |b| {
            b.iter(|| {
                let mut acc = 0usize;
                for &(product, min, max) in &inputs {
                    let pairs =
                        collect_factor_pairs(black_box(product), black_box(min), black_box(max));
                    acc += pairs.len();
                }
                black_box(acc)
            });
        });
    }

    if should_run("collect_factor_pairs_functional") {
        c.bench_function("collect_factor_pairs_functional", |b| {
            b.iter(|| {
                let mut acc = 0usize;
                for &(product, min, max) in &inputs {
                    let pairs = collect_factor_pairs_functional(
                        black_box(product),
                        black_box(min),
                        black_box(max),
                    );
                    acc += pairs.len();
                }
                black_box(acc)
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
    targets = bench_has_even_digits, bench_palindrome_checks, bench_largest_search_products, bench_smallest_search_products, bench_factor_pair_collection
}

criterion_main!(benches);
