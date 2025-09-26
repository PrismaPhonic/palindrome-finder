use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::time::Duration;

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

fn bench_has_even_digits(c: &mut Criterion) {
    let inputs = input_numbers();

    c.bench_function("has_even_digits_branch", |b| {
        b.iter(|| {
            let mut acc = 0u32;
            for &n in &inputs {
                acc += has_even_digits_branch(black_box(n)) as u32;
            }
            black_box(acc)
        });
    });

    c.bench_function("has_even_digits_ilog10", |b| {
        b.iter(|| {
            let mut acc = 0u32;
            for &n in &inputs {
                acc += has_even_digits_ilog10(black_box(n)) as u32;
            }
            black_box(acc)
        });
    });

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

criterion_group! {
    name = benches;
    config = Criterion::default()
        .sample_size(1000)
        .warm_up_time(Duration::from_millis(500))
        .measurement_time(Duration::from_secs(30));
    targets = bench_has_even_digits
}

criterion_main!(benches);

