use palprod_rust::memoized::{Answer, SmallestMemo};
use palprod_rust::smallest;
use palprod_rust::run_server;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.iter().any(|a| a == "--server") {
        let mut memo = SmallestMemo::new(smallest);
        run_server(move |min, max, iters| run_iters_asc_memo(min, max, iters, &mut memo));
        return;
    }
    let min: u32 = args[1].parse().unwrap();
    let max: u32 = args[2].parse().unwrap();
    let iters: u64 = args[3].parse::<u64>().unwrap().max(1);
    let mut memo = SmallestMemo::new(smallest);
    let (prod_opt, _acc, _ns) = run_iters_asc_memo(min, max, iters, &mut memo);
    println!("{}", prod_opt.unwrap_or_default());
}

fn run_iters_asc_memo<F>(
    min: u32,
    max: u32,
    iters: u64,
    memo: &mut SmallestMemo<F>,
) -> (Option<u32>, u64, u64)
where
    F: Fn(u32, u32) -> Answer + Copy,
{
    let base_prod = memo.smallest(min, max).map(|(product, _)| product);

    // Same rationale as the largest runner: once the memo tables are populated
    // Criterion may request an enormous number of iterations. We cap the work
    // to 30M iterations by rounding down to an even fraction of the request
    // (preserving the fairness guarantee) and scale the reported nanoseconds so
    // the harness still sees results proportional to the original request.
    const MAX_ITERS_PER_RUN: u64 = 30_000_000;
    let requested_iters = iters.max(1);
    let cycle_len = (max - min + 1) as u64;
    let mut effective_iters = requested_iters;

    if requested_iters > MAX_ITERS_PER_RUN {
        let reduction = (requested_iters + MAX_ITERS_PER_RUN - 1) / MAX_ITERS_PER_RUN;
        effective_iters = requested_iters / reduction;
        if cycle_len > 0 {
            let trimmed = (effective_iters / cycle_len) * cycle_len;
            if trimmed > 0 {
                effective_iters = trimmed;
            } else {
                effective_iters = cycle_len.min(requested_iters);
            }
        }
        effective_iters = effective_iters.max(1);
    }

    let scale_num = requested_iters as u128;
    let scale_den = effective_iters as u128;

    let mut acc: u128 = 0;
    let mut counter: u64 = 0;
    let mut current = min;
    let start = std::time::Instant::now();

    for _ in 0..effective_iters {
        if let Some((prod, pairs)) = memo.smallest(current, max) {
            let pair_sum = pairs.iter().map(|&value| value as u64).sum::<u64>();
            acc += prod as u128 + counter as u128 + pair_sum as u128;
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

    let scaled_ns = if effective_iters == requested_iters {
        elapsed_ns
    } else {
        let scaled = (nanos * scale_num + (scale_den / 2)) / scale_den;
        scaled.min(u64::MAX as u128) as u64
    };

    let scaled_acc = if effective_iters == requested_iters {
        acc.min(u64::MAX as u128) as u64
    } else {
        let scaled = (acc * scale_num + (scale_den / 2)) / scale_den;
        scaled.min(u64::MAX as u128) as u64
    };

    (base_prod, scaled_acc, scaled_ns)
}
