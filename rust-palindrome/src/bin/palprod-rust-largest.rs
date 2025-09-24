use palprod_rust::{largest, run_server};
use std::env;

fn do_iters(min: u32, max: u32, iters: u64) -> (Option<u32>, u64) {
    let mut acc: u64 = 0;
    let mut counter: u64 = 0;
    let mut current_max = max;

    for _ in 0..iters {
        if let Some((prod, pairs)) = largest(min, current_max) {
            acc += prod as u64 + counter + pairs.into_iter().map(|v| v as u64).sum::<u64>();
            counter += 1;
        }

        current_max = if current_max <= min {
            max
        } else {
            current_max - 1
        };
    }

    let base_prod = largest(min, max).map(|(p, _)| p);
    (base_prod, acc)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.iter().any(|a| a == "--server") {
        run_server(do_iters);
        return;
    }
    let min: u32 = args[1].parse().unwrap();
    let max: u32 = args[2].parse().unwrap();
    let iters: u64 = args[3].parse::<u64>().unwrap().max(1);
    let (prod_opt, _acc) = do_iters(min, max, iters);
    println!("{}", prod_opt.unwrap_or_default());
}
