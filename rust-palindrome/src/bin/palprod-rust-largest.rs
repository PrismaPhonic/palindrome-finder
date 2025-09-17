use palprod_rust::{largest, run_server};
use std::env;
use std::process::exit;

fn do_iters(min: u64, max: u64, iters: u64) -> (Option<u64>, u64) {
    let range_count = max - min + 1;
    let iters_per_range = iters / range_count;
    let remainder = iters % range_count;
    
    let mut acc: u64 = 0;
    let mut counter: u64 = 0;
    
    // Run iters_per_range times for each range
    for idx in 0..range_count {
        let current_max = max - idx;
        for _ in 0..iters_per_range {
            if let Some((prod, pairs)) = largest(min, current_max) {
                let sum_pairs: u64 = pairs.iter().copied().sum();
                acc += prod + sum_pairs + counter;
                counter += 1;
            }
        }
    }
    
    // Handle remainder as direct additional iterations
    for idx in 0..remainder {
        let current_max = max - idx;
        if let Some((prod, pairs)) = largest(min, current_max) {
            let sum_pairs: u64 = pairs.iter().copied().sum();
            acc += prod + sum_pairs + counter;
            counter += 1;
        }
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
    if args.len() != 4 {
        eprintln!(
            "usage: {} <min> <max> <iters>  |  {} --server",
            args[0], args[0]
        );
        exit(2);
    }
    let min: u64 = args[1].parse().unwrap();
    let max: u64 = args[2].parse().unwrap();
    let iters: u64 = args[3].parse::<u64>().unwrap().max(1);
    let (prod_opt, _acc) = do_iters(min, max, iters);
    println!("{}", prod_opt.unwrap_or_default());
}
