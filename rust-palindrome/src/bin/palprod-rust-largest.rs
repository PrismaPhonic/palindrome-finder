use palprod_rust::{largest, run_server};
use std::env;
use std::process::exit;

fn do_iters(min: u64, max: u64, iters: u64) -> (Option<u64>, u64) {
    let mut acc: u64 = 0;
    let mut counter: u64 = 0;
    let mut current_max = max;

    for _n in 0..iters {
        if let Some((prod, pairs)) = largest(min, current_max) {
            let sum_pairs: u64 = pairs.iter().copied().sum();
            acc += prod + sum_pairs + counter;
            counter += 1;
        }

        current_max = if current_max <= min { max } else { current_max - 1 };
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
