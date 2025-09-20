use palprod_rust::{run_server, smallest};
use std::env;
use std::process::exit;

fn do_iters(min: u64, max: u64, iters: u64) -> (Option<u64>, u64) {
    let mut acc: u64 = 0;
    let mut counter: u64 = 0;
    let mut current_min = min;

    for _ in 0..iters {
        if let Some((prod, pairs)) = smallest(current_min, max) {
            acc += prod + counter + pairs.into_iter().sum::<u64>();
            counter += 1;
        }

        current_min = if current_min >= max {
            min
        } else {
            current_min + 1
        };
    }

    let base_prod = smallest(min, max).map(|(p, _)| p);
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
