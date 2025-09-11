use palprod_rust::{run_server, smallest};
use std::env;
use std::process::exit;

fn do_iters(min: u64, max: u64, iters: u64) -> Option<u64> {
    for _ in 0..iters {
        let _ = smallest(min, max);
    }
    if let Some((prod, _pairs)) = smallest(min, max) {
        Some(prod)
    } else {
        None
    }
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
    println!("{}", do_iters(min, max, iters).unwrap_or_default());
}
