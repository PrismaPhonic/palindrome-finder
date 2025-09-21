use palprod_rust::run_server;
use palprod_rust::functional::do_iters_largest_functional;
use std::env;
use std::process::exit;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.iter().any(|a| a == "--server") {
        run_server(do_iters_largest_functional);
        return;
    }
    if args.len() != 4 {
        eprintln!(
            "usage: {} <min> <max> <iters>  |  {} --server",
            args[0], args[0]
        );
        exit(2);
    }
    let min: u32 = args[1].parse().unwrap();
    let max: u32 = args[2].parse().unwrap();
    let iters: u64 = args[3].parse::<u64>().unwrap().max(1);
    let (prod_opt, _acc) = do_iters_largest_functional(min, max, iters);
    println!("{}", prod_opt.unwrap_or_default());
}
