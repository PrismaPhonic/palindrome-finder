use palprod_rust::functional::largest_functional;
use palprod_rust::{run_iters_desc, run_server};
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.iter().any(|a| a == "--server") {
        run_server(|min, max, iters| run_iters_desc(min, max, iters, largest_functional));
        return;
    }
    let min: u32 = args[1].parse().unwrap();
    let max: u32 = args[2].parse().unwrap();
    let iters: u64 = args[3].parse::<u64>().unwrap().max(1);
    let (prod_opt, _acc, _ns) = run_iters_desc(min, max, iters, largest_functional);
    println!("{}", prod_opt.unwrap_or_default());
}
