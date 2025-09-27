use palprod_rust::functional::do_iters_largest_functional;
use palprod_rust::run_server;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.iter().any(|a| a == "--server") {
        run_server(do_iters_largest_functional);
        return;
    }
    let min: u32 = args[1].parse().unwrap();
    let max: u32 = args[2].parse().unwrap();
    let iters: u64 = args[3].parse::<u64>().unwrap().max(1);
    let (prod_opt, _acc) = do_iters_largest_functional(min, max, iters);
    println!("{}", prod_opt.unwrap_or_default());
}
