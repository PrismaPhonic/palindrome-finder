use palprod_rust::{run_iters_asc, run_server, smallest};
use std::env;

#[inline(always)]
fn do_iters(min: u32, max: u32, iters: u64) -> (Option<u32>, u64, u64) {
    run_iters_asc(min, max, iters, |current_min, current_max| {
        smallest(current_min, current_max)
    })
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
    let (prod_opt, _acc, _ns) = do_iters(min, max, iters);
    println!("{}", prod_opt.unwrap_or_default());
}
