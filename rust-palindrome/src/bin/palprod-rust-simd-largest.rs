use palprod_rust::run_server;
use palprod_rust::simd::run_iters_largest;

fn main() {
    run_server(run_iters_largest);
}
