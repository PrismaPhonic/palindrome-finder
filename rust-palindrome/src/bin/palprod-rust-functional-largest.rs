use palprod_rust::functional::run_iters_largest;
use palprod_rust::run_server;

fn main() {
    run_server(run_iters_largest);
}
